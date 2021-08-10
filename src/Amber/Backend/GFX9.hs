module Amber.Backend.GFX9 (assemble) where

import Amber.FFI.LLVM
import Amber.Program
import Control.Monad.State
import Data.Bit (Bit (..), Vector, listBits)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.List (intersperse)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as BV
import Debug.Trace (trace)
import GHC.Stack (HasCallStack)

assemble :: LlvmAsm -> Gfx9Program () -> IO ByteString
assemble llvm (Gfx9Program p) = do
  (_, ctx) <-
    runStateT p $
      Gfx9Context
        { gfx9Instructions = Seq.empty,
          gfx9SRegUsage = BV.replicate 106 (Bit False)
        }
  forM_ (gfx9Instructions ctx) $ \i ->
    emitAsm llvm $ emitInstruction i
  assembleElf llvm

emitInstruction :: Gfx9Instruction -> BB.Builder
emitInstruction (Gfx9Instruction opcode ops) =
  BB.shortByteString opcode <> " " <> mconcat (intersperse ", " (emitOp <$> ops))
  where
    emitOp (Gfx9Reg r) = BB.stringUtf8 r
    emitOp (Gfx9Lit l) = BB.intDec l
    emitOp (Gfx9Label l) = BB.stringUtf8 l
emitInstruction (Gfx9InstructionLabel label) =
  BB.stringUtf8 label <> ":"

data Gfx9Context = Gfx9Context
  { gfx9Instructions :: Seq Gfx9Instruction,
    gfx9SRegUsage :: Vector Bit
  }
  deriving (Show)

data Gfx9Instruction
  = Gfx9Instruction ShortByteString [Gfx9Operand]
  | Gfx9InstructionLabel String
  deriving (Show)

data Gfx9Operand = Gfx9Reg String | Gfx9Lit Int | Gfx9Label String
  deriving (Show)

newtype Gfx9Program a = Gfx9Program (StateT Gfx9Context IO a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadState Gfx9Context, MonadIO)

gfx9Instruction :: ShortByteString -> [Gfx9Operand] -> Gfx9Program ()
gfx9Instruction opcode ops =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Gfx9Instruction opcode ops})

gfx9InsertLabel :: String -> Gfx9Program ()
gfx9InsertLabel l =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Gfx9InstructionLabel l})

instance Program Gfx9Program Gfx9Operand where
  reg s = return $ Gfx9Reg s
  lit l = Gfx9Lit l
  useSRegs cnt f = do
    idx <- gets $ firstFreeReg . listBits . gfx9SRegUsage
    let regIdxs = [idx .. idx + cnt - 1]
    modify $ \s -> s {gfx9SRegUsage = gfx9SRegUsage s BV.// ((,Bit True) <$> regIdxs)}
    state $ \s -> ((), trace ("Regs: " <> show (gfx9SRegUsage s)) s)
    f $ Gfx9Reg $ "s[" <> show idx <> (if cnt > 1 then ":" <> show (idx + cnt - 1) else "") <> "]"
    modify $ \s -> s {gfx9SRegUsage = gfx9SRegUsage s BV.// ((,Bit False) <$> regIdxs)}
    where
      firstFreeReg [] = 0
      firstFreeReg [r] = alignReg $ r + 1
      firstFreeReg (r1 : r2 : rest)
        | r2 - alignReg (r1 + 1) >= cnt = alignReg (r1 + 1)
        | otherwise = firstFreeReg (r2 : rest)
      alignReg r
        | r `rem` regAlignment == 0 = r
        | otherwise = r + (regAlignment - r `rem` regAlignment)
      regAlignment = min 4 cnt -- quad alignment is sufficient for cnt >= 4

  move a b = gfx9Instruction "s_mov_b32" [a, b]

instance IntOps Gfx9Program Gfx9Operand where
  --   throwError $ "e:\n" <> prettyCallStack callStack
  add dst@(Gfx9Reg _) src0 src1 =
    gfx9Instruction "s_add_u32" [dst, src0, src1]
  add _ _ _ = fail "Invalid operands"

instance ControlFlow Gfx9Program Gfx9Operand where
  label l n = gfx9InsertLabel l >> n
  jump l = gfx9Instruction "s_branch" [Gfx9Label l]
