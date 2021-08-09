module Amber.Backend.GFX9 (assemble) where

import Amber.FFI.LLVM
import Amber.Program
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.List (intersperse)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)

assemble :: LlvmAsm -> Gfx9Program () -> IO ByteString
assemble llvm (Gfx9Program p) = do
  (_, ctx) <- runStateT p $ Gfx9Context {gfx9Instructions = Seq.empty}
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

gfx9Instruction :: ShortByteString -> [Gfx9Operand] -> Gfx9Program ()
gfx9Instruction opcode ops =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Gfx9Instruction opcode ops})

gfx9InsertLabel :: String -> Gfx9Program ()
gfx9InsertLabel l =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Gfx9InstructionLabel l})

data Gfx9Context = Gfx9Context {gfx9Instructions :: Seq Gfx9Instruction}

data Gfx9Instruction = Gfx9Instruction ShortByteString [Gfx9Operand]
  | Gfx9InstructionLabel String

newtype Gfx9Program a = Gfx9Program (StateT Gfx9Context IO a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadState Gfx9Context, MonadIO)

data Gfx9Operand = Gfx9Reg String | Gfx9Lit Int | Gfx9Label String

instance Program Gfx9Program Gfx9Operand where
  reg s = return $ Gfx9Reg s
  lit l = Gfx9Lit l
  move a b = gfx9Instruction "s_mov_b32" [a, b]

instance IntOps Gfx9Program Gfx9Operand where
  --   throwError $ "e:\n" <> prettyCallStack callStack
  add dst@(Gfx9Reg _) src0 src1 =
    gfx9Instruction "s_add_u32" [dst, src0, src1]
  add _ _ _ = fail "Invalid operands"

instance ControlFlow Gfx9Program Gfx9Operand where
  label l n = gfx9InsertLabel l >> n
  jump l = gfx9Instruction "s_branch" [Gfx9Label l]
