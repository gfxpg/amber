module Amber.Backend.GFX9 (evalProgram) where

import Amber.FFI.LLVM
import Amber.Program
import Control.Monad.Except (MonadError (..))
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
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

evalProgram :: Gfx9Program () -> Either String (Seq Instruction)
evalProgram (Gfx9Program p) = gfx9Instructions . snd <$> runStateT p initCtx
  where
    initCtx =
      Gfx9Context
        { gfx9Instructions = Seq.empty,
          gfx9SRegUsage = BV.replicate 106 (Bit False)
        }

data Gfx9Context = Gfx9Context
  { gfx9Instructions :: Seq Instruction,
    gfx9SRegUsage :: Vector Bit
  }
  deriving (Show)

newtype Gfx9Program a = Gfx9Program (StateT Gfx9Context (Either String) a)
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Gfx9Context)

gfx9Instruction :: [ShortByteString] -> [Operand] -> Gfx9Program ()
gfx9Instruction opcode ops =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Instruction opcode ops})

gfx9InsertLabel :: String -> Gfx9Program ()
gfx9InsertLabel l =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> InstructionLabel l})

instance Program Gfx9Program Operand where
  reg s = throwError $ "reg: not supported\n" <> prettyCallStack callStack
  lit l = OpLit l
  useSRegs cnt f = do
    idx <- gets $ firstFreeReg . listBits . gfx9SRegUsage
    let regIdxs = [idx .. idx + cnt - 1]
    modify $ \s -> s {gfx9SRegUsage = gfx9SRegUsage s BV.// ((,Bit True) <$> regIdxs)}
    state $ \s -> ((), trace ("Regs: " <> show (gfx9SRegUsage s)) s)
    f (OpSgpr regIdxs)
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

  move a b = gfx9Instruction ["s", "mov", "b32"] [a, b]

instance IntOps Gfx9Program Operand where
  add dst@(OpSgpr _) src0 src1 =
    gfx9Instruction ["s", "add", "u32"] [dst, src0, src1]
  add _ _ _ = throwError "add: invalid operands"

instance ControlFlow Gfx9Program Operand where
  label l n = gfx9InsertLabel l >> n
  jump l = gfx9Instruction ["s", "branch"] [OpLabel l]
