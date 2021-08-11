module Amber.Backend.GFX9 (evalListing) where

import Amber.Assembler
import Amber.FFI.LLVM
import Amber.Listing
import qualified Amber.Utils.RegisterUsageMap as RegUsage
import Control.Monad.Except (MonadError (..))
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.List (intersperse)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

globalShaderEntryLabel :: String
globalShaderEntryLabel = "amber_sh_entry"

evalListing :: Gfx9Listing () -> Either String AssemblerProgram
evalListing (Gfx9Listing p) = program . (\s -> trace (show s) s) . snd <$> runStateT p initCtx
  where
    initCtx =
      Gfx9Context
        { gfx9Instructions = Seq.singleton (InstructionGlobalLabel globalShaderEntryLabel),
          gfx9SRegUsage = RegUsage.nRegMap 106
        }
    program ctx =
      AssemblerProgram
        { pgmInstructions = gfx9Instructions ctx,
          pgmEntryPoint = globalShaderEntryLabel,
          pgmSgprCount = RegUsage.numUsed (gfx9SRegUsage ctx),
          pgmVgprCount = 0 -- TODO
        }

data Gfx9Context = Gfx9Context
  { gfx9Instructions :: Seq Instruction,
    gfx9SRegUsage :: RegUsage.RegisterUsageMap
  }
  deriving (Show)

newtype Gfx9Listing a = Gfx9Listing (StateT Gfx9Context (Either String) a)
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Gfx9Context)

gfx9Instruction :: [ShortByteString] -> [Operand] -> Gfx9Listing ()
gfx9Instruction opcode ops =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> Instruction opcode ops})

gfx9InsertLabel :: String -> Gfx9Listing ()
gfx9InsertLabel l =
  modify (\s -> s {gfx9Instructions = gfx9Instructions s |> InstructionLabel l})

instance Listing Gfx9Listing Operand where
  reg s = throwError $ "reg: not supported\n" <> prettyCallStack callStack
  lit l = OpLit l

  useSRegs cnt f = do
    let regAlignment = min 4 cnt -- quad alignment is sufficient for cnt >= 4
    regs <- gets $ RegUsage.findUnused cnt regAlignment . gfx9SRegUsage
    case regs of
      Nothing -> throwError $ "cannot allocate " <> show cnt <> " SGPRs\n" <> prettyCallStack callStack
      Just regs -> do
        modify $ \s -> s {gfx9SRegUsage = RegUsage.markUsage True regs (gfx9SRegUsage s)}
        f $ OpSgpr regs
        modify $ \s -> s {gfx9SRegUsage = RegUsage.markUsage False regs (gfx9SRegUsage s)}

  move a b = gfx9Instruction ["s", "mov", "b32"] [a, b]

instance IntOps Gfx9Listing Operand where
  add dst@(OpSgpr _) src0 src1 =
    gfx9Instruction ["s", "add", "u32"] [dst, src0, src1]
  add _ _ _ = throwError "add: invalid operands"

instance ControlFlow Gfx9Listing Operand where
  label l n = gfx9InsertLabel l >> n
  jump l = gfx9Instruction ["s", "branch"] [OpLabel l]
