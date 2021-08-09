module Amber.Backend.GFX9 (assemble) where

import Amber.FFI.LLVM
import Amber.Program
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.List (intersperse)
import GHC.Stack (HasCallStack)

assemble :: LlvmAsm -> Gfx9Program () -> IO ByteString
assemble llvm (Gfx9Program p) = do
  program <- runStateT p $ Gfx9Context {gfx9Llvm = llvm}
  assembleElf llvm

data Gfx9Context = Gfx9Context {gfx9Llvm :: LlvmAsm}

newtype Gfx9Program a = Gfx9Program (StateT Gfx9Context IO a)
  deriving (Functor, Applicative, Monad, MonadFail, MonadState Gfx9Context, MonadIO)

data Gfx9Operand = Gfx9Reg String | Gfx9Lit Int

data Gfx9Node = Gfx9Operand Gfx9Operand

gfx9Emit :: BB.Builder -> Gfx9Program ()
gfx9Emit bb = do
  llvm <- gets gfx9Llvm
  liftIO $ emitAsm llvm bb

instance IntOps Gfx9Program Gfx9Node where
  --   throwError $ "e:\n" <> prettyCallStack callStack
  add (Gfx9Operand dst) (Gfx9Operand src0) (Gfx9Operand src1) =
    gfx9Emit $ "s_add_u32 " <> emit dst <> ", " <> emit src0 <> ", " <> emit src1
  add _ _ _ = fail "Invalid operands"

instance ControlFlow Gfx9Program Gfx9Node where
  label l n = gfx9Emit (BB.string8 l <> ":") >> n
  jump l = gfx9Emit ("s_branch " <> BB.string8 l)

instance Emit Gfx9Operand where
  emit (Gfx9Reg r) = BB.string8 r
  emit (Gfx9Lit l) = BB.intDec l

instance Emit Gfx9Node where
  emit (Gfx9Operand o) = emit o

instance Program Gfx9Program Gfx9Node where
  reg s = return $ Gfx9Operand $ Gfx9Reg s
  move a b = gfx9Emit $ "s_mov_b32 " <> emit a <> ", " <> emit b
  lit l = Gfx9Operand $ Gfx9Lit l
