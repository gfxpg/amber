{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Amber.Program where

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Builder as BB
import GHC.Stack (HasCallStack)

-- "s_mov_b32 s0, 1" is represented as Instruction ["s", "mov", "b32"] [OpSgpr [0], OpLit 1]
data Instruction
  = Instruction [ShortByteString] [Operand]
  | InstructionLabel String
  deriving (Eq, Show)

data Operand
  = OpSgpr [Int]
  | OpVgpr [Int]
  | OpLit Int
  | OpLitFp Float
  | OpLabel String
  deriving (Eq, Show)

class Program m n => ControlFlow m n | m -> n, n -> m where
  label :: String -> m () -> m ()
  jump :: String -> m ()

class Monad m => Program m n | m -> n, n -> m where
  lit :: Int -> n
  reg :: HasCallStack => String -> m n
  useSRegs :: Int -> (n -> m ()) -> m ()
  move :: n -> n -> m ()

class Program m n => IntOps m n | m -> n where
  add :: HasCallStack => n -> n -> n -> m ()
