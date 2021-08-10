{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Amber.Program where

import Control.Monad.Fail (MonadFail)
import qualified Data.ByteString.Builder as BB
import GHC.Stack (HasCallStack)

class Program m n => ControlFlow m n | m -> n, n -> m where
  label :: String -> m () -> m ()
  jump :: String -> m ()

class Monad m => Program m n | m -> n, n -> m where
  lit :: Int -> n
  reg :: String -> m n

  useSRegs :: Int -> (n -> m ()) -> m ()

  move :: n -> n -> m ()

class Program m n => IntOps m n | m -> n where
  add :: HasCallStack => n -> n -> n -> m ()

class Emit a where
  emit :: a -> BB.Builder
