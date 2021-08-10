{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Amber.Listing where

import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.Sequence (Seq)
import GHC.Stack (HasCallStack)

class Monad m => Listing m n | m -> n, n -> m where
  lit :: Int -> n
  reg :: HasCallStack => String -> m n
  useSRegs :: Int -> (n -> m ()) -> m ()
  move :: n -> n -> m ()

class Listing m n => ControlFlow m n | m -> n, n -> m where
  label :: String -> m () -> m ()
  jump :: String -> m ()

class Listing m n => IntOps m n | m -> n where
  add :: HasCallStack => n -> n -> n -> m ()
