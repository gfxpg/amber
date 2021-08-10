module Main where

import Amber.Backend.GFX9 (evalListing)
import Amber.Assembler
import Amber.Listing
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import Data.ByteString.Short (ShortByteString)
import GHC.Stack (HasCallStack)

lst :: (Listing m n, IntOps m n, ControlFlow m n) => m ()
lst = do
  jump "test"
  -- a <- reg "error"
  useSRegs 1 $ \reg0 ->
    useSRegs 4 $ \regAligned ->
      useSRegs 1 $ \reg1 ->
        label "test" $ testLabel reg0 reg1

testLabel :: (HasCallStack, Listing m n, IntOps m n, ControlFlow m n) => n -> n -> m ()
testLabel a b = do
  move a b
  move b $ lit 13
  add a a b
  jump "test"

main :: IO ()
main = do
  elf <- case evalListing lst of
    Right p -> assembleElf p
    Left e -> fail $ "ERROR: " <> e
  B.writeFile "out.elf" elf
