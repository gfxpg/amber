module Main where

import Amber.Backend.GFX9 (evalProgram)
import Amber.ElfAssembler
import Amber.Program
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import Data.ByteString.Short (ShortByteString)
import GHC.Stack (HasCallStack)

pgm :: (Program m n, IntOps m n, ControlFlow m n) => m ()
pgm = do
  jump "test"
  a <- reg "error"
  useSRegs 1 $ \reg0 ->
    useSRegs 4 $ \regAligned ->
      useSRegs 1 $ \reg1 ->
        label "test" $ testLabel reg0 reg1

testLabel :: (HasCallStack, Program m n, IntOps m n, ControlFlow m n) => n -> n -> m ()
testLabel a b = do
  move a b
  move b $ lit 13
  add a a b
  jump "test"

main :: IO ()
main = do
  elf <- case evalProgram pgm of
    Right p -> assembleElf p
    Left e -> fail $ "ERROR: " <> e
  B.writeFile "out.elf" elf
