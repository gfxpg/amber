module Main where

import qualified Amber.Backend.GFX9 as GFX9
import Amber.FFI.LLVM
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
  llvm <- createLlvmAsm "amdgcn--amdpal" "gfx900"
  emitAsm llvm $
    ".amdgpu_pal_metadata\n"
      <> "amdpal.pipelines: [{.hardware_stages: {.cs: {.entry_point: "
      <> "cs_entry, .sgpr_count: 1, .vgpr_count: 1}}, "
      <> ".internal_pipeline_hash: [0x1, 0x2], .registers: {0x2c07: 0}}]\n"
      <> ".end_amdgpu_pal_metadata"
  elf <- GFX9.assemble llvm pgm
  B.writeFile "out.elf" elf
