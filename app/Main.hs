module Main where

import Amber.FFI.LLVM
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import Data.ByteString.Short (ShortByteString)

instruction :: ShortByteString -> Int -> BB.Builder
instruction reg o = mconcat ["s_mov_b32", " "] <> BB.shortByteString reg <> ", " <> BB.intDec o

main :: IO ()
main = do
  llvm <- createLlvmAsm "amdgcn--amdpal" "gfx900"
  emitAsm llvm $
    ".amdgpu_pal_metadata\n"
      <> "amdpal.pipelines: [{.hardware_stages: {.cs: {.entry_point: "
      <> "cs_entry, .sgpr_count: 1, .vgpr_count: 1}}, "
      <> ".internal_pipeline_hash: [0x1, 0x2], .registers: {0x2c07: 0}}]\n"
      <> ".end_amdgpu_pal_metadata"
  forM_ [1..10000] $ \i ->
    emitAsm llvm $ instruction "s32" i
  elf <- assembleElf llvm
  B.writeFile "out.elf" elf
