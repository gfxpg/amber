module Amber.ElfAssembler where

import qualified Amber.FFI.LLVM as LLVM
import Amber.Program
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.List (intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

assembleElf :: Seq Instruction -> IO ByteString
assembleElf instructions = do
  llvm <- LLVM.createLlvmAsm "amdgcn--amdpal" "gfx900"
  LLVM.emitAsm llvm $
    ".amdgpu_pal_metadata\n"
      <> "amdpal.pipelines: [{.hardware_stages: {.cs: {.entry_point: "
      <> "cs_entry, .sgpr_count: 1, .vgpr_count: 1}}, "
      <> ".internal_pipeline_hash: [0x1, 0x2], .registers: {0x2c07: 0}}]\n"
      <> ".end_amdgpu_pal_metadata"
  forM_ instructions $ \i ->
    LLVM.emitAsm llvm $ emitInstruction i
  LLVM.assembleElf llvm

emitJoin :: BB.Builder -> [BB.Builder] -> BB.Builder
emitJoin sep = mconcat . intersperse sep

emitInstruction :: Instruction -> BB.Builder
emitInstruction (Instruction opcode ops) =
  emitJoin "_" (BB.shortByteString <$> opcode) <> " " <> emitJoin ", " (emitOp <$> ops)
  where
    emitOp (OpSgpr [r]) = "s" <> BB.intDec r
    emitOp (OpSgpr rs) = "s[" <> BB.intDec (head rs) <> ":" <> BB.intDec (last rs) <> "]"
    emitOp (OpVgpr [r]) = "v" <> BB.intDec r
    emitOp (OpVgpr rs) = "v[" <> BB.intDec (head rs) <> ":" <> BB.intDec (last rs) <> "]"
    emitOp (OpLabel l) = BB.stringUtf8 l
    emitOp (OpLit i) = BB.intDec i
    emitOp (OpLitFp f) = BB.floatDec f
emitInstruction (InstructionLabel label) =
  BB.stringUtf8 label <> ":"
