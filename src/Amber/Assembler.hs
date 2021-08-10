module Amber.Assembler where

import qualified Amber.FFI.LLVM as LLVM
import Amber.Listing
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Short (ShortByteString)
import Data.Int (Int32)
import Data.List (intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data AssemblerProgram = AssemblerProgram
  { pgmInstructions :: Seq Instruction,
    pgmEntryPoint :: String,
    pgmSgprCount :: Int,
    pgmVgprCount :: Int
  }

-- "s_mov_b32 s0, 1" is represented as Instruction ["s", "mov", "b32"] [OpSgpr [0], OpLit 1]
data Instruction
  = Instruction [ShortByteString] [Operand]
  | InstructionLabel String
  | InstructionGlobalLabel String
  deriving (Eq, Show)

data Operand
  = OpSgpr [Int]
  | OpVgpr [Int]
  | OpLit Int
  | OpLitFp Float
  | OpLabel String
  deriving (Eq, Show)

assembleElf :: AssemblerProgram -> IO ByteString
assembleElf pgm = do
  llvm <- LLVM.createLlvmAsm "amdgcn--amdpal" "gfx900"
  LLVM.emitAsm llvm $ asmMetadata pgm
  forM_ (pgmInstructions pgm) $ \i ->
    LLVM.emitAsm llvm $ asmInstruction i
  LLVM.assembleElf llvm

bbJoin :: BB.Builder -> [BB.Builder] -> BB.Builder
bbJoin sep = mconcat . intersperse sep

asmMetadata :: AssemblerProgram -> BB.Builder
asmMetadata pgm =
  ".amdgpu_pal_metadata\n"
    <> "amdpal.pipelines: [{.hardware_stages: "
    <> "{.cs: {.entry_point: "
    <> BB.stringUtf8 (pgmEntryPoint pgm)
    <> ", .sgpr_count: "
    <> BB.intDec (pgmSgprCount pgm)
    <> ", .vgpr_count: "
    <> BB.intDec (pgmVgprCount pgm)
    <> "}}"
    <> ", .internal_pipeline_hash: [0x1, 0x2]"
    <> ", .registers: {"
    <> driverRegisterStr
    <> "}}]\n.end_amdgpu_pal_metadata"
  where
    driverRegisterStr =
      bbJoin ", " $
        (\(k, v) -> BB.int32HexFixed k <> ": " <> BB.int32HexFixed v) <$> driverRegisters
    driverRegisters :: [(Int32, Int32)]
    driverRegisters = [(0x2c0a, rsrc1)]
    rsrc1 = 0 -- TODO

asmInstruction :: Instruction -> BB.Builder
asmInstruction (Instruction opcode ops) =
  bbJoin "_" (BB.shortByteString <$> opcode) <> " " <> bbJoin ", " (emitOp <$> ops)
  where
    emitOp (OpSgpr [r]) = "s" <> BB.intDec r
    emitOp (OpSgpr rs) = "s[" <> BB.intDec (head rs) <> ":" <> BB.intDec (last rs) <> "]"
    emitOp (OpVgpr [r]) = "v" <> BB.intDec r
    emitOp (OpVgpr rs) = "v[" <> BB.intDec (head rs) <> ":" <> BB.intDec (last rs) <> "]"
    emitOp (OpLabel l) = BB.stringUtf8 l
    emitOp (OpLit i) = BB.intDec i
    emitOp (OpLitFp f) = BB.floatDec f
asmInstruction (InstructionLabel label) =
  BB.stringUtf8 label <> ":"
asmInstruction (InstructionGlobalLabel label) =
  ".global " <> BB.stringUtf8 label <> "\n" <> BB.stringUtf8 label <> ":"
