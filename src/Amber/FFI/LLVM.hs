{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Amber.FFI.LLVM (LlvmAsm, createLlvmAsm, emitAsm, assembleElf) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word8)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

data LlvmAsm = LlvmAsm {llvmAsmRef :: ForeignPtr (), llvmAsmBuffer :: CStringLen}

type LlvmAsmRefRaw = Ptr ()

foreign import ccall unsafe "llvmasm.h CreateLLVMAsm"
  c_createLlvmAsm :: IO LlvmAsmRefRaw

foreign import ccall unsafe "llvmasm.h &DestroyLLVMAsm"
  c_destroyLlvmAsm :: FunPtr (LlvmAsmRefRaw -> IO ())

foreign import ccall unsafe "llvmasm.h InitLLVMAsm"
  c_initLlvmAsm :: LlvmAsmRefRaw -> CString -> CSize -> CString -> CSize -> IO CBool

foreign import ccall unsafe "llvmasm.h GetLLVMAsmError"
  c_getLlvmAsmError :: LlvmAsmRefRaw -> Ptr CString -> Ptr CSize -> IO ()

foreign import ccall unsafe "llvmash.h GetAsmBuffer"
  c_getAsmBuffer :: LlvmAsmRefRaw -> Ptr CString -> Ptr CSize -> IO ()

foreign import ccall unsafe "llvmasm.h EmitBuffered"
  c_emitBuffered :: LlvmAsmRefRaw -> CSize -> IO CBool

foreign import ccall unsafe "llvmasm.h EndProgram"
  c_endProgram :: LlvmAsmRefRaw -> Ptr CString -> Ptr CSize -> IO CBool

createLlvmAsm :: ByteString -> ByteString -> IO LlvmAsm
createLlvmAsm triple chip = do
  refRaw <- c_createLlvmAsm
  ref <- newForeignPtr c_destroyLlvmAsm refRaw
  unsafeUseAsCStringLen triple $ \(triplePtr, tripleLen) -> do
    unsafeUseAsCStringLen chip $ \(chipPtr, chipLen) -> do
      success <- c_initLlvmAsm refRaw triplePtr (fromIntegral tripleLen) chipPtr (fromIntegral chipLen)
      when (success == 0) $ fail =<< getLlvmAsmError refRaw
      buffer <- readCStringLenPtr $ c_getAsmBuffer refRaw
      return $ LlvmAsm {llvmAsmRef = ref, llvmAsmBuffer = buffer}

emitAsm :: LlvmAsm -> BB.Builder -> IO ()
emitAsm asm b = do
  let (buffer, bufferSize) = llvmAsmBuffer asm
  remSize <- collect (BB.runBuilder b) (castPtr buffer) bufferSize
  let !asmLen = fromIntegral $ bufferSize - remSize
  withForeignPtr (llvmAsmRef asm) $ \refRaw -> do
    success <- c_emitBuffered refRaw asmLen
    when (success == 0) $ fail =<< getLlvmAsmError refRaw
  where
    collect :: BB.BufferWriter -> Ptr Word8 -> Int -> IO Int
    collect bw ptr remSize = do
      (len, next) <- bw ptr remSize
      let !remSize' = remSize - len
          !ptr' = ptr `plusPtr` len
      case next of
        BB.Done -> return remSize'
        BB.More requiredSize wr'
          | requiredSize < remSize' -> collect wr' ptr' remSize'
          | otherwise -> fail "Instruction buffer size has been exceeded"
        BB.Chunk _ wr' -> collect wr' ptr' remSize'

assembleElf :: LlvmAsm -> IO ByteString
assembleElf asm =
  withForeignPtr (llvmAsmRef asm) $ \refRaw ->
    B.packCStringLen
      =<< readCStringLenPtr
        ( \ptr len -> do
            success <- c_endProgram refRaw ptr len
            when (success == 0) $ fail =<< getLlvmAsmError refRaw
        )

getLlvmAsmError :: LlvmAsmRefRaw -> IO String
getLlvmAsmError !ref =
  peekCStringLen =<< readCStringLenPtr (c_getLlvmAsmError ref)

readCStringLenPtr :: (Ptr CString -> Ptr CSize -> IO ()) -> IO CStringLen
readCStringLenPtr gen =
  alloca $ \strPtr ->
    alloca $ \strLen -> do
      gen strPtr strLen
      (,) <$> peek strPtr <*> (fromIntegral <$> peek strLen)
