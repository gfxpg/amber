{-# LANGUAGE ForeignFunctionInterface #-}

module Amber.FFI where

import Foreign.ForeignPtr
import Foreign.Ptr

type LLVMAsmRef = ForeignPtr ()

type LLVMAsmRefRaw = Ptr ()

foreign import ccall "llvmasm.h CreateLLVMAsm"
  createLlvmAsm :: IO LLVMAsmRefRaw

foreign import ccall "llvmasm.h &DestroyLLVMAsm"
  destroyLlvmAsm :: FunPtr (LLVMAsmRefRaw -> IO ())

getLlvmAsmRef :: IO LLVMAsmRef
getLlvmAsmRef = do
  refRaw <- createLlvmAsm
  newForeignPtr destroyLlvmAsm refRaw
