module Main where

import Amber.FFI

main :: IO ()
main = do
  r <- getLlvmAsmRef
  return ()
