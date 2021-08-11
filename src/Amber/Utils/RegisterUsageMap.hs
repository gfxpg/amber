module Amber.Utils.RegisterUsageMap (RegisterUsageMap, nRegMap, numUsed, findUnused, markUsage) where

import Control.Monad.State (MonadState)
import Data.Bit (Bit (..), Vector, listBits)
import qualified Data.Vector.Unboxed as BV

data RegisterUsageMap = RegisterUsageMap (Vector Bit) Int -- max number of registers used
  deriving (Show)

nRegMap :: Int -> RegisterUsageMap
nRegMap n = RegisterUsageMap (BV.replicate n (Bit False)) 0

numUsed :: RegisterUsageMap -> Int
numUsed (RegisterUsageMap _ used) = used

findUnused :: Int -> Int -> RegisterUsageMap -> Maybe [Int]
findUnused cnt alignment (RegisterUsageMap bitmap _) =
  case firstReg + cnt - 1 of
    lastReg | lastReg < BV.length bitmap -> Just [firstReg .. lastReg]
    _ -> Nothing
  where
    firstReg = firstFreeReg $ listBits bitmap
    firstFreeReg [] = 0
    firstFreeReg [r] = alignReg $ r + 1
    firstFreeReg (r1 : r2 : rest)
      | r2 - alignReg (r1 + 1) >= cnt = alignReg (r1 + 1)
      | otherwise = firstFreeReg (r2 : rest)
    alignReg r
      | r `rem` alignment == 0 = r
      | otherwise = r + (alignment - r `rem` alignment)

markUsage :: Bool -> [Int] -> RegisterUsageMap -> RegisterUsageMap
markUsage mark regs (RegisterUsageMap bitmap used) =
  RegisterUsageMap (bitmap BV.// ((,Bit mark) <$> regs)) (max used newlyUsed)
  where
    newlyUsed = if mark then maximum regs + 1 else 0
