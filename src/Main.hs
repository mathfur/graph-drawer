{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Optimalize

instance InterCostable Int Int where
  interCost n m = ((fromIntegral n)^2 + (fromIntegral m)^2 - 100)^2

instance Optimalize Int where
  walk n = do
    s <- step
    return (n+s)
  cost n = if 0<=n then 0 else ((fromIntegral n)*100)^2

main = do
  a <- evalOptimize 100 (0 :: Int,0 :: Int)
  let (x, y) = head a
  print (x, y)
  print $ x^2 + y^2
