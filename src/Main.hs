{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

import Optimalize

instance Optimalize (Int, Int) where
  walk (n, m) = do
    s1 <- step
    s2 <- step
    let (n_, m_)  = (n + s1, m + s2)
    let n__ = if 0 <= n_ then n_ else 0
    let m__ = if 0 <= m_ then m_ else 0
    return $ (n__, m__)
  cost (n, m) = ((fromIntegral n)^2 + (fromIntegral m)^2 - 100)^2

main = do
  a <- evalOptimize 100 $ (0 :: Int,0 :: Int)
  let (x, y) = head a
  print $ x^2 + y^2
