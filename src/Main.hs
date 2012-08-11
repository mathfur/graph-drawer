{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Optimalize

newtype Point = Point (Int, Int) deriving Show

instance Optimalize Point where
  walk (Point (n, m)) = do
    n_ <- walk n
    m_ <- walk m
    return $ Point (n_, m_)
  cost (Point (n, m)) = (cost n) + (cost m) + ((fromIntegral n)^2 + (fromIntegral m)^2 - 100)^2

instance Optimalize Int where
  walk n = do
    s <- step
    return (n+s)
  cost n = if 0<=n then 0 else ((fromIntegral n)*100)^2

main = do
  a <- evalOptimize 100 $ Point (0,0)
  let (Point (x, y)) = head a
  print (x, y)
  print $ x^2 + y^2
