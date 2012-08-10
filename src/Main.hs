{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Optimalize

main = do
  x <- evalOptimize 100 (0 :: Int)
  print $ head x
