{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import System.Random
import Control.Monad.State
import Helpers

class Optimalize a where
  walk :: a -> State StdGen a
  satisfy :: a -> Bool

instance Optimalize Int where
  walk n = do
    step <- choice [-1, 1]
    return (n + step)
  satisfy n = (n - 50)^2 < 3^2

calcurateOptimal :: (Optimalize a) => Int -> State StdGen a -> State StdGen a
calcurateOptimal max_iterate v0 = do
  let as = (iterate (walk=<<) v0)
  (return.head.filter satisfy)=<<(sequence as)

optimalResult :: (Optimalize a) => a -> State StdGen a
optimalResult v0 = calcurateOptimal 100 (return v0)


evalOptimize :: IO Int
evalOptimize = do
  g <- getStdGen
  return $ evalState (optimalResult (3 :: Int)) g

main = do
  x <- evalOptimize
  print x
