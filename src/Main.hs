{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import System.Random
import Control.Monad.State
import Helpers

class Optimalize a where
  walk :: a -> State StdGen a
  cost :: a -> Float
  satisfy :: a -> Bool

instance (Optimalize a) => Optimalize [a] where
  walk as = mapM walk as
  cost as = foldr (*) 1 $ map cost as
  satisfy as = cost as < 1

instance Optimalize Int where
  walk n = do
    step <- choice [-1, 1]
    return (n + step)
  cost n = (fromIntegral n - 50)^2
  satisfy n = cost n < 1

o_walk :: (Optimalize a) => State StdGen a -> State StdGen a
o_walk v0 = do
  let v1 = walk=<<v0
  a0 <- v0
  a1 <- v1
  if ((cost a1 <= cost a0) || (prob_p 0.5)) then v1 else v0
    where
      prob :: Float -> Float
      prob t = 1 -  1/1 + t
      prob_p :: Float -> Bool
      prob_p t = (0.01 < prob t)

calcurateOptimal :: (Optimalize a) => Int -> State StdGen a -> State StdGen a
calcurateOptimal max_iterate v0 = do
  let as = (iterate (walk=<<) v0)
  (return.head.filter satisfy)=<<(sequence as)

optimalResult :: (Optimalize a) => a -> State StdGen a
optimalResult v0 = calcurateOptimal 1 (return v0)


evalOptimize :: IO [Int]
evalOptimize = do
  g <- getStdGen
  return $ evalState (optimalResult ([3, 3] :: [Int])) g

main = do
  x <- evalOptimize
  print x
