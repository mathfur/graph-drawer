{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import System.Random
import Control.Monad.State
import Helpers

class Optimalize a where
  walk :: a -> State TemperaturedStdGen a
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

-- ランダムウォーク => 温度からコスト測る, 必要ならランダムウォーク前に戻す -> 温度少し下げる
o_walk :: (Optimalize a) => a -> State TemperaturedStdGen a
o_walk a0 = do
  let v0 = return a0
  let v1 = walk a0
  a1 <- v1
  t <- temperature
  downTemperature
  if ((cost a1 <= cost a0) || (prob_p t)) then v1 else v0
    where
      prob :: Float -> Float
      prob t = 1 -  1/1 + t
      prob_p :: Float -> Bool
      prob_p t = (0.01 < prob t)

calcurateOptimal :: (Optimalize a) => Int -> a -> State TemperaturedStdGen [a]
calcurateOptimal len a0
  | len == 1 = return [a0]
  | otherwise = do
    as <- calcurateOptimal (len-1) a0
    a  <- walk $ head as
    return $ a:as

optimalResult :: (Optimalize a) => a -> State TemperaturedStdGen [a]
optimalResult a0 = calcurateOptimal 20 a0

evalOptimize :: IO [Int]
evalOptimize = do
  g <- getStdGen
  return $ evalState (optimalResult (0 :: Int)) (0, g)

main = do
  x <- evalOptimize
  print x
