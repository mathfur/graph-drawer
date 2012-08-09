{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import System.Random
import Control.Monad.State
import Helpers
import Debug.Trace

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
    let n_ = n + step
    return $ if 0 <= n_ then n_ else 0
  -- ランダムウォーク => 温度からコスト測る, 必要ならランダムウォーク前に戻す -> 温度少し下げる
  cost n = ((fromIntegral n)^2 - 300)^2
  satisfy n = cost n < 2

walkAccordingToTemperature :: (Optimalize a) => a -> State TemperaturedStdGen a
walkAccordingToTemperature a0 = do
  let v0 = return a0
  let v1 = walk a0
  a1 <- v1
  downTemperature
  b <- prob_p
  if ((cost a1 <= cost a0) || b) then v1 else v0

calcurateOptimal :: (Optimalize a) => Int -> a -> State TemperaturedStdGen [a]
calcurateOptimal len a0
  | len == 1 = return [a0]
  | otherwise = do
    as <- calcurateOptimal (len-1) a0
    a  <- walkAccordingToTemperature $ head as
    return $ a:as

optimalResult :: (Optimalize a) => a -> State TemperaturedStdGen [a]
optimalResult a0 = calcurateOptimal 1000 a0

evalOptimize :: IO [Int]
evalOptimize = do
  g <- getStdGen
  return $ evalState (optimalResult (0 :: Int)) (1, g)

main = do
  x <- evalOptimize
  print $ head x
