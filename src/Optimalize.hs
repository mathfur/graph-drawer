{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  
---- Copyright   :  
---- License     :  
----
---- Maintainer  :
---- Stability   : 
---- Portability :
----
----
-------------------------------------------------------------------------------

module Optimalize where

import System.Random
import Control.Monad.State
import Debug.Trace

type TprState = State (Float, StdGen)

class Optimalize a where
  walk :: a -> TprState a
  cost :: a -> Float

--------------------------------------------------------------
-- | Optimalize instances

instance (Optimalize a) => Optimalize [a] where
  walk as = mapM walk as
  cost as = foldr (*) 1 $ map cost as

instance Optimalize Int where
  walk n = do
    step <- choice [-1, 1]
    let n_ = n + step
    return $ if 0 <= n_ then n_ else 0
  cost n = ((fromIntegral n)^2 - 300)^2

--------------------------------------------------------------
-- | functions for Optimalize

calcurateOptimal :: (Optimalize a) => Int -> a -> TprState [a]
calcurateOptimal len a0
  | len == 1 = return [a0]
  | otherwise = do
    as <- calcurateOptimal (len-1) a0
    a  <- walkAccordingToTemperature $ head as
    return $ a:as
      where
        walkAccordingToTemperature :: (Optimalize a) => a -> TprState a
        walkAccordingToTemperature a0 = do
          let v0 = return a0
          let v1 = walk a0
          a1 <- v1
          downTemperature
          movable <- highTempIsMovable
          if ((cost a1 <= cost a0) || movable) then v1 else v0
            where
              downTemperature :: TprState ()
              downTemperature = do
                (t, g) <- get
                put (0.95*t, g)
              highTempIsMovable :: TprState Bool
              highTempIsMovable = do
                (t, g) <- get
                let (threshold, g') = randomR (0.0 :: Float, 1.0) g
                put (t, g')
                return (threshold < f t)
                where
                  f :: Float -> Float
                  f t = 1 - (1/(1 + t))

evalOptimize :: (Optimalize a) => Int -> a -> IO [a]
evalOptimize len init_val = do
  g <- getStdGen
  return $ evalState (calcurateOptimal len init_val) (1, g)

choice :: (Optimalize a) => [a] -> TprState a
choice as = do
  (t, g) <- get
  let (index, g') = randomR (0, (length as)-1) g
  put (t, g')
  return $ as !! index
