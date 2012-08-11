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
import Control.Applicative

type TprState = State (Float, StdGen)

class Optimalize a where
  walk :: a -> TprState a
  cost :: a -> Float

class InterCostable a b where
  interCost :: a -> b -> Float

--------------------------------------------------------------
-- | Optimalize instances

instance (Optimalize a) => Optimalize [a] where
  walk as = mapM walk as
  cost as = foldr (*) 1 $ map cost as

instance (Optimalize a, Optimalize b, InterCostable a b) =>  Optimalize (a, b) where
  walk (n, m) = ((,) <$> (walk n) <*> (walk m))
  cost (n, m) = (cost n) + (cost m) + (interCost n m)

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
          let v1 = walk a0
          a1 <- v1
          downTemperature
          movable <- highTempIsMovable
          if ((cost a1 <= cost a0) || movable) then (return a1) else (return a0)
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

choice :: [a] -> TprState a
choice as = do
  (t, g) <- get
  let (index, g') = randomR (0, (length as)-1) g
  put (t, g')
  return $ as !! index

step :: TprState Int
step = choice [-1, 1]
