module Helpers where

import System.Random
import Control.Monad.State

type TemperaturedStdGen = (Float, StdGen)

choice :: [a] -> State TemperaturedStdGen a
choice as = do
  (t, g) <- get
  let (index, g') = randomR (0, (length as)-1) g
  put (t, g')
  return $ as !! index

temperature :: State TemperaturedStdGen Float
temperature = get>>=(return.fst)

downTemperature :: State TemperaturedStdGen ()
downTemperature = do
  (t, g) <- get
  put (0.95*t, g)

prob_p :: State TemperaturedStdGen Bool
prob_p = do
  (t, g) <- get
  let (threshold, g') = randomR (0.0 :: Float, 1.0) g
  return (threshold < prob t)
  where
    prob :: Float -> Float
    prob t = 1 - (1/(1 + t))
