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
  put (9.5*t, g)
