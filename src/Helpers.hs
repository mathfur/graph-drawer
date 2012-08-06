module Helpers where

import System.Random
import Control.Monad.State

newtype TemperaturedStdGen = TemperaturedStdGen { runTempr :: (Float, StdGen) }

choice :: [a] -> State TemperaturedStdGen a
choice as = do
  (t, g) <- (get>>=(return.runTempr))
  let (index, g') = randomR (0, (length as)-1) g
  put $ TemperaturedStdGen (t, g')
  return $ as !! index

temperature :: State TemperaturedStdGen Float
temperature = get>>=(return.fst.runTempr)

downTemperature :: State TemperaturedStdGen ()
downTemperature = do
  (t, g) <- (get>>=(return.runTempr))
  put $ TemperaturedStdGen (9.5*t, g)
