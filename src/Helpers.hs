module Helpers where

import System.Random
import Control.Monad.State

choice :: [a] -> State StdGen a
choice as = do
  g <- get
  let (index, g') = randomR (0, (length as)-1) g
  put g'
  return $ as !! index
