import System.Random
import Control.Monad.State
import Helpers

walk :: Int -> State StdGen Int
walk n = do
  step <- choice [-1, 1]
  return (n + step)

f :: StdGen -> Int
f = fst . runState (walk 3)

main = do
  g <- getStdGen
  print $ f g
