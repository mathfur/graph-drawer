import System.Random
import Control.Monad.State
import Helpers

walk :: Int -> State StdGen Int
walk n = do
  step <- choice [-1, 1]
  return (n + step)

satisfy :: Int -> Bool
satisfy n = (n - 50)^2 < 3^2

calcurateOptimal :: (a -> State StdGen a) -> (a -> Bool) -> Int -> State StdGen a -> State StdGen a
calcurateOptimal f p max_iterate v0 = do
  let as = (iterate (f=<<) v0)
  (return.head.filter p)=<<(sequence as)

executeWalk :: StdGen -> Int -> Int
executeWalk g n = fst $ runState (walk n) g

main = do
  g <- getStdGen
  print $ fst $ runState (calcurateOptimal walk satisfy 100 (return 3)) g
