{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

import Optimalize
import Control.Applicative
import Data.Ix
import Data.List
import Debug.Trace
import System.FilePath
import System.Directory

type Point = (Int, Int)  -- => Optimalize
type Line = (Point, Point) -- => 
newtype Line_ = Line_ (Point, Point)
newtype Chain = Chain [Point] deriving Show

--------------------------
instance Optimalize Int where
  walk n = do
    s <- step
    return (n+s)
  cost n = if 0<=n then 0 else ((fromIntegral n)*100)^2

instance InterCostable Int Int where
  interCost n m = 0.0

--------------------------
instance InterCostable Point Point where
  interCost p1 p2 = 0

instance Optimalize Line_ where
  walk (Line_ (p1, p2)) = Line_ <$> ((,) <$> (walk p1) <*> (walk p2))
  cost (Line_ (p1, p2)) = fromIntegral $ if (distance p1 p2 < 10) then 100 else 0
    where
      distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ ((x2 - x1)^2 + (y2 - y1)^2)

--------------------------
instance InterCostable Line Line where
  interCost ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    | (x1, y1) == (x2, y2) = 100   -- not line
    | (x3, y3) == (x4, y4) = 100   -- not line
    | (b == 0) = 100               -- parallel
    | (0 <= a/b && a/b <= 1) = 100 -- intersect
    | otherwise = 0
      where
        a = fromIntegral $ (y1 - y2)*(x1 - x3) + (x2 - x1)*(y1 - y3)
        b = fromIntegral $ (x4 - x3)*(y1 - y2) - (x1 - x2)*(y4 - y3)

--------------------------

instance Optimalize Chain where
  walk (Chain ps) = Chain <$> mapM walk ps
  cost (Chain ps) = (sum $ map cost ps) + fromPointsToCost ps
    where
      fromPointsToCost :: [Point] -> Float
      fromPointsToCost ps = sum $ map (\as -> (interCost (as !! 0) (as !! 1))) $ filter (\ls -> length ls == 2) $ subsequences $ adjacents ps

adjacents :: [a] -> [(a, a)]
adjacents as
  | len == 0 = []
  | len == 1 = []
  | otherwise = map (\i -> (as !! i, as !! (i+1))) $ range (0, len - 2)
  where
    len = length as

class SVGable a where
  toInnerSVG :: a -> String

showSVG :: (SVGable a) => a -> String
showSVG x = "<html><head><body><svg xmlns='http://www.w3.org/2000/svg' version='1.1'>" ++(toInnerSVG x)++ "</svg></body></html>"

instance SVGable Line where
  toInnerSVG ((x1, y1), (x2, y2)) = "<line x1='" ++(show x1)++ "' y1='" ++(show y1)++ "' x2='" ++(show x2)++ "' y2='" ++(show y2)++ "' style='stroke:rgb(255, 0, 0);stroke-width:2'/>"

instance (SVGable a) => SVGable [a] where
  toInnerSVG as = intercalate "\n" $ map toInnerSVG as

fromChainToLines :: Chain -> [Line]
fromChainToLines (Chain ps) = adjacents ps

main = do
  a <- evalOptimize 10000 $ Chain $ take 20 $ repeat (0,0)
  home <- getHomeDirectory
  writeFile (home </> "src/tmp/tmp.html") $ showSVG $ fromChainToLines $ head a
  print a
