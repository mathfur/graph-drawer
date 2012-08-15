{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

import Optimalize
import Control.Applicative
import Data.Ix
import Data.List
import Debug.Trace
import System.FilePath
import System.Directory

type Point = (Int, Int)  -- => Optimalize
newtype Line = Line (Point, Point)
newtype Chain = Chain [Point] deriving Show
newtype Chains = Chains [Chain] deriving Show

fromLinesToChain :: [Line] -> Chain
fromLinesToChain ls
  | length ls == 0 = Chain []
  | otherwise      = Chain $ (map (\(Line (p1, p2)) -> p1) ls) ++ [((\(Line (p1, p2)) -> p2) $ last ls)]

fromChainToLines :: Chain -> [Line]
fromChainToLines (Chain ps)
  | length ps == 0 = []
  | otherwise      = map Line $ zip (init ps) (tail ps)

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

instance Optimalize Line where
  walk (Line (p1, p2)) = Line <$> ((,) <$> (walk p1) <*> (walk p2))
  cost (Line (p1, p2)) = fromIntegral $ if (distance p1 p2 < 10) then 100 else 0
    where
      distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ ((x2 - x1)^2 + (y2 - y1)^2)

--------------------------
instance InterCostable Line Line where
  interCost (Line ((x1, y1), (x2, y2))) (Line ((x3, y3), (x4, y4)))
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
      fromPointsToCost ps = sum $ map (\as -> (interCost (Line (as !! 0)) (Line (as !! 1)))) $ filter (\ls -> length ls == 2) $ subsequences $ adjacents ps

instance Optimalize Chains where
  walk (Chains cs) = Chains <$> mapM walk cs
  cost (Chains cs) = (sum $ map cost cs) + toCost(linePairsFromChain (Chains cs))
    where
      linePairsFromChain :: Chains -> [(Line, Line)]
      linePairsFromChain (Chains cs) = (pairs cs)>>=(return.(\(x, y) -> (fromChainToLines x, fromChainToLines y)))>>=(uncurry allpair)

      toCost :: [(Line, Line)] -> Float
      toCost = sum . map (\((l1,l2)) -> interCost l1 l2)

      allpair :: [a] -> [b] -> [(a, b)]
      allpair as bs = concatMap (\b -> map ((flip (,)) b) as) bs

pairs :: [a] -> [(a, a)]
pairs as = map (\bs -> (bs !! 0, bs !! 1)) $ filter (\x -> length x == 2) $ subsequences as


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
  toInnerSVG (Line ((x1, y1), (x2, y2))) = "<line x1='" ++(show x1)++ "' y1='" ++(show y1)++ "' x2='" ++(show x2)++ "' y2='" ++(show y2)++ "' style='stroke:rgb(255, 0, 0);stroke-width:2'/>"

instance (SVGable a) => SVGable [a] where
  toInnerSVG as = intercalate "\n" $ map toInnerSVG as

main = do
  let chain1 = Chain $ take 6 $ repeat (0,0)
  let chain2 = Chain $ take 6 $ repeat (100,100)
  a <- evalOptimize 10000 $ Chains [chain1, chain2]
  home <- getHomeDirectory
  writeFile (home </> "src/tmp/tmp.html") $ showSVG $ map fromChainToLines $ (\(Chains cs) -> cs) $ head a
  print a
