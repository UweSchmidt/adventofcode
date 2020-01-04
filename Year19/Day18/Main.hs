{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/18

module Main where

import AOC.Prelude
import Data.Grid
import Data.Graph.DirectedLabeled (Graph, NodeSet)

import qualified Data.Graph.DirectedLabeled as G
import qualified Data.Set as S

-- ----------------------------------------

withTrace :: Bool
withTrace = False -- True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-18"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = newScreen >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = newScreen >>> solve2 >>> show

solve1 :: Screen -> Int
solve1 = undefined

solve2 :: Screen -> Int
solve2 = undefined

  -- --------------------

data Tile     = Wall
              | Open
              | Entrance
              | Key  Char
              | Door Char

type Distance = Int

type Screen   = Grid Tile
type Area     = Graph Point Tile Distance
type PointSet = NodeSet Point

-- --------------------

deriving instance Show Tile
deriving instance Eq   Tile

-- --------------------

test xs = scanArea sc0 p0 d0 open0 cls0 g0
  where
    d0    = 0
    open0 = openMoves (S.singleton p0) sc0
    cls0  = S.singleton p0
    p0    = fromJust . findEntrance $ sc0
    sc0   = newScreen $ xs
    g0    = initArea p0

openMoves :: PointSet -> Screen -> PointSet
openMoves = undefined

initArea :: Point -> Area
initArea p = G.insertNode p Entrance G.empty

scanArea :: Screen   ->
            Point    -> Distance ->
            PointSet -> PointSet ->
            Area     -> ((PointSet, PointSet), Area)
scanArea sc org dist open visited area =
  ((open, visited), area)

-- --------------------

findEntrance :: Screen -> Maybe Point
findEntrance = listToMaybe . pointsGrid . filterGrid f
  where
    f _p Entrance = True
    f _p _t       = False

-- --------------------

newScreen :: String -> Screen
newScreen =
  mkGrid Open tileToChar charToTile . parseScreen

tileToChar :: Tile -> Char
tileToChar Wall     = '#'
tileToChar Open    = '.'
tileToChar Entrance = '@'
tileToChar (Key  c) = toLower c
tileToChar (Door c) = toUpper c

charToTile :: Char -> Maybe Tile
charToTile '#' = Just Wall
charToTile '.' = Just Open
charToTile '@' = Just Entrance
charToTile c
  | isUpper c  = Just $ Door c
  | isLower c  = Just $ Key  c
  | otherwise  = Nothing

-- --------------------

parseScreen :: String -> [(Point, Tile)]
parseScreen =
  concat . zipWith parseLine [0..] . lines
  where
    parseLine :: Int -> [Char] -> [(Point, Tile)]
    parseLine y =
      catMaybes . zipWith parseChar [0..]
      where
        parseChar :: Int -> Char -> Maybe (Point, Tile)
        parseChar x c =  ((x, y),) <$> charToTile c

-- --------------------

ex1 :: String
ex1 = unlines
  [ "#########"
  , "#b.A.@.a#"
  , "#########"
  ]

ex2 :: String
ex2 = unlines
  [ "########################"
  , "#f.D.E.e.C.b.A.@.a.B.c.#"
  , "######################.#"
  , "#d.....................#"
  , "########################"
  ]

rs1 :: Int
rs1 = 76

inp :: String
inp = ""

res1 :: Int
res1 = undefined

res2 :: Int
res2 = undefined

-- ----------------------------------------
