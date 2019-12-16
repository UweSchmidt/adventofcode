{-# LANGUAGE TupleSections #-}

-- solution for
-- http://adventofcode.com/2019/day/10

module Main where

import Util.Main1 (main12)

import Control.Arrow ((>>>), first, second)
import Data.List (foldl')
import qualified Data.Relation as R
import qualified Data.Set      as S

-- ----------------------------------------

main :: IO ()
main = main12 "2019-10"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = fromString >>> solve1 >>> (fst >>> show)

captcha2 :: String -> String
captcha2 = fromString >>> solve2 >>> show

-- --------------------

type Point    = (Int, Int)
type PointSet = S.Set  Point
type PointRel = R.Rel' Point
type Size     = (Int, Int)
type Input    = (Size, PointSet)

-- --------------------

solve2 :: Input -> Int
solve2 ps =
  undefined

solve1 :: Input -> (Int, Point)
solve1 inpt@(_size, points)=
  maxVisible points . noOfHidden . uncurry hiddenSeq $ inpt

maxVisible :: PointSet -> [(Int, Point)] -> (Int,Point)
maxVisible points =
  maximum . map (first (\ n -> noOfPoints - n - 1))
  where
    noOfPoints = S.size points

noOfHidden :: PointRel -> [(Int, Point)]
noOfHidden =
  R.foldrS (\ p ps rs -> (S.size ps, p) : rs) []

-- compute the relation of all points p1 and p2
-- which are hidden by a point p3 lies
-- strait on the line between p1 and p2

hiddenSeq :: Size -> PointSet -> PointRel
hiddenSeq sz points = go R.empty $ S.toAscList points
  where
    go :: PointRel -> [Point] -> PointRel
    go hrel [] = hrel
    go hrel (p : ps) = go (hrel' `R.union` hrel) ps
      where
        hrel' = allHidden sz points p ps

-- compute all hidden points, when looking from p1
-- at every point p2 not yet processed

allHidden :: Size -> PointSet -> Point -> [Point] -> PointRel
allHidden sz points p1 ps
  = R.symmetric . R.singletonS p1 $ go S.empty ps
  where
    go :: PointSet -> [Point] -> PointSet
    go hset []         = hset
    go hset [_]        = hset
    go hset (p2 : ps2)
      | p2 `S.member` hset = go hset  ps2
      | otherwise          = go (hset' `S.union` hset) ps2
      where
        hset' = hidden sz points p1 p2

-- compute all points hidden by p2
-- when looking from p1 at p2

hidden :: Size -> PointSet -> Point -> Point -> PointSet
hidden sz@(h, w) points p1@(y1, x1) p2@(y2, x2) =
  (`S.intersection` points)
  . S.fromList
  . takeWhile (inbox sz)
  . drop 1
  . iterate (\ (yi, xi) -> (yi + dy, xi + dx))
  $ p2
  where
    dy' = y2 - y1
    dx' = x2 - x1
    gd  = dy' `gcd` dx'
    dy  = dy' `div` gd
    dx  = dx' `div` gd

inbox :: Size -> Point -> Bool
inbox (h, w) (y, x) =
  0 <= x && x < w
  &&
  0 <= y && y < h

fromString :: String -> Input
fromString cs =
  ((h, w) , S.fromList $ concat ps)
  where
    ls = lines cs
    h  = length ls
    w  = length $ head ls
    ps = toYS . map toXS $ ls

    toXS = map fst . filter ((== '#') . snd) . zip [0..]
    toYS = zipWith toPoint [0..]

    toPoint :: Int -> [Int] -> [Point]
    toPoint y xs = map (y,) xs

-- ----------------------------------------

ex1 :: String
ex1 = unlines
  [ "......#.#."
  , "#..#.#...."
  , "..#######."
  , ".#.#.###.."
  , ".#..#....."
  , "..#....#.#"
  , "#..#....#."
  , ".##.#..###"
  , "##...#..#."
  , ".#....####"
  ]

rs1 :: (Int, Point)
rs1 = (33, (8,5))

test1 :: Bool
test1 = solve1 (fromString ex1) == rs1

ex2 :: String
ex2 = unlines
  [ "#.#...#.#."
  , ".###....#."
  , ".#....#..."
  , "##.#.#.#.#"
  , "....#.#.#."
  , ".##..###.#"
  , "..#...##.."
  , "..##....##"
  , "......#..."
  , ".####.###."
  ]

rs2 :: (Int, Point)
rs2 = (35, (2,1))

test2 :: Bool
test2 = solve1 (fromString ex2) == rs2

ex4 = unlines
  [ ".#..##.###...#######"
  , "##.############..##."
  , ".#.######.########.#"
  , ".###.#######.####.#."
  , "#####.##.#.##.###.##"
  , "..#####..#.#########"
  , "####################"
  , "#.####....###.#.#.##"
  , "##.#################"
  , "#####.##.###..####.."
  , "..######..##.#######"
  , "####.##.####...##..#"
  , ".#####..#.######.###"
  , "##...#.##########..."
  , "#.##########.#######"
  , ".####.#.###.###.#.##"
  , "....##.##.###..#####"
  , ".#.#.###########.###"
  , "#.#.#.#####.####.###"
  , "###.##.####.##.#..##"
  ]

rs4 :: (Int, Point)
rs4 = (210, (13,11))

test4 :: Bool
test4 = solve1 (fromString ex4) == rs4

-- --------------------

inp :: String
inp = unlines
  [ ".............#..#.#......##........#..#"
  , ".#...##....#........##.#......#......#."
  , "..#.#.#...#...#...##.#...#............."
  , ".....##.................#.....##..#.#.#"
  , "......##...#.##......#..#.......#......"
  , "......#.....#....#.#..#..##....#......."
  , "...................##.#..#.....#.....#."
  , "#.....#.##.....#...##....#####....#.#.."
  , "..#.#..........#..##.......#.#...#....#"
  , "...#.#..#...#......#..........###.#...."
  , "##..##...#.#.......##....#.#..#...##..."
  , "..........#.#....#.#.#......#.....#...."
  , "....#.........#..#..##..#.##........#.."
  , "........#......###..............#.#...."
  , "...##.#...#.#.#......#........#........"
  , "......##.#.....#.#.....#..#.....#.#...."
  , "..#....#.###..#...##.#..##............#"
  , "...##..#...#.##.#.#....#.#.....#...#..#"
  , "......#............#.##..#..#....##...."
  , ".#.#.......#..#...###...........#.#.##."
  , "........##........#.#...#.#......##...."
  , ".#.#........#......#..........#....#..."
  , "...............#...#........##..#.#...."
  , ".#......#....#.......#..#......#......."
  , ".....#...#.#...#...#..###......#.##...."
  , ".#...#..##................##.#........."
  , "..###...#.......#.##.#....#....#....#.#"
  , "...#..#.......###.............##.#....."
  , "#..##....###.......##........#..#...#.#"
  , ".#......#...#...#.##......#..#........."
  , "#...#.....#......#..##.............#..."
  , "...###.........###.###.#.....###.#.#..."
  , "#......#......#.#..#....#..#.....##.#.."
  , ".##....#.....#...#.##..#.#..##.......#."
  , "..#........#.......##.##....#......#..."
  , "##............#....#.#.....#..........."
  , "........###.............##...#........#"
  , "#.........#.....#..##.#.#.#..#....#...."
  , "..............##.#.#.#...........#....."
  ]

res1 :: (Int, Point)
res1 = (299,(29,26))

res2 :: Int
res2 = undefined

-- ----------------------------------------
