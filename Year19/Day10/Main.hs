{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

-- solution for
-- http://adventofcode.com/2019/day/10

module Main where

import Util.Main1    (main12)
import Control.Arrow ((***), (>>>), first, second)
import Data.Function (on)
import Data.List     (sort)

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

data Arc     = Arc Quad Tangent
type Quad    = Int               -- 0..3
type Tangent = (Int, Int)        -- tangent as rational value, x,y >= 0

deriving instance Show Arc
deriving instance Eq   Arc

instance Ord Arc where
  Arc q1 (x1, y1) <= Arc q2 (x2, y2)
    | q1 /= q2  = q1 <= q2
    | t1 /= t2  = t1 <= t2
    | otherwise = y1 <= y2
    where
      t1 = x1 * y2
      t2 = x2 * y1

nullArc :: Arc
nullArc = Arc 0 (0, 0)

pointToArc :: Point -> Arc
pointToArc p0@(0, 0)         = Arc 0 p0
pointToArc p                 = p2a $ second negate p  -- to math coodinates

arcToPoint :: Arc -> Point
arcToPoint (Arc 0 p0@(0, 0)) = p0
arcToPoint arc               = second negate $ a2p arc -- back to screen coodinates

p2a :: Point -> Arc
p2a p@(x, y)
  | x >= 0 && y > 0 = Arc 0        p         -- 1. quad
  | otherwise       = Arc (q' + 1) p'
  where
    Arc q' p' = p2a $ rcc p

a2p :: Arc -> Point
a2p (Arc q t)
  | q == 0    = t
  | otherwise = rc $ a2p (Arc (q - 1) t)

rc, rcc :: Point -> Point
rc  (x', y') = (y', negate x')     -- rotate clockwise
rcc (x', y') = (negate y', x')     -- rotate counter clockwise

tlp :: Point -> Point -> Point
tlp (dx, dy) (x, y) = (x - dx, y - dy)

eqAngle :: Arc -> Arc -> Bool
eqAngle a1 a2
  | a1 == nullArc = a2 == nullArc
  | a2 == nullArc = a1 == nullArc
eqAngle (Arc q1 (x1, y1)) (Arc q2 (x2, y2)) =
  q1 == q2
  &&
  x1 * y2 == x2 * y1

pointsToArcs :: Point -> [Point] -> [Arc]
pointsToArcs orig = sort . map (p2a . tlp orig)

sortByArc :: Point -> [Point] -> [Point]
sortByArc orig = map a2p . pointsToArcs orig

groupByAngle :: [Point] -> [[Point]]
groupByAngle = groupBy (eqAngle `on` pointToArc)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x : xs) = (x : es) : groupBy p xs'
  where
    (es, xs') = span (p x) xs

transpGroups :: [[a]] -> [a]
transpGroups gss
  | null gss' = xs
  | otherwise = xs ++ transpGroups gss'
  where
    (xs, gss') = unconsGroups gss

unconsGroups :: [[a]] -> ([a], [[a]])
unconsGroups = foldr uncons ([], [])
  where
    uncons (x : []) (hs, tss) = (x : hs, tss)
    uncons (x : xs) (hs, tss) = (x : hs, xs : tss)
    uncons []       res       = res

solve2 :: Input -> Int
solve2 input = (\ (x, y) -> x * 100 + y) . head . drop 199 $ vaporSeq
  where
     origin   = snd . solve1 $ input
     vaporSeq = vaporize origin (snd input)

vaporize :: Point -> PointSet -> [Point]
vaporize origin ps =
  S.toAscList
  >>> map (tlp origin) -- translate coodinates to origin = (0,0)
  >>> map pointToArc
  >>> sort
  >>> map arcToPoint
  >>> drop 1           -- drop origin
  >>> groupByAngle
  >>> transpGroups
  >>> map (tlp origin')  -- translate coodinates back to screen coordinates
  $ ps
  where
    origin' = (negate *** negate) origin

-- --------------------

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
hidden sz points (x1, y1) p2@(x2, y2) =
  iterate (\ (xi, yi) -> (xi + dx, yi + dy))
  >>> drop 1
  >>> takeWhile (inbox sz)
  >>> S.fromList
  >>> (`S.intersection` points)
  $ p2
  where
    dy' = y2 - y1
    dx' = x2 - x1
    gd  = dy' `gcd` dx'
    dy  = dy' `div` gd
    dx  = dx' `div` gd

inbox :: Size -> Point -> Bool
inbox (w, h) (x, y) =
  0 <= x && x < w
  &&
  0 <= y && y < h

fromString :: String -> Input
fromString cs =
  ((w, h) , S.fromList $ concat ps)
  where
    ls = lines cs
    h  = length ls
    w  = length $ head ls
    ps = toYS . map toXS $ ls

    toXS = map fst . filter ((== '#') . snd) . zip [0..]
    toYS = zipWith toPoint [0..]

    toPoint :: Int -> [Int] -> [Point]
    toPoint y xs = map (,y) xs

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
rs1 = (33, (5,8))

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
rs2 = (35, (1,2))

test2 :: Bool
test2 = solve1 (fromString ex2) == rs2

ex4 :: String
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
rs4 = (210, (11,13))

test4 :: Bool
test4 = solve1 (fromString ex4) == rs4

-- --------------------
-- part 2

ex5 :: String
ex5 = unlines
  [ ".#....#####...#.."
  , "##...##.#####..##"
  , "##...#...#.#####."
  , "..#.....#...###.."
  , "..#.#.....#....##"
  ]

orig5 :: Point
orig5 = (8,3)

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
res1 = (299,(26,29))

res2 :: Int
res2 = 1419

-- ----------------------------------------
