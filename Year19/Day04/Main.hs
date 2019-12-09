-- solution for
-- http://adventofcode.com/2019/day/4


module Main where

import Util.Main1 (main12)

import Data.Char (isDigit)
import Data.List (zipWith4)

-- ----------------------------------------

main :: IO ()
main = main12 "2019-02"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

solve1 :: (Int, Int) -> Int
solve1 (x, y) = length $ solve anyEqual x y

solve2 :: (Int, Int) -> Int
solve2 (x, y) = length $ solve onePair x y

solve :: ([Char] -> Bool) -> Int -> Int -> [Int]
solve anyEq lb ub =
  filterPassword [lb `max` lb' .. ub `min` ub']
  where
    lb', ub' :: Int
    lb' = 10 ^ (5::Int)
    ub' = 10 ^ (6::Int) - 1

    filterPassword =
      map read
      . filter ((&&) <$> isSorted <*> anyEq)
      . map show


isSorted, anyEqual, onePair :: [Char] -> Bool

isSorted cs = and $ zipWith (<=) cs (drop 1 cs)

anyEqual cs = or  $ zipWith (==) cs (drop 1 cs)

onePair  cs = or  $ zipWith4 isPair cs' (drop 1 cs') (drop 2 cs') (drop 3 cs')
  where
    cs' = '-' : cs ++ ['-']
    isPair c0 c1 c2 c3 = c0 /= c1 && c1 == c2 && c2 /= c3

fromString :: String -> (Int, Int)
fromString xs =
  (read ys, read $ drop 1 zs)
  where
    (ys, zs) = span isDigit xs

-- ----------------------------------------

inp :: String
inp = "240298-784956"

res1, res2 :: Int
res1 = 42
res2 = 43

-- ----------------------------------------
