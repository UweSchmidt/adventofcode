-- solution for
-- http://adventofcode.com/2019/day/??


module Main where

import Util.Main1 (main12)

-- ----------------------------------------

main :: IO ()
main = main12 "2019-??"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = toString . solve1 . fromString

captcha2 :: String -> String
captcha2 = toString . solve2 . fromString

type Input  = [Int]  -- dummy
type Output = Int    -- dummy

solve1 :: Input -> Output
solve1 = undefined

solve2 :: Input -> Output
solve2 = undefined

fromString :: String -> Input
fromString = undefined

toString :: Output -> String
toString = show

-- ----------------------------------------

inp :: String
inp = ""

res1, res2 :: Output
res1 = undefined
res2 = undefined

-- ----------------------------------------
