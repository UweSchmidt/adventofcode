-- solution for
-- http://adventofcode.com/2019/day/2


module Main where

import Util.Main1 (main12)
import Data.Intcode ( IntcodeProg
                    , runIntcode
                    , patchIntcode
                    , fromCVS
                    )

-- ----------------------------------------

main :: IO ()
main = main12 "2019-02"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = show
           . either (error "no result") id
           . runIntcode
           . patchIntcode 12 1
           . patchIntcode 2 2
           . fromCVS

captcha2 :: String -> String
captcha2 = show . solve2 100 19690720 . fromCVS


solve2 :: Int -> Int -> IntcodeProg -> Int
solve2 n req p =
  toR . head . map snd . filter ((== Right req) . fst) $ xs
  where
    toR (noun, verb)
       = noun * 100 + verb

    n1 = n - 1
    xs = [ ( run x y p
           , (x, y)
           )
         | x <- [0 .. n1]
         , y <- [0 .. n1]
         ]

    run x y = runIntcode . patchIntcode x 1 . patchIntcode y 2

-- ----------------------------------------

inp :: String
inp = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,2,23,6,27,2,6,27,31,2,13,31,35,1,10,35,39,2,39,13,43,1,43,13,47,1,6,47,51,1,10,51,55,2,55,6,59,1,5,59,63,2,9,63,67,1,6,67,71,2,9,71,75,1,6,75,79,2,79,13,83,1,83,10,87,1,13,87,91,1,91,10,95,2,9,95,99,1,5,99,103,2,10,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0"

res1, res2 :: Int
res1 = 5434663
res2 = 4559

-- ----------------------------------------
