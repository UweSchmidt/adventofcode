-- solution for
-- http://adventofcode.com/2019/day/7


module Main where

import Util.Main1 (main12)
import Data.Intcode ( IntcodeProg
                    , runIntcode2
                    , fromCVS
                    )

import Control.Arrow ((>>>))
import Data.List     (permutations)

-- ----------------------------------------

main :: IO ()
main = main12 "2019-07"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = toString . solve1 . fromString

captcha2 :: String -> String
captcha2 = toString . solve2 . fromString

-- --------------------

type Input  = IntcodeProg
type Output = Int

type PhaseSetting  = Int
type PhaseSettings = [PhaseSetting]

type Amplifier  = Int -> Int
type Amplifiers = [Amplifier]
type RawIron    = IntcodeProg -> PhaseSetting -> Amplifier

rawIron :: RawIron
rawIron prog ps x =
  head . snd . snd $ runIntcode2 [ps, x] prog

mkAmplifiers :: IntcodeProg -> PhaseSettings -> Amplifier
mkAmplifiers prog pss =
  foldr (>>>) id $ zipWith ($) (repeat progLoaded) pss
  where
    progLoaded = rawIron prog

allPerms :: IntcodeProg -> Int -> Int
allPerms prog n =
  maximum $ map (flip (mkAmplifiers prog) 0) perms
  where
    perms = permutations [0 .. n-1]

-- --------------------

solve :: Int -> Input -> Output
solve i p = undefined
  where
    ((e, _p), (_inp, outp)) = runIntcode2 [i] p

solve1 :: Input -> Output
solve1 = flip allPerms 5

solve2 :: Input -> Output
solve2 = undefined

fromString :: String -> Input
fromString = fromCVS

toString :: Output -> String
toString = show

-- ----------------------------------------

ex1, ex2, ex3 :: String
ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23," ++
      "101,5,23,23,1,24,23,23,4,23,99,0,0"
ex3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," ++
      "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

rs1, rs2, rs3 :: Output
rs1 = 43210
rs2 = 54321
rs3 = 65210

test1, test2, test3 :: Bool
test1 = solve1 (fromString ex1) == rs1
test2 = solve1 (fromString ex2) == rs2
test3 = solve1 (fromString ex3) == rs3

inp :: String
inp = "3,8,1001,8,10,8,105,1,0,0,21,34,43,60,81,94,175,256,337,418,99999,3,9,101,2,9,9,102,4,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,102,3,9,9,4,9,99,3,9,102,4,9,9,1001,9,2,9,1002,9,3,9,101,4,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99"

res1, res2 :: Int
res1 = 11828
res2 = undefined

-- ----------------------------------------
