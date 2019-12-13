-- solution for
-- http://adventofcode.com/2019/day/7


module Main where

import Util.Main1 (main12)
import Data.Intcode ( IntcodeProg
                    , runIntcode2'
                    , fromCVS
                    )

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
type Amp    = PhaseSetting  -> [Int] -> [Int]
type AmpSeq = PhaseSettings -> [Int] -> [Int]

mkAmp :: IntcodeProg -> Int -> Amp
mkAmp prog = amp
  where
    amp cpu phs xs = runIntcode2' cpu (phs : xs) prog

mkAmps :: (Int -> Amp) -> [Int] -> [Amp]
mkAmps amp cpus = map (amp $) cpus

composeAmps :: [Amp] -> AmpSeq
composeAmps as phss = foldr (.) id $ reverse amps
  where
    amps :: [[Int] -> [Int]]
    amps = zipWith ($) as phss

mkAmpSeq :: IntcodeProg -> AmpSeq
mkAmpSeq prog phss xs =
  composeAmps amps' phss xs
  where
    amps' :: [Amp]
    amps' = mkAmps amp' [1 .. length phss]

    amp' :: Int -> Amp
    amp' = mkAmp prog

run1 :: AmpSeq -> PhaseSettings -> Int
run1 amps phss = head $ run1' [0] amps phss

run1' :: [Int] -> AmpSeq -> PhaseSettings -> [Int]
run1' is amps phss =  amps phss is

run2 :: AmpSeq -> PhaseSettings -> Int
run2 amps phss =
  let xs = run1' (0 : xs) amps phss
  in last xs

maxPhs :: (PhaseSettings -> Int) -> PhaseSettings -> Int
maxPhs amps = maximum . map (amps $) . permutations

-- --------------------

solve1 :: Input -> Output
solve1 prog = maxPhs (run1 (mkAmpSeq prog)) [0..4]

solve2 :: Input -> Output
solve2 prog = maxPhs (run2 (mkAmpSeq prog)) [5..9]

fromString :: String -> Input
fromString = fromCVS

toString :: Output -> String
toString = show

-- ----------------------------------------

ex1, ex2, ex3, ex4, ex5 :: String
ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23," ++
      "101,5,23,23,1,24,23,23,4,23,99,0,0"
ex3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," ++
      "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

ex4 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26," ++
      "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

ex5 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54," ++
      "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4," ++
      "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

rs1, rs2, rs3, rs4, rs5 :: Output
rs1 = 43210
rs2 = 54321
rs3 = 65210

rs4 = 139629729
rs5 = 18216

test1, test2, test3, test4, test5 :: Bool
test1 = solve1 (fromString ex1) == rs1
test2 = solve1 (fromString ex2) == rs2
test3 = solve1 (fromString ex3) == rs3

test4 = solve2 (fromString ex4) == rs4
test5 = solve2 (fromString ex5) == rs5

inp :: String
inp = "3,8,1001,8,10,8,105,1,0,0,21,34,43,60,81,94,175,256,337,418,99999,3,9,101,2,9,9,102,4,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,102,3,9,9,4,9,99,3,9,102,4,9,9,1001,9,2,9,1002,9,3,9,101,4,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99"

res1, res2 :: Int
res1 = 11828
res2 = 1714298

-- ----------------------------------------
