-- solution for
-- http://adventofcode.com/2019/day/7


module Main where

import Util.Main1 (main12)
import Data.Intcode ( IntcodeProg
                    , Stdin
                    , Stdout
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
type Amp    =  PhaseSetting  -> [Int] -> [Int]
type AmpSeq = [PhaseSetting] -> [Int] -> [Int]

mkAmp :: IntcodeProg -> Amp
mkAmp prog = amp
  where
    amp phs xs = snd . snd $ runIntcode2 (phs : xs) prog

mkAmpSeq :: Amp -> AmpSeq
mkAmpSeq amp = amps
  where
    amps phss = foldr (>>>) id $ map (amp $) phss

run1 :: AmpSeq -> PhaseSettings -> Int
run1 amps phss = head $ amps phss [0]

run1' :: Int -> AmpSeq -> PhaseSettings -> [Int]
run1' i amps phss =  amps phss [i]

run2 :: AmpSeq -> PhaseSettings -> Int
run2 amps phss =
  let rs = amps phss (0 : rs)
  in head rs

run2' :: AmpSeq -> PhaseSettings -> [Int]
run2' amps phss =
  let rs = amps phss (0 : rs)
  in rs

maxPhs :: (PhaseSettings -> Int) -> PhaseSettings -> Int
maxPhs amps = maximum . map (amps $) . permutations

{-
type Amplifier' a  = a -> a
type RawIron'   a  = IntcodeProg -> PhaseSetting -> Amplifier' a

mkRawIron' :: (a -> Stdin) -> (Stdout -> a) -> RawIron' a
mkRawIron' fromIn toOut prog ps x =
  toOut . snd . snd $ runIntcode2 (ps : fromIn x) prog

mkAmplifiers' :: (a -> Stdin) -> (Stdout -> a) -> IntcodeProg
              -> PhaseSettings -> Amplifier' a
mkAmplifiers' fromIn toOut prog ps =
  foldr (>>>) id $ zipWith ($) (repeat progLoaded) ps
  where
    progLoaded = toOut . snd . snd $ runIntcode2 (ps : fromIn x) prog

mkAmplifiers1 :: IntcodeProg -> PhaseSettings -> Amplifier' Int
mkAmplifiers1 = mkAmplifiers' (:[]) head

mkAmplifiers2 :: IntcodeProg -> PhaseSettings -> Amplifier' [Int]
mkAmplifiers2 = mkAmplifiers' id id
-}
type RawIron   = IntcodeProg -> PhaseSetting -> Int -> Int
type Amplifier = Int -> Int

rawIron :: RawIron
rawIron prog ps x =
  head . snd . snd $ runIntcode2 [ps, x] prog

mkAmplifiers :: IntcodeProg -> PhaseSettings -> Amplifier
mkAmplifiers prog pss =
  foldl (>>>) id $ zipWith ($) (repeat progLoaded) pss
  where
    progLoaded = rawIron prog

allPerms :: PhaseSettings -> IntcodeProg -> Int
allPerms ps prog =
  maximum $ map (flip (mkAmplifiers prog) 0) perms
  where
    perms = permutations ps

type Amplifier2  = [Int] -> [Int]
type RawIron2    = IntcodeProg -> PhaseSetting -> Amplifier2

rawIron2 :: RawIron2
rawIron2 prog ps xs =
  snd . snd $ runIntcode2 (ps : xs) prog

-- --------------------

solve1 :: Input -> Output
solve1 prog = maxPhs (run1 (mkAmpSeq $ mkAmp prog)) [0..4]

-- nudelt
solve2 :: Input -> Output
solve2 prog = maxPhs (run2 (mkAmpSeq $ mkAmp prog)) [5..9]

fromString :: String -> Input
fromString = fromCVS

toString :: Output -> String
toString = show

-- ----------------------------------------

ex1, ex2, ex3, ex4 :: String
ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
ex2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23," ++
      "101,5,23,23,1,24,23,23,4,23,99,0,0"
ex3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," ++
      "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

ex4 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26," ++
      "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

rs1, rs2, rs3 :: Output
rs1 = 43210
rs2 = 54321
rs3 = 65210

rs4 = 139629729

test1, test2, test3, test4 :: Bool
test1 = solve1 (fromString ex1) == rs1
test2 = solve1 (fromString ex2) == rs2
test3 = solve1 (fromString ex3) == rs3

test4 = run2 (mkAmpSeq . mkAmp $ fromString ex1) [9,8,7,6,5] == rs4

inp :: String
inp = "3,8,1001,8,10,8,105,1,0,0,21,34,43,60,81,94,175,256,337,418,99999,3,9,101,2,9,9,102,4,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,102,4,9,9,1001,9,4,9,102,3,9,9,4,9,99,3,9,102,4,9,9,1001,9,2,9,1002,9,3,9,101,4,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99"

res1, res2 :: Int
res1 = 11828
res2 = undefined

-- ----------------------------------------
