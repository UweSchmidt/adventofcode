{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/16

module Main where

import AOC.Prelude

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

-- ----------------------------------------

withTrace :: Bool
withTrace = True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-16"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = parseDigits >>> solve1 >>> char8

captcha2 :: String -> String
captcha2 = parseDigits >>> solve2 >>> char8

solve1 :: IntVector -> IntVector
solve1 inp = undefined

solve2 :: IntVector -> IntVector
solve2 input = undefined

char8 :: IntVector -> String
char8 = take 8 >>> map show >>> concat

-- --------------------

type Vector a  = [a]
type Matrix a  = [Vector a]

type IntVector = Vector Int
type IntMatrix = Matrix Int


patMatrix :: Int -> IntMatrix
patMatrix n = map (take n) $ take n pattern'
  where
    pattern' :: IntMatrix
    pattern' = map toRow [1..]
      where
        toRow n' = drop 1 . rep $ pat n'
        pat   n' = concatMap (replicate n') basepat
        rep  xs  = xs ++ rep xs
        basepat  = [0, 1, 0, -1]

multMatVec :: Num a => Matrix a -> Vector a -> Vector a
multMatVec m v = map (sum . zipWith (*) v) m

multMatMat :: Num a => Matrix a -> Matrix a -> Matrix a
multMatMat m1 m2 =
  map (multMatVec m1') m2
  where
    m1' = transpose m1

lastDigitV :: IntVector -> IntVector
lastDigitV = map ((`mod` 10) . abs)

lastDigitM :: IntMatrix -> IntMatrix
lastDigitM = map lastDigitV

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldr (.) id $ replicate n f

phase :: Int -> IntVector -> IntVector
phase n v =
  nTimes n (lastDigitV . multMatVec (patMatrix $ length v)) v

-- --------------------

matToStr :: Show a => Matrix a -> String
matToStr = unlines . map show

parseDigits :: String -> IntVector
parseDigits = map (read . (:[])) . filter (`elem` ['0' .. '9'])

-- ----------------------------------------
--
-- tests

pm :: Show a => Matrix a -> IO ()
pm = putStrLn . matToStr

v1, o1, v4, v4' :: IntVector
v1 = parseDigits ex1
o1 = replicate 8 1
p1 = patMatrix (length v1)
v4  = nTimes 4 (lastDigitV . multMatVec p1) v1
v4' = lastDigitV $ multMatVec (nTimes 3 (lastDigitM . multMatMat p1) p1) v1

v2 = p1 `multMatVec` v1

p1'2 = p1 `multMatMat` transpose p1

-- --------------------

ex1, ex2, ex3, ex4 :: String
ex1 = "12345678"
ex2 = "80871224585914546619083218645595"
ex3 = "19617804207202209144916044189917"
ex4 = "69317163492948606335995924319873"

rs2, rs3, rs4 :: String
rs2 = "24176176"
rs3 = "73745418"
rs4 = "52432133"

ts :: String -> String
ts = parseDigits >>> phase 100 >>> char8

-- ----------------------------------------

inp :: String
inp = "59782619540402316074783022180346847593683757122943307667976220344797950034514416918778776585040527955353805734321825495534399127207245390950629733658814914072657145711801385002282630494752854444244301169223921275844497892361271504096167480707096198155369207586705067956112600088460634830206233130995298022405587358756907593027694240400890003211841796487770173357003673931768403098808243977129249867076581200289745279553289300165042557391962340424462139799923966162395369050372874851854914571896058891964384077773019120993386024960845623120768409036628948085303152029722788889436708810209513982988162590896085150414396795104755977641352501522955134675"

res1 :: Int
res1 = undefined

res2 :: Int
res2 = undefined

-- ----------------------------------------
