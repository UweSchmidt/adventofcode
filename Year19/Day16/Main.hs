{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/16

module Main where

import AOC.Prelude

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
captcha1 = parseDigits >>> solve1 >>> chars 8

captcha2 :: String -> String
captcha2 = parseDigits >>> solve2 >>> chars 8

solve1 :: IntVector -> IntVector
solve1 = phase 100

solve2 :: IntVector -> IntVector
solve2 input = nTimes 100 sumUp inp1
  where
    offset :: Int
    offset = read $ chars 7 input

    rep    = 10000
    inp1   = drop offset . concat $ replicate rep input
    -- len1   = length input - offset

chars :: Int -> IntVector -> String
chars x = take x >>> map show >>> concat

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

sumUp :: IntVector -> IntVector
sumUp = snd . foldr add (0, [])
  where
    add !i (!sm, res) = (sm' , sm' : res)
      where
        sm' = (i + sm) `mod` 10

-- --------------------

matToStr :: Show a => Matrix a -> String
matToStr = unlines . map show

parseDigits :: String -> IntVector
parseDigits = map (read . (:[])) . filter (`elem` ['0' .. '9'])

-- --------------------

ex1, ex2, ex3, ex4 :: String
ex1 = "12345678"
ex2 = "80871224585914546619083218645595"
ex3 = "19617804207202209144916044189917"
ex4 = "69317163492948606335995924319873"

ex6, ex7, ex8 :: String
ex6 = "03036732577212944063491565474664"
ex7 = "02935109699940807407585447034323"
ex8 = "03081770884921959731165446850517"

rs2, rs3, rs4 :: String
rs2 = "24176176"
rs3 = "73745418"
rs4 = "52432133"

rs6, rs7, rs8 :: String
rs6 = "84462026"
rs7 = "78725270"
rs8 = "53553731"

ts :: String -> String
ts = parseDigits >>> phase 100 >>> chars 8

-- ----------------------------------------

inp :: String
inp = "59782619540402316074783022180346847593683757122943307667976220344797950034514416918778776585040527955353805734321825495534399127207245390950629733658814914072657145711801385002282630494752854444244301169223921275844497892361271504096167480707096198155369207586705067956112600088460634830206233130995298022405587358756907593027694240400890003211841796487770173357003673931768403098808243977129249867076581200289745279553289300165042557391962340424462139799923966162395369050372874851854914571896058891964384077773019120993386024960845623120768409036628948085303152029722788889436708810209513982988162590896085150414396795104755977641352501522955134675"

res1 :: String
res1 = "27229269"

res2 :: String
res2 = "26857164"

-- ----------------------------------------
