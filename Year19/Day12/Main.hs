{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

-- solution for
-- http://adventofcode.com/2019/day/11


module Main where

import Util.Main1 (main12)
import Control.Arrow ((>>>), second)
import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, string, space)

-- ----------------------------------------

main :: IO ()
main = main12 "2019-11"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = parseInp >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = parseInp >>> solve2 >>> show

solve1 :: Moons -> Int
solve1 = totalEnergy . nSteps 1000

solve2 :: Moons -> String
solve2 moons =
  undefined

solve moons =
  undefined

-- --------------------

type Triple   = P3 Int
type Point    = Triple
type Velocity = Triple
type Gravity  = Triple
type Moons    = ([Point], [Velocity])

zero :: Point
zero = pure 0

totalEnergy :: Moons -> Int
totalEnergy (ps, vs) =
  sum $ zipWith (*) potentialEnergies kineticEnergies
  where
    potentialEnergies = map manhattanDist ps
    kineticEnergies   = map manhattanDist vs

nSteps :: Int -> Moons -> Moons
nSteps 0 ms = ms
nSteps n ms = nSteps (n - 1) (uncurry nextStep ms)

nextStep :: [Point] -> [Velocity] -> Moons
nextStep ps vs =
  (ps', vs')
  where
    vs' = newVelocities ps vs
    ps' = newPositions  ps vs'

newPositions :: [Point] -> [Velocity]-> [Point]
newPositions = zipWith (+)

newVelocities :: [Point] -> [Velocity] -> [Velocity]
newVelocities ps vs = vs'
  where
    gs       = gravity ps
    vs'      = zipWith (+) vs gs

gravity :: [Point] -> [Gravity]
gravity ps = map gravity1 ps
  where
    gravity1 :: Point -> Gravity
    gravity1 p1 = sum $ map dist ps
      where
        dist :: Point -> Gravity
        dist p2 = signum (p2 - p1)

manhattanDist :: Point -> Int
manhattanDist p = x + y + z
  where
    P3 x y z = abs p

-- --------------------

data P3 a = P3 a a a

instance Functor P3 where
  fmap f (P3 x y z) = P3 (f x) (f y) (f z)

deriving instance Show a => Show (P3 a) -- where
--  show = show' . fmap show
--         where
--           show' (P3 x y z) = "(" ++ x ++ "," ++ y ++ "," ++ z ++ ")"

instance Applicative P3 where
  P3 f g h <*> P3 x y z = P3 (f x) (g y) (h z)
  pure x = P3 x x x

instance Num Point where
  p1 + p2     = (+) <$> p1 <*> p2
  p1 - p2     = (-) <$> p1 <*> p2
  p1 * p2     = (*) <$> p1 <*> p2
  abs p       = abs <$> p
  signum p    = signum <$> p
  fromInteger = pure . fromInteger

-- --------------------

type Parser a = Parsec String String a

parseInp :: String -> Moons
parseInp cs =
  maybe (error "Input error") toMs $
  parseMaybe pPoints cs
  where
    toMs ps = (ps, map (const zero) ps)

pPoints :: Parser [Point]
pPoints = many pPoint

pPoint :: Parser Point
pPoint = between (single '<') (single '>') pXYZ <* space

pXYZ :: Parser Point
pXYZ = do x <- string ("x=") *> space *> pInt <* single ',' <* space
          y <- string ("y=") *> space *> pInt <* single ',' <* space
          z <- string ("z=") *> space *> pInt
          return $ P3 x y z

pInt :: Parser Int
pInt =  do sign <- option id (single '-' *> return negate)
           val  <- read <$> some digitChar
           return $ sign val

-- ----------------------------------------

ex1 :: String
ex1 = unlines
  [ "<x=-1, y=0, z=2>"
  , "<x=2, y=-10, z=-7>"
  , "<x=4, y=-8, z=8>"
  , "<x=3, y=5, z=-1>"
  ]

rs1 :: Int
rs1 = 179

-- ----------------------------------------

inp :: String
inp = unlines
  [ "<x=-13, y=14, z= -7>"
  , "<x=-18, y= 9, z=  0>"
  , "<x=  0, y=-3, z= -3>"
  , "<x=-15, y= 3, z=-13>"
  ]

res1 :: Int
res1 = undefined

res2 :: Int
res2 = undefined

-- ----------------------------------------
