{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/14

module Main where

import AOC.Prelude

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

-- ----------------------------------------

withTrace :: Bool
withTrace = True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-14"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = parseRules >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = parseRules >>> solve2 >>> show

solve1 :: Rules -> Int
solve1 rules = undefined

solve2 :: Rules -> Int
solve2 rules = undefined

type Material   = String
type Quantity   = (Material, Int)
type Quantities = S.Set Quantity
type Rule       = (Quantity, Quantities)
type Rules      = M.HashMap Material Rule

-- --------------------

parseRules :: String -> Rules
parseRules = undefined

-- ----------------------------------------

inp :: String
inp = ""

res1 :: Int
res1 = undefined

res2 :: Int
res2 = undefined

-- ----------------------------------------
