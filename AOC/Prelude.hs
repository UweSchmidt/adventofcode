module AOC.Prelude
  ( module AOC.Prelude
  , module AOC.Main1
  , module AOC.RenderBoard

  -- access and modify
  , module Control.Arrow
  , module Control.Lens

  , module Data.Char
  , module Data.Foldable
  , module Data.List
  , module Data.Maybe

  -- parser stuff
  , module Text.Megaparsec
  , module Text.Megaparsec.Char

  -- trace
  , module Debug.Trace
  )
where

import Control.Arrow    ( (>>>), (***), (&&&), first, second )
import Control.Lens     hiding (Empty, uncons)

import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe

import Debug.Trace      ( trace )

import Text.Megaparsec  hiding (Pos, noneOf)
import Text.Megaparsec.Char

import AOC.Main1
import AOC.RenderBoard

-- --------------------

type Pos  = (Int, Int)
type Dist = (Int, Int)

origin :: Pos
origin = (0, 0)

scalePos :: Int -> Pos -> Pos
scalePos sc p = p & both %~ (* sc)

shiftPos :: Dist -> Pos -> Pos
shiftPos (dx, dy) (x, y) = (x + dx, y + dy)

rotate90CW :: Pos -> Pos
rotate90CW (x, y) = (y, -x)

rotate90CCW :: Pos -> Pos
rotate90CCW (x, y) = (-y, x)

-- --------------------

type Parser a = Parsec String String a

parseInput :: Parser r -> String -> r
parseInput p =
  either (error . show) id . parse p "input"

natural :: Parser Int
natural = read <$> some (satisfy isDigit) <?> "number"

-- --------------------

trace' :: Bool -> String -> a -> a
trace' True msg = trace msg
trace' _    _   = id

-- --------------------
