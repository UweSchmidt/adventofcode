module AOC.Prelude
  ( module AOC.Prelude
  , module AOC.Main1
  , module AOC.RenderBoard

  -- access and modify
  , module Control.Arrow
  , module Control.Lens

  -- parser stuff
  , module Text.Megaparsec
  , module Text.Megaparsec.Char

  -- trace
  , module Debug.Trace
  )
where

import Control.Arrow    ( (>>>), (***), (&&&), first, second )
import Control.Lens     hiding (Empty)

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

-- --------------------

trace' :: Bool -> String -> a -> a
trace' True msg = trace msg
trace' _    _   = id

-- --------------------
