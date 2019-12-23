module AOC.RenderBoard
  ( renderBoard )
where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Map.Strict     as MS

-- --------------------

type Pos       = (Int, Int)
type Board m a = m Pos a

renderBoard :: Lookup m
            => Char -> (a -> Char)
            -> Board m a -> [String]
renderBoard defC toC board =
  map toRow [minY..maxY]
  where
    maxX = maximum . map fst . keysB $ board
    maxY = maximum . map snd . keysB $ board
    minX = minimum . map fst . keysB $ board
    minY = minimum . map snd . keysB $ board

    toRow y = map toChar [minX..maxX]
      where
        toChar x = maybe defC toC $ lookupB (x, y) board

class Lookup m where
  lookupB :: Pos -> m Pos a -> Maybe a
  keysB   :: m Pos a -> [Pos]


instance Lookup HMS.HashMap where
  lookupB = HMS.lookup
  keysB   = HMS.keys

instance Lookup MS.Map where
  lookupB = MS.lookup
  keysB   = MS.keys

-- --------------------
