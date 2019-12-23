module Util.RenderBoard
  ( renderBoard )
where

import qualified Data.HashMap.Strict as M

-- --------------------

type Pos     = (Int, Int)
type Board a = M.HashMap Pos a

renderBoard :: Char -> (a -> Char) -> Board a -> [String]
renderBoard defC toC board =
  map toRow [minY..maxY]
  where
    maxX = maximum . map fst . M.keys $ board
    maxY = maximum . map snd . M.keys $ board
    minX = minimum . map fst . M.keys $ board
    minY = minimum . map snd . M.keys $ board

    toRow y = map toChar [minX..maxX]
      where
        toChar x = maybe defC toC $ M.lookup (x, y) board

-- --------------------
