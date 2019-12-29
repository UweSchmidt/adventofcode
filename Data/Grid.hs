{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Grid
  ( Point
  , Move(..)
  , move
  , reachable

  , Grid
  , mkGrid

  , gridMap         -- lenses
  , gridDefaultVal
  , gridValChar

  , lookupGrid
  , insertGrid
  , insertListGrid
  , deleteGrid
  , filterGrid
  , renderGrid
  , pointsGrid
  , elemsGrid
  )
where

import Control.Arrow
import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Semigroup

import qualified Data.HashMap.Strict as HM

-- ----------------------------------------

type Point = (Int, Int)

data Move  = North | South | West | East

deriving instance Show    Move
deriving instance Eq      Move
deriving instance Enum    Move
deriving instance Bounded Move

move :: Move -> Point -> Point
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move West  (x, y) = (x - 1, y)
move East  (x, y) = (x + 1, y)

reachable :: Point -> [Point]
reachable p = map (flip move p) [minBound .. maxBound]

-- ----------------------------------------

type PointMap a = HM.HashMap Point a

data Grid a =
  Grid { _gMap      :: PointMap a
       , _gDefault  :: a
       , _gToChar   :: a -> Char
       , _gFromChar :: Char -> Maybe a
       }

mkGrid :: a -> (a -> Char) -> (Char -> Maybe a) -> [(Point, a)] -> Grid a
mkGrid def' to' fr' xs' =
  Grid { _gMap      = HM.fromList xs'
       , _gDefault  = def'
       , _gToChar   = to'
       , _gFromChar = fr'
       }

gridMap :: Lens' (Grid a) (PointMap a)
gridMap k g = (\new -> g {_gMap = new}) <$> k (_gMap g)

gridDefaultVal :: Lens' (Grid a) a
gridDefaultVal k g = (\new -> g {_gDefault = new}) <$> k (_gDefault g)

gridValChar :: Grid a -> Prism' Char a
gridValChar g = prism' (_gToChar g) (_gFromChar g)


lookupGrid :: Point -> Grid a -> a
lookupGrid p g =
  fromMaybe (g ^. gridDefaultVal) $ HM.lookup p (g ^. gridMap)

insertGrid :: Point -> a -> Grid a -> Grid a
insertGrid p v g = g & gridMap . at p .~ Just v

deleteGrid :: Point -> Grid a -> Grid a
deleteGrid p g = g & gridMap . at p .~ Nothing

insertListGrid :: [(Point, a)] -> Grid a -> Grid a
insertListGrid ps g = foldl' ins g ps
  where
    ins acc (p, v) = insertGrid p v acc

pointsGrid :: Grid a -> [Point]
pointsGrid g = g ^. gridMap . to HM.keys

elemsGrid :: Grid a -> [(Point, a)]
elemsGrid g = g ^. gridMap . to HM.toList

filterGrid :: (Point -> a -> Bool) -> Grid a -> Grid a
filterGrid p g = g & gridMap %~ HM.filterWithKey p

renderGrid :: Grid a -> [String]
renderGrid g =
  map toRow [minY..maxY]
  where
    -- read from bottom to top
    -- .1 take all points in map
    -- .2 wrap x- and y-coordinates in Max and Min monoid
    -- .3 mconcat: maximum/minimum in parallel
    -- .4 unwrap: max- and min-results
    --
    ((maxX, minX), (maxY, minY)) =
      ((getMax *** getMin) *** (getMax *** getMin))
      .
      mconcat
      .
      map ((Max &&& Min) *** (Max &&& Min))
      .
      HM.keys
      $
      g ^. gridMap

    toRow y = map toChar [minX..maxX]
      where
        toChar x = gridValChar g # lookupGrid (x, y) g

-- ----------------------------------------
