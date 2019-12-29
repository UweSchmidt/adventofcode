{-# LANGUAGE RankNTypes #-}

module Data.Grid
  ( Point
  , Grid
  , mkGrid

  , gridMap         -- lenses
  , gridDefaultVal
  , gridValChar

  , lookupGrid
  , insertGrid
  , deleteGrid
  , filterGrid
  , renderGrid
  )
where

import Control.Arrow
import Control.Lens
import Data.Maybe
import Data.Semigroup

import qualified Data.HashMap.Strict as HM

-- ----------------------------------------

type Point      = (Int, Int)
type PointMap a = HM.HashMap Point a

data Grid a =
  Grid { _gMap      :: PointMap a
       , _gDefault  :: a
       , _gToChar   :: a -> Char
       , _gFromChar :: Char -> Maybe a
       }

mkGrid :: a -> (a -> Char) -> (Char -> Maybe a) -> Grid a
mkGrid def' to' fr' =
  Grid { _gMap      = HM.empty
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
insertGrid p v g = g & gridMap %~ HM.insert p v

deleteGrid :: Point -> Grid a -> Grid a
deleteGrid p g = g & gridMap %~ HM.delete p

filterGrid :: (a -> Bool) -> Grid a -> Grid a
filterGrid p g = g & gridMap %~ HM.filter p

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
