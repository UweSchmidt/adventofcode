{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}

-- solution for
-- http://adventofcode.com/2019/day/11


module Main where

import Util.Main1 (main12)
import Data.Intcode -- ( IntcodeProg
                    -- , runIntcode2'
                    -- , fromCVS
                    -- )
import Control.Arrow ((>>>), second)
import Control.Lens

import qualified Data.Map as M

-- ----------------------------------------

main :: IO ()
main = main12 "2019-11"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = fromCVS >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = fromCVS >>> solve2 >>> id

solve1 :: IntcodeProg -> Int
solve1 prog =
   M.size . snd $ robotRun initPState (initState prog)

solve2 :: IntcodeProg -> String
solve2 prog =
  unlines . toBanner . snd $ robotRun initPState' (initState prog)
  where
    initPState' = initPState & _2 %~ insertHull origin White

type Pos    = (Int, Int)
type Hull   = M.Map Pos Color
data Color  = Black | White
data Turn   = LeftTurn | RightTurn
data Dir    = North | East | South | West
type Move   = (Color, Turn)
type Robby  = (Pos, Dir)
type PState = (Robby, Hull)

deriving instance Show Color
deriving instance Eq   Color
deriving instance Enum Color

deriving instance Show    Dir
deriving instance Eq      Dir
deriving instance Enum    Dir
deriving instance Bounded Dir

deriving instance Show Turn
deriving instance Eq   Turn
deriving instance Enum Turn

origin :: Pos
origin = (0,0)

initHull :: Hull
initHull = M.empty

insertHull :: Pos -> Color -> Hull -> Hull
insertHull = M.insert

lookupHull :: Pos -> Hull -> Color
lookupHull k = maybe Black id . M.lookup k

initRobby :: Robby
initRobby = (origin, North)

initPState :: PState
initPState = (initRobby, initHull)

initState :: IntcodeProg -> ICState
initState p = runMachine $ mkMachine [] p

move :: Color -> Turn -> PState -> PState
move color turn ((pos, dir), hull) = ((pos', dir'), hull')
  where
    hull'  = insertHull pos color hull
    dir'   = changeDir turn dir
    pos'   = mv dir' pos

changeDir :: Turn -> Dir -> Dir
changeDir LeftTurn d
  | d == minBound  = maxBound
  | otherwise      = toEnum $ fromEnum d - 1
changeDir RightTurn d
  | d == maxBound  = minBound
  | otherwise      = toEnum $ fromEnum d + 1

mv :: Dir -> Pos -> Pos
mv North (x, y) = (x, y - 1)
mv East  (x, y) = (x + 1, y)
mv South (x, y) = (x, y + 1)
mv West  (x, y) = (x - 1, y)

robotRun :: PState -> ICState -> PState
robotRun ps ics
  | Just s <- mrs = uncurry robotRun s
  | otherwise     = ps
  where
    mrs = robotStep ps ics

robotStep :: PState -> ICState -> Maybe (PState, ICState)
robotStep ps@((pos, _dir), h) ics
  | Just (color, turn) <- mmv = Just (move color turn ps, ics')
  | otherwise                 = Nothing
  where
    (mmv, ics') = step ics (lookupHull pos h)

step :: ICState -> Color -> (Maybe Move, ICState)
step ics0 c
  | OutputWritten <- status'
  , [color, turn] <- stdout' = ( Just (toEnum color, toEnum turn)
                               , ics' & stdout .~ []  -- clear output
                               )
  | Terminated    <- status' = ( Nothing
                               , ics'
                               )
  | otherwise                = error $ show ics'
  where
    ics     = ics0 & stdin  .~ [fromEnum c]    -- set input in Intcode machine
    ics'    = runMachine0 . runMachine0 $ ics  -- start machine 2 times
    status' = ics' ^. status                   -- for generating 2 ouput values
    stdout' = ics' ^. stdout

toBanner :: Hull -> [String]
toBanner h =
  map toRow [0..maxY]
  where
    maxX = maximum . map fst . M.keys $ h
    maxY = maximum . map snd . M.keys $ h

    toRow y = map toChar [0..maxX]
      where
        toChar x = toC $ lookupHull (x, y) h
          where
            toC Black = '.'
            toC White = '#'

-- ----------------------------------------

inp :: String
inp = "3,8,1005,8,337,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,29,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,51,1,1008,18,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,76,1006,0,55,1,1108,6,10,1,108,15,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,110,2,1101,13,10,1,101,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,139,1006,0,74,2,107,14,10,1,3,1,10,2,1104,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,177,2,1108,18,10,2,1108,3,10,1,109,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,210,1,1101,1,10,1,1007,14,10,2,1104,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,244,1,101,3,10,1006,0,31,1006,0,98,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,277,1006,0,96,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,302,1,3,6,10,1006,0,48,2,101,13,10,2,2,9,10,101,1,9,9,1007,9,1073,10,1005,10,15,99,109,659,104,0,104,1,21101,937108976384,0,1,21102,354,1,0,1105,1,458,21102,1,665750077852,1,21101,0,365,0,1105,1,458,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,21478178856,0,1,21101,412,0,0,1105,1,458,21102,3425701031,1,1,21102,1,423,0,1106,0,458,3,10,104,0,104,0,3,10,104,0,104,0,21102,984458351460,1,1,21102,1,446,0,1105,1,458,21101,0,988220908388,1,21101,457,0,0,1105,1,458,99,109,2,22101,0,-1,1,21102,1,40,2,21101,489,0,3,21101,479,0,0,1105,1,522,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,484,485,500,4,0,1001,484,1,484,108,4,484,10,1006,10,516,1102,0,1,484,109,-2,2105,1,0,0,109,4,1201,-1,0,521,1207,-3,0,10,1006,10,539,21102,1,0,-3,21201,-3,0,1,21202,-2,1,2,21101,1,0,3,21101,558,0,0,1105,1,563,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,586,2207,-4,-2,10,1006,10,586,22102,1,-4,-4,1106,0,654,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,1,605,0,1106,0,563,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,624,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,646,22101,0,-1,1,21102,646,1,0,106,0,521,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"

res1 :: Int
res1 = 1863

res2 :: String
res2 = "BLULZJLZ"   -- as ASCII graphic

-- ----------------------------------------
