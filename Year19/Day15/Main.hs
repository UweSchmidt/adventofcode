{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/15


module Main where

import AOC.Prelude
import Data.Intcode     ( IntcodeProg
                        , ICState
                        , Status(..)
                        , status
                        , stdin
                        , stdout
                        , mkMachine
                        , runMachine0
                        , runMachine
                        , fromCVS
                        )

import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as S

-- ----------------------------------------

withTrace :: Bool
withTrace = False -- True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-15"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = fromCVS >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = fromCVS >>> solve2 >>> show

solve1 :: IntcodeProg -> Int
solve1 = snd . runGame . initGameState

solve2 :: IntcodeProg -> Int
solve2 prog = undefined

data Move      = North      | South | West      | East
data Tile      = Unexplored | Wall  | Visited   | Droid
data MoveStat  = WallSeen   | Moved | MovedToOxy

type Area      = M.HashMap Pos Tile
type Steps     = Int
type OpenTile  = ((Pos, Steps), ICState)
type OpenQueue = S.Seq OpenTile
type GameState = (OpenQueue, Area)

deriving instance Show Tile
deriving instance Eq   Tile
deriving instance Enum Tile

deriving instance Show Move
deriving instance Eq   Move
deriving instance Enum Move

deriving instance Show MoveStat
deriving instance Eq   MoveStat
deriving instance Enum MoveStat

isoMoveInt :: Iso' Move Int
isoMoveInt = iso ((+ 1) . fromEnum)
                 (toEnum . (\x -> x - 1))

isoTileChar :: Iso' Tile Char
isoTileChar = iso toC undefined
  where
    toC Unexplored = ' '
    toC Wall       = '#'
    toC Visited    = '.'
    toC Droid      = 'D'

isoMoveStatInt :: Iso' MoveStat Int
isoMoveStatInt = iso fromEnum toEnum

mv :: Move -> Pos -> Pos
mv North (x, y) = (x, y - 1)
mv South (x, y) = (x, y + 1)
mv West  (x, y) = (x - 1, y)
mv East  (x, y) = (x + 1, y)

-- --------------------

initArea :: Area
initArea = M.singleton origin Droid

lookupArea :: Pos -> Area -> Tile
lookupArea p = fromMaybe Unexplored . M.lookup p

insArea :: Pos -> Tile -> Area -> Area
insArea = M.insert

initQueue :: IntcodeProg -> OpenQueue
initQueue prog = S.singleton ((origin, 0), ics0)
  where
    ics0 = runMachine $ mkMachine [] prog

initGameState :: IntcodeProg -> GameState
initGameState prog = (initQueue prog, initArea)

runGame :: GameState -> (Pos, Steps)
runGame =
  either next id . nextGameState
  where
    next gs = trc (gsToString gs) $
              runGame gs

nextGameState :: GameState -> Either GameState (Pos, Steps)
nextGameState (os, area)
  | S.null os  = error "no more fields to visit"
  | otherwise  = either (Left . appendQueue) Right $ step4 ot area
  where
    appendQueue :: GameState -> GameState
    appendQueue = first (ots1 <>)

    -- this is a hack, there must be a simpler solution
    -- but ghc doesn't tell it to me
    (ot, ots1) = first (flip S.index 0) $ S.splitAt 1 os


-- try to move to all 4 directions

step4 :: OpenTile -> Area -> Either GameState (Pos, Steps)
step4 ot area0 = go (Left (mempty, area')) [North .. East]
  where
    area' = insArea (fst . fst $ ot) Visited area0

    go acc@(Right _)               _ms = acc
    go acc                          [] = acc
    go acc@(Left (_os, area)) (m : ms) = go (mergeRes acc res') ms
      where
        res' = step1 ot m area

mergeRes :: Either GameState (Pos, Steps)
         -> Either GameState (Pos, Steps)
         -> Either GameState (Pos, Steps)
mergeRes  r1@(Right _ )    _r2              = r1
mergeRes _r1               r2@(Right _)     = r2
mergeRes (Left (os1, _a1)) (Left (os2, a2)) = Left (os1 <> os2, a2)

step1 :: OpenTile -> Move -> Area -> Either GameState (Pos, Steps)
step1 ((p, i), ics) m area
  | Unexplored <- lookupArea p1 area =
      case mstatus of
        WallSeen   -> Left  (mempty, insArea p1 Wall  area)
        Moved      -> Left  (S.singleton ((p1, i1), ics1), insArea p1 Droid area)
        MovedToOxy -> Right (p1, i1)
  | otherwise       = Left  (mempty, area)
  where
    p1 = mv m p
    i1 = i + 1
    (mstatus, ics1) = step m ics

step :: Move -> ICState -> (MoveStat, ICState)
step m ics0
  | WaitForInput <- ics1 ^. status
  , [res]        <- ics1 ^. stdout = (isoMoveStatInt # res, ics1)
  | otherwise                      = error $ "droid status = " ++
                                             show (ics1 ^. status)

  where
    ics1 = runMachine
           ( ics0 & status .~ OK                   -- enable restart
                  & stdin  .~ [m ^. isoMoveInt]   -- set input to move
                  & stdout .~ []                   -- clear old output
           )

-- --------------------

gsToString :: GameState -> String
gsToString (ots, area) =
  unlines $
  renderArea area
  ++
  [ "open states = " ++ (show . fmap fst . foldr (:) [] $ ots)]

renderArea :: Area -> [String]
renderArea =
  renderBoard ' ' (^. isoTileChar)

-- ----------------------------------------

inp :: String
inp = "3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,101,0,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,101,0,1035,1040,101,0,1038,1043,1002,1037,1,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,102,1,1038,1043,1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,39,1032,1006,1032,165,1008,1040,39,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,74,1044,1106,0,224,1101,0,0,1044,1106,0,224,1006,1044,247,102,1,1039,1034,102,1,1040,1035,1002,1041,1,1036,1002,1043,1,1038,1002,1042,1,1037,4,1044,1105,1,0,15,82,44,17,88,23,99,42,83,68,98,44,75,66,15,14,89,20,34,89,18,1,84,70,84,69,55,89,65,10,76,63,83,20,80,60,48,47,98,65,82,84,68,89,52,76,63,86,61,75,4,52,82,79,24,28,93,94,95,40,66,76,81,50,31,94,81,54,19,91,92,61,18,28,79,77,43,69,19,5,87,35,14,23,94,10,76,32,73,90,20,86,67,90,80,8,86,25,89,89,26,48,37,81,49,25,87,92,17,46,84,96,95,60,79,52,19,13,93,30,93,99,17,13,89,96,36,93,81,89,18,2,97,42,45,63,86,20,26,76,97,29,75,56,7,97,93,2,78,9,79,8,57,84,38,80,53,98,89,34,71,85,17,96,50,31,93,64,7,81,72,85,32,83,31,99,69,90,88,33,88,81,41,80,46,47,93,75,34,95,8,98,24,7,76,77,17,23,95,72,82,98,24,91,95,50,38,92,91,32,95,40,77,80,84,82,7,90,23,13,92,40,82,37,80,56,24,79,99,64,90,55,58,46,33,4,88,92,7,84,19,45,16,75,94,40,93,21,87,94,79,39,83,52,92,14,21,77,82,5,84,85,48,75,19,26,91,28,99,87,81,86,24,53,98,52,25,2,75,39,82,24,51,77,47,92,53,94,27,34,85,22,25,36,92,79,29,2,10,19,95,13,96,82,56,99,3,91,62,99,43,49,7,91,96,77,89,7,99,86,24,92,57,24,49,3,96,77,35,75,11,86,21,1,82,67,84,90,75,96,9,83,1,47,78,7,98,30,11,88,52,78,58,98,47,90,46,78,14,77,88,3,97,87,70,75,24,98,5,80,87,93,95,22,37,59,85,23,41,89,91,9,7,90,61,3,95,96,92,25,57,47,38,88,14,15,84,31,79,20,79,77,22,33,90,70,89,78,51,24,93,81,21,79,82,17,75,88,78,26,87,24,38,96,50,81,6,46,93,39,91,92,81,39,91,5,79,58,9,87,50,83,63,87,2,29,92,37,81,55,59,99,91,35,9,96,18,82,66,4,89,44,87,92,6,79,88,9,9,63,88,71,77,91,35,29,87,87,51,20,94,19,57,93,72,89,4,77,10,87,20,67,80,79,71,1,75,28,87,88,87,55,37,80,85,5,55,5,97,12,62,88,82,27,6,99,93,42,91,16,75,80,6,20,96,6,84,6,46,84,23,92,93,32,90,79,3,54,7,97,92,92,33,79,9,5,10,90,76,19,76,1,85,83,58,2,91,83,77,59,63,89,26,97,67,96,52,88,62,65,23,91,94,51,31,80,24,5,72,40,81,9,85,79,12,98,44,45,81,25,30,60,5,76,92,62,18,32,78,25,16,76,97,18,96,39,96,60,78,78,47,99,48,82,98,57,96,98,73,89,18,12,91,8,66,85,57,94,22,76,88,98,39,58,96,91,61,98,89,7,77,91,13,96,20,86,2,88,91,27,75,32,29,79,51,81,4,86,10,37,79,84,67,49,75,20,94,91,23,33,92,38,91,37,76,79,55,91,43,80,25,98,77,91,88,44,15,97,45,3,86,73,87,30,91,62,80,80,16,85,54,88,54,75,88,65,18,85,22,90,79,36,10,77,86,65,30,38,85,3,90,44,48,75,81,80,32,59,90,91,41,95,72,79,11,66,26,96,20,4,68,88,23,95,31,98,12,98,56,94,95,80,68,78,39,79,93,85,55,96,4,77,14,80,46,95,84,84,6,93,35,95,46,85,92,81,69,85,92,87,0,0,21,21,1,10,1,0,0,0,0,0,0"

test1 :: IO ()
test1 = undefined $ fromCVS inp

res1 :: Int
res1 = 272

res2 :: Int
res2 = undefined

-- ----------------------------------------
