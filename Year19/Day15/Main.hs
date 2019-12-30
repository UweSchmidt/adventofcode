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
                        , runMachine
                        , fromCVS
                        )

import qualified Data.HashMap.Strict as M
import qualified Data.Sequence       as S

-- ----------------------------------------

withTrace :: Bool
withTrace = False -- True

trc :: String -> a -> a
trc = trace' withTrace

trcGameState :: Game ()
trcGameState = when withTrace $ do
  gs <- use id
  trace (gsToString gs) $ return ()

-- --------------------

main :: IO ()
main = main12 "2019-15"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = fromCVS >>> solve1 >>> either id show

captcha2 :: String -> String
captcha2 = fromCVS >>> solve2 >>> either id show

solve1 :: IntcodeProg -> Either String Int
solve1 prog =
  either (Left . show) (Right . snd . fst) $ res
  where
    (res, _gs) = runGame runGame1M (initGameState prog)

solve2 :: IntcodeProg -> Either String Int
solve2 prog =
  either (Left . show) Right $ res
  where
    (res, _gs) = runGame runGame2M (initGameState prog)

-- --------------------

data Move      = North      | South | West      | East
data Tile      = Unexplored | Wall  | Visited   | Droid | Oxygen
data MoveStat  = WallSeen   | Moved | MovedToOxy

type Area      = M.HashMap Pos Tile
type Steps     = Int
type OpenTile  = ((Pos, Steps), ICState)
type OpenQueue = S.Seq OpenTile
type GameState = (OpenQueue, Area, Steps)

data GameExc   = OxygenFound OpenTile
               | AreaScanCompleted Steps
               | Impossible String

type Game = ExceptT GameExc (State GameState)

deriving instance Show Tile
deriving instance Eq   Tile
deriving instance Enum Tile

deriving instance Show    Move
deriving instance Eq      Move
deriving instance Enum    Move
deriving instance Bounded Move

deriving instance Show MoveStat
deriving instance Eq   MoveStat
deriving instance Enum MoveStat

deriving instance Show GameExc

-- --------------------
--
-- start monadic computation

runGame :: Game a -> GameState -> (Either GameExc a, GameState)
runGame cmd st0 = runState (runExceptT cmd) st0

-- basic monadic actions

-- pos with oxygen found

oxygenFound :: OpenTile -> Game a
oxygenFound = throwError . OxygenFound

-- no more positions in queue

areaScanCompleted :: Game a
areaScanCompleted = do
  use steps >>= throwError . AreaScanCompleted

-- something went wrong

impossible :: String -> Game a
impossible = throwError . Impossible

-- e.g. no oxygen source

noOxygenFound :: Game a
noOxygenFound = impossible "no oxygen found"

-- put an open tile into queue

queue :: OpenTile -> Game ()
queue ot = openQueue %= (<> S.singleton ot)

-- get next queued tile

dequeue :: Game OpenTile
dequeue =
  do q <- use openQueue
     if S.null q
       then areaScanCompleted
       else do -- this is a hack, there must be a simpler solution
               -- but ghc doesn't tell it to me
               let (ot, ots1) = first (flip S.index 0) $ S.splitAt 1 q
               openQueue .= ots1
               return ot

-- update area position

setTile :: Pos -> Tile -> Game ()
setTile p t = area %= insArea p t

-- update step counter

setSteps :: Steps -> Game ()
setSteps s = steps .= s

-- --------------------

openQueue :: Lens' GameState OpenQueue
openQueue = _1

area :: Lens' GameState Area
area = _2

steps :: Lens' GameState Steps
steps = _3

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
    toC Oxygen     = 'O'

isoMoveStatInt :: Iso' MoveStat Int
isoMoveStatInt = iso fromEnum toEnum

move :: Move -> Pos -> Pos
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move West  (x, y) = (x - 1, y)
move East  (x, y) = (x + 1, y)

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
initGameState prog = (initQueue prog, initArea, 0)

-- --------------------

runGame1M :: Game OpenTile
runGame1M =
  (game1M >> noOxygenFound)
    `catchError`
    checkOxygenFound
  where
    checkOxygenFound (OxygenFound ot) = return ot
    checkOxygenFound _e               = noOxygenFound

    game1M :: Game ()
    game1M = forever $ do
      trcGameState
      dequeue >>= step4M

runGame2M :: Game Steps
runGame2M = do
  ((pos, _s), ics) <- runGame1M
  area      %= M.filter (== Wall)           -- reset visited fields
  steps     .= 0                            -- reset steps counter
  openQueue .= S.singleton ((pos, 0), ics)  -- init new queue of open states
                                            -- with oxygen pos
  (game2O >> areaScanCompleted)
    `catchError`
    checkScanCompleted
  where
    checkScanCompleted (AreaScanCompleted s) = return s
    checkScanCompleted e                     = impossible . show $ e

    game2O :: Game ()
    game2O = forever $ do
      trcGameState
      dequeue >>= step4O

step4M :: OpenTile -> Game ()
step4M = step4M' Visited

step4O :: OpenTile -> Game ()
step4O = step4M' Oxygen

step4M' :: Tile -> OpenTile -> Game ()
step4M' t ot = do
  setTile  (fst . fst $ ot) t              -- mark area as visited/oxygen filled
  setSteps (snd . fst $ ot)                -- store # of steps to reach this pos
  mapM_ (step1M ot) [minBound .. maxBound] -- insert neighbours into open queue

step1M :: OpenTile -> Move -> Game ()
step1M ((p, i), ics) m = do
  t <- lookupArea p1 <$> use area
  case t of
    Unexplored -> do
      (mstatus, ics1) <- stepM m ics
      let ot1 = (o1, ics1)
      case mstatus of
        WallSeen   -> do setTile p1 Wall

        Moved      -> do setTile p1 Droid
                         queue ot1

        MovedToOxy -> do setTile p1 Oxygen
                         queue ot1
                         oxygenFound ot1

    _otherTile -> return ()
  where
    o1 = (p1, i1)
    p1 = move m p
    i1 = i + 1

stepM :: Move -> ICState -> Game (MoveStat, ICState)
stepM m ics0
  | WaitForInput <- ics1 ^. status
  , [res]        <- ics1 ^. stdout = return (isoMoveStatInt # res, ics1)
  | otherwise                      = impossible $
                                       "droid status = " ++
                                       show (ics1 ^. status)

  where
    ics1 = runMachine
           ( ics0 & status .~ OK                   -- enable restart
                  & stdin  .~ [m ^. isoMoveInt]   -- set input to move
                  & stdout .~ []                   -- clear old output
           )

-- --------------------

gsToString :: GameState -> String
gsToString gs =
  unlines $
  renderArea (gs ^. area)
  ++
  [ "open states = " ++ (show . fmap fst . foldr (:) [] $ gs ^. openQueue)]

renderArea :: Area -> [String]
renderArea =
  renderBoard ' ' (^. isoTileChar)

-- ----------------------------------------

inp :: String
inp = "3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,101,0,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1105,1,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,101,0,1035,1040,101,0,1038,1043,1002,1037,1,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,102,1,1038,1043,1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,39,1032,1006,1032,165,1008,1040,39,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,74,1044,1106,0,224,1101,0,0,1044,1106,0,224,1006,1044,247,102,1,1039,1034,102,1,1040,1035,1002,1041,1,1036,1002,1043,1,1038,1002,1042,1,1037,4,1044,1105,1,0,15,82,44,17,88,23,99,42,83,68,98,44,75,66,15,14,89,20,34,89,18,1,84,70,84,69,55,89,65,10,76,63,83,20,80,60,48,47,98,65,82,84,68,89,52,76,63,86,61,75,4,52,82,79,24,28,93,94,95,40,66,76,81,50,31,94,81,54,19,91,92,61,18,28,79,77,43,69,19,5,87,35,14,23,94,10,76,32,73,90,20,86,67,90,80,8,86,25,89,89,26,48,37,81,49,25,87,92,17,46,84,96,95,60,79,52,19,13,93,30,93,99,17,13,89,96,36,93,81,89,18,2,97,42,45,63,86,20,26,76,97,29,75,56,7,97,93,2,78,9,79,8,57,84,38,80,53,98,89,34,71,85,17,96,50,31,93,64,7,81,72,85,32,83,31,99,69,90,88,33,88,81,41,80,46,47,93,75,34,95,8,98,24,7,76,77,17,23,95,72,82,98,24,91,95,50,38,92,91,32,95,40,77,80,84,82,7,90,23,13,92,40,82,37,80,56,24,79,99,64,90,55,58,46,33,4,88,92,7,84,19,45,16,75,94,40,93,21,87,94,79,39,83,52,92,14,21,77,82,5,84,85,48,75,19,26,91,28,99,87,81,86,24,53,98,52,25,2,75,39,82,24,51,77,47,92,53,94,27,34,85,22,25,36,92,79,29,2,10,19,95,13,96,82,56,99,3,91,62,99,43,49,7,91,96,77,89,7,99,86,24,92,57,24,49,3,96,77,35,75,11,86,21,1,82,67,84,90,75,96,9,83,1,47,78,7,98,30,11,88,52,78,58,98,47,90,46,78,14,77,88,3,97,87,70,75,24,98,5,80,87,93,95,22,37,59,85,23,41,89,91,9,7,90,61,3,95,96,92,25,57,47,38,88,14,15,84,31,79,20,79,77,22,33,90,70,89,78,51,24,93,81,21,79,82,17,75,88,78,26,87,24,38,96,50,81,6,46,93,39,91,92,81,39,91,5,79,58,9,87,50,83,63,87,2,29,92,37,81,55,59,99,91,35,9,96,18,82,66,4,89,44,87,92,6,79,88,9,9,63,88,71,77,91,35,29,87,87,51,20,94,19,57,93,72,89,4,77,10,87,20,67,80,79,71,1,75,28,87,88,87,55,37,80,85,5,55,5,97,12,62,88,82,27,6,99,93,42,91,16,75,80,6,20,96,6,84,6,46,84,23,92,93,32,90,79,3,54,7,97,92,92,33,79,9,5,10,90,76,19,76,1,85,83,58,2,91,83,77,59,63,89,26,97,67,96,52,88,62,65,23,91,94,51,31,80,24,5,72,40,81,9,85,79,12,98,44,45,81,25,30,60,5,76,92,62,18,32,78,25,16,76,97,18,96,39,96,60,78,78,47,99,48,82,98,57,96,98,73,89,18,12,91,8,66,85,57,94,22,76,88,98,39,58,96,91,61,98,89,7,77,91,13,96,20,86,2,88,91,27,75,32,29,79,51,81,4,86,10,37,79,84,67,49,75,20,94,91,23,33,92,38,91,37,76,79,55,91,43,80,25,98,77,91,88,44,15,97,45,3,86,73,87,30,91,62,80,80,16,85,54,88,54,75,88,65,18,85,22,90,79,36,10,77,86,65,30,38,85,3,90,44,48,75,81,80,32,59,90,91,41,95,72,79,11,66,26,96,20,4,68,88,23,95,31,98,12,98,56,94,95,80,68,78,39,79,93,85,55,96,4,77,14,80,46,95,84,84,6,93,35,95,46,85,92,81,69,85,92,87,0,0,21,21,1,10,1,0,0,0,0,0,0"

res1 :: Int
res1 = 272

res2 :: Int
res2 = 398

-- ----------------------------------------
