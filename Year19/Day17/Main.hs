{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/17


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

import Data.Grid

-- ----------------------------------------

withTrace :: Bool
withTrace = False -- True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-17"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = fromCVS >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = fromCVS >>> solve2 >>> show

solve1 :: IntcodeProg -> Int
solve1 prog =
  sum . map (uncurry (*)) . filterIntersections $ sc
  where
    sc = newScreen . fst . asciiCode [] $ prog

solve2 :: IntcodeProg -> Int
solve2 prog =
  last $ out2
  where
    out2  = fst . asciiInpCode inp2 $ prog2
    prog2 = (2 :) . drop 1 $ prog  -- patch 1. instr to a multiply
    inp2  = unlines $
            showMacros macros
            ++
            ["n"]  -- no trace please


-- --------------------

data Tile   = Scaffold  | ScaffoldX
            | OpenSpace | Outside
            | VacuumRobot (Maybe Move)

type Screen = Grid Tile

type Cmd    = (CD, Int)
data CD     = L | R

type Macro  = (MC, Cmds)
data MC     = A | B | C

type Cmds   = [Cmd]
type Macros = [Macro]

-- --------------------

deriving instance Show Tile
deriving instance Eq   Tile

deriving instance Show CD
deriving instance Eq   CD

deriving instance Show MC
deriving instance Eq   MC

-- --------------------
--
-- part 1

filterIntersections :: Screen -> [Point]
filterIntersections sc =
  pointsGrid . filterGrid isO $ sc
  where
    isO :: Point -> Tile -> Bool
    isO p' Scaffold = scaff4 p'
    isO _  _        = False

    scaff4 p' = length ps == 4
      where
        ps = filter (== Scaffold) . map (flip lookupGrid sc) $ reachable p'

-- for testing only

markIntersections :: Screen -> Screen
markIntersections sc =
  insertListGrid (map (, ScaffoldX) ps) sc
  where
    ps = filterIntersections sc

-- --------------------
--
-- part 2

getVacuumRobot :: Screen -> Maybe (Point, Move)
getVacuumRobot sc
  | [(p, VacuumRobot (Just m))] <- ps = Just (p, m)
  | otherwise                         = Nothing
  where
    ps = elemsGrid $ filterGrid isVR sc

    isVR _p' (VacuumRobot (Just _mv)) = True
    isVR _   _                        = False

drawPath :: Cmds -> ((Point, Move), Screen) -> ((Point, Move), Screen)
drawPath cs pms = foldl' drawCmd pms cs
  where
    drawCmd :: ((Point, Move), Screen) -> Cmd -> ((Point, Move), Screen)
    drawCmd ((p, m), sc) (cd, l) = ((p', m'), sc')
      where
        (m', d')  = case cd of
                      L -> (turnCCW m, l)
                      R -> (turnCW  m, l)
        moves     = replicate d' m'
        (p', sc') = foldl' drawStep (p, sc) moves

        drawStep :: (Point, Screen) -> Move -> (Point, Screen)
        drawStep (p', sc') m' = (p'', sc'')
          where
            p''  = move m' p'
            sc'' = insertGrid p'' Scaffold sc'

-- --------------------

newScreen :: String -> Screen
newScreen =
  mkGrid Outside tileToChar charToTile . parseScreen

tileToChar :: Tile -> Char
tileToChar Scaffold = '#'
tileToChar ScaffoldX = 'O'
tileToChar OpenSpace = '.'
tileToChar Outside = '?'
tileToChar (VacuumRobot v) =
  maybe 'X' mto v
  where
    mto North = '^'
    mto South = 'v'
    mto East  = '>'
    mto West  = '<'

charToTile :: Char -> Maybe Tile
charToTile '#' = Just Scaffold
charToTile 'O' = Just ScaffoldX
charToTile '.' = Just OpenSpace
charToTile '?' = Just Outside
charToTile 'X' = Just $ VacuumRobot Nothing
charToTile '^' = Just $ VacuumRobot $ Just North
charToTile 'v' = Just $ VacuumRobot $ Just South
charToTile '>' = Just $ VacuumRobot $ Just East
charToTile '<' = Just $ VacuumRobot $ Just West
charToTile _   = Nothing

-- --------------------

parseScreen :: String -> [(Point, Tile)]
parseScreen =
  concat . zipWith parseLine [0..] . lines
  where
    parseLine :: Int -> [Char] -> [(Point, Tile)]
    parseLine y =
      catMaybes . zipWith parseChar [0..]
      where
        parseChar :: Int -> Char -> Maybe (Point, Tile)
        parseChar x c =  ((x, y),) <$> charToTile c

asciiCode :: [Int] -> IntcodeProg -> ([Char], ICState)
asciiCode inp' prog =
  intCode inp' prog & _1 %~ map toEnum

intCode :: [Int] -> IntcodeProg -> ([Int], ICState)
intCode inp' prog =
  (ics0 ^. stdout, ics0)
  where
    ics0 = runMachine $ mkMachine inp' prog

asciiInpCode :: [Char] -> IntcodeProg -> ([Int], ICState)
asciiInpCode inp' = intCode (map fromEnum inp')

-- --------------------
--
-- parsers

parseCmds :: String -> Cmds
parseCmds = parseInput pCmds

pCmds :: Parser Cmds
pCmds = sepBy1 pCmd (single ',')

pCmd :: Parser Cmd
pCmd = pDir <*> (single ',' *> natural)

pDir :: Parser (Int -> Cmd)
pDir = (single 'L' *> return (L,))
       <|>
       (single 'R' *> return (R,))

-- pretty printers

showCmds :: Cmds -> String
showCmds = intercalate "," . map showCmd
  where
    showCmd (L, d) = "L," ++ show d
    showCmd (R, d) = "R," ++ show d

showMacros :: Macros -> [String]
showMacros mcs =
  [ intercalate "," $ map (show . fst) mcs ]
  ++
  (map (showCmds . snd) $ nub mcs)

joinMacros :: Macros -> Cmds
joinMacros = concatMap snd

-- ----------------------------------------

test6, test5, test3, test2, test1 :: IO ()
test1 = putStrLn . unlines . renderGrid . markIntersections . newScreen $ ex1
test2 = putStrLn . unlines . renderGrid . markIntersections . newScreen .
        fst . asciiCode [] $ fromCVS inp
test3 = putStrLn . unlines $ showMacros macros

test4 :: Bool
test4 = cmds == joinMacros macros

test5 = putStrLn . unlines . renderGrid $ sc3
  where
    sc0 = newScreen . fst . asciiCode [] $ fromCVS inp
    sc1 = filterGrid (const (/= Scaffold)) sc0
    sc2 = insertGrid p (VacuumRobot $ Just m) sc1
    ((_p1, _m1), sc3) = drawPath cmds' ((p, m), sc2)
    Just (p, m) = getVacuumRobot sc0
    cmds' = parseCmds path

test6 = putStrLn . show . last $ out2
  where
    out2  = fst . asciiInpCode inp2 $ prog2
    prog2 = (2 :) . drop 1 $ fromCVS inp
    inp2  = unlines $ showMacros macros ++ ["n"]

ex1 :: String
ex1 = unlines
  [ "..#.........."
  , "..#.........."
  , "#######...###"
  , "#.#...#...#.#"
  , "#############"
  , "..#...#...#.."
  , "..#####...^.."
  ]

rs1 :: Int
rs1 = 76

inp :: String
inp = "1,330,331,332,109,3664,1102,1,1182,15,1101,0,1455,24,1001,0,0,570,1006,570,36,102,1,571,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,15,1,15,1008,15,1455,570,1006,570,14,21101,58,0,0,1105,1,786,1006,332,62,99,21102,333,1,1,21102,1,73,0,1105,1,579,1102,1,0,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,102,1,574,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1105,1,81,21101,340,0,1,1105,1,177,21102,1,477,1,1106,0,177,21101,514,0,1,21102,1,176,0,1105,1,579,99,21102,1,184,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,102,1,572,1182,21102,375,1,1,21101,211,0,0,1106,0,579,21101,1182,11,1,21102,222,1,0,1105,1,979,21102,1,388,1,21101,233,0,0,1105,1,579,21101,1182,22,1,21102,1,244,0,1105,1,979,21102,401,1,1,21101,0,255,0,1106,0,579,21101,1182,33,1,21101,266,0,0,1106,0,979,21101,414,0,1,21102,277,1,0,1105,1,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,0,1182,1,21102,313,1,0,1105,1,622,1005,575,327,1102,1,1,575,21102,1,327,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,46,22,0,109,4,2102,1,-3,587,20102,1,0,-1,22101,1,-3,-3,21102,1,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1106,0,597,109,-4,2105,1,0,109,5,2102,1,-4,629,21002,0,1,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,652,21001,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,702,0,0,1105,1,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,731,1,0,1106,0,786,1106,0,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21102,1,756,0,1105,1,786,1105,1,774,21202,-1,-11,1,22101,1182,1,1,21102,774,1,0,1106,0,622,21201,-3,1,-3,1105,1,640,109,-5,2106,0,0,109,7,1005,575,802,20101,0,576,-6,21002,577,1,-5,1105,1,814,21101,0,0,-1,21101,0,0,-5,21102,0,1,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,47,-3,22201,-6,-3,-3,22101,1455,-3,-3,2102,1,-3,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21102,1,1,-1,1106,0,924,1205,-2,873,21101,0,35,-4,1105,1,924,1202,-3,1,878,1008,0,1,570,1006,570,916,1001,374,1,374,2101,0,-3,895,1101,2,0,0,1201,-3,0,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,921,21001,0,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,47,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,47,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21101,0,973,0,1105,1,786,99,109,-7,2105,1,0,109,6,21102,0,1,-4,21101,0,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1105,1,1041,21101,0,-4,-2,1105,1,1041,21101,-5,0,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2102,1,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1105,1,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21101,0,439,1,1105,1,1150,21102,1,477,1,1106,0,1150,21102,1,514,1,21102,1,1149,0,1105,1,579,99,21101,0,1157,0,1106,0,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,2102,1,-5,1176,2101,0,-4,0,109,-6,2106,0,0,14,5,42,1,3,1,42,1,3,1,42,1,3,1,42,1,3,1,42,1,3,1,42,1,3,1,5,13,24,1,3,1,5,1,11,1,24,1,1,5,3,1,1,13,22,1,1,1,1,1,1,1,3,1,1,1,9,1,1,1,22,1,1,1,1,13,5,1,1,1,22,1,1,1,3,1,3,1,1,1,3,1,5,1,1,1,22,7,3,1,1,1,1,11,24,1,7,1,1,1,1,1,1,1,5,1,18,9,7,1,1,5,5,5,14,1,15,1,3,1,11,1,14,1,15,1,3,1,11,1,14,1,15,1,3,1,11,1,6,9,5,11,3,1,11,1,6,1,13,1,13,1,11,1,6,1,13,1,9,7,9,1,6,1,13,1,9,1,3,1,1,1,9,1,6,1,13,13,1,1,1,1,9,8,23,1,1,1,1,1,1,1,16,1,23,5,1,1,16,1,25,1,3,1,16,1,25,1,3,1,16,1,25,1,3,1,16,1,25,1,3,1,16,1,25,1,3,1,16,7,19,1,3,1,22,1,19,1,3,1,22,1,19,5,22,1,46,1,46,1,46,1,46,1,46,5,46,1,46,1,46,1,46,1,46,1,46,1,46,1,46,13,24"

res1 :: Int
res1 = undefined

res2 :: Int
res2 = undefined

-- converted by hand from screen output
path :: String
path = "L,6,R,8,L,4,R,8,L,12,L,12,R,10,L,4,L,12,R,10,L,4,L,12,L,6,L,4,L,4,L,12,R,10,L,4,L,12,L,6,L,4,L,4,L,12,R,10,L,4,L,12,L,6,L,4,L,4,L,6,R,8,L,4,R,8,L,12,L,6,R,8,L,4,R,8,L,12"

cmds :: Cmds
cmds = parseCmds path

-- solution found by playing with Data.List.Split. splitOn
-- and looking for multiple occurences of a prefix of cmds

macA, macB, macC :: Macro
macA = (A, [(L, 6),(R, 8),(L, 4),(R, 8),(L, 12)])
macB = (B, [(L, 12), (R, 10),(L, 4)])
macC = (C, [(L, 12),(L, 6),(L, 4),(L, 4)])

macros :: Macros
macros = [macA, macB, macB, macC, macB, macC, macB, macC, macA, macA]

-- ----------------------------------------
