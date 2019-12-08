module Data.Intcode
  ( IntcodeProg
  , IntcodeRes
  , runIntcode
  , runIntcode'
  , runIntcodeProg
  , fromCVS
  , patchIntcode
  )
where

import           Control.Arrow ((***))
import           Control.Lens hiding (op)
import           Control.Monad.Except
import           Control.Monad.State.Strict

import qualified Data.IntMap.Strict as M
import           Data.IntMap.Strict (IntMap)

-- ----------------------------------------

type IntcodeProg = [Int]

type IntcodeRes = Either String Int

type Mem = IntMap Int

type Opcode = Int

data ICState = ICS { _pc  :: Int
                   , _mem :: Mem
                   }

type Action = ExceptT String (State ICState)

-- lenses

pc :: Lens' ICState Int
pc k ics = (\new -> ics {_pc = new}) <$> k (_pc ics)

mem :: Lens' ICState Mem
mem k ics = (\new -> ics {_mem = new}) <$> k (_mem ics)

-- run the machine

runIntcode :: IntcodeProg -> IntcodeRes
runIntcode p =
  either Left (const $ Right $ head . M.elems $ _mem r) e
  where
    (e, r) = runIntcodeProg p

runIntcode' :: IntcodeProg -> (String, IntcodeProg)
runIntcode' p =
  either id (const mempty) *** (M.elems . _mem) $ runIntcodeProg p


runIntcodeProg :: IntcodeProg -> (Either String (), ICState)
runIntcodeProg p = runState (runExceptT runProg) (initState p)

progToMem :: IntcodeProg -> Mem
progToMem = M.fromList . zip [0..]

initState :: IntcodeProg -> ICState
initState p = ICS 0 (progToMem p)

-- ----------------------------------------

runProg :: Action ()
runProg = do
  op <- getOpcode
  unless (op == 99) $
    execInstr op >> runProg

execInstr :: Opcode -> Action ()
execInstr op
  | Just f <- binOp op
              = do v1 <- getArg >>= getVal
                   v2 <- getArg >>= getVal
                   a3 <- getArg
                   putVal (f v1 v2) a3

  | otherwise = illegalOpcode op

binOp :: Opcode -> Maybe (Int -> Int -> Int)
binOp op = lookup op binOps
  where
    binOps = [ (1, (+))
             , (2, (*))
             ]

incrPc :: Action ()
incrPc = pc %= (+1)

putVal :: Int -> Int -> Action ()
putVal v a = do
  void (getVal a)    -- check for address violation
  mem . at a .= Just v

getVal :: Int -> Action Int
getVal a = do
  v <- use (mem . at a)
  maybe (addressViolation a) return v

getAddr :: Action Int
getAddr = use pc >>= getVal

getArg :: Action Int
getArg = do
  v <- getAddr
  incrPc
  return v

getOpcode :: Action Int
getOpcode = getArg

illegalOpcode :: Int -> Action a
illegalOpcode op =
  throwError $
    "illegal opcode " ++ show op

addressViolation :: Int -> Action a
addressViolation a = do
  m <- use mem
  let mx = maybe 0 fst $ M.lookupMax m
  throwError $
    "address violation: 0 <= pc <= " ++ show mx ++ ", but pc = " ++ show a

-- ----------------------------------------

fromCVS :: String -> IntcodeProg
fromCVS = read . ("[" ++) . (++ "]")

patchIntcode :: Int -> Int -> IntcodeProg -> IntcodeProg
patchIntcode v i p =
  px ++ [v] ++ drop 1 sx
  where
    (px, sx) = splitAt i p

-- ----------------------------------------

ex1, ex2 :: IntcodeProg
ex1 = fromCVS "1,9,10,3,2,3,11,0,99,30,40,50"
ex2 = fromCVS "1,1,1,4,99,5,6,0,99"

res1, res2 :: (String, IntcodeProg)
res1 = ("", [3500,9,10,70,2,3,11,0,99,30,40,50])
res2 = ("", [30,1,1,4,2,5,6,0,99])

test1, test2 :: Bool
test1 = runIntcode' ex1 == res1
test2 = runIntcode' ex2 == res2


-- ----------------------------------------
