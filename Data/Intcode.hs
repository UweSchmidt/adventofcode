{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Intcode
  ( IntcodeProg
  , IntcodeRes
  , Stdin
  , Stdout
  , runIntcode
  , runIntcode'
  , runIntcode2
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
type IntcodeRes  = Either String Int
type IntcodeRes2 = ((String, IntcodeProg), (Stdin, Stdout))
type Stdin       = [Int]
type Stdout      = [Int]

type Mem         = IntMap Int
type Addr        = Int
data Opcode      = Add | Mul | Input | Output | Halt
data ParamMode   = PositionMode | ImmediateMode
type Instr       = (Opcode, (ParamMode, ParamMode, ParamMode))

data ICState' m  = ICS { _pc     :: Int
                       , _mem    :: m
                       , _stdin  :: [Int]
                       , _stdout :: [Int]
                       }
type ICState     = ICState' Mem

type Action      = ExceptT String (State ICState)

deriving instance Show Opcode
deriving instance Enum Opcode
deriving instance Eq   Opcode
deriving instance Show ParamMode
deriving instance Enum ParamMode
deriving instance Show a => Show (ICState' a)
deriving instance Functor ICState'

-- --------------------
--
-- lenses

pc :: Lens' ICState Int
pc k ics = (\new -> ics {_pc = new}) <$> k (_pc ics)

mem :: Lens' ICState Mem
mem k ics = (\new -> ics {_mem = new}) <$> k (_mem ics)

stdin :: Lens' ICState [Int]
stdin k ics = (\new -> ics {_stdin = new}) <$> k (_stdin ics)

stdout :: Lens' ICState [Int]
stdout k ics = (\new -> ics {_stdout = new}) <$> k (_stdout ics)

-- --------------------
--
-- run the machine

runIntcode :: IntcodeProg -> IntcodeRes
runIntcode p =
  either Left (const $ Right $ head . M.elems $ _mem r) e
  where
    (e, r) = runIntcodeProg p

runIntcode' :: IntcodeProg -> (String, IntcodeProg)
runIntcode' p =
  either id (const mempty) *** (M.elems . _mem) $ runIntcodeProg p

runIntcode2 :: Stdin -> IntcodeProg -> IntcodeRes2
runIntcode2 inp p = toRes $ runIntcodeProgStdin inp p
  where
    toRes (e, s) = ( ( either id (const "") e
                     , s ^.  mem . to M.elems
                     )
                   , ( s ^. stdin
                     , s ^. stdout
                     )
                   )

runIntcodeProg :: IntcodeProg -> (Either String (), ICState)
runIntcodeProg = runIntcodeProgStdin []

runIntcodeProgStdin :: Stdin -> IntcodeProg -> (Either String (), ICState)
runIntcodeProgStdin inp p =
  runState (runExceptT runProg) (initState inp p)

progToMem :: IntcodeProg -> Mem
progToMem = M.fromList . zip [0..]

initState :: Stdin -> IntcodeProg -> ICState
initState inp p = ICS 0 (progToMem p) inp []

-- ----------------------------------------

runProg :: Action ()
runProg = do
  ins <- getInstr
  unless (ins ^. _1 . to haltOp) $
    execInstr ins >> runProg

execInstr :: Instr -> Action ()
execInstr (op, (pm1, pm2, pm3))
  | Just f <- binOp op
                 = do v1 <- getParam pm1
                      v2 <- getParam pm2
                      putParam pm3 (f v1 v2)

  | op == Input  = do v <- getInput
                      putParam pm1 v

  | op == Output = do v <- getParam pm1
                      putOut v

  | otherwise    = return ()

haltOp :: Opcode -> Bool
haltOp = (== Halt)

binOp :: Opcode -> Maybe (Int -> Int -> Int)
binOp op = lookup op binOps
  where
    binOps = [ (Add, (+))
             , (Mul, (*))
             ]

incrPc :: Action ()
incrPc = pc %= (+1)

getInput :: Action Int
getInput = do
  inp <- use stdin
  case inp of
    x : xs -> do stdin .= xs
                 return x
    _      -> endOfInput

putOut :: Int -> Action ()
putOut v = stdout %= (v :)

putVal :: Int -> Addr -> Action ()
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

getParam :: ParamMode -> Action Int
getParam PositionMode  = getArg >>= getVal
getParam ImmediateMode = getArg

putParam :: ParamMode -> Int -> Action ()
putParam PositionMode v = do a <- getArg
                             putVal v a
putParam pm           _ = illegalParamMode $ fromEnum pm

getInstr :: Action Instr
getInstr = getArg >>= decodeInstr

decodeInstr :: Int -> Action Instr
decodeInstr i =
  (,) <$> decodeOp oc
      <*> ( (,,) <$> decodePM pm1
                 <*> decodePM pm2
                 <*> decodePM pm3
          )
  where
    (pm123, oc) = i     `divMod` 100
    (pm23, pm1) = pm123 `divMod`  10
    (pm3,  pm2) = pm23  `divMod`  10

decodeOp :: Int -> Action Opcode
decodeOp i
  | 1 <= i && i <= fromEnum Halt = return $ toEnum (i - 1)
  | i == 99                      = return Halt
  | otherwise                    = illegalOpcode i

decodePM :: Int -> Action ParamMode
decodePM i
  | not (i == 0 || i == 1) = illegalParamMode i
  | otherwise              = return $ toEnum i

illegalOpcode :: Int -> Action a
illegalOpcode op =
  throwError $
    "illegal opcode " ++ show op

illegalParamMode :: Int -> Action a
illegalParamMode pm =
  throwError $
    "illegal param mode " ++ show pm

addressViolation :: Int -> Action a
addressViolation a = do
  m <- use mem
  let mx = maybe 0 fst $ M.lookupMax m
  throwError $
    "address violation: 0 <= pc <= " ++ show mx ++ ", but pc = " ++ show a

endOfInput :: Action a
endOfInput = throwError "end of input"

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
ex3 = fromCVS "3,0,4,0,99"
ex4 = fromCVS "1002,4,3,4,33"
ex5 = fromCVS "1101,100,-1,4,0"

inp3 :: Stdin
inp3 = [42]

res1, res2, res4, res5 :: (String, IntcodeProg)
res1 = ("", [3500,9,10,70,2,3,11,0,99,30,40,50])
res2 = ("", [30,1,1,4,2,5,6,0,99])
res4 = ("", [1002,4,3,4,99])
res5 = undefined

res3 :: IntcodeRes2
res3 = (("",[42,0,4,0,99]),([],[42]))


test1, test2, test3, test4 :: Bool
test1 = runIntcode'      ex1 == res1
test2 = runIntcode'      ex2 == res2
test3 = runIntcode2 inp3 ex3 == res3
test4 = runIntcode'      ex4 == res4
test5 = runIntcode'      ex5 == res5

-- ----------------------------------------
