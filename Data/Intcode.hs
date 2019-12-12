{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Intcode
  ( IntcodeProg
  , IntcodeRes
  , Stdin
  , Stdout
  , Status(..)

  , pc
  , mem
  , status
  , stdin
  , stdout

  , runIntcode
  , runIntcode2

  , mkMachine
  , runMachine
  , hasTerminated

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

import Debug.Trace

-- ----------------------------------------

type IntcodeProg = [Int]
type IntcodeRes  = Either String Int
type IntcodeRes2 = ((String, IntcodeProg), (Stdin, Stdout))
type Stdin       = [Int]
type Stdout      = [Int]

type Mem         = IntMap Int
type Addr        = Int
data Opcode      = Add | Mul
                 | Input | Output
                 | JumpTrue | JumpFalse
                 | LessThan | Equals
                 | Halt
data ParamMode   = PositionMode | ImmediateMode
type Instr       = (Opcode, (ParamMode, ParamMode, ParamMode))

data Status      = OK | Terminated | WaitForInput | OutputWritten | Exc String

data ICState' m  = ICS { _pc     :: Int
                       , _status :: Status
                       , _mem    :: m
                       , _stdin  :: [Int]
                       , _stdout :: [Int]
                       }
type ICState     = ICState' Mem

type Action      = ExceptT Status (State ICState)

deriving instance Show Opcode
deriving instance Enum Opcode
deriving instance Eq   Opcode
deriving instance Show ParamMode
deriving instance Enum ParamMode
deriving instance Show a => Show (ICState' a)
deriving instance Functor ICState'
deriving instance Eq   Status
deriving instance Show Status

-- --------------------
--
-- lenses

pc :: Lens' ICState Int
pc k ics = (\new -> ics {_pc = new}) <$> k (_pc ics)

status :: Lens' ICState Status
status k ics = (\new -> ics {_status = new}) <$> k (_status ics)

mem :: Lens' ICState Mem
mem k ics = (\new -> ics {_mem = new}) <$> k (_mem ics)

stdin :: Lens' ICState [Int]
stdin k ics = (\new -> ics {_stdin = new}) <$> k (_stdin ics)

stdout :: Lens' ICState [Int]
stdout k ics = (\new -> ics {_stdout = new}) <$> k (_stdout ics)

-- --------------------
--
-- run the machine
--
-- too many run functions

runIntcode :: IntcodeProg -> IntcodeRes
runIntcode p =
  either (Left . show) (const $ Right . head . M.elems $ ics ^. mem) res
  where
    res = hasTerminated ics
    ics = runMachine $ mkMachine [] p

runIntcode2 :: Stdin -> IntcodeProg -> IntcodeRes2
runIntcode2 inp p = res
  where
    ics = runMachine $ mkMachine inp p
    chk = hasTerminated ics
    res = ( ( either show (const "") chk
            , ics ^.  mem . to M.elems
            )
          , ( ics ^. stdin
            , ics ^. stdout
            )
          )

-- ----------------------------------------
--
-- just for testing

runIntcode' :: IntcodeProg -> (String, IntcodeProg)
runIntcode' = fst . runIntcode2 []

-- --------------------

progToMem :: IntcodeProg -> Mem
progToMem = M.fromList . zip [0..]

mkMachine :: Stdin -> IntcodeProg -> ICState
mkMachine inp p = ICS 0 OK (progToMem p) inp []

hasTerminated :: ICState -> Either Status Stdout
hasTerminated ics =
  case ics ^. status of
    Terminated -> Right $ ics ^. stdout
    _          -> Left  $ ics ^. status


-- runs until output written, input missing, or halt
runMachine0 :: ICState -> ICState
runMachine0 = snd . runState (runExceptT runProg)

-- continues running after output
runMachine :: ICState -> ICState
runMachine ics0
  | ics1 ^. status == OutputWritten = runMachine (ics1 & status .~ OK)
  | otherwise                       = ics1
  where
    ics1 = runMachine0 ics0

-- lazy output list
runMachine' :: ICState -> [Int]
runMachine' ics0
  | ics1 ^. status == OutputWritten = ics1 ^. stdout ++ runMachine' ics2
  | otherwise                       = ics1 ^. stdout
  where
    ics1 = runMachine ics0
    ics2 = ics1 & stdout .~ []
                & status .~ OK

-- ----------------------------------------

runProg :: Action ()
runProg = do
  cont <- statusOK
  when cont $ do
    ins <- getInstr
    -- traceShowM ins
    execInstr ins `catchError` (status .=)
    runProg

execInstr :: Instr -> Action ()
execInstr (op, (pm1, pm2, pm3))
  | Just f <- binOp op
                 = do v1 <- getParam pm1
                      v2 <- getParam pm2
                      putParam pm3 (f v1 v2)

  | Just p <- branchOp op
                 = do v   <- getParam pm1
                      pc' <- getParam pm2
                      when (p v) $
                        pc .= pc'

  | op == Input  = do v <- getInput
                      traceShowM ("input:  ", v)
                      putParam pm1 v

  | op == Output = do v <- getParam pm1
                      traceShowM ("output: ", v)
                      putOut v
                      outputWritten

  | op == Halt   = status .= Terminated

  | otherwise    = return ()

haltOp :: Opcode -> Bool
haltOp = (== Halt)

binOp :: Opcode -> Maybe (Int -> Int -> Int)
binOp op = lookup op binOps
  where
    binOps = [ (Add,      (+))
             , (Mul,      (*))
             , (LessThan, \ x y -> fromEnum $ x <  y)
             , (Equals,   \ x y -> fromEnum $ x == y)
             ]

branchOp :: Opcode -> Maybe (Int -> Bool)
branchOp op = lookup op branchOps
  where
    branchOps = [ (JumpTrue,  (/= 0))
                , (JumpFalse, (== 0))
                ]

statusOK :: Action Bool
statusOK = do s <- use status
              return $ case s of
                         OK -> True
                         _  -> False

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
putOut v = stdout %= (++ [v])

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
  throwError . Exc $
    "illegal opcode " ++ show op

illegalParamMode :: Int -> Action a
illegalParamMode pm =
  throwError . Exc $
    "illegal param mode " ++ show pm

addressViolation :: Int -> Action a
addressViolation a = do
  m <- use mem
  let mx = maybe 0 fst $ M.lookupMax m
  throwError . Exc $
    "address violation: 0 <= pc <= " ++ show mx ++ ", but pc = " ++ show a

endOfInput :: Action a
endOfInput = throwError WaitForInput

outputWritten :: Action a
outputWritten = throwError OutputWritten

-- ----------------------------------------

fromCVS :: String -> IntcodeProg
fromCVS = read . ("[" ++) . (++ "]")

patchIntcode :: Int -> Int -> IntcodeProg -> IntcodeProg
patchIntcode v i p =
  px ++ [v] ++ drop 1 sx
  where
    (px, sx) = splitAt i p

-- ----------------------------------------

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9
  , ex10, ex11, ex12 :: IntcodeProg

ex1 = fromCVS "1,9,10,3,2,3,11,0,99,30,40,50"
ex2 = fromCVS "1,1,1,4,99,5,6,0,99"
ex3 = fromCVS "3,0,4,0,99"
ex4 = fromCVS "1002,4,3,4,33"
ex5 = fromCVS "1101,100,-1,4,0"
ex6 = fromCVS "3,9,8,9,10,9,4,9,99,-1,8"
ex7 = fromCVS "3,9,7,9,10,9,4,9,99,-1,8"
ex8 = fromCVS "3,3,1108,-1,8,3,4,3,99"
ex9 = fromCVS "3,3,1107,-1,8,3,4,3,99"

ex10 = fromCVS "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
ex11 = fromCVS "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
ex12 = fromCVS "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

-- --------------------

inp3, inp61, inp62, inp71, inp72
  , inp100, inp101, inp120, inp121, inp122 :: Stdin

inp3  = [42]
inp61 = [8]
inp62 = [9]
inp71 = [7]
inp72 = [8]

inp100 = [0]
inp101 = [42]

inp120 = [7]
inp121 = [8]
inp122 = [9]

-- --------------------

res1, res2, res4, res5 :: (String, IntcodeProg)

res1 = ("", [3500,9,10,70,2,3,11,0,99,30,40,50])
res2 = ("", [30,1,1,4,2,5,6,0,99])
res4 = ("", [1002,4,3,4,99])
res5 = ("", [1101,100,-1,4,99])

res3 :: IntcodeRes2
res3 = (("",[42,0,4,0,99]),([],[42]))

res61, res62
  , res120, res121, res122:: Stdout

res61 = [1]
res62 = [0]

res120 = [999]
res121 = [1000]
res122 = [1001]

-- --------------------

test1, test2, test3, test4 :: Bool
test1  = runIntcode'      ex1 == res1
test2  = runIntcode'      ex2 == res2
test3  = runIntcode2 inp3 ex3 == res3
test4  = runIntcode'      ex4 == res4
test5  = runIntcode'      ex5 == res5
test61 = (snd . snd $ runIntcode2 inp61 ex6) == res61
test62 = (snd . snd $ runIntcode2 inp62 ex6) == res62
test71 = (snd . snd $ runIntcode2 inp71 ex7) == res61
test72 = (snd . snd $ runIntcode2 inp72 ex7) == res62
test81 = (snd . snd $ runIntcode2 inp61 ex8) == res61
test82 = (snd . snd $ runIntcode2 inp62 ex8) == res62
test91 = (snd . snd $ runIntcode2 inp71 ex9) == res61
test92 = (snd . snd $ runIntcode2 inp72 ex9) == res62

test100 = (snd . snd $ runIntcode2 inp100 ex10) == res62
test101 = (snd . snd $ runIntcode2 inp101 ex10) == res61
test110 = (snd . snd $ runIntcode2 inp100 ex11) == res62
test111 = (snd . snd $ runIntcode2 inp101 ex11) == res61

test120 = (snd . snd $ runIntcode2 inp120 ex12) == res120
test121 = (snd . snd $ runIntcode2 inp121 ex12) == res121
test122 = (snd . snd $ runIntcode2 inp122 ex12) == res122


-- ----------------------------------------
