{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

-- solution for
-- http://adventofcode.com/2019/day/14

module Main where

import AOC.Prelude

import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import qualified Data.Relation       as R

-- ----------------------------------------

withTrace :: Bool
withTrace = True

trc :: String -> a -> a
trc = trace' withTrace

-- --------------------

main :: IO ()
main = main12 "2019-14"
       inp captcha1
       inp captcha2

captcha1 :: String -> String
captcha1 = parseRules >>> solve1 >>> show

captcha2 :: String -> String
captcha2 = parseRules >>> solve2 >>> show

solve1 :: Rules -> Int
solve1 rules = solve rules 1

solve2 :: Rules -> Int
solve2 rules =
  fst . last . uncurry (zero f) $ lb'ub f
  where
    f x = solve rules x - trillion

solve :: Rules -> Int -> Int
solve rules amount =
  reduce rules prios amount
  where
    dps   = rulesToMatDeps rules
    prios = toMatPrios dps $ S.singleton "ORE"

reduce :: Rules -> MatSeq -> Int -> Int
reduce rules prios amount =
  fromMaybe (-1) . fmap snd . listToMaybe . M.toList $ ore
  where
    ore = reduceMat rules prios $ M.singleton "FUEL" amount


approx :: (Int -> Int) -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
approx f lim'f = go
  where
    go l@(lx, ly) u@(ux, uy)     -- inv: ly <= lim'f
      | ux == lx + 1 = l : []
      | uy <= lim'f  = l : go u (ux2, f ux2)
      | otherwise    = undefined
      where
        ux2 = ux * 2

zero :: (Int -> Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
zero f = go
  where
    go p1@(x1, _y1) p2@(x2, _y2)
      | x2 == x1 + 1 = p1  : []
      | y12 >  0     = p12 : go p1 p12
      | y12 <  0     = p12 : go p12 p2
      | otherwise    = p12 : []
      where
        x12 = (x1 + x2) `div` 2
        y12 = f x12
        p12 = (x12, y12)

-- search for a pair of points x1 and x2 with f x1 <= 0 and f x2 > 0

lb'ub :: (Int -> Int) -> ((Int, Int), (Int,Int))
lb'ub f
  | y > 0     = error "function positive: f 1 > 0"
  | otherwise = go p
  where
    p@(_x, y) = (1, f 1)

    go p1@(x1, _y1)
      | y2 > 0    = (p1, p2)
      | otherwise = go p2
      where
        x2 = 2 * x1
        y2 = f x2
        p2 = (x2, y2)

type Material   = String
type Quantity   = (Material, Int)
type Quantities = M.HashMap Material Int
type Rule       = (Quantity, Quantities)
type Rules      = M.HashMap Material Rule
type MatDeps    = R.Rel' Material
type MatSet     = S.Set Material
type MatPrios   = M.HashMap Material Int
type MatSeq     = [Material]

fuel :: Quantities
fuel = M.singleton "FUEL" 1

ores :: MatSet
ores = S.singleton "ORE"

trillion :: Int
trillion = 1000000000000

emptyQuantities :: Quantities
emptyQuantities = M.empty

insQuantities :: Quantity -> Quantities -> Quantities
insQuantities = uncurry (M.insertWith (+))

delQuantities :: Material -> Quantities -> Quantities
delQuantities = M.delete

unionQuantities :: Quantities -> Quantities -> Quantities
unionQuantities = M.unionWith (+)

scaleQuantities :: Int -> Quantities -> Quantities
scaleQuantities sc = M.map (* sc)

emptyRules :: Rules
emptyRules = M.empty

insRules :: Rule -> Rules -> Rules
insRules r = M.insert (r ^. _1 . _1) r

rulesToMatDeps :: Rules -> MatDeps
rulesToMatDeps =
  M.foldl' r2md R.empty
  where
    r2md md ((m, _i), qs) =
      md `R.union` M.foldlWithKey' q2md R.empty qs
      where
        q2md md' m' _i' =
          md' `R.union` R.singleton m' m

-- the list of rules to be stepwise applied
-- to reduce FUEL to ORE

toMatPrios :: MatDeps -> MatSet -> MatSeq
toMatPrios md ms0 =
  toPrioList $ go (toMM 0 ms0) 0 ms0
  where
    toPrioList =
      reverse . map snd . sort . map (\(x,y)->(y,x)) . M.toList

    toMM :: Int -> MatSet -> MatPrios
    toMM i =
      S.foldl' (\ mm m -> M.insert m i mm) M.empty

    go :: MatPrios -> Int -> MatSet -> MatPrios
    go acc i ms
      | S.null ms = acc
      | otherwise = go acc1 i1 ms1
      where
        i1   = i + 1
        ms1  = R.applyS md ms
        acc1 = toMM i1 ms1 `M.union` acc

reduceMat :: Rules -> MatSeq -> Quantities -> Quantities
reduceMat rs ms qm0 =
  foldl' red1 qm0 ms
  where
    red1 qm m =
      maybe qm red2 $ M.lookup m rs
      where
        red2 _rule@((_m, ir), qmr) =
          maybe qm red3 $ M.lookup m qm
          where
            red3 i =            -- produce i units of material m with rule r
              qm' `unionQuantities` qm1
              where
                sc1 = (i + ir - 1) `div` ir       -- apply rule sc times
                qm1 = scaleQuantities sc1 qmr     -- add new materials
                qm' = delQuantities m qm          -- rem old material

-- --------------------

parseRules :: String -> Rules
parseRules = parseInput pRules

pRules :: Parser Rules
pRules =
  foldl' (flip insRules) emptyRules
    <$> some pRule

pRule :: Parser Rule
pRule =
  flip (,) <$> pQuantities
           <*> (string "=>" *> space *> pQuantity)

pQuantities :: Parser Quantities
pQuantities =
  foldl' (flip insQuantities) emptyQuantities
    <$> sepBy1 pQuantity (single ',' >> space) <* space

pQuantity :: Parser Quantity
pQuantity =
  flip (,) <$> (natural <* space)
           <*> (some upperChar <* space)

-- ----------------------------------------

ex1 :: String
ex1 = unlines
  [ "10 ORE => 10 A"
  , "1 ORE => 1 B"
  , "7 A, 1 B => 1 C"
  , "7 A, 1 C => 1 D"
  , "7 A, 1 D => 1 E"
  , "7 A, 1 E => 1 FUEL"
  ]

ex2 :: String
ex2 = unlines
  [ "9 ORE => 2 A"
  , "8 ORE => 3 B"
  , "7 ORE => 5 C"
  , "3 A, 4 B => 1 AB"
  , "5 B, 7 C => 1 BC"
  , "4 C, 1 A => 1 CA"
  , "2 AB, 3 BC, 4 CA => 1 FUEL"
  ]

ex3 :: String
ex3 = unlines
  [ "157 ORE => 5 NZVS"
  , "165 ORE => 6 DCFZ"
  , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
  , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
  , "179 ORE => 7 PSHF"
  , "177 ORE => 5 HKGWZ"
  , "7 DCFZ, 7 PSHF => 2 XJWVT"
  , "165 ORE => 2 GPVTF"
  , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
  ]

ex4 :: String
ex4 = unlines
  [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
  , "17 NVRVD, 3 JNWZP => 8 VPVL"
  , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
  , "22 VJHF, 37 MNCFX => 5 FWMGM"
  , "139 ORE => 4 NVRVD"
  , "144 ORE => 7 JNWZP"
  , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
  , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
  , "145 ORE => 6 MNCFX"
  , "1 NVRVD => 8 CXFTF"
  , "1 VJHF, 6 MNCFX => 4 RFSQX"
  , "176 ORE => 6 VJHF"
  ]

ex5 :: String
ex5 = unlines
  [ "171 ORE => 8 CNZTR"
  , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
  , "114 ORE => 4 BHXH"
  , "14 VRPVC => 6 BMBT"
  , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
  , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
  , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
  , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
  , "5 BMBT => 4 WPTQ"
  , "189 ORE => 9 KTJDG"
  , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
  , "12 VRPVC, 27 CNZTR => 2 XDBXC"
  , "15 KTJDG, 12 BHXH => 5 XCVML"
  , "3 BHXH, 2 VRPVC => 7 MZWV"
  , "121 ORE => 7 VRPVC"
  , "7 XCVML => 6 RJRHP"
  , "5 BHXH, 4 VRPVC => 5 LTCX"
  ]

rs1, rs2, rs3, rs4, rs5 :: Int
rs1 = 31
rs2 = 165
rs3 = 13312
rs4 = 180697
rs5 = 2210736

-- ----------------------------------------

inp :: String
inp = unlines
  [ "1 QDKHC => 9 RFSZD"
  , "15 FHRN, 17 ZFSLM, 2 TQFKQ => 3 JCHF"
  , "4 KDPV => 4 TQFKQ"
  , "1 FSTRZ, 5 QNXWF, 2 RZSD => 3 FNJM"
  , "15 VQPC, 1 TXCJ => 3 WQTL"
  , "1 PQCQN, 6 HKXPJ, 16 ZFSLM, 6 SJBPT, 1 TKZNJ, 13 JBDF, 1 RZSD => 6 VPCP"
  , "1 LJGZP => 7 VNGD"
  , "1 CTVB, 1 HVGW => 1 LJGZP"
  , "6 HVGW, 1 HJWT => 2 VDKF"
  , "10 PQCQN, 7 WRQLB, 1 XMCH => 3 CDMX"
  , "14 VNGD, 23 ZFSLM, 2 FHRN => 4 SJBPT"
  , "1 FSTRZ, 4 VTWB, 2 BLJC => 4 CKFW"
  , "2 ZTFH, 19 CKFW, 2 FHRN, 4 FNJM, 9 NWTVF, 11 JBDF, 1 VDKF, 2 DMRCN => 4 HMLTV"
  , "1 KVZXR => 5 FPMSL"
  , "8 XBZJ => 8 QDKHC"
  , "1 VQPC => 9 FHRN"
  , "15 RKTFX, 5 HKXPJ => 4 ZFSLM"
  , "1 HKXPJ, 8 LQCTQ, 21 VJGKN => 5 QCKFR"
  , "1 DCLQ, 1 TQFKQ => 4 KVZXR"
  , "4 NWTVF, 20 QNXWF => 9 JFLQD"
  , "11 QFVR => 3 RZSD"
  , "9 RFSZD, 6 WQTL => 7 JBDF"
  , "4 BLJC, 3 LQCTQ, 1 QCKFR => 8 QFVR"
  , "6 VNGD => 5 VQPC"
  , "7 CTMR, 10 SJBPT => 9 VTWB"
  , "1 VTWB => 9 DMRCN"
  , "6 BCGLR, 4 TPTN, 29 VNGD, 25 KDQC, 40 JCHF, 5 HMLTV, 4 CHWS, 2 CDMX, 1 VPCP => 1 FUEL"
  , "1 TQFKQ, 3 FPMSL, 7 KDPV => 6 RKTFX"
  , "8 HKXPJ, 2 WQTL => 6 WRQLB"
  , "146 ORE => 3 KDPV"
  , "9 KDQC => 2 XMCH"
  , "1 BGVXG, 21 KVZXR, 1 LQCTQ => 4 CTVB"
  , "1 LQCTQ => 5 VJGKN"
  , "16 VNGD, 5 VMBM => 1 CTMR"
  , "5 VCVTM, 1 FPMSL => 5 HKXPJ"
  , "4 HKXPJ => 5 BLJC"
  , "14 FHRN, 6 ZFSLM => 1 NWTVF"
  , "7 QCKFR, 2 VNGD => 7 VMBM"
  , "4 TXCJ, 1 VDKF => 2 QNXWF"
  , "136 ORE => 6 BGVXG"
  , "5 LQCTQ, 11 DCLQ => 9 XBZJ"
  , "3 VQPC => 7 ZTFH"
  , "114 ORE => 3 ZWFZX"
  , "1 HJWT, 18 KDPV => 7 TXCJ"
  , "1 VJGKN => 2 VCVTM"
  , "2 KVZXR => 1 HJWT"
  , "12 ZWFZX, 1 FHRN, 9 JFLQD => 1 CHWS"
  , "3 QFVR => 5 FSTRZ"
  , "5 XBZJ => 4 HVGW"
  , "1 ZWFZX => 8 LQCTQ"
  , "16 WQTL, 10 TXCJ => 9 KDQC"
  , "3 FHRN, 12 LJGZP => 5 TPTN"
  , "1 JCHF => 7 PQCQN"
  , "7 KDPV, 17 BGVXG => 7 DCLQ"
  , "1 CKFW, 3 TKZNJ, 4 PQCQN, 1 VQPC, 32 QFVR, 1 FNJM, 13 FSTRZ => 3 BCGLR"
  , "2 FSTRZ => 5 TKZNJ"
  ]


res1 :: Int
res1 = 365768

res2 :: Int
res2 = 3756877

-- ----------------------------------------
