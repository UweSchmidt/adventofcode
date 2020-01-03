{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.DirectedLabeled
  ( Map
  , Set
  , NodeSet
  , EdgeSet
  , Graph
  , Edge
  , empty
  , null
  , size
  , nodes
  , edges
  , dom
  , rng
  , roots
  , leaves
  , isolated

  , lookupNode
  , lookupEdge
  , lookupSuccs
  , memberNodes
  , memberEdges
  , successors

  , insertNode
  , insertEdge
  , deleteNode
  , deleteEdge
  , deleteNodes
  , deleteEdges
  , restrictNodes

  , mapNodes
  , mapEdges
  , fromList
  , toList

  , unionWith
  , union
  , composeWith
  , compose
  , trClosureWith
  , invert
  , symmetric

    -- undirected (symmetric) edges
  , insertEdgeSym
  , deleteEdgeSym
  , deleteEdgesSym
  , neighboursSym
  , fromListSym
  , toListSym
  )
where

import Prelude hiding (foldr, null, filter)

import Control.Arrow
import Control.Lens
import Data.Maybe
import Data.List (foldl')

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Prelude  as P

-- ----------------------------------------

type Map k v = M.Map k v
type Set s   = S.Set s


type NodeSet n       = Set n
type EdgeSet n       = Set (n, n)
type Graph   n nl el = ( NodeMap n nl
                       , Map n (EdgeMap n el)
                       )
type EdgeMap n    el = Map n el
type NodeMap n nl    = Map n nl
type Edge    n    el = ((n, n), el)

-- ----------------------------------------

empty :: Graph n nl el
empty = (M.empty, M.empty)

null ::  Graph n nl el -> Bool
null = M.null . fst

size :: Graph n nl el -> Int
size = M.size .fst

nodes :: Ord n => Graph n nl el -> NodeSet n
nodes = S.fromList . M.keys . fst

edges :: Ord n => Graph n nl el -> EdgeSet n
edges = foldlWithNodes' step S.empty . snd
  where
    step acc n1 n2 _el = S.insert (n1, n2) acc

-- all nodes with outgoing edges
dom :: Ord n => Graph n nl el -> NodeSet n
dom = M.foldlWithKey' step S.empty . snd
  where
    step acc n1 _m1 = S.insert n1 acc

-- all nodes with incomming edges
rng :: Ord n => Graph n nl el -> NodeSet n
rng = foldlWithNodes' step S.empty . snd
  where
    step acc _n1 n2 _el = S.insert n2 acc

-- nodes only with outgoing edges
roots :: Ord n => Graph n nl el -> NodeSet n
roots g = nodes g `S.difference` rng g

-- nodes only with incomming edges
leaves :: Ord n => Graph n nl el -> NodeSet n
leaves g = nodes g `S.difference` dom g

-- isolated nodes
isolated :: Ord n => Graph n nl el -> NodeSet n
isolated g = roots g `S.intersection` leaves g


lookupNode :: Ord n => n -> Graph n nl el -> Maybe nl
lookupNode n = M.lookup n . fst

lookupEdge :: Ord n => n -> n -> Graph n nl el -> Maybe el
lookupEdge n1 n2 g = do
  m1 <- M.lookup n1 . snd $ g
  M.lookup n2 m1

lookupSuccs :: Ord n => n -> Graph n nl el -> NodeSet n
lookupSuccs n1 =
  maybe S.empty f . M.lookup n1 . snd
  where
    f = M.foldlWithKey' step S.empty
      where
        step acc n2 _el = S.insert n2 acc

memberNodes :: Ord n => n -> Graph n nl el -> Bool
memberNodes n = isJust . lookupNode n

memberEdges :: Ord n => n -> n -> Graph n nl el -> Bool
memberEdges n1 n2 = isJust . lookupEdge n1 n2

successors :: Ord n => NodeSet n -> Graph n nl el -> NodeSet n
successors ns g = S.foldl' go S.empty ns
  where
    go acc n' = acc `S.union` lookupSuccs n' g

-- ----------------------------------------
--
-- simple insert/delete operations

insertNode :: Ord n => n -> nl -> Graph n nl el -> Graph n nl el
insertNode n lab = first (M.insert n lab)

insertEdge :: Ord n => n -> n -> el -> Graph n nl el -> Graph n nl el
insertEdge n1 n2 el = second (insertEdgeMap n1 n2 el)

deleteNode :: Ord n => n -> Graph n nl el -> Graph n nl el
deleteNode x =
  first (M.delete x)
  .
  second (M.mapMaybe del . M.delete x)
  where
    del m
      | M.null m' = Nothing
      | otherwise = Just m'
      where
        m' = M.delete x m

deleteEdge :: Ord n => n -> n -> Graph n nl el -> Graph n nl el
deleteEdge x y = second (M.update del x)
  where
    del m
      | M.null m' = Nothing
      | otherwise = Just m'
     where
       m' = M.delete y m

-- --------------------

mapNodes :: Ord n => (nl -> nl') -> Graph n nl el -> Graph n nl' el
mapNodes f (nm, em) = (M.map f nm, em)

mapEdges :: Ord n => (el -> el') -> Graph n nl el -> Graph n nl el'
mapEdges f (nm, em) = (nm, M.map (M.map f) em)

-- --------------------

fromList :: Ord n => [(n, nl)] -> [((n, n), el)] -> Graph n nl el
fromList nls els = insEdges . insNodes $ empty
  where
    insNodes g = foldl' insN g nls
    insEdges g = foldl' insE g els

    insN g' (n, nl)        = insertNode n nl g'
    insE g' ((n1, n2), el) = insertEdge n1 n2 el g'

toList :: Graph n nl el -> ([(n, nl)], [((n, n), el)])
toList = n2l *** e2l
  where
    n2l = M.toList
    e2l = foldrWithNodes' tol []
      where
        tol n1 n2 el xs = ((n1, n2), el) : xs

-- --------------------
--
-- delete a set of nodes/edges

-- delete all nodes in first arg
deleteNodes :: Ord n => NodeSet n -> Graph n nl el -> Graph n nl el
deleteNodes =
  flip (S.foldl' (flip deleteNode))


deleteEdges :: Ord n => EdgeSet n -> Graph n nl el -> Graph n nl el
deleteEdges =
  flip (S.foldl' (flip (uncurry deleteEdge)))   -- hahaha

-- delete all but the nodes in first arg
restrictNodes :: Ord n => NodeSet n -> Graph n nl el -> Graph n nl el
restrictNodes ns g =
  deleteNodes (nodes g `S.difference` ns) g

-- --------------------
--
-- union of graphs

unionWith :: Ord n
          => (nl -> nl -> nl)
          -> (el -> el -> el)
          -> Graph n nl el -> Graph n nl el -> Graph n nl el
unionWith nop eop (ns1, es1) (ns2,es2) =
  ( M.unionWith nop ns1 ns2
  , unionEdges  eop es1 es2
  )

union :: (Ord n, Semigroup nl, Semigroup el)
      => Graph n nl el -> Graph n nl el -> Graph n nl el
union = unionWith (<>) (<>)

-- --------------------
--
-- composition of graphs

composeWith :: Ord n
            => (nl -> nl -> nl)
            -> (el -> el -> el)
            -> Graph n nl el -> Graph n nl el -> Graph n nl el
composeWith nop eop (ns1, es1) (ns2,es2) =
  ( M.unionWith  nop ns1 ns2
  , composeEdges eop es1 es2
  )

compose :: (Ord n, Semigroup nl, Semigroup el)
        => Graph n nl el -> Graph n nl el -> Graph n nl el
compose = composeWith (<>) (<>)

-- ----------------------------------------
--
-- invert the arrows

invert :: Ord n => Graph n nl el -> Graph n nl el
invert = second invertEdges
  where
    invertEdges = foldlWithNodes' inv M.empty
      where
        inv g' n1 n2 el = insertEdgeMap n2 n1 el g'

symmetric :: Ord n => Graph n nl el -> Graph n nl el
symmetric g = unionWith const const g (invert g)

trClosureWith :: Ord n
              => (el -> el -> el)
              -> Graph n nl el -> Graph n nl el
trClosureWith eop = second (trClosureEdges eop)

-- ----------------------------------------
--
-- helper for processing edges

foldlWithNodes' :: (a -> n -> n -> el -> a) -> a -> Map n (EdgeMap n el) -> a
foldlWithNodes' op0 = M.foldlWithKey' op1
  where
    op1 acc n1 = M.foldlWithKey' (flip op0 n1) acc

foldrWithNodes' :: (n -> n -> el -> a -> a) -> a -> Map n (EdgeMap n el) -> a
foldrWithNodes' op0 = M.foldrWithKey' op1
  where
    op1 n1 = flip (M.foldrWithKey' (op0 n1))

-- --------------------

sizeEdgeMap :: Ord n => Map n (EdgeMap n el) -> Int
sizeEdgeMap = M.foldl' op1 0
  where
    op1 acc m1 = acc + M.size m1

insertEdgeMap :: Ord n
              => n -> n -> el
              -> Map n (EdgeMap n el) -> Map n (EdgeMap n el)
insertEdgeMap n1 n2 el = M.insertWith M.union n1 (M.singleton n2 el)

unionEdges :: Ord n
           => (el -> el -> el)
           -> Map n (EdgeMap n el) -> Map n (EdgeMap n el) -> Map n (EdgeMap n el)
unionEdges eop es1 es2 = M.unionWith (M.unionWith eop) es1 es2

composeEdges :: forall n el . Ord n
             => (el -> el -> el)
             -> Map n (EdgeMap n el) -> Map n (EdgeMap n el) -> Map n (EdgeMap n el)
composeEdges eop gm1 gm2 =
  M.mapMaybe comp gm1
  where
    comp :: EdgeMap n el -> Maybe (EdgeMap n el)
    comp m1
      | M.null m1' = Nothing
      | otherwise  = Just m1'
      where
        m1' = M.foldlWithKey' ins M.empty m1
          where
            ins :: EdgeMap n el -> n -> el -> EdgeMap n el
            ins acc n2 el1 =
              M.foldlWithKey' ins2 acc (fromMaybe M.empty $ M.lookup n2 gm2)
              where
                ins2 :: EdgeMap n el -> n -> el -> EdgeMap n el
                ins2 acc' n3 el3 = M.insert n3 (el1 `eop` el3) acc'

trClosureEdges :: Ord n
               => (el -> el -> el)
               -> Map n (EdgeMap n el) -> Map n (EdgeMap n el)
trClosureEdges eop =
  fixpoint . map (id &&& sizeEdgeMap) . iterate step
  where
    step em = unionEdges eop em (composeEdges eop em em)

    fixpoint (x1 : xs@(x2 : _))
      | snd x1 == snd x2 = fst x1
      | otherwise        = fixpoint xs
    fixpoint _ = error "finite result from iterate"

-- ----------------------------------------
--
-- operations for undirected graphs

insertEdgeSym :: Ord n => n -> n -> el -> Graph n nl el -> Graph n nl el
insertEdgeSym x y lab = insertEdge x y lab . insertEdge y x lab

deleteEdgeSym :: Ord n => n -> n -> Graph n nl el -> Graph n nl el
deleteEdgeSym x y = deleteEdge x y .  deleteEdge y x

deleteEdgesSym :: Ord n => EdgeSet n -> Graph n nl el -> Graph n nl el
deleteEdgesSym =
  flip (S.foldl' (flip (uncurry deleteEdgeSym)))

neighboursSym :: Ord n => NodeSet n -> Graph n nl el -> NodeSet n
neighboursSym = successors

fromListSym :: Ord n => [(n, nl)] -> [((n, n), el)] -> Graph n nl el
fromListSym nls els = fromList nls (P.foldr dup [] els)
  where
    dup e rs = e : first (\(x, y) -> (y, x)) e : rs

toListSym :: Ord n => Graph n nl el -> ([(n, nl)], [((n, n), el)])
toListSym = second nod . toList
  where
    nod = P.filter (uncurry (<=) . fst)

-- ----------------------------------------

type G1 = Graph Int Bool String

g1 :: G1
g1 = fromList ns1 (es1 ++ es2)
  where
    ns1 = map (\ i -> (i, odd i)) [0..4]
    es1 = [((i, (i*3) `mod` 5), [c]) | i <- [0..4], c <- "abcde"]
    es2 = [((i, (i+1) `mod` 5), [c]) | i <- [0..4], c <- "zyxwv"]

-- ----------------------------------------
