{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
             TypeFamilies, GADTs, EmptyDataDecls
             #-}

module Data.IGraph.Types where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

import Foreign.Ptr
import Foreign.ForeignPtr


--------------------------------------------------------------------------------
-- C stuff

data Void

type GraphPtr     = Ptr Void
type VectorPtr    = Ptr Void
type VectorPtrPtr = Ptr Void


--------------------------------------------------------------------------------
-- Graph representation

-- | The internal graph representation wrapped into a GADT to carry around the
-- @Gr d a@ class constraint.
data Graph d a where
  G :: Gr d a => G d a -> Graph d a

-- | The internal graph representation.
data G d a = Graph { -- graphNodeNumber        :: !(Int)
                     graphEdgeNumber        :: !(Int)
                   , graphIdToNode          :: !(HashMap Int a)
                   , graphNodeToId          :: !(HashMap a Int)
                   , graphEdges             :: !(HashMap Int (HashSet Int))
                   , graphForeignPtr        :: !(Maybe (ForeignPtr Void))
                   }

-- | Graph class. Minimal definition: @data Edge d a@ with `Hashable' and `Eq'
-- instances, `toEdge', `edgeFrom', `edgeTo', `isDirected'
class (Eq a, Hashable a, Eq (Edge d a), Hashable (Edge d a)) => Gr d a where

  data Edge d a
  toEdge     :: a -> a -> Edge d a
  edgeFrom   :: Edge d a -> a
  edgeTo     :: Edge d a -> a
  isDirected :: Graph d a -> Bool

-- | Undirected graph
data U

instance (Eq a, Hashable a) => Gr U a where
  isDirected _ = False
  data Edge U a = U_Edge a a
  toEdge = U_Edge
  edgeFrom (U_Edge a _) = a
  edgeTo   (U_Edge _ b) = b

instance Eq a => Eq (Edge U a) where
  (U_Edge a b) == (U_Edge c d) = (a,b) == (c,d) || (a,b) == (d,c)

instance Hashable a => Hashable (Edge U a) where
  hash (U_Edge a b) = hash (a,b) + hash (b,a) -- to make sure (a,b) receives the same hash as (b,a)

instance Show a => Show (Edge U a) where
  show (U_Edge a b) = "Edge U {" ++ show a ++ " <-> " ++ show b ++ "}"

-- | Directed graph
data D

instance (Eq a, Hashable a) => Gr D a where
  isDirected _ = True
  data Edge D a = D_Edge a a deriving Eq
  toEdge = D_Edge
  edgeFrom (D_Edge a _) = a
  edgeTo   (D_Edge _ b) = b

instance Hashable a => Hashable (Edge D a) where
  hash (D_Edge a b) = hash (a,b)

instance Show a => Show (Edge D a) where
  show (D_Edge a b) = "Edge D {" ++ show a ++ " -> " ++ show b ++ "}"
