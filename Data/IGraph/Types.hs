module Data.IGraph.Types where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashSet as S (HashSet, foldr)

import Foreign.Ptr
import Foreign.ForeignPtr

import Data.IGraph.Internal.Constants

--------------------------------------------------------------------------------
-- C stuff

data Grph
type GraphPtr        = Ptr Grph

data Vec
type VectorPtr       = Ptr Vec
data VecPtr
type VectorPtrPtr    = Ptr VecPtr
newtype Vector       = Vector    { unV  :: ForeignPtr Vec }
newtype VectorP      = VectorP   { unVP :: ForeignPtr VecPtr }

-- vector of graphs
data GraphVec
type GraphVecPtr     = Ptr GraphVec
newtype GraphVectorP = GraphVectorP { unGVP :: ForeignPtr GraphVec }

data Mat
type MatrixPtr       = Ptr Mat
newtype Matrix       = Matrix    { unM  :: ForeignPtr Mat }

data SpMat
type SpMatrixPtr     = Ptr SpMat
--newtype SparseMatrix = SparseMatrix { unSM :: ForeignPtr SpMat }

data Vs
type VsPtr            = Ptr Vs
type VsIdent a        = (a -> Maybe Int)
newtype VsForeignPtr  = VsF { unVsF :: ForeignPtr Vs }

data Es
type EsPtr            = Ptr Es
newtype EsForeignPtr  = EsF { unEsF :: ForeignPtr Es }

data Arpack
type ArpackPtr        = Ptr Arpack

--------------------------------------------------------------------------------
-- Graph representation

-- | The internal graph representation wrapped into a GADT to carry around the
-- @E d a@ class constraint.
data Graph d a where
  G :: E d a => G d a -> Graph d a

-- | The internal graph representation.
data G d a
  = Graph { graphNodeNumber        :: !Int
          , graphEdgeNumber        :: !Int
          , graphIdToNode          :: !(HashMap Int a)
          , graphNodeToId          :: !(HashMap a Int)
          , graphEdges             :: !(HashSet (Edge d a))
          , graphForeignPtr        :: ForeignPtr Grph
          , graphArpackOptions     :: ForeignPtr Arpack
          , graphNeiMode           :: NeiMode
          }

-- | Class for graph edges, particularly for undirected edges @Edge U a@ and
-- directed edges @Edge D a@ and weighted edges.
class (Eq a, Hashable a, Eq (Edge d a), Hashable (Edge d a)) => E d a where
  data Edge d a
  isDirected :: Graph d a -> Bool
  isWeighted :: Graph d a -> Bool
  toEdge     :: a -> a -> Edge d a
  edgeFrom   :: Edge d a -> a
  edgeTo     :: Edge d a -> a
  edgeWeight :: Edge d a -> Maybe Int
  setWeight  :: Edge d a -> Int -> Edge d a
  getWeights :: Graph d a -> Maybe [Int]

-- | Undirected graph
data U

instance (Eq a, Hashable a) => E U a where
  isDirected _ = False
  isWeighted _ = False
  data Edge U a = U_Edge a a
  toEdge = U_Edge
  edgeFrom (U_Edge a _) = a
  edgeTo   (U_Edge _ b) = b
  edgeWeight _ = Nothing
  setWeight e _ = e
  getWeights _ = Nothing

instance Eq a => Eq (Edge U a) where
  (U_Edge a b) == (U_Edge c d) = (a,b) == (c,d) || (a,b) == (d,c)

instance Hashable a => Hashable (Edge U a) where
  -- to make sure (a,b) receives the same hash as (b,a):
  hashWithSalt s (U_Edge a b) = hashWithSalt s (a,b) + hashWithSalt s (b,a)

instance Ord a => Ord (Edge U a) where
  (U_Edge a b) <= (U_Edge c d) = (a,b) <= (c,d)

instance Show a => Show (Edge U a) where
  show (U_Edge a b) = "Edge U {" ++ show a ++ " <-> " ++ show b ++ "}"

-- | Directed graph
data D

instance (Eq a, Hashable a) => E D a where
  isDirected _ = True
  isWeighted _ = False
  data Edge D a = D_Edge a a deriving Eq
  toEdge = D_Edge
  edgeFrom (D_Edge a _) = a
  edgeTo   (D_Edge _ b) = b
  edgeWeight _ = Nothing
  setWeight e _ = e
  getWeights _ = Nothing

instance Hashable a => Hashable (Edge D a) where
  hashWithSalt s (D_Edge a b) = hashWithSalt s (a,b)

instance Show a => Show (Edge D a) where
  show (D_Edge a b) = "Edge D {" ++ show a ++ " -> " ++ show b ++ "}"

instance Ord a => Ord (Edge D a) where
  (D_Edge a b) <= (D_Edge c d) = (a,b) <= (c,d)


class IsUnweighted d where
  liftIsDirected :: Graph (Weighted d) a -> Bool

instance IsUnweighted U where
  liftIsDirected _ = False

instance IsUnweighted D where
  liftIsDirected _ = True


class IsDirected d where
  type ToUndirected d
  directedToUndirected :: E (ToUndirected d) a => Edge d a -> Edge (ToUndirected d) a

instance IsDirected D where
  type ToUndirected D = U
  directedToUndirected (D_Edge a b) = (U_Edge a b)


class IsUndirected u where
  type ToDirected u
  undirectedToDirected :: E (ToDirected u) a => Edge u a -> Edge (ToDirected u) a

instance IsUndirected U where
  type ToDirected U = D
  undirectedToDirected (U_Edge a b) = (D_Edge a b)


-- | Weighted graphs, weight defaults to 0
data Weighted d

instance (E d a, IsUnweighted d) => E (Weighted d) a where
  isDirected = liftIsDirected
  isWeighted _ = True
  data Edge (Weighted d) a = W (Edge d a) Int
  toEdge a b = W (toEdge a b) 0
  edgeFrom (W e _) = edgeFrom e
  edgeTo   (W e _) = edgeTo   e
  edgeWeight (W _ w)  = Just w
  setWeight (W e _) w = W e w
  getWeights (G g)    = Just $ S.foldr (\(W _  w) r -> w:r) [] (graphEdges g)

instance E d a => Eq (Edge (Weighted d) a) where
  (W e w) == (W e' w') = w == w' && e == e'

instance E d a => Hashable (Edge (Weighted d) a) where
  hashWithSalt s (W e w) = hashWithSalt s (edgeFrom e, edgeTo e, w)

instance Show (Edge d a) => Show (Edge (Weighted d) a) where
  show (W e w) = show e ++ "(" ++ show w ++ ")"

instance (E d a, Ord (Edge d a)) => Ord (Edge (Weighted d) a) where
  (W e1 w1) <= (W e2 w2) = (e1,w1) <= (e2,w2)

instance IsDirected (Weighted D) where
  type ToUndirected (Weighted D) = Weighted (ToUndirected D)
  directedToUndirected (W e w) = W (directedToUndirected e) w

instance IsUndirected (Weighted U) where
  type ToDirected (Weighted U) = Weighted (ToDirected U)
  undirectedToDirected (W e w) = W (undirectedToDirected e) w

--------------------------------------------------------------------------------
-- Vertex & edge selectors

data VertexSelector a
  = VsAll
  | VsNone
  | Vs1      a
  | VsList   [a]
  | VsAdj    a
  | VsNonAdj a

data EdgeSelector d a
  = EsAll
  | EsNone
  | EsIncident  a
  | EsSeq       a a
  | EsFromTo    (VertexSelector a) (VertexSelector a)
  | Es1         (Edge d a)
  | EsList      [Edge d a]
