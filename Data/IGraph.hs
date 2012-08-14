{-# LANGUAGE PatternGuards, ForeignFunctionInterface #-}

-- | Haskell bindings to the igraph C library.
--
-- See <http://igraph.sourceforge.net/doc/html/index.html> in the specified
-- section for more documentation about a specific function.
module Data.IGraph
  ( -- * Base types
    Graph (..), {- G, -} E (..), D, U

    -- ** The IGraph monad
  , IGraph, runIGraph, evalIGraph, execIGraph, localGraph

    -- * Construction
  , emptyGraph, fromList
  , insertEdge, deleteEdge, deleteNode
    -- * Query
  , numberOfNodes, numberOfEdges, member, nodes, edges --, neighbours

    -- * Chapter 11. Vertex and Edge Selectors and Sequences, Iterators

    -- ** 11\.2 Vertex selector constructors
  , VertexSelector, vsAll, vsAdj, vsNonadj, vsNone, vs1, vsList, vsSeq

    -- ** 11\.3. Generic vertex selector operations
  , vsIsAll , vsSize
  , selectedVertices

    -- * Chapter 13\. Structural Properties of Graphs

    -- ** 13\.1 Basic properties
  , areConnected

    -- ** 13\.2 Shortest Path Related Functions
  , shortestPaths, getShortestPath
  , NeiMode(..)

    -- ** 13\.3 Neighborhood of a vertex
  , neighborhood

    -- ** 13\.4 Graph Components
  , subcomponent
  , subgraph
  , isConnected
  --, decompose
  , Connectedness(..)

  --, initIGraph
  --, setVertexIds, setVertexIds'
  --, getVertexIds, getVertexIds'
  ) where

import Data.IGraph.Basics
import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types

import Data.Map (Map)
import qualified Data.Map as Map

import Foreign
import Foreign.C


--------------------------------------------------------------------------------
-- 11.2 Vertex selector constructors

foreign import ccall "igraph_vs_all"
  c_igraph_vs_all :: VsPtr -> IO CInt

vsAll :: VertexSelector a
vsAll = Vs $ \_ -> do
  fvs <- newVs
  _e  <- withVs' fvs c_igraph_vs_all
  return fvs

foreign import ccall "igraph_vs_adj"
  c_igraph_vs_adj :: VsPtr -> CInt -> CInt -> IO CInt

vsAdj :: a -> NeiMode -> (VertexSelector a)
vsAdj n m = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs' fvs $ \vsp ->
      c_igraph_vs_adj vsp (fromIntegral i) (fromIntegral (fromEnum m))
  return fvs

foreign import ccall "igraph_vs_nonadj"
  c_igraph_vs_nonadj :: VsPtr -> CInt -> CInt -> IO CInt

vsNonadj :: a -> NeiMode -> VertexSelector a
vsNonadj n m = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs' fvs $ \vsp ->
      c_igraph_vs_nonadj vsp (fromIntegral i) (fromIntegral (fromEnum m))
  return fvs

foreign import ccall "igraph_vs_none"
  c_igraph_vs_none :: VsPtr -> IO CInt

vsNone :: VertexSelector a
vsNone = Vs $ \_ -> do
  vs <- newVs
  _e <- withVs' vs c_igraph_vs_none
  return vs

foreign import ccall "igraph_vs_1"
  c_igraph_vs_1 :: VsPtr -> CInt -> IO CInt

vs1 :: a -> VertexSelector a
vs1 n = Vs $ \ident -> do
  fvs <- newVs
  _e <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs' fvs $ \vsp -> c_igraph_vs_1 vsp (fromIntegral i)
  return fvs

foreign import ccall "igraph_vs_vector"
  c_igraph_vs_vector :: VsPtr -> VectorPtr -> IO CInt

vsList :: [a] -> VertexSelector a
vsList l = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (sequence (map ident l)) $ \is -> do
    v  <- listToVector is
    withVs' fvs $ \vsp -> withVector v $ \vp ->
      c_igraph_vs_vector vsp vp
  return fvs

foreign import ccall "igraph_vs_seq"
  c_igraph_vs_seq :: VsPtr -> CInt -> CInt -> IO CInt

vsSeq :: a -> a -> VertexSelector a
vsSeq f t = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (zipM (ident f) (ident t)) $ \(fi,ti) ->
    withVs' fvs $ \vsp ->
      c_igraph_vs_seq vsp (fromIntegral fi) (fromIntegral ti)
  return fvs
 where
  zipM ma mb = do
    a <- ma
    b <- mb
    return (a,b)

onSuccessfulVsIdent :: VsForeignPtr -> Maybe a -> (a -> IO CInt) -> IO CInt
onSuccessfulVsIdent fvs Nothing  _ = withVs' fvs c_igraph_vs_none
onSuccessfulVsIdent _   (Just a) f = f a

--------------------------------------------------------------------------------
-- 11.3. Generic vertex selector operations

foreign import ccall "igraph_vs_is_all"
  c_igraph_vs_is_all :: VsPtr -> IO CInt

vsIsAll :: VertexSelector a -> IGraph (Graph d a) Bool
vsIsAll vs = runUnsafeIO $ \g -> do
  r <- withVs vs g $ \vsp ->
    c_igraph_vs_is_all vsp
  return (r == 1, g)

foreign import ccall "igraph_vs_size"
  c_igraph_vs_size :: GraphPtr -> VsPtr -> Ptr CInt -> IO CInt

vsSize :: VertexSelector a -> IGraph (Graph d a) Int
vsSize vs = runUnsafeIO $ \g -> alloca $ \rp -> do
  (_e, g') <- withGraph g $ \gp -> withVs vs g $ \vsp ->
    c_igraph_vs_size gp vsp rp
  ci <- peek rp
  return (fromIntegral ci, g')

foreign import ccall "selected_vertices"
  c_selected_vertices :: GraphPtr -> VsPtr -> VectorPtr -> IO Int

selectedVertices :: VertexSelector a -> IGraph (Graph d a) [a]
selectedVertices vs = do
  s <- vsSize vs
  runUnsafeIO $ \g -> do
    v <- newVector s
    (_e,g') <- withGraph g $ \gp -> withVs vs g $ \vsp -> withVector v $ \vp ->
      c_selected_vertices gp vsp vp
    l <- vectorToList v
    return (map (idToNode'' g . round) l, g')


--------------------------------------------------------------------------------
-- 13.1 Basic properties

foreign import ccall "igraph_are_connected"
  c_igraph_are_connected :: GraphPtr -> CInt -> CInt -> Ptr CInt -> IO CInt

areConnected :: a -> a -> IGraph (Graph d a) Bool
areConnected n1 n2 = do
  mi1 <- nodeToId n1
  mi2 <- nodeToId n2
  case (mi1, mi2) of
       (Just i1, Just i2) -> runUnsafeIO $ \g -> withGraph g $ \gp -> alloca $ \bp -> do
           e <- c_igraph_are_connected gp (fromIntegral i1) (fromIntegral i2) bp
           if e == 0 then
              peek bp >>= return . (== 1)
            else
              error $ "areConnected: igraph error " ++ show e
       _ -> return False


--------------------------------------------------------------------------------
-- 13.2 Shortest Path Related Functions

foreign import ccall "shortest_paths"
  c_igraph_shortest_paths :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr -> CInt -> IO CInt

shortestPaths :: Ord a => VertexSelector a -> VertexSelector a -> NeiMode -> IGraph (Graph d a) (Map a (Map a (Maybe Int)))
shortestPaths vf vt m = do
  l <- runUnsafeIO $ \g -> do
    ma <- newMatrix 0 0
    (_e,g') <- withGraph g $ \gp -> withMatrix ma $ \mp -> withVs vf g $ \vfp -> withVs vt g $ \vtp ->
      c_igraph_shortest_paths gp mp vfp vtp (fromIntegral $ fromEnum m)
    l <- matrixToList ma
    return (l,g')
  nf <- selectedVertices vf
  nt <- selectedVertices vt
  return $ Map.fromList . zip nf $ map (Map.fromList . zip nt . map roundMaybe) l
 where
  roundMaybe d = if d == 1/0 then Nothing else Just (round d)

foreign import ccall "igraph_get_shortest_path"
  c_igraph_get_shortest_path :: GraphPtr -> VectorPtr -> VectorPtr -> CInt -> CInt -> CInt -> IO CInt

getShortestPath :: a -> a -> NeiMode -> IGraph (Graph d a) ([a],[Edge d a])
getShortestPath n1 n2 m = do
  mi1 <- nodeToId n1
  mi2 <- nodeToId n2
  case (mi1, mi2) of
       (Just i1, Just i2) -> runUnsafeIO $ \g -> withGraph g $ \gp -> do
         v1 <- listToVector ([] :: [Int])
         v2 <- listToVector ([] :: [Int])
         e <- withVector v1 $ \vp1 -> withVector v2 $ \vp2 ->
              c_igraph_get_shortest_path gp vp1 vp2 (fromIntegral i1) (fromIntegral i2) (fromIntegral (fromEnum m))
         if e == 0 then do
            vert <- vectorToVertices g v1
            edgs <- vectorToEdges    g v2
            return ( vert, edgs )
          else
            error $ "getShortestPath: igraph error " ++ show e
       _ -> error "getShortestPath: Invalid nodes"


--------------------------------------------------------------------------------
-- 13.3 Neighborhood of a vertex

foreign import ccall "neighborhood"
  c_igraph_neighborhood :: GraphPtr -> VectorPtrPtr -> VsPtr -> CInt -> CInt -> IO CInt

neighborhood :: VertexSelector a -> Int -> NeiMode -> IGraph (Graph d a) [[a]]
neighborhood vs o m = runUnsafeIO $ \g -> do
  v        <- newVectorPtr 10
  (_e, g') <- withGraph g $ \gp -> withVs vs g $ \vsp -> withVectorPtr v $ \vp ->
    c_igraph_neighborhood gp vp vsp (fromIntegral o) (fromIntegral (fromEnum m))
  ids      <- vectorPtrToList v
  return (map (map (idToNode'' g . round)) ids, g')


--------------------------------------------------------------------------------
-- 13.4 Graph Components

foreign import ccall "igraph_subcomponent"
  c_igraph_subcomponent :: GraphPtr -> VectorPtr -> CDouble -> CInt -> IO CInt

subcomponent :: a -> NeiMode -> IGraph (Graph d a) [a]
subcomponent a m = do
  mi <- nodeToId a 
  case mi of
       Just i -> runUnsafeIO $ \g -> do
         v <- newVector 0
         (_e, g') <- withGraph g $ \gp -> withVector v $ \vp ->
           c_igraph_subcomponent gp vp (fromIntegral i) (fromIntegral $ fromEnum m)
         vx <- vectorToVertices g v
         return (vx, g')
       _ -> return []

{- same problem as with `subgraph'
foreign import ccall "igraph_induced_subcomponent"
  c_igraph_induced_subcomponent :: GraphPtr 
-}

foreign import ccall "subgraph"
  c_igraph_subgraph :: GraphPtr -> GraphPtr -> VsPtr -> IO CInt

subgraph :: VertexSelector a -> IGraph (Graph d a) (Graph d a)
subgraph vs = do
  setVertexIds
  runUnsafeIO $ \g ->
    withGraph g $ \gp -> do
      withVs vs g $ \vsp ->
        withGraph_ (emptyWithCtxt g) $ \gp' -> do
          _e <- c_igraph_subgraph gp gp' vsp
          subgraphFromPtr g gp'
 where
  emptyWithCtxt :: Graph d a -> Graph d a
  emptyWithCtxt (G _) = emptyGraph

foreign import ccall "igraph_is_connected"
  c_igraph_is_connected :: GraphPtr -> Ptr CInt -> CInt -> IO CInt

isConnected :: Connectedness -> IGraph (Graph d a) Bool
isConnected c = runUnsafeIO $ \g -> do
  withGraph g $ \gp -> alloca $ \b -> do
    _ <- c_igraph_is_connected gp b (fromIntegral $ fromEnum c)
    r <- peek b
    return $ r == 1

{- Somehow doesn't work:

foreign import ccall "igraph_decompose"
  c_igraph_decompose :: GraphPtr -> VectorPtrPtr -> CInt -> CLong -> CInt -> IO CInt

decompose :: Graph d a -> Connectedness -> Int -> Int -> [[a]]
decompose g m ma mi = unsafePerformIO $ do
  initIGraph
  vp <- newVectorPtr 0
  _e <- withGraph_ g $ \gp -> withVectorPtr vp $ \vpp ->
    c_igraph_decompose gp vpp (fromIntegral $ fromEnum m) (fromIntegral ma) (fromIntegral mi)
  vectorPtrToVertices g vp

-}
