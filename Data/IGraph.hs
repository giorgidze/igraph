{-# LANGUAGE PatternGuards, ForeignFunctionInterface #-}

module Data.IGraph
  ( Graph (..), G, Gr (..), D, U
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
  --, subgraph
  , isConnected
  , Connectedness(..)
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
  _e  <- withVs fvs c_igraph_vs_all
  return fvs

foreign import ccall "igraph_vs_adj"
  c_igraph_vs_adj :: VsPtr -> CInt -> CInt -> IO CInt

vsAdj :: a -> NeiMode -> (VertexSelector a)
vsAdj n m = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs fvs $ \vsp ->
      c_igraph_vs_adj vsp (fromIntegral i) (fromIntegral (fromEnum m))
  return fvs

foreign import ccall "igraph_vs_nonadj"
  c_igraph_vs_nonadj :: VsPtr -> CInt -> CInt -> IO CInt

vsNonadj :: a -> NeiMode -> VertexSelector a
vsNonadj n m = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs fvs $ \vsp ->
      c_igraph_vs_nonadj vsp (fromIntegral i) (fromIntegral (fromEnum m))
  return fvs

foreign import ccall "igraph_vs_none"
  c_igraph_vs_none :: VsPtr -> IO CInt

vsNone :: VertexSelector a
vsNone = Vs $ \_ -> do
  vs <- newVs
  _e <- withVs vs c_igraph_vs_none
  return vs

foreign import ccall "igraph_vs_1"
  c_igraph_vs_1 :: VsPtr -> CInt -> IO CInt

vs1 :: a -> VertexSelector a
vs1 n = Vs $ \ident -> do
  fvs <- newVs
  _e <- onSuccessfulVsIdent fvs (ident n) $ \i ->
    withVs fvs $ \vsp -> c_igraph_vs_1 vsp (fromIntegral i)
  return fvs

foreign import ccall "igraph_vs_vector"
  c_igraph_vs_vector :: VsPtr -> VectorPtr -> IO CInt

vsList :: [a] -> VertexSelector a
vsList l = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (sequence (map ident l)) $ \is -> do
    v  <- listToVector is
    withVs fvs $ \vsp -> withVector v $ \vp ->
      c_igraph_vs_vector vsp vp
  return fvs

foreign import ccall "igraph_vs_seq"
  c_igraph_vs_seq :: VsPtr -> CInt -> CInt -> IO CInt

vsSeq :: a -> a -> VertexSelector a
vsSeq f t = Vs $ \ident -> do
  fvs <- newVs
  _e  <- onSuccessfulVsIdent fvs (zipM (ident f) (ident t)) $ \(fi,ti) ->
    withVs fvs $ \vsp ->
      c_igraph_vs_seq vsp (fromIntegral fi) (fromIntegral ti)
  return fvs
 where
  zipM ma mb = do
    a <- ma
    b <- mb
    return (a,b)

onSuccessfulVsIdent :: VsForeignPtr -> Maybe a -> (a -> IO CInt) -> IO CInt
onSuccessfulVsIdent fvs Nothing  _ = withVs fvs c_igraph_vs_none
onSuccessfulVsIdent _   (Just a) f = f a


--------------------------------------------------------------------------------
-- 11.3. Generic vertex selector operations

foreign import ccall "igraph_vs_is_all"
  c_igraph_vs_is_all :: VsPtr -> IO CInt

vsIsAll :: VertexSelector a -> Bool
vsIsAll vs = unsafePerformIO $ do
  fvs <- applyVs (const Nothing) vs
  r <- withVs fvs $ \vsp ->
    c_igraph_vs_is_all vsp
  return $ r == 1

foreign import ccall "igraph_vs_size"
  c_igraph_vs_size :: GraphPtr -> VsPtr -> Ptr CInt -> IO CInt

vsSize :: Graph d a -> VertexSelector a -> Int
vsSize g vs = unsafePerformIO $ alloca $ \rp -> do
  fvs <- applyVs (nodeToId g) vs
  _e <- withGraph_ g $ \gp -> withVs fvs $ \vsp ->
    c_igraph_vs_size gp vsp rp
  ci <- peek rp
  return $ fromIntegral ci

foreign import ccall "selected_vertices"
  c_selected_vertices :: GraphPtr -> VsPtr -> VectorPtr -> IO Int

selectedVertices :: Graph d a -> VertexSelector a -> [a]
selectedVertices g vs = unsafePerformIO $ do
  v   <- newVector (vsSize g vs)
  fvs <- applyVs (nodeToId g) vs
  _e  <- withGraph_ g $ \gp -> withVs fvs $ \vsp -> withVector v $ \vp ->
    c_selected_vertices gp vsp vp
  l   <- vectorToList v
  return $ map (idToNode' g . round) l


--------------------------------------------------------------------------------
-- 13.1 Basic properties

foreign import ccall "igraph_are_connected"
  c_igraph_are_connected :: GraphPtr -> CInt -> CInt -> Ptr CInt -> IO CInt

areConnected :: Graph d a -> a -> a -> Bool
areConnected g@(G _) n1 n2
  | Just i1 <- nodeToId g n1
  , Just i2 <- nodeToId g n2
  = unsafePerformIO $ withGraph_ g $ \gp -> alloca $ \bp -> do
    e <- c_igraph_are_connected gp (fromIntegral i1) (fromIntegral i2) bp
    if e == 0 then
       peek bp >>= return . (== 1)
     else
       error $ "areConnected: igraph error " ++ show e
  | otherwise = False


--------------------------------------------------------------------------------
-- 13.2 Shortest Path Related Functions

foreign import ccall "shortest_paths"
  c_igraph_shortest_paths :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr -> CInt -> IO CInt

shortestPaths :: Ord a => Graph d a -> VertexSelector a -> VertexSelector a -> NeiMode -> Map a (Map a (Maybe Int))
shortestPaths g@(G _) vsf vst m = unsafePerformIO $ do
  ma <- newMatrix 0 0
  vf <- applyVs (nodeToId g) vsf
  vt <- applyVs (nodeToId g) vst
  _e <- withGraph_ g $ \gp -> withMatrix ma $ \mp -> withVs vf $ \vfp -> withVs vt $ \vtp ->
    c_igraph_shortest_paths gp mp vfp vtp (fromIntegral $ fromEnum m)
  l  <- matrixToList ma
  let nf = selectedVertices g vsf
      nt = selectedVertices g vst
  return $ Map.fromList . zip nf $ map (Map.fromList . zip nt . map roundMaybe) l
 where
  roundMaybe d = if d == 1/0 then Nothing else Just (round d)

foreign import ccall "igraph_get_shortest_path"
  c_igraph_get_shortest_path :: GraphPtr -> VectorPtr -> VectorPtr -> CInt -> CInt -> CInt -> IO CInt

getShortestPath :: Graph d a -> a -> a -> NeiMode -> ([a],[Edge d a])
getShortestPath g@(G _) n1 n2 m
  | Just i1 <- nodeToId g n1
  , Just i2 <- nodeToId g n2
  = unsafePerformIO $ withGraph_ g $ \gp -> do
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
  | otherwise = error "getShortestPath: Invalid nodes"


--------------------------------------------------------------------------------
-- 13.3 Neighborhood of a vertex

foreign import ccall "neighborhood"
  c_igraph_neighborhood :: GraphPtr -> VectorPtrPtr -> VsPtr -> CInt -> CInt -> IO CInt

neighborhood :: Graph d a -> VertexSelector a -> Int -> NeiMode -> [[a]]
neighborhood g vs o m = unsafePerformIO $ do
  fvs <- applyVs (nodeToId g) vs
  v   <- newVectorPtr 10
  _e  <- withGraph_ g $ \gp -> withVs fvs $ \vsp -> withVectorPtr v $ \vp ->
    c_igraph_neighborhood gp vp vsp (fromIntegral o) (fromIntegral (fromEnum m))
  ids <- vectorPtrToList v
  return $ map (map (idToNode' g . round)) ids


--------------------------------------------------------------------------------
-- 13.4 Graph Components

{- Problem with `subgraphs' is that we use ID->Label node identification, but
 - the subgraph function reassigns ids to vertices, making it impossible for us
 - to re-identify the vertices by their IDs. Not sure how this could be
 - implemented at all…

foreign import ccall "subgraph"
  c_igraph_subgraph :: GraphPtr -> GraphPtr -> VsPtr -> IO CInt

subgraph :: Graph d a -> VertexSelector a -> Graph d a
subgraph g@(G _) vs = unsafePerformIO $ do
  fvs <- applyVs (nodeToId g) vs
  withGraph_ g $ \gp -> withVs fvs $ \vsp ->
    withGraph_ (emptyWithCtxt g) $ \gp' -> do
      _e      <- c_igraph_subgraph gp gp' vsp
      (G g'') <- subgraphFromPtr g gp'
      fgp'    <- newForeignPtr c_igraph_destroy gp'
      return $ G g'' { graphForeignPtr = Just fgp' }
 where
  emptyWithCtxt :: Graph d a -> Graph d a
  emptyWithCtxt (G _) = emptyGraph
-}

foreign import ccall "igraph_is_connected"
  c_igraph_is_connected :: GraphPtr -> Ptr CInt -> CInt -> IO CInt

isConnected :: Graph d a -> Connectedness -> Bool
isConnected g@(G _) c = unsafePerformIO $ withGraph_ g $ \gp -> alloca $ \b -> do
  _ <- c_igraph_is_connected gp b (fromIntegral $ fromEnum c)
  r <- peek b
  return $ r == 1
