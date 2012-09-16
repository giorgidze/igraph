-- | Haskell bindings to the igraph C library.
--
-- See <http://igraph.sourceforge.net/doc/html/index.html> in the specified
-- section for more documentation about a specific function.
module Data.IGraph
  ( -- * Base types
    Graph (..), E (..), D, U

    -- * Construction
  , emptyGraph
  , fromList
  , insertEdge
  , deleteEdge
  , deleteNode
    -- * Query
  , numberOfNodes
  , numberOfEdges
  , member
  , nodes
  , edges
  , neighbours

    -- * Chapter 11. Vertex and Edge Selectors and Sequences, Iterators

    -- ** 11\.2 Vertex selector constructors
  , VertexSelector(..), NeiMode(..)

    -- ** 11\.3. Generic vertex selector operations
  , vsSize, selectedVertices

    -- * Chapter 13\. Structural Properties of Graphs

    -- ** 13\.1 Basic properties
  , areConnected

    -- ** 13\.2 Shortest Path Related Functions
  , shortestPaths, getShortestPath

    -- ** 13\.4 Graph Components
  , subcomponent
  -- , subgraph
  , isConnected
  --, decompose
  , Connectedness(..)

  --, initIGraph
  --, setVertexIds, setVertexIds'
  --, getVertexIds, getVertexIds'
  ) where

import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types

import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HML

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)


--------------------------------------------------------------------------------
-- 11.2 Vertex selector constructors

{- nothing here -}


--------------------------------------------------------------------------------
-- 11.3. Generic vertex selector operations

foreign import ccall "igraph_vs_size"
  c_igraph_vs_size :: Ptr Void -> VsPtr -> Ptr CInt -> IO CInt

vsSize :: Graph d a -> VertexSelector a -> Int
vsSize g vs = unsafePerformIO $ alloca $ \rp  ->
  withGraph g $ \gp ->
    withVs vs g $ \vsp -> do
      _ <- c_igraph_vs_size gp vsp rp
      ci <- peek rp 
      return (fromIntegral ci)

foreign import ccall "selected_vertices"
  c_selected_vertices :: Ptr Void -> VsPtr -> VectorPtr -> IO Int

selectedVertices :: Graph d a -> VertexSelector a -> [a]
selectedVertices g vs = unsafePerformIO $ do
  let s = vsSize g vs
  v <- newVector s
  _ <- withGraph g  $ \gp  ->
       withVs vs g  $ \vsp ->
       withVector v $ \vp  ->
        c_selected_vertices gp vsp vp
  l <- vectorToList v
  return (map (idToNode'' g . round) l)


--------------------------------------------------------------------------------
-- 13.1 Basic properties

foreign import ccall "igraph_are_connected"
  c_igraph_are_connected :: Ptr Void -> CInt -> CInt -> Ptr CInt -> IO CInt

areConnected :: Graph d a -> a -> a -> Bool
areConnected g n1 n2 = case (nodeToId g n1, nodeToId g n2) of
  (Just i1, Just i2) -> unsafePerformIO $ withGraph g $ \gp -> alloca $ \bp -> do
     _ <- c_igraph_are_connected gp (fromIntegral i1) (fromIntegral i2) bp
     fmap (== 1) (peek bp)
  _ -> False

--------------------------------------------------------------------------------
-- 13.2 Shortest Path Related Functions

foreign import ccall "shortest_paths"
  c_igraph_shortest_paths :: Ptr Void -> MatrixPtr -> VsPtr -> VsPtr -> CInt -> IO CInt

shortestPaths :: (Ord a, Hashable a)
              => Graph d a
              -> VertexSelector a
              -> VertexSelector a
              -> NeiMode
              -> HashMap (a,a) (Maybe Int) -- ^ (lazy) `HashMap'
shortestPaths g vf vt m =
  let ls = unsafePerformIO $ do
             ma <- newMatrix 0 0
             _e <- withGraph g $ \gp ->
                   withMatrix ma $ \mp ->
                   withVs vf g $ \vfp ->
                   withVs vt g $ \vtp ->
                     c_igraph_shortest_paths
                       gp
                       mp
                       vfp
                       vtp
                       (fromIntegral $ fromEnum m)
             matrixToList ma
      nf = selectedVertices g vf
      nt = selectedVertices g vt
  in  HML.fromList [ ((f,t), len)
                   | (f,lf)  <- zip nf ls
                   , (t,len) <- zip nt (map roundMaybe lf)
                   ]
 where
  roundMaybe d = if d == 1/0 then Nothing else Just (round d)

foreign import ccall "igraph_get_shortest_path"
  c_igraph_get_shortest_path :: Ptr Void -> VectorPtr -> VectorPtr -> CInt -> CInt -> CInt -> IO CInt

getShortestPath :: Graph d a -> a -> a -> NeiMode -> ([a],[Edge d a])
getShortestPath g n1 n2 m =
  let mi1 = nodeToId g n1
      mi2 = nodeToId g n2
  in  case (mi1, mi2) of
           (Just i1, Just i2) -> unsafePerformIO $
             withGraph g $ \gp -> do
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

{-
foreign import ccall "neighborhood"
  c_igraph_neighborhood :: GraphPtr d a -> VectorPtrPtr -> VsPtr -> CInt -> CInt -> IO CInt

neighborhood :: VertexSelector a -> Int -> NeiMode -> IGraph (Graph d a) [[a]]
neighborhood vs o m = runUnsafeIO $ \g -> do
  v        <- newVectorPtr 10
  (_e, g') <- withGraph g $ \gp -> withVs vs g $ \vsp -> withVectorPtr v $ \vp ->
    c_igraph_neighborhood gp vp vsp (fromIntegral o) (fromIntegral (fromEnum m))
  ids      <- vectorPtrToList v
  return (map (map (idToNode'' g . round)) ids, g')
-}


--------------------------------------------------------------------------------
-- 13.4 Graph Components

foreign import ccall "igraph_subcomponent"
  c_igraph_subcomponent :: Ptr Void -> VectorPtr -> CDouble -> CInt -> IO CInt

subcomponent :: Graph d a -> a -> NeiMode -> [a]
subcomponent g a m = case nodeToId g a of
  Just i -> unsafePerformIO $ do
    v <- newVector 0
    _ <- withGraph g $ \gp -> withVector v $ \vp ->
      c_igraph_subcomponent gp vp (fromIntegral i) (fromIntegral $ fromEnum m)
    vectorToVertices g v
  _ -> []

{- same problem as with `subgraph'
foreign import ccall "igraph_induced_subcomponent"
  c_igraph_induced_subcomponent :: GraphPtr d a
-}

-- foreign import ccall "subgraph"
--   c_igraph_subgraph :: GraphPtr d a -> GraphPtr d a -> VsPtr -> IO CInt
-- 
-- subgraph :: VertexSelector a -> IGraph (Graph d a) (Graph d a)
-- subgraph vs = do
--   setVertexIds
--   (_e, G Graph{ graphForeignPtr = Just subg }) <- runUnsafeIO $ \g ->
--     withGraph g $ \gp -> do
--       withVs vs g $ \vsp ->
--         withGraph (emptyWithCtxt g) $ \gp' -> do
--           c_igraph_subgraph gp gp' vsp
--   g <- get
--   return $ foreignGraph g subg
--   --return $ G $ ForeignGraph subg (graphIdToNode g)
--  where
--   emptyWithCtxt :: Graph d a -> Graph d a
--   emptyWithCtxt (G _) = emptyGraph

foreign import ccall "igraph_is_connected"
  c_igraph_is_connected :: Ptr Void -> Ptr CInt -> CInt -> IO CInt

isConnected :: Graph d a -> Connectedness -> Bool
isConnected g c = unsafePerformIO $ withGraph g $ \gp -> alloca $ \b -> do
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
