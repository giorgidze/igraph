-- | Haskell bindings to the igraph C library.
--
-- See <http://igraph.sourceforge.net/doc/html/index.html> in the specified
-- section for more documentation about a specific function.
module Data.IGraph
  ( -- * Base types
    Graph (..), E (..)
  , D, U, IsUnweighted
  , Weighted, toEdgeWeighted, getWeight

    -- * Construction
  , emptyGraph
  , fromList, fromListWeighted
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
  , VertexSelector(..) --, NeiMode(..)

    -- ** 11\.3. Generic vertex selector operations
  , vsSize, selectedVertices

    -- * Chapter 13\. Structural Properties of Graphs

    -- ** 13\.1 Basic properties
  , areConnected

    -- ** 13\.2 Shortest Path Related Functions
  , shortestPaths,       shortestPathsDijkstra, shortestPathsBellmanFord, shortestPathsJohnson
  , getShortestPaths,    getShortestPathsDijkstra
  , getShortestPath,     getShortestPathDijkstra
  , getAllShortestPaths, getAllShortestPathsDijkstra
  , averagePathLength
  , pathLengthHist
  , diameter, diameter', diameter''
  , girth, girth'
  , eccentricity
  , radius

    -- ** 13\.4 Graph Components
  , subcomponent
  -- , subgraph
  , isConnected
  -- , decompose
  , Connectedness(..)

    -- ** 13\.5 Centrality Measures
  , closeness
  ) where

import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types

import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M

import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)


--------------------------------------------------------------------------------
-- 11.2 Vertex selector constructors

{- nothing here -}


--------------------------------------------------------------------------------
-- 11.3. Generic vertex selector operations

foreign import ccall "igraph_vs_size"
  c_igraph_vs_size :: GraphPtr -> VsPtr -> Ptr CInt -> IO CInt

vsSize :: Graph d a -> VertexSelector a -> Int
vsSize g vs = unsafePerformIO $ alloca $ \rp  ->
  withGraph g $ \gp ->
    withVs vs g $ \vsp -> do
      _ <- c_igraph_vs_size gp vsp rp
      ci <- peek rp 
      return (fromIntegral ci)

foreign import ccall "selected_vertices"
  c_selected_vertices :: GraphPtr -> VsPtr -> VectorPtr -> IO Int

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
  c_igraph_are_connected :: GraphPtr -> CInt -> CInt -> Ptr CInt -> IO CInt

areConnected :: Graph d a -> a -> a -> Bool
areConnected g n1 n2 = case (nodeToId g n1, nodeToId g n2) of
  (Just i1, Just i2) -> unsafePerformIO $ withGraph g $ \gp -> alloca $ \bp -> do
     _ <- c_igraph_are_connected gp (fromIntegral i1) (fromIntegral i2) bp
     fmap (== 1) (peek bp)
  _ -> False

--------------------------------------------------------------------------------
-- 13.2 Shortest Path Related Functions

foreign import ccall "shortest_paths"
  c_igraph_shortest_paths :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr -> CInt -> IO CInt

shortestPaths :: (Ord a, Hashable a)
              => Graph d a
              -> VertexSelector a
              -> VertexSelector a
              -> Map (a,a) (Maybe Int)
shortestPaths g vf vt =
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
                       (fromIntegral $ fromEnum Out)
             matrixToList ma
      nf = selectedVertices g vf
      nt = selectedVertices g vt
  in  M.fromList [ ((f,t), len)
                 | (f,lf)  <- zip nf ls
                 , (t,len) <- zip nt (map roundMaybe lf)
                 ]

roundMaybe :: Double -> Maybe Int
roundMaybe d = if d == 1/0 then Nothing else Just (round d)

{-- TODO:

2.2. igraph_shortest_paths_dijkstra — Weighted shortest paths from some sources.

  DONE: -}

foreign import ccall "shortest_paths_dijkstra"
  c_igraph_shortest_paths_dijkstra :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr
                                   -> VectorPtr -> CInt -> IO CInt

shortestPathsDijkstra :: (Ord a, Hashable a)
                      => Graph (Weighted d) a
                      -> VertexSelector a
                      -> VertexSelector a
                      -> Map (a,a) (Maybe Int)
shortestPathsDijkstra g vf vt =
  let ls = unsafePerformIO $ do
             ma <- newMatrix 0 0
             _e <- withGraph g $ \gp ->
                   withWeights g $ \wp ->
                   withMatrix ma $ \mp ->
                   withVs vf g $ \vfp ->
                   withVs vt g $ \vtp ->
                     c_igraph_shortest_paths_dijkstra
                       gp
                       mp
                       vfp
                       vtp
                       wp
                       (fromIntegral $ fromEnum Out)
             matrixToList ma
      nf = selectedVertices g vf
      nt = selectedVertices g vt
  in  M.fromList [ ((f,t), len)
                 | (f,lf)  <- zip nf ls
                 , (t,len) <- zip nt (map roundMaybe lf)
                 ]

{-
2.3. igraph_shortest_paths_bellman_ford — Weighted shortest paths from some sources allowing negative weights.

 DONE: -}

foreign import ccall "shortest_paths_bellman_ford"
  c_igraph_shortest_paths_bellman_ford :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr -> VectorPtr
                                       -> CInt -> IO CInt

shortestPathsBellmanFord :: (Ord a, Hashable a)
                         => Graph (Weighted d) a
                         -> VertexSelector a
                         -> VertexSelector a
                         -> Map (a,a) (Maybe Int)
shortestPathsBellmanFord g vf vt =
  let ls = unsafePerformIO $ do
             ma <- newMatrix 0 0
             _e <- withGraph g $ \gp ->
                   withWeights g $ \wp ->
                   withMatrix ma $ \mp ->
                   withVs vf g $ \vfp ->
                   withVs vt g $ \vtp ->
                     c_igraph_shortest_paths_bellman_ford
                       gp
                       mp
                       vfp
                       vtp
                       wp
                       (fromIntegral $ fromEnum Out)
             matrixToList ma
      nf = selectedVertices g vf
      nt = selectedVertices g vt
  in  M.fromList [ ((f,t), len)
                 | (f,lf)  <- zip nf ls
                 , (t,len) <- zip nt (map roundMaybe lf)
                 ]

{-
2.4. igraph_shortest_paths_johnson — Calculate shortest paths from some sources using Johnson's algorithm.

  DONE: -}

foreign import ccall "shortest_paths_johnson"
  c_igraph_shortest_paths_johnson :: GraphPtr -> MatrixPtr -> VsPtr -> VsPtr -> VectorPtr
                                  -> IO CInt

shortestPathsJohnson :: (Ord a, Hashable a)
                     => Graph (Weighted d) a
                     -> VertexSelector a
                     -> VertexSelector a
                     -> Map (a,a) (Maybe Int)
shortestPathsJohnson g vf vt =
  let ls = unsafePerformIO $ do
             ma <- newMatrix 0 0
             _e <- withGraph g $ \gp ->
                   withWeights g $ \wp ->
                   withMatrix ma $ \mp ->
                   withVs vf g $ \vfp ->
                   withVs vt g $ \vtp ->
                     c_igraph_shortest_paths_johnson
                       gp
                       mp
                       vfp
                       vtp
                       wp
             matrixToList ma
      nf = selectedVertices g vf
      nt = selectedVertices g vt
  in  M.fromList [ ((f,t), len)
                 | (f,lf)  <- zip nf ls
                 , (t,len) <- zip nt (map roundMaybe lf)
                 ]

{-
2.5. igraph_get_shortest_paths — Calculates the shortest paths from/to one vertex.

  DONE: -}

foreign import ccall "get_shortest_paths"
  c_igraph_get_shortest_paths :: GraphPtr -> VectorPtrPtr -> VectorPtrPtr -> CInt -> VsPtr -> CInt -> IO CInt

-- | This doesn't work? TODO
getShortestPaths :: Graph d a
                 -> a                     -- ^ from
                 -> VertexSelector a      -- ^ to
                 -> [ ([a],[Edge d a]) ]  -- ^ list of @(vertices, edges)@
getShortestPaths g f vt =
  let mfi = nodeToId g f
   in case mfi of
           Nothing -> error "getShortestPaths: Invalid node"
           Just fi -> unsafePerformIO $ do
             vpv <- newVectorPtr 0
             vpe <- newVectorPtr 0
             _e  <- withGraph g $ \gp ->
                      withVectorPtr vpv $ \vpvp ->
                      withVectorPtr vpe $ \vpep ->
                      withVs vt g $ \vtp ->
                       c_igraph_get_shortest_paths
                         gp
                         vpvp
                         vpep
                         (fromIntegral fi)
                         vtp
                         (fromIntegral $ fromEnum Out)
             v <- vectorPtrToVertices g vpv
             e <- vectorPtrToEdges    g vpe
             return $ zip v e

{-

2.6. igraph_get_shortest_path — Shortest path from one vertex to another one.

  DONE: -}

foreign import ccall "igraph_get_shortest_path"
  c_igraph_get_shortest_path :: GraphPtr -> VectorPtr -> VectorPtr -> CInt -> CInt -> CInt -> IO CInt

getShortestPath :: Graph d a -> a -> a -> ([a],[Edge d a])
getShortestPath g n1 n2 =
  let mi1 = nodeToId g n1
      mi2 = nodeToId g n2
  in  case (mi1, mi2) of
           (Just i1, Just i2) -> unsafePerformIO $
             withGraph g $ \gp -> do
               v1 <- listToVector ([] :: [Int])
               v2 <- listToVector ([] :: [Int])
               e <- withVector v1 $ \vp1 -> withVector v2 $ \vp2 ->
                     c_igraph_get_shortest_path gp vp1 vp2 (fromIntegral i1) (fromIntegral i2) (fromIntegral (fromEnum Out))
               if e == 0 then do
                   vert <- vectorToVertices g v1
                   edgs <- vectorToEdges    g v2
                   return ( vert, edgs )
                 else
                   error $ "getShortestPath: igraph error " ++ show e
           _ -> error "getShortestPath: Invalid nodes"

{-

2.7. igraph_get_shortest_paths_dijkstra — Calculates the weighted shortest paths from/to one vertex.

  DONE: -}

foreign import ccall "get_shortest_paths_dijkstra"
  c_igraph_get_shortest_paths_dijkstra :: GraphPtr -> VectorPtrPtr -> VectorPtrPtr
                                       -> CInt -> VsPtr -> VectorPtr -> CInt -> IO CInt

-- | This doesn't work? TODO
getShortestPathsDijkstra :: Graph (Weighted d) a
                         -> a                     -- ^ from
                         -> VertexSelector a      -- ^ to
                         -> [ ([a],[Edge (Weighted d) a]) ]  -- ^ list of @(vertices, edges)@
getShortestPathsDijkstra g f vt =
  let mfi = nodeToId g f
   in case mfi of
           Nothing -> error "getShortestPathsDijkstra: Invalid node"
           Just fi -> unsafePerformIO $ do
             vpv <- newVectorPtr 0
             vpe <- newVectorPtr 0
             _e  <- withGraph g $ \gp ->
                    withWeights g $ \wp ->
                    withVectorPtr vpv $ \vpvp ->
                    withVectorPtr vpe $ \vpep ->
                    withVs vt g $ \vtp ->
                     c_igraph_get_shortest_paths_dijkstra
                       gp
                       vpvp
                       vpep
                       (fromIntegral fi)
                       vtp
                       wp
                       (fromIntegral $ fromEnum Out)
             v <- vectorPtrToVertices g vpv
             e <- vectorPtrToEdges    g vpe
             return $ zip v e

{-
2.8. igraph_get_shortest_path_dijkstra — Weighted shortest path from one vertex to another one.

  DONE: -}

foreign import ccall "igraph_get_shortest_path_dijkstra"
  c_igraph_get_shortest_path_dijkstra :: GraphPtr -> VectorPtr -> VectorPtr -> CInt
                                      -> CInt -> VectorPtr -> CInt -> IO CInt

getShortestPathDijkstra :: Graph (Weighted d) a -> a -> a -> ([a],[Edge (Weighted d) a])
getShortestPathDijkstra g n1 n2 =
  let mi1 = nodeToId g n1
      mi2 = nodeToId g n2
  in  case (mi1, mi2) of
           (Just i1, Just i2) -> unsafePerformIO $ do
             v1 <- newVector 0
             v2 <- newVector 0
             e  <- withGraph g $ \gp ->
                   withWeights g $ \wp ->
                   withVector v1 $ \vp1 ->
                   withVector v2 $ \vp2 ->
                     c_igraph_get_shortest_path_dijkstra
                       gp
                       vp1
                       vp2
                       (fromIntegral i1)
                       (fromIntegral i2)
                       wp
                       (fromIntegral $ fromEnum Out)
             if e == 0 then do
                vert <- vectorToVertices g v1
                edgs <- vectorToEdges    g v2
                return ( vert, edgs )
              else
                error $ "getShortestPathDijkstra: igraph error " ++ show e
           _ -> error "getShortestPathDijkstra: Invalid nodes"

{-
2.9. igraph_get_all_shortest_paths — Finds all shortest paths (geodesics) from a vertex to all other vertices.

  DONE: -}

foreign import ccall "get_all_shortest_paths"
  c_igraph_get_all_shortest_paths :: GraphPtr
                                  -> VectorPtrPtr
                                  -> VectorPtr
                                  -> CInt
                                  -> VsPtr
                                  -> CInt
                                  -> IO CInt

getAllShortestPaths :: Graph d a
                    -> a                  -- ^ from
                    -> VertexSelector a   -- ^ to
                    -> [[a]]  -- ^ list of vertices along the shortest path from
                              -- @from@ to each other (reachable) vertex
getAllShortestPaths g f vt =
  let mfi = nodeToId g f
   in case mfi of
           Nothing -> error "getAllShortestPaths: Invalid node"
           Just fi -> unsafePerformIO $ do
             vpr <- newVectorPtr 0
             _e  <- withGraph g $ \gp ->
                      withVectorPtr vpr $ \vprp ->
                      withVs vt g $ \vtp ->
                        c_igraph_get_all_shortest_paths
                          gp
                          vprp 
                          nullPtr -- NULL
                          (fromIntegral fi)
                          vtp
                          (fromIntegral $ fromEnum Out)
             vectorPtrToVertices g vpr

{-

2.10. igraph_get_all_shortest_paths_dijkstra — Finds all shortest paths (geodesics) from a vertex to all other vertices.

  DONE: -}

foreign import ccall "get_all_shortest_paths_dijkstra"
  c_igraph_get_all_shortest_paths_dijkstra :: GraphPtr
                                           -> VectorPtrPtr
                                           -> VectorPtr
                                           -> CInt
                                           -> VsPtr
                                           -> VectorPtr
                                           -> CInt
                                           -> IO CInt

getAllShortestPathsDijkstra :: Graph (Weighted d) a
                            -> a                  -- ^ from
                            -> VertexSelector a   -- ^ to
                            -> [[a]]  -- ^ list of vertices along the shortest path from
                                      -- @from@ to each other (reachable) vertex
getAllShortestPathsDijkstra g f vt =
  let mfi = nodeToId g f
   in case mfi of
           Nothing -> error "getAllShortestPaths: Invalid node"
           Just fi -> unsafePerformIO $ do
             vpr <- newVectorPtr 0
             _e  <- withGraph g $ \gp ->
                    withWeights g $ \wp ->
                    withVectorPtr vpr $ \vprp ->
                    withVs vt g $ \vtp ->
                      c_igraph_get_all_shortest_paths_dijkstra
                        gp
                        vprp 
                        nullPtr -- NULL
                        (fromIntegral fi)
                        vtp
                        wp
                        (fromIntegral $ fromEnum Out)
             vectorPtrToVertices g vpr

{-
2.11. igraph_average_path_length — Calculates the average geodesic length in a graph.

  DONE: -}

foreign import ccall "igraph_average_path_length"
  c_igraph_average_path_length :: GraphPtr -> Ptr CDouble -> Bool -> Bool -> IO CInt

averagePathLength :: Graph d a
                  -> Bool     -- ^ Boolean, whether to consider directed paths. Ignored for undirected graphs.
                  -> Bool     -- ^ What to do if the graph is not connected. If
                              -- TRUE the average of the geodesics within the
                              -- components will be returned, otherwise the
                              -- number of vertices is used for the length of
                              -- non-existing geodesics. (The rationale behind
                              -- this is that this is always longer than the
                              -- longest possible geodesic in a graph.)
                  -> Double
averagePathLength g b1 b2 = unsafePerformIO $ do
  alloca $ \dp -> do
    _e <- withGraph g $ \gp ->
            c_igraph_average_path_length 
              gp
              dp
              b1
              b2
    realToFrac `fmap` peek dp

{-

2.12. igraph_path_length_hist — Create a histogram of all shortest path lengths.

  DONE: -}

foreign import ccall "igraph_path_length_hist"
  c_igraph_path_length_hist :: GraphPtr -> VectorPtr -> Ptr CDouble -> Bool -> IO CInt

pathLengthHist :: Graph d a
               -> Bool  -- ^ Whether to consider directed paths in a directed
                       -- graph (if not zero). This argument is ignored for
                       -- undirected graphs.
               -> ([Double],Double)
pathLengthHist g b = unsafePerformIO $ do
  v <- newVector 0
  d <- alloca $ \dp -> do
    _e <- withGraph g $ \gp ->
            withVector v $ \vp ->
             c_igraph_path_length_hist
               gp
               vp
               dp
               b
    realToFrac `fmap` peek dp
  l <- vectorToList v
  return (l,d)

{-

2.13. igraph_diameter — Calculates the diameter of a graph (longest geodesic).

  DONE: -}

foreign import ccall "igraph_diameter"
  c_igraph_diameter :: GraphPtr
                    -> Ptr CInt
                    -> Ptr CInt
                    -> Ptr CInt
                    -> VectorPtr
                    -> Bool
                    -> Bool
                    -> IO CInt

diameter :: Graph d a
         -> Bool -- ^ directed?
         -> Bool -- ^ unconnected?
         -> Int
diameter g b1 b2 = unsafePerformIO $ do
  alloca $ \ip -> do
    _e <- withGraph g $ \gp ->
      c_igraph_diameter
        gp
        ip
        nullPtr
        nullPtr
        nullPtr
        b1
        b2
    fromIntegral `fmap` peek ip

diameter' :: Graph d a
          -> Bool
          -> Bool
          -> (Int, (a,a)) -- ^ the diameter of the graph and the starting/end vertices
diameter' g b1 b2 = unsafePerformIO $ do
  alloca $ \ip -> do
    alloca $ \fip -> do
      alloca $ \tip -> do
        _e <- withGraph g $ \gp ->
                c_igraph_diameter
                  gp
                  ip
                  fip
                  tip
                  nullPtr
                  b1
                  b2
        d  <- fromIntegral `fmap` peek ip
        fi <- fromIntegral `fmap` peek fip
        ti <- fromIntegral `fmap` peek tip
        return (d, (idToNode'' g fi, idToNode'' g ti))

diameter'' :: Graph d a
           -> Bool
           -> Bool
           -> (Int, [a]) -- ^ the diameter of the graph and the actual longest path
diameter'' g b1 b2 = unsafePerformIO $ do
  alloca $ \ip -> do
    v  <- newVector 0
    _e <- withGraph g $ \gp ->
            withVector v $ \vp ->
              c_igraph_diameter
                gp
                ip
                nullPtr
                nullPtr
                vp
                b1
                b2
    d <- fromIntegral `fmap` peek ip
    p <- vectorToVertices g v
    return (d,p)

{-

2.14. igraph_diameter_dijkstra — Weighted diameter using Dijkstra's algorithm, non-negative weights only.

  int igraph_diameter_dijkstra(const igraph_t *graph,
                               const igraph_vector_t *weights,
                               igraph_real_t *pres,
                               igraph_integer_t *pfrom,
                               igraph_integer_t *pto,
                               igraph_vector_t *path,
                               igraph_bool_t directed,
                               igraph_bool_t unconn);

2.15. igraph_girth — The girth of a graph is the length of the shortest circle in it.

  DONE: -}

foreign import ccall "igraph_girth"
  c_igraph_girth :: GraphPtr -> Ptr CInt -> VectorPtr -> IO CInt

girth :: Graph d a -> Int
girth g = unsafePerformIO $ do
  alloca $ \ip -> do
    _e <- withGraph g $ \gp ->
            c_igraph_girth
              gp
              ip
              nullPtr
    fromIntegral `fmap` peek ip

girth' :: Graph d a
       -> (Int, [a])  -- ^ girth with the actual shortest circle
girth' g = unsafePerformIO $ do
  alloca $ \ip -> do
    v <- newVector 0
    _e <- withGraph g $ \gp ->
            withVector v $ \vp ->
              c_igraph_girth
                gp
                ip
                vp
    gr <- fromIntegral `fmap` peek ip
    s  <- vectorToVertices g v
    return (gr, s)

{-

2.16. igraph_eccentricity — Eccentricity of some vertices

  DONE: -}

foreign import ccall "eccentricity"
  c_igraph_eccentricity :: GraphPtr -> VectorPtr -> VsPtr -> CInt -> IO CInt

eccentricity :: Graph d a -> VertexSelector a -> [(a,Int)]
eccentricity g vs = unsafePerformIO $ do
  v  <- newVector 0
  _e <- withGraph g $ \gp ->
          withVs vs g $ \vsp ->
            withVector v $ \vp ->
              c_igraph_eccentricity
                gp
                vp
                vsp
                (fromIntegral $ fromEnum Out)
  l <- map round `fmap` vectorToList v
  return $ zip (selectedVertices g vs) l


{-

2.17. igraph_radius — Radius of a graph

  DONE: -}

foreign import ccall "igraph_radius"
  c_igraph_radius :: GraphPtr -> Ptr CDouble -> CInt -> IO CInt

radius :: Graph d a -> Int
radius g = unsafePerformIO $ do
  alloca $ \dp -> do
    _e <- withGraph g $ \gp ->
            c_igraph_radius
              gp
              dp
              (fromIntegral $ fromEnum Out)
    round `fmap` peek dp


--------------------------------------------------------------------------------
-- 13.3 Neighborhood of a vertex

{- TODO:

3.1. igraph_neighborhood_size — Calculates the size of the neighborhood of a given vertex.

  int igraph_neighborhood_size(const igraph_t *graph, igraph_vector_t *res,
                               igraph_vs_t vids, igraph_integer_t order,
                               igraph_neimode_t mode);

3.2. igraph_neighborhood — Calculate the neighborhood of vertices.

  DONE:

-}

{-

foreign import ccall "neighborhood"
  c_igraph_neighborhood :: GraphPtr d a -> VectorPtrPtr -> VsPtr -> CInt -> CInt -> IO CInt

neighborhood :: VertexSelector a -> Int -> IGraph (Graph d a) [[a]]
neighborhood vs o = runUnsafeIO $ \g -> do
  v        <- newVectorPtr 10
  (_e, g') <- withGraph g $ \gp -> withVs vs g $ \vsp -> withVectorPtr v $ \vp ->
    c_igraph_neighborhood gp vp vsp (fromIntegral o) (fromIntegral (fromEnum Out))
  ids      <- vectorPtrToList v
  return (map (map (idToNode'' g . round)) ids, g')

-}

{-

3.3. igraph_neighborhood_graphs — Create graphs from the neighborhood(s) of some vertex/vertices.

  int igraph_neighborhood_graphs(const igraph_t *graph, igraph_vector_ptr_t *res,
                                 igraph_vs_t vids, igraph_integer_t order,
                                 igraph_neimode_t mode);

-}


--------------------------------------------------------------------------------
-- 13.4 Graph Components

{- TODO

4.1. igraph_subcomponent — The vertices in the same component as a given vertex.

  DONE: -}

foreign import ccall "igraph_subcomponent"
  c_igraph_subcomponent :: GraphPtr -> VectorPtr -> CDouble -> CInt -> IO CInt

subcomponent :: Graph d a -> a -> [a]
subcomponent g a = case nodeToId g a of
  Just i -> unsafePerformIO $ do
    v <- newVector 0
    _ <- withGraph g $ \gp -> withVector v $ \vp ->
      c_igraph_subcomponent gp vp (fromIntegral i) (fromIntegral $ fromEnum Out)
    vectorToVertices g v
  _ -> []

{-

4.2. igraph_induced_subgraph — Creates a subgraph induced by the specified vertices.

  int igraph_induced_subgraph(const igraph_t *graph, igraph_t *res, 
                              const igraph_vs_t vids, igraph_subgraph_implementation_t impl);

foreign import ccall "igraph_induced_subcomponent"
  c_igraph_induced_subcomponent :: GraphPtr d a

4.3. igraph_subgraph_edges — Creates a subgraph with the specified edges and their endpoints.

  int igraph_subgraph_edges(const igraph_t *graph, igraph_t *res, 
                            const igraph_es_t eids, igraph_bool_t delete_vertices);

4.4. igraph_subgraph — Creates a subgraph induced by the specified vertices.

  int igraph_subgraph(const igraph_t *graph, igraph_t *res, 
                      const igraph_vs_t vids);

-}

{-
foreign import ccall "subgraph"
  c_igraph_subgraph :: GraphPtr d a -> GraphPtr d a -> VsPtr -> IO CInt

subgraph :: VertexSelector a -> IGraph (Graph d a) (Graph d a)
subgraph vs = do
  setVertexIds
  (_e, G Graph{ graphForeignPtr = Just subg }) <- runUnsafeIO $ \g ->
    withGraph g $ \gp -> do
      withVs vs g $ \vsp ->
        withGraph (emptyWithCtxt g) $ \gp' -> do
          c_igraph_subgraph gp gp' vsp
  g <- get
  return $ foreignGraph g subg
  --return $ G $ ForeignGraph subg (graphIdToNode g)
 where
  emptyWithCtxt :: Graph d a -> Graph d a
  emptyWithCtxt (G _) = emptyGraph
-}

{-

4.5. igraph_clusters — Calculates the (weakly or strongly) connected components in a graph.

  int igraph_clusters(const igraph_t *graph, igraph_vector_t *membership, 
                      igraph_vector_t *csize, igraph_integer_t *no,
                      igraph_connectedness_t mode);

4.6. igraph_is_connected — Decides whether the graph is (weakly or strongly) connected.

  DONE: -}

foreign import ccall "igraph_is_connected"
  c_igraph_is_connected :: GraphPtr -> Ptr CInt -> CInt -> IO CInt

isConnected :: Graph d a -> Connectedness -> Bool
isConnected g c = unsafePerformIO $ withGraph g $ \gp -> alloca $ \b -> do
  _ <- c_igraph_is_connected gp b (fromIntegral $ fromEnum c)
  r <- peek b
  return $ r == 1

{-

4.7. igraph_decompose — Decompose a graph into connected components.

foreign import ccall "igraph_decompose"
  c_igraph_decompose :: GraphPtr -> VectorPtrPtr -> CInt -> CLong -> CInt -> IO CInt

decompose :: Graph d a -> Connectedness -> Int -> Int -> [Graph d a]
decompose g m ma mi = unsafePerformIO $ do
  vp <- newVectorPtr 0
  _e <- withGraph g $ \gp ->
        withVectorPtr vp $ \vpp ->
          c_igraph_decompose
            gp
            vpp
            (fromIntegral $ fromEnum m)
            (fromIntegral ma)
            (fromIntegral mi)
  -- vectorPtrToVertices g vp -- wrong since the vectorptr contains graphs!

4.8. igraph_decompose_destroy — Free the memory allocated by igraph_decompose().

  void igraph_decompose_destroy(igraph_vector_ptr_t *complist);

necessary? I dunno :) no.

4.9. igraph_biconnected_components — Calculate biconnected components

  int igraph_biconnected_components(const igraph_t *graph,
                                    igraph_integer_t *no,
                                    igraph_vector_ptr_t *tree_edges,
                                    igraph_vector_ptr_t *component_edges,
                                    igraph_vector_ptr_t *components,
                                    igraph_vector_t *articulation_points);

4.10. igraph_articulation_points — Find the articulation points in a graph.

  int igraph_articulation_points(const igraph_t *graph,
                                 igraph_vector_t *res);

-}

--------------------------------------------------------------------------------
-- 13.5 Centrality Measures

{- TODO:

5.1. igraph_closeness — Closeness centrality calculations for some vertices.

  int igraph_closeness(const igraph_t *graph, igraph_vector_t *res,
                       const igraph_vs_t vids, igraph_neimode_t mode, 
                       const igraph_vector_t *weights);

  DONE: -}

foreign import ccall "closeness"
  c_igraph_closeness :: GraphPtr -> VectorPtr -> VsPtr -> CInt -> VectorPtr -> IO CInt

closeness :: Ord a => Graph d a -> VertexSelector a -> Map a Double
closeness g vs = unsafePerformIO $ do
  v  <- newVector 0
  _e <- withGraph g $ \gp ->
        withOptionalWeights g $ \wp ->
        withVs vs g $ \vsp ->
        withVector v $ \vp ->
          c_igraph_closeness
            gp
            vp
            vsp
            (fromIntegral $ fromEnum Out)
            wp
  scores <- vectorToList v
  return $ M.fromList $ zip (selectedVertices g vs) scores

{-

5.2. igraph_betweenness — Betweenness centrality of some vertices.

  int igraph_betweenness(const igraph_t *graph, igraph_vector_t *res,
                         const igraph_vs_t vids, igraph_bool_t directed, 
                         const igraph_vector_t* weights, igraph_bool_t nobigint);

5.3. igraph_edge_betweenness — Betweenness centrality of the edges.

  int igraph_edge_betweenness(const igraph_t *graph, igraph_vector_t *result,
                              igraph_bool_t directed, 
                              const igraph_vector_t *weights);

5.4. igraph_pagerank — Calculates the Google PageRank for the specified vertices.

  int igraph_pagerank(const igraph_t *graph, igraph_vector_t *vector,
                      igraph_real_t *value, const igraph_vs_t vids,
                      igraph_bool_t directed, igraph_real_t damping, 
                      const igraph_vector_t *weights,
                      igraph_arpack_options_t *options);

5.5. igraph_pagerank_old — Calculates the Google PageRank for the specified vertices.

  int igraph_pagerank_old(const igraph_t *graph, igraph_vector_t *res, 
                          const igraph_vs_t vids, igraph_bool_t directed,
                          igraph_integer_t niter, igraph_real_t eps, 
                          igraph_real_t damping, igraph_bool_t old);

5.6. igraph_personalized_pagerank — Calculates the personalized Google PageRank for the specified vertices.

  int igraph_personalized_pagerank(const igraph_t *graph, igraph_vector_t *vector,
                                   igraph_real_t *value, const igraph_vs_t vids,
                                   igraph_bool_t directed, igraph_real_t damping, 
                                   igraph_vector_t *reset,
                                   const igraph_vector_t *weights,
                                   igraph_arpack_options_t *options);

5.7. igraph_personalized_pagerank_vs — Calculates the personalized Google PageRank for the specified vertices.

  int igraph_personalized_pagerank_vs(const igraph_t *graph, igraph_vector_t *vector,
                                      igraph_real_t *value, const igraph_vs_t vids,
                                      igraph_bool_t directed, igraph_real_t damping, 
                                      igraph_vs_t reset_vids,
                                      const igraph_vector_t *weights,
                                      igraph_arpack_options_t *options);

5.8. igraph_constraint — Burt's constraint scores.

  int igraph_constraint(const igraph_t *graph, igraph_vector_t *res,
                        igraph_vs_t vids, const igraph_vector_t *weights);

5.9. igraph_maxdegree — Calculate the maximum degree in a graph (or set of vertices).

  int igraph_maxdegree(const igraph_t *graph, igraph_integer_t *res,
                       igraph_vs_t vids, igraph_neimode_t mode, 
                       igraph_bool_t loops);

5.10. igraph_strength — Strength of the vertices, weighted vertex degree in other words.

  int igraph_strength(const igraph_t *graph, igraph_vector_t *res,
                      const igraph_vs_t vids, igraph_neimode_t mode,
                      igraph_bool_t loops, const igraph_vector_t *weights);

5.11. igraph_eigenvector_centrality — Eigenvector centrality of the vertices

  int igraph_eigenvector_centrality(const igraph_t *graph, 
                                    igraph_vector_t *vector,
                                    igraph_real_t *value, 
                                    igraph_bool_t directed, igraph_bool_t scale,
                                    const igraph_vector_t *weights,
                                    igraph_arpack_options_t *options);

5.12. igraph_hub_score — Kleinberg's hub scores

  int igraph_hub_score(const igraph_t *graph, igraph_vector_t *vector,
                       igraph_real_t *value, igraph_bool_t scale,
                       const igraph_vector_t *weights,
                       igraph_arpack_options_t *options);

5.13. igraph_authority_score — Kleinerg's authority scores

  int igraph_authority_score(const igraph_t *graph, igraph_vector_t *vector,
                             igraph_real_t *value, igraph_bool_t scale,
                             const igraph_vector_t *weights,
                             igraph_arpack_options_t *options);

-}

--------------------------------------------------------------------------------
-- 13.6 Estimating Centrality Measures

{- TODO:

6.1. igraph_closeness_estimate — Closeness centrality estimations for some vertices.

  int igraph_closeness_estimate(const igraph_t *graph,
                                igraph_vector_t *res, 
                                const igraph_vs_t vids,
                                igraph_neimode_t mode,
                                igraph_real_t cutoff,
                                const igraph_vector_t *weights);

6.2. igraph_betweenness_estimate — Estimated betweenness centrality of some vertices.

  int igraph_betweenness_estimate(const igraph_t *graph,
                                  igraph_vector_t *res, 
                                  const igraph_vs_t vids,
                                  igraph_bool_t directed,
                                  igraph_real_t cutoff, 
                                  const igraph_vector_t *weights, 
                                  igraph_bool_t nobigint);

6.3. igraph_edge_betweenness_estimate — Estimated betweenness centrality of the edges.

  int igraph_edge_betweenness_estimate(const igraph_t *graph,
                                       igraph_vector_t *result,
                                       igraph_bool_t directed,
                                       igraph_real_t cutoff,
                                       const igraph_vector_t *weights);

-}

--------------------------------------------------------------------------------
-- 13.7 Centralization

{- TODO:

7.1. igraph_centralization — Calculate the centralization score from the node level scores

  igraph_real_t igraph_centralization(const igraph_vector_t *scores,
                                      igraph_real_t theoretical_max,
                                      igraph_bool_t normalized);

7.2. igraph_centralization_degree — Calculate vertex degree and graph centralization

  int igraph_centralization_degree(const igraph_t *graph,
                                   igraph_vector_t *res, 
                                   igraph_neimode_t mode, igraph_bool_t loops,
                                   igraph_real_t *centralization,
                                   igraph_real_t *theoretical_max,
                                   igraph_bool_t normalized);

7.3. igraph_centralization_betweenness — Calculate vertex betweenness and graph centralization

  int igraph_centralization_betweenness(const igraph_t *graph, 
                                        igraph_vector_t *res,
                                        igraph_bool_t directed,
                                        igraph_bool_t nobigint,
                                        igraph_real_t *centralization,
                                        igraph_real_t *theoretical_max,
                                        igraph_bool_t normalized);

7.4. igraph_centralization_closeness — Calculate vertex closeness and graph centralization

  int igraph_centralization_closeness(const igraph_t *graph, 
                                      igraph_vector_t *res, 
                                      igraph_neimode_t mode, 
                                      igraph_real_t *centralization,
                                      igraph_real_t *theoretical_max,
                                      igraph_bool_t normalized);

7.5. igraph_centralization_eigenvector_centrality — Calculate eigenvector centrality scores and graph centralization

  int igraph_centralization_eigenvector_centrality(const igraph_t *graph,
                                                   igraph_vector_t *vector,
                                                   igraph_real_t *value,
                                                   igraph_bool_t directed,
                                                   igraph_bool_t scale,
                                                   igraph_arpack_options_t *options,
                                                   igraph_real_t *centralization,
                                                   igraph_real_t *theoretical_max,
                                                   igraph_bool_t normalized);

7.6. igraph_centralization_degree_tmax — Theoretical maximum for graph centralization based on degree

  int igraph_centralization_degree_tmax(const igraph_t *graph, 
                                        igraph_integer_t nodes,
                                        igraph_neimode_t mode,
                                        igraph_bool_t loops,
                                        igraph_real_t *res);

7.7. igraph_centralization_betweenness_tmax — Theoretical maximum for graph centralization based on betweenness

  int igraph_centralization_betweenness_tmax(const igraph_t *graph, 
                                             igraph_integer_t nodes,
                                             igraph_bool_t directed,
                                             igraph_real_t *res);

7.8. igraph_centralization_closeness_tmax — Theoretical maximum for graph centralization based on closeness

  int igraph_centralization_closeness_tmax(const igraph_t *graph,
                                           igraph_integer_t nodes,
                                           igraph_neimode_t mode,
                                           igraph_real_t *res);

7.9. igraph_centralization_eigenvector_centrality_tmax — Theoretical maximum centralization for eigenvector centrality

  int igraph_centralization_eigenvector_centrality_tmax(const igraph_t *graph,
                                                        igraph_integer_t nodes,
                                                        igraph_bool_t directed,
                                                        igraph_bool_t scale, 
                                                        igraph_real_t *res);

-}

--------------------------------------------------------------------------------
-- 13.8 Similarity Measures

--------------------------------------------------------------------------------
-- 13.9 Spanning Trees

--------------------------------------------------------------------------------
-- 13.10 Transitivity or Clustering Coefficient

--------------------------------------------------------------------------------
-- 13.11 Directedness conversion

--------------------------------------------------------------------------------
-- 13.12 Spectral properties

--------------------------------------------------------------------------------
-- 13.13 Non-simple graphs: multiple and loop edges

--------------------------------------------------------------------------------
-- 13.14 Mixing patterns

--------------------------------------------------------------------------------
-- 13.15 K-Cores

--------------------------------------------------------------------------------
-- 13.16 Topological sorting, directed acyclic graphs

--------------------------------------------------------------------------------
-- 13.17 Maximum cardinality search, graph decomposition, chordal graphs

--------------------------------------------------------------------------------
-- 13.18 Matchings

--------------------------------------------------------------------------------
-- 13.19 Line graphs

--------------------------------------------------------------------------------
-- 13.20 Unfolding a graph into a tree

--------------------------------------------------------------------------------
-- 13.21 Other Operations

