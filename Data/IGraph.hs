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

{-- TODO:

2.2. igraph_shortest_paths_dijkstra — Weighted shortest paths from some sources.

  int igraph_shortest_paths_dijkstra(const igraph_t *graph,
                                     igraph_matrix_t *res,
                                     const igraph_vs_t from,
                                     const igraph_vs_t to,
                                     const igraph_vector_t *weights, 
                                     igraph_neimode_t mode);

2.3. igraph_shortest_paths_bellman_ford — Weighted shortest paths from some sources allowing negative weights.

  int igraph_shortest_paths_bellman_ford(const igraph_t *graph,
                                         igraph_matrix_t *res,
                                         const igraph_vs_t from,
                                         const igraph_vs_t to,
                                         const igraph_vector_t *weights, 
                                         igraph_neimode_t mode);

2.4. igraph_shortest_paths_johnson — Calculate shortest paths from some sources using Johnson's algorithm.

  int igraph_shortest_paths_johnson(const igraph_t *graph,
                                    igraph_matrix_t *res,
                                    const igraph_vs_t from,
                                    const igraph_vs_t to,
                                    const igraph_vector_t *weights);

2.5. igraph_get_shortest_paths — Calculates the shortest paths from/to one vertex.

  int igraph_get_shortest_paths(const igraph_t *graph, 
                                igraph_vector_ptr_t *vertices,
                                igraph_vector_ptr_t *edges,
                                igraph_integer_t from, const igraph_vs_t to, 
                                igraph_neimode_t mode);

2.6. igraph_get_shortest_path — Shortest path from one vertex to another one.

  done:

-}

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

{-

2.7. igraph_get_shortest_paths_dijkstra — Calculates the weighted shortest paths from/to one vertex.

  int igraph_get_shortest_paths_dijkstra(const igraph_t *graph,
                                         igraph_vector_ptr_t *vertices,
                                         igraph_vector_ptr_t *edges,
                                         igraph_integer_t from,
                                         igraph_vs_t to,
                                         const igraph_vector_t *weights,
                                         igraph_neimode_t mode);

2.8. igraph_get_shortest_path_dijkstra — Weighted shortest path from one vertex to another one.

  int igraph_get_shortest_path_dijkstra(const igraph_t *graph,
                                        igraph_vector_t *vertices,
                                        igraph_vector_t *edges,
                                        igraph_integer_t from,
                                        igraph_integer_t to,
                                        const igraph_vector_t *weights,
                                        igraph_neimode_t mode);

2.9. igraph_get_all_shortest_paths — Finds all shortest paths (geodesics) from a vertex to all other vertices.

  int igraph_get_all_shortest_paths(const igraph_t *graph,
                                    igraph_vector_ptr_t *res, 
                                    igraph_vector_t *nrgeo,
                                    igraph_integer_t from, const igraph_vs_t to,
                                    igraph_neimode_t mode);

2.10. igraph_get_all_shortest_paths_dijkstra — Finds all shortest paths (geodesics) from a vertex to all other vertices.

  int igraph_get_all_shortest_paths_dijkstra(const igraph_t *graph,
                                             igraph_vector_ptr_t *res, 
                                             igraph_vector_t *nrgeo,
                                             igraph_integer_t from, igraph_vs_t to,
                                             const igraph_vector_t *weights,
                                             igraph_neimode_t mode);

2.11. igraph_average_path_length — Calculates the average geodesic length in a graph.

  int igraph_average_path_length(const igraph_t *graph, igraph_real_t *res,
                                 igraph_bool_t directed, igraph_bool_t unconn);

2.12. igraph_path_length_hist — Create a histogram of all shortest path lengths.

  int igraph_path_length_hist(const igraph_t *graph, igraph_vector_t *res,
                              igraph_real_t *unconnected, igraph_bool_t directed);

2.13. igraph_diameter — Calculates the diameter of a graph (longest geodesic).

  int igraph_diameter(const igraph_t *graph, igraph_integer_t *pres, 
                      igraph_integer_t *pfrom, igraph_integer_t *pto, 
                      igraph_vector_t *path,
                      igraph_bool_t directed, igraph_bool_t unconn);

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

int igraph_girth(const igraph_t *graph, igraph_integer_t *girth, 
                 igraph_vector_t *circle);

2.16. igraph_eccentricity — Eccentricity of some vertices

  int igraph_eccentricity(const igraph_t *graph, 
                          igraph_vector_t *res,
                          igraph_vs_t vids,
                          igraph_neimode_t mode);

2.17. igraph_radius — Radius of a graph

  int igraph_radius(const igraph_t *graph, igraph_real_t *radius, 
                    igraph_neimode_t mode);

-}

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

neighborhood :: VertexSelector a -> Int -> NeiMode -> IGraph (Graph d a) [[a]]
neighborhood vs o m = runUnsafeIO $ \g -> do
  v        <- newVectorPtr 10
  (_e, g') <- withGraph g $ \gp -> withVs vs g $ \vsp -> withVectorPtr v $ \vp ->
    c_igraph_neighborhood gp vp vsp (fromIntegral o) (fromIntegral (fromEnum m))
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

  DONE:

-}

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
  c_igraph_is_connected :: Ptr Void -> Ptr CInt -> CInt -> IO CInt

isConnected :: Graph d a -> Connectedness -> Bool
isConnected g c = unsafePerformIO $ withGraph g $ \gp -> alloca $ \b -> do
  _ <- c_igraph_is_connected gp b (fromIntegral $ fromEnum c)
  r <- peek b
  return $ r == 1

{-

4.7. igraph_decompose — Decompose a graph into connected components.

  DONE: -}

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

{-

4.8. igraph_decompose_destroy — Free the memory allocated by igraph_decompose().

  void igraph_decompose_destroy(igraph_vector_ptr_t *complist);

necessary? I dunno :)

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
