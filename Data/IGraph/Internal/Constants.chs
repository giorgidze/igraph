{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-overlapping-patterns
            -fspec-constr-count=6
            #-}

module Data.IGraph.Internal.Constants where

#include <igraph/igraph_constants.h>
#include <igraph/igraph_iterators.h>

{#context prefix = "igraph" #}

sizeOfVit :: Int
sizeOfVit = {#sizeof vit_t #}

sizeOfVector :: Int
sizeOfVector = {#sizeof vector_t #}

{#enum i_directed_t as Directed
  { IGRAPH_UNDIRECTED as Undirected, IGRAPH_DIRECTED as Directed }
  deriving (Eq, Show) #}

{-
{#enum igraph_i_loops_t as Loops
  { IGRAPH_NO_LOOPS as NoLoops, IGRAPH_LOOPS as Loops }
  deriving (Eq, Show) #}

{#enum igraph_i_multiple_t as Multiple
  { IGRAPH_NO_MULTIPLE as NoMultiple, IGRAPH_MULTIPLE as Multiple }
  deriving (Eq, Show) #}

{#enum igraph_order_t as Order
  { IGRAPH_ASCENDING as Ascending, IGRAPH_DESCENDING as Descending }
  deriving (Eq, Show) #}

{#enum igraph_optimal_t as Optimal
  { IGRAPH_MINIMUM as Minimum, IGRAPH_MAXIMUM as Maximum }
  deriving (Eq, Show) #}
-}

-- | A constant specifying how edge directions are considered in directed
-- graphs. Valid modes are: `Out' follows edge directions; `In' follows
-- the opposite directions; and `All' ignores edge directions. This argument
-- is ignored for undirected graphs.
{#enum neimode_t as NeiMode
  { IGRAPH_OUT as Out, IGRAPH_IN as In, IGRAPH_ALL as All, IGRAPH_TOTAL as Total }
  deriving (Eq, Show) #}

-- | For a directed graph this specifies whether to calculate weak or strong
-- connectedness. This argument is ignored for undirected graphs.
{#enum connectedness_t as Connectedness
  { IGRAPH_WEAK as Weak, IGRAPH_STRONG as Strong }
  deriving (Eq, Show) #}

{-
{#enum igraph_reciprocity_t as Reciprocity
  { IGRAPH_RECIPROCITY_DEFAULT as ReciprocityDefault, IGRAPH_RECIPROCITY_RATIO as ReciprocityRatio }
  deriving (Eq, Show) #}

{#enum igraph_adjacency_t as Adjacency
  { IGRAPH_ADJ_DIRECTED as AdjDirected, IGRAPH_ADJ_UNDIRECTED as AdjUndirected
  , IGRAPH_ADJ_MAX as AdjMax, IGRAPH_ADJ_UPPER as AdjUpper, IGRAPH_ADJ_LOWER as AdjLower
  , IGRAPH_ADJ_MIN as AdjMin, IGRAPH_ADJ_PLUS as AdjPlus }
  deriving (Eq, Show) #}

{#enum igraph_star_mode_t as StarMode
  { IGRAPH_STAR_OUT as StarOut, IGRAPH_STAR_IN as StarIn
  , IGRAPH_STAR_UNDIRECTED as StarUndirected, IGRAPH_STAR_MUTUAL as StarMutual }
  deriving (Eq, Show) #}

{#enum igraph_tree_mode_t as TreeMode
  { IGRAPH_TREE_OUT as TreeOut, IGRAPH_TREE_IN as TreeIn
  , IGRAPH_TREE_UNDIRECTED as TreeUndirected }
  deriving (Eq, Show) #}

{#enum igraph_erdos_renyi_t as ErdosRenyi
  { IGRAPH_ERDOS_RENYI_GNP as ErdosRenyiGnp
  , IGRAPH_ERDOS_RENYI_GNM as ErdosRenyiGnm }
  deriving (Eq, Show) #}

{#enum igraph_get_adjacency_t as GetAdjacency
  { IGRAPH_GET_ADJACENCY_UPPER as GetAdjacencyUpper
  , IGRAPH_GET_ADJACENCY_LOWER as GetAdjacencyLower
  , IGRAPH_GET_ADJACENCY_BOTH  as GetAdjacencyBoth }
  deriving (Eq, Show) #}

{#enum igraph_degseq_t as Degseq
  { IGRAPH_DEGSEQ_SIMPLE as DegseqSimple
  , IGRAPH_DEGSEQ_VL     as DegseqVl }
  deriving (Eq, Show) #}

{#enum igraph_fileformat_type_t as FileformatType
  { IGRAPH_FILEFORMAT_EDGELIST as FileformatEdgelist
  , IGRAPH_FILEFORMAT_NCOL     as FileformatNcol
  , IGRAPH_FILEFORMAT_PAJEK    as FileformatPajek
  , IGRAPH_FILEFORMAT_LGL      as FileformatLgl
  , IGRAPH_FILEFORMAT_GRAPHML  as FileformatGraphml }
  deriving (Eq, Show) #}

{#enum igraph_rewiring_t as Rewiring
  { IGRAPH_REWIRING_SIMPLE as RewiringSimple }
  deriving (Eq, Show) #}

--
-- TODO
--

{#enum igraph_edgeorder_type_t as
  { IGRAPH_EDGEORDER_ID=0,
         IGRAPH_EDGEORDER_FROM,
         IGRAPH_EDGEORDER_TO }
  deriving (Eq, Show) #}

{#enum igraph_to_directed_t as
  { IGRAPH_TO_DIRECTED_ARBITRARY=0,
         IGRAPH_TO_DIRECTED_MUTUAL }
  deriving (Eq, Show) #}

{#enum igraph_to_undirected_t as
  { IGRAPH_TO_UNDIRECTED_EACH=0,
         IGRAPH_TO_UNDIRECTED_COLLAPSE,
               IGRAPH_TO_UNDIRECTED_MUTUAL}
  deriving (Eq, Show) #}

{#enum igraph_vconn_nei_t as
  { IGRAPH_VCONN_NEI_ERROR=0,
         IGRAPH_VCONN_NEI_NUMBER_OF_NODES,
         IGRAPH_VCONN_NEI_IGNORE,
         IGRAPH_VCONN_NEI_NEGATIVE }
  deriving (Eq, Show) #}

{#enum igraph_spincomm_update_t as
  { IGRAPH_SPINCOMM_UPDATE_SIMPLE=0,
         IGRAPH_SPINCOMM_UPDATE_CONFIG }
  deriving (Eq, Show) #}

{#enum igraph_lazy_adlist_simplify_t as
  { IGRAPH_DONT_SIMPLIFY=0,
         IGRAPH_SIMPLIFY }
  deriving (Eq, Show) #}

{#enum igraph_transitivity_mode_t as
  { IGRAPH_TRANSITIVITY_NAN=0,
               IGRAPH_TRANSITIVITY_ZERO }
  deriving (Eq, Show) #}

{#enum igraph_spinglass_implementation_t as
  { IGRAPH_SPINCOMM_IMP_ORIG=0,
         IGRAPH_SPINCOMM_IMP_NEG }
  deriving (Eq, Show) #}

{#enum igraph_community_comparison_t as
  { IGRAPH_COMMCMP_VI = 0,
               IGRAPH_COMMCMP_NMI,
               IGRAPH_COMMCMP_SPLIT_JOIN,
               IGRAPH_COMMCMP_RAND,
               IGRAPH_COMMCMP_ADJUSTED_RAND }
  deriving (Eq, Show) #}

{#enum igraph_add_weights_t as
  { IGRAPH_ADD_WEIGHTS_NO = 0,
               IGRAPH_ADD_WEIGHTS_YES,
               IGRAPH_ADD_WEIGHTS_IF_PRESENT }
  deriving (Eq, Show) #}

{#enum igraph_barabasi_algorithm_t as
  { IGRAPH_BARABASI_BAG = 0,
         IGRAPH_BARABASI_PSUMTREE,
         IGRAPH_BARABASI_PSUMTREE_MULTIPLE}
  deriving (Eq, Show) #}

{#enum igraph_fas_algorithm_t as
  { IGRAPH_FAS_EXACT_IP = 0,
         IGRAPH_FAS_APPROX_EADES }
  deriving (Eq, Show) #}

{#enum igraph_subgraph_implementation_t as
  { IGRAPH_SUBGRAPH_AUTO = 0,
             IGRAPH_SUBGRAPH_COPY_AND_DELETE,
         IGRAPH_SUBGRAPH_CREATE_FROM_SCRATCH
       }
  deriving (Eq, Show) #}

{#enum igraph_imitate_algorithm_t as ImitateAlgorithm
  { ImitateAugmented, ImitateBlind, ImitateContracted }
  deriving (Eq, Show) #}
-}

{-
typedef igraph_real_t  igraph_scalar_function_t(const igraph_vector_t *var,
            const igraph_vector_t *par,
            void* extra);
typedef void igraph_vector_function_t(const igraph_vector_t *var,
              const igraph_vector_t *par,
              igraph_vector_t* res, void* extra);
-}
