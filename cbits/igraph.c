#include <stdlib.h>
#include <stdio.h>
#include <igraph/igraph.h>

igraph_t* c_igraph_create (const igraph_vector_t* edges, igraph_bool_t directed)
{
    igraph_t* graph = (igraph_t*) malloc (sizeof (igraph_t));
    igraph_create(graph, edges, 0, directed);
    return graph;
}

void c_igraph_destroy (igraph_t* graph)
{
    if (graph)
        igraph_destroy(graph);
    //free(graph);
    graph = NULL;
    return;
}

igraph_vector_t* c_igraph_vector_create (long int size)
{
    igraph_vector_t* vector = (igraph_vector_t*) malloc (sizeof (igraph_vector_t));
    igraph_vector_init(vector, size);
    igraph_vector_null(vector);
    return vector;
}

void c_igraph_vector_destroy (igraph_vector_t* vector)
{
    if (vector)
        igraph_vector_destroy(vector);
    //free(vector);
    vector = NULL;
    return;
}

igraph_vector_ptr_t* c_igraph_vector_ptr_create (long int size)
{
    igraph_vector_ptr_t* vector_ptr = (igraph_vector_ptr_t*) malloc (sizeof (igraph_vector_ptr_t));
    igraph_vector_ptr_init(vector_ptr, size);

    long int i = 0;
    for (i = 0; i < size; i++)
    {
        igraph_vector_ptr_set(vector_ptr, i, c_igraph_vector_create(0));
    }

    return vector_ptr;
}

void c_igraph_vector_ptr_destroy (igraph_vector_ptr_t* vector_ptr)
{

    long int i = 0;
    for (i = 0; i < igraph_vector_ptr_size(vector_ptr); i++)
    {
        c_igraph_vector_destroy(igraph_vector_ptr_e(vector_ptr, i));
    }

    if (vector_ptr)
        igraph_vector_ptr_destroy(vector_ptr);
    //free(vector_ptr);
    vector_ptr = NULL;
    return;
}

igraph_matrix_t* c_igraph_matrix_create (long int nrow, long int ncol)
{
    igraph_matrix_t* matrix = (igraph_matrix_t*) malloc (sizeof (igraph_matrix_t));
    igraph_matrix_init(matrix, nrow, ncol);
    igraph_matrix_null(matrix);
    return matrix;
}

void c_igraph_matrix_destroy (igraph_matrix_t* matrix)
{
    if (matrix)
        igraph_matrix_destroy(matrix);
    //free(matrix);
    matrix = NULL;
    return;
}

igraph_vs_t* c_igraph_vs_create ()
{
    igraph_vs_t* vs = (igraph_vs_t*) malloc (sizeof (igraph_vs_t));
    return vs;
}

void c_igraph_vs_destroy (igraph_vs_t* vs)
{
    if (vs)
        igraph_vs_destroy(vs);
    //free(vs);
    vs = NULL;
    return;
}

igraph_es_t* c_igraph_es_create()
{
    igraph_es_t* es = (igraph_es_t*) malloc(sizeof(igraph_es_t));
    return es;
}

void c_igraph_es_destroy(igraph_es_t* es)
{
    if (es)
        igraph_es_destroy(es);
    //free(es);
    es = NULL;
    return;
}

int es_fromto(igraph_es_t *es, igraph_vs_t *from, igraph_vs_t *to)
{
  return igraph_es_fromto(es, *from, *to);
}

igraph_arpack_options_t* c_arpack_create()
{
  igraph_arpack_options_t *arpack = (igraph_arpack_options_t*) malloc(sizeof(igraph_arpack_options_t));
  igraph_arpack_options_init(arpack);
  return arpack;
}

void c_arpack_destroy(igraph_arpack_options_t* arpack)
{
  if (arpack)
    free(arpack);
  arpack = NULL;
}

/*******************************************************************************
 * 
 * Helpers
 *
 */

igraph_vector_ptr_t* edges(const igraph_t *graph)
{
    igraph_vector_ptr_t* res = (igraph_vector_ptr_t*) malloc(sizeof(igraph_vector_ptr_t));
    igraph_vector_ptr_init(res, 2);
    igraph_vector_ptr_set(res, 0, (void*)&graph->from);
    igraph_vector_ptr_set(res, 1, (void*)&graph->to);
    return res;
}


/*******************************************************************************
 *
 * 11.3 Generic vertex selector operations
 *
 */

int selected_vertices(const igraph_t *graph, const igraph_vs_t *vs, igraph_vector_t* vector)
{
    igraph_vit_t vit;
    igraph_vit_create(graph, *vs, &vit);
    int i = 0;
    while (!IGRAPH_VIT_END(vit)) {
        igraph_vector_set(vector, i, IGRAPH_VIT_GET(vit));
        IGRAPH_VIT_NEXT(vit);
        i++;
    }
    igraph_vit_destroy(&vit);
    return 0;
}

/*******************************************************************************
 *
 * 11.8 Generic edge selector operations
 *
 */

int selected_edges(const igraph_t *graph, const igraph_es_t *es, igraph_vector_t* v)
{
    igraph_eit_t eit;
    igraph_eit_create(graph, *es, &eit);
    int i = 0;
    while (!IGRAPH_EIT_END(eit)) {
        igraph_vector_set(v, i, IGRAPH_EIT_GET(eit));
        IGRAPH_EIT_NEXT(eit);
        i++;
    }
    igraph_eit_destroy(&eit);
    return 0;
}

/*******************************************************************************
 *
 * 13.2 Shortest Path Related Functions
 *
 */

int shortest_paths(const igraph_t *graph, igraph_matrix_t *res, 
        const igraph_vs_t *from, const igraph_vs_t *to,
        igraph_neimode_t mode)
{
    return igraph_shortest_paths(graph, res, *from, *to, mode);
}

int shortest_paths_dijkstra(const igraph_t *graph, igraph_matrix_t *res, 
        const igraph_vs_t *from, const igraph_vs_t *to, const igraph_vector_t *weights,
        igraph_neimode_t mode)
{
    return igraph_shortest_paths_dijkstra(graph, res, *from, *to, weights, mode);
}

int shortest_paths_bellman_ford(const igraph_t *graph, igraph_matrix_t *res, 
        const igraph_vs_t *from, const igraph_vs_t *to, const igraph_vector_t *weights,
        igraph_neimode_t mode)
{
    return igraph_shortest_paths_bellman_ford(graph, res, *from, *to, weights, mode);
}

int shortest_paths_johnson(const igraph_t *graph, igraph_matrix_t *res, 
        const igraph_vs_t *from, const igraph_vs_t *to, const igraph_vector_t *weights)
{
    return igraph_shortest_paths_johnson(graph, res, *from, *to, weights);
}

int get_shortest_paths(const igraph_t *graph, 
                igraph_vector_ptr_t *vertices,
                igraph_vector_ptr_t *edges,
                igraph_integer_t from, const igraph_vs_t *to, 
                igraph_neimode_t mode)
{
    return igraph_get_shortest_paths(graph, vertices, edges, from, *to, mode);
}

int get_shortest_paths_dijkstra(const igraph_t *graph, 
                igraph_vector_ptr_t *vertices,
                igraph_vector_ptr_t *edges,
                igraph_integer_t from, const igraph_vs_t *to, 
                igraph_vector_t *weights, igraph_neimode_t mode)
{
    return igraph_get_shortest_paths_dijkstra(graph, vertices, edges, from, *to, weights, mode);
}

int get_all_shortest_paths(const igraph_t *graph,
                igraph_vector_ptr_t *res, 
                igraph_vector_t *nrgeo,
                igraph_integer_t from, const igraph_vs_t *to,
                igraph_neimode_t mode)
{
    return igraph_get_all_shortest_paths(graph, res, nrgeo, from, *to, mode);
}

int get_all_shortest_paths_dijkstra(const igraph_t *graph,
                igraph_vector_ptr_t *res, 
                igraph_vector_t *nrgeo,
                igraph_integer_t from, const igraph_vs_t *to,
                igraph_vector_t *weights, igraph_neimode_t mode)
{
    return igraph_get_all_shortest_paths_dijkstra(graph, res, nrgeo, from, *to, weights, mode);
}

int eccentricity(const igraph_t *graph, 
                igraph_vector_t *res,
                igraph_vs_t *vids,
                igraph_neimode_t mode)
{
    return igraph_eccentricity(graph, res, *vids, mode);
}

/*******************************************************************************
 *
 * 13.3 Neighborhood of a vertex
 *
 */

int neighborhood(const igraph_t *graph, igraph_vector_ptr_t *res,
      igraph_vs_t *vids, igraph_integer_t order,
      igraph_neimode_t mode)
{
    return igraph_neighborhood(graph, res, *vids, order, mode);
}

int neighborhood_graphs (const igraph_t *graph, igraph_vector_ptr_t *res, igraph_vs_t* vids, igraph_integer_t order, igraph_neimode_t mode)
{
    return igraph_neighborhood_graphs(graph, res, *vids, order, mode);
}


/*******************************************************************************
 *
 * 13.4 Graph Components
 *
 */

int induced_subgraph(const igraph_t *graph, igraph_t *res, 
        const igraph_vs_t *vids, igraph_subgraph_implementation_t impl)
{
    return igraph_induced_subgraph(graph, res, *vids, impl);
}


int subgraph(const igraph_t *graph, igraph_t *res, const igraph_vs_t* vids)
{
    return igraph_subgraph(graph, res, *vids);
}

int subgraph_edges(const igraph_t *graph, igraph_t *res,
                   const igraph_es_t *eids, igraph_bool_t delete_vertices)
{
    return igraph_subgraph_edges(graph, res, *eids, delete_vertices);
}

/*******************************************************************************
 *
 * 13.5 Centrality Measures
 *
 */

int closeness(const igraph_t *graph, igraph_vector_t *res, const igraph_vs_t* vids, igraph_neimode_t mode, const igraph_vector_t *weights)
{
    return igraph_closeness(graph, res, *vids, mode, weights);
}

int betweenness(const igraph_t *graph, igraph_vector_t *res, const igraph_vs_t* vids, igraph_bool_t directed, const igraph_vector_t* weights, igraph_bool_t nobigint)
{
    return igraph_betweenness(graph, res, *vids, directed, weights, nobigint);
}

int pagerank(const igraph_t *graph, igraph_vector_t *vector,
        igraph_real_t *value, const igraph_vs_t *vids,
        igraph_bool_t directed, igraph_real_t damping, 
        const igraph_vector_t *weights,
        igraph_arpack_options_t *options)
{
    return igraph_pagerank(graph, vector, value, *vids, directed, damping, weights, options);
}

int personalized_pagerank(const igraph_t *graph, igraph_vector_t *vector,
        igraph_real_t *value, const igraph_vs_t *vids,
        igraph_bool_t directed, igraph_real_t damping, 
        igraph_vector_t *reset,
        const igraph_vector_t *weights,
        igraph_arpack_options_t *options)
{
    return igraph_personalized_pagerank(graph, vector, value, *vids, directed, damping, reset, weights, options);
}

int personalized_pagerank_vs(const igraph_t *graph, igraph_vector_t *vector,
        igraph_real_t *value, const igraph_vs_t* vids,
        igraph_bool_t directed, igraph_real_t damping, 
        igraph_vs_t* reset_vids,
        const igraph_vector_t *weights,
        igraph_arpack_options_t *options)
{
    return igraph_personalized_pagerank_vs(graph, vector, value, *vids, directed, damping, *reset_vids, weights, options);
}

int constraint(const igraph_t *graph, igraph_vector_t *res,
          igraph_vs_t* vids, const igraph_vector_t *weights)
{
    return igraph_constraint(graph, res, *vids, weights);
}

int maxdegree(const igraph_t *graph, igraph_integer_t *res,
         igraph_vs_t *vids, igraph_neimode_t mode, 
         igraph_bool_t loops)
{
    return igraph_maxdegree(graph, res, *vids, mode, loops);
}

int strength(const igraph_t *graph, igraph_vector_t *res,
        const igraph_vs_t *vids, igraph_neimode_t mode,
        igraph_bool_t loops, const igraph_vector_t *weights)
{
    return igraph_strength(graph, res, *vids, mode, loops, weights);
}

/*******************************************************************************
 *
 * 13.6 Estimating Centrality Measures
 *
 */

int closeness_estimate(const igraph_t *graph, igraph_vector_t *res, 
                       const igraph_vs_t *vids, igraph_neimode_t mode,
                       igraph_real_t cutoff,
                       const igraph_vector_t *weights)
{
    return igraph_closeness_estimate(graph, res, *vids, mode, cutoff, weights);
}

int betweenness_estimate(const igraph_t *graph,
                         igraph_vector_t *res, 
                         const igraph_vs_t *vids,
                         igraph_bool_t directed,
                         igraph_real_t cutoff, 
                         const igraph_vector_t *weights, 
                         igraph_bool_t nobigint)
{
    return igraph_betweenness_estimate(graph, res, *vids, directed, cutoff, weights, nobigint);
}

/*******************************************************************************
 *
 * 13.8 Similarity Measures
 *
 */

int bibcoupling(const igraph_t *graph, igraph_matrix_t *res, 
                const igraph_vs_t *vids)
{
  return igraph_bibcoupling(graph, res, *vids);
}

int cocitation(const igraph_t *graph, igraph_matrix_t *res,
               const igraph_vs_t *vids)
{
  return igraph_cocitation(graph, res, *vids);
}

int similarity_jaccard(const igraph_t *graph, igraph_matrix_t *res,          
    const igraph_vs_t *vids, igraph_neimode_t mode, igraph_bool_t loops)
{
  return igraph_similarity_jaccard(graph, res, *vids, mode, loops);
}

int similarity_jaccard_es(const igraph_t *graph, igraph_vector_t *res,
  const igraph_es_t *es, igraph_neimode_t mode, igraph_bool_t loops)
{
  return igraph_similarity_jaccard_es(graph, res, *es, mode, loops);
}

int similarity_dice(const igraph_t *graph, igraph_matrix_t *res,
    const igraph_vs_t *vids, igraph_neimode_t mode, igraph_bool_t loops)
{
  return igraph_similarity_dice(graph, res, *vids, mode, loops);
}

int similarity_dice_es(const igraph_t *graph, igraph_vector_t *res,
  const igraph_es_t *es, igraph_neimode_t mode, igraph_bool_t loops)
{
  return igraph_similarity_dice_es(graph, res, *es, mode, loops);
}

int similarity_inverse_log_weighted(const igraph_t *graph,          
  igraph_matrix_t *res, const igraph_vs_t *vids, igraph_neimode_t mode)
{
  return igraph_similarity_inverse_log_weighted(graph, res, *vids, mode);
}

/*******************************************************************************
 *
 * 13.10
 *
 */

int transitivity_local_undirected(const igraph_t *graph,
           igraph_vector_t *res,
           const igraph_vs_t *vids,
           igraph_transitivity_mode_t mode)
{
  return igraph_transitivity_local_undirected(graph, res, *vids, mode);
}

int transitivity_barrat(const igraph_t *graph,
                igraph_vector_t *res,
                const igraph_vs_t *vids,
                const igraph_vector_t *weights,
                igraph_transitivity_mode_t mode)
{
  return igraph_transitivity_barrat(graph, res, *vids, weights, mode);
}

/*******************************************************************************
 *
 * 13.13
 *
 */

int is_loop(const igraph_t *graph, igraph_vector_bool_t *res, 
       igraph_es_t *es)
{
  return igraph_is_loop(graph, res, *es);
}

