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
    free(graph);
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
    free(vector);
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
    free(vector_ptr);
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
    free(matrix);
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
    free(vs);
    return;
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
 * 12. Graph, Vertex and Edge Attributes
 *
 */

int at_init(igraph_t *graph, igraph_vector_ptr_t *attr) { return 0; }
void at_destroy(igraph_t *graph) { return; }
int at_copy(igraph_t *to, const igraph_t *from, igraph_bool_t ga,
      igraph_bool_t va, igraph_bool_t ea) { return 0; }
int at_add_vertices(igraph_t *graph, long int nv, igraph_vector_ptr_t *attr) { return 0; }
int at_permute_vertices(const igraph_t *graph,
      igraph_t *newgraph,
      const igraph_vector_t *idx) { return 0; }
int at_combine_vertices(const igraph_t *graph,
      igraph_t *newgraph,
      const igraph_vector_ptr_t *merges,
      const igraph_attribute_combination_t *comb) { return 0; }
int at_add_edges(igraph_t *graph, const igraph_vector_t *edges,
     igraph_vector_ptr_t *attr) { return 0; }
int at_permute_edges(const igraph_t *graph,
         igraph_t *newgraph, const igraph_vector_t *idx) { return 0; }
int at_combine_edges(const igraph_t *graph,
         igraph_t *newgraph,
         const igraph_vector_ptr_t *merges,
         const igraph_attribute_combination_t *comb) { return 0; }
int at_get_info(const igraph_t *graph,
    igraph_strvector_t *gnames, igraph_vector_t *gtypes,
    igraph_strvector_t *vnames, igraph_vector_t *vtypes,
    igraph_strvector_t *enames, igraph_vector_t *etypes) { return 0; }
igraph_bool_t at_has_attr(const igraph_t *graph, igraph_attribute_elemtype_t type,
        const char *name) { return 0; }
int at_gettype(const igraph_t *graph, igraph_attribute_type_t *type,
   igraph_attribute_elemtype_t elemtype, const char *name) { return 0; }
int at_get_numeric_graph_attr(const igraph_t *graph, const char *name,
      igraph_vector_t *value) { return 0; }
int at_get_string_graph_attr(const igraph_t *graph, const char *name,
           igraph_strvector_t *value) { return 0; }
int at_get_numeric_vertex_attr(const igraph_t *graph, const char *name,
       igraph_vs_t vs,
       igraph_vector_t *value) { return 0; }
int at_get_string_vertex_attr(const igraph_t *graph, const char *name,
      igraph_vs_t vs,
      igraph_strvector_t *value) { return 0; }
int at_get_numeric_edge_attr(const igraph_t *graph, const char *name,
           igraph_es_t es,
           igraph_vector_t *value) { return 0; }
int at_get_string_edge_attr(const igraph_t *graph, const char *name,
          igraph_es_t es,
          igraph_strvector_t *value) { return 0; }

igraph_attribute_table_t* create_attribute_table()
{
  igraph_attribute_table_t* t = (igraph_attribute_table_t*) malloc(sizeof(igraph_attribute_table_t));
  t->init = &at_init;
  t->destroy = &at_destroy;
  t->copy = &at_copy;
  t->add_vertices = &at_add_vertices;
  t->permute_vertices = &at_permute_vertices;
  t->combine_vertices = &at_combine_vertices;
  t->add_edges = &at_add_edges;
  t->permute_edges = &at_permute_edges;
  t->combine_edges = &at_combine_edges;
  t->get_info = &at_get_info;
  t->gettype = &at_gettype;
  t->get_numeric_graph_attr = &at_get_numeric_graph_attr;
  t->get_string_graph_attr = &at_get_string_graph_attr;
  t->get_numeric_vertex_attr = &at_get_numeric_vertex_attr;
  t->get_string_vertex_attr = &at_get_string_vertex_attr;
  t->get_numeric_edge_attr = &at_get_numeric_edge_attr;
  t->get_string_edge_attr = &at_get_string_edge_attr;
  return t;
}

void destroy_attribute_table(igraph_attribute_table_t* t)
{
    free(t);
    return;
}

void set_attribute_permute_vertices(igraph_attribute_table_t* t,
    int (*pv)(const igraph_t *graph, igraph_t *newgraph, const igraph_vector_t *idx))
{
    t->permute_vertices = pv;
    return;
}

void set_attribute_permute_edges(igraph_attribute_table_t* t,
    int (*pe)(const igraph_t *graph, igraph_t *newgraph, const igraph_vector_t *idx))
{
    t->permute_edges = pe;
    return;
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

int subgraph(const igraph_t *graph, igraph_t *res, const igraph_vs_t* vids)
{
    return igraph_subgraph(graph, res, *vids);
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
