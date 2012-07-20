#include <stdlib.h>
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


igraph_vector_t* c_igraph_betweenness(const igraph_t* graph)
{
    igraph_vector_t* result = c_igraph_vector_create(0);
    igraph_betweenness(graph, result, igraph_vss_all(), IGRAPH_DIRECTED, NULL, 0);
    return result;
}


igraph_vector_t* c_igraph_closeness_in(const igraph_t* graph, int vertex)
{
    igraph_vector_t* result = c_igraph_vector_create(0);
    igraph_closeness(graph, result, igraph_vss_1(vertex), IGRAPH_IN, NULL);
    return result;
}


igraph_vector_t* c_igraph_closeness_out(const igraph_t* graph, int vertex)
{
    igraph_vector_t* result = c_igraph_vector_create(0);
    igraph_closeness(graph, result, igraph_vss_1(vertex), IGRAPH_OUT, NULL);
    return result;
}


igraph_vector_t* c_igraph_eigenvector_centrality(const igraph_t* graph)
{
    igraph_vector_t* result = c_igraph_vector_create(0);
    igraph_arpack_options_t arpack_options;
    igraph_arpack_options_init(&arpack_options);
    igraph_eigenvector_centrality(graph, result, NULL, 1, 1, NULL, &arpack_options);
    return result;
}


igraph_vector_t* c_igraph_clusters(const igraph_t* graph)
{
    igraph_vector_t* result = c_igraph_vector_create(0);
    igraph_clusters(graph, result, NULL, NULL, IGRAPH_WEAK);
    return result;
}


igraph_vector_ptr_t* c_igraph_get_shortest_paths_in (const igraph_t* graph, int vertex)
{
    igraph_vector_ptr_t* result = c_igraph_vector_ptr_create(igraph_vcount(graph));
    igraph_get_shortest_paths(graph, result, NULL, vertex, igraph_vss_all(), IGRAPH_IN);
    return result;
}
