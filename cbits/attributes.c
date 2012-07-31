#include <stdlib.h>
#include <igraph/igraph.h>

/**
 * Whether the module was initialized already
 */
static igraph_bool_t igraphhaskell_initialized = 0;

int igraphhaskell_initialize()
{
  if (igraphhaskell_initialized)
    return 1;
  igraph_i_set_attribute_table(&igraph_cattribute_table);
  igraphhaskell_initialized = 1;
  return 0;
}

void igraphhaskell_graph_set_vertex_ids(igraph_t* g)
{
  igraph_vector_t v;
  long int i;

  igraph_vector_init(&v, igraph_vcount(g));
  for (i = 0; i < igraph_vcount(g); i++)
  {
    VECTOR(v)[i] = (igraph_real_t) i;
  }
  SETVANV(g, "ID", &v);
}

int igraphhaskell_graph_get_vertex_ids(const igraph_t* g, igraph_vector_t* v)
{
  if (igraph_cattribute_has_attr(g, IGRAPH_ATTRIBUTE_VERTEX, "ID"))
    VANV(g, "ID", v);
  else
    return 1;
  return 0;
}
