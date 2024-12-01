/* gedatsu_def_graph.h */
#ifndef GEDATSU_DEF_GRAPH_H
#define GEDATSU_DEF_GRAPH_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct{
  int n_vertex;
  int n_internal_vertex;
  int* vertex_id;
  int* vertex_domain_id;
  int* index;
  int* item;
}GEDATSU_GRAPH;

#ifdef __cplusplus
}
#endif

#endif
