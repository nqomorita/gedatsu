#include <stdlib.h>
#include "gedatsu_def_graph_c.h"
#include "monolis_utils.h"

/* graph 構造体の初期化関数 */
void gedatsu_graph_initialize(
  GEDATSU_GRAPH* graph)
{
  graph->n_vertex = 0;
  graph->n_internal_vertex = 0;

  graph->vertex_id = NULL;
  graph->vertex_domain_id = NULL;
  graph->index = NULL;
  graph->item = NULL;
}

/* graph 構造体の終了関数 */
void gedatsu_graph_finalize(
  GEDATSU_GRAPH* graph)
{
  graph->n_vertex = 0;
  graph->n_internal_vertex = 0;

  monolis_dealloc_I_1d(&graph->vertex_id);
  monolis_dealloc_I_1d(&graph->vertex_domain_id);
  monolis_dealloc_I_1d(&graph->index);
  monolis_dealloc_I_1d(&graph->item);
}
