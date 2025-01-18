#include <stdbool.h>
#include "gedatsu_def_graph_c.h"
#include "gedatsu_graph_handler_c.h"
#include "monolis_utils.h"

/* グラフのノード数を指定 */
void gedatsu_graph_set_n_vertex(
  GEDATSU_GRAPH* graph,
  int n_vertex)
{
  if(graph->n_vertex > 0){
    monolis_std_log_string("gedatsu_graph_set_n_vertex");
    monolis_std_log_string("graph nodes are already defined");
  }

  if(n_vertex < 1){
    monolis_std_log_string("gedatsu_graph_set_n_vertex");
    monolis_std_log_string("n_vertex is less than 1");
  }

  graph->vertex_id = monolis_alloc_I_1d(graph->vertex_id, n_vertex);
  graph->vertex_domain_id = monolis_alloc_I_1d(graph->vertex_domain_id, n_vertex);
  graph->index = monolis_alloc_I_1d(graph->index, n_vertex + 1);

  graph->n_vertex = n_vertex;
}

/* グラフのノード数を取得 */
void gedatsu_graph_get_n_vertex(
  GEDATSU_GRAPH* graph,
  int* n_vertex)
{
  *n_vertex = graph->n_vertex;
}

/* グラフのエッジ数を取得 */
void gedatsu_graph_get_n_edge(
  GEDATSU_GRAPH* graph,
  int* n_edge)
{
  *n_edge = graph->index[graph->n_vertex];
}

/* 領域番号 domain_id に属するノード数を取得 */
void gedatsu_graph_get_n_vertex_in_internal_region(
  GEDATSU_GRAPH* graph,
  int  domain_id,
  int* n_vertex)
{
  *n_vertex = 0;
  for (int i = 0; i < graph->n_vertex; ++i) {
    if(graph->vertex_domain_id[i] == domain_id){
      *n_vertex = *n_vertex + 1;
    }
  }
}

/* 領域番号 domain_id に属するノード番号を取得 */
void gedatsu_graph_get_vertex_id_in_internal_region(
  GEDATSU_GRAPH* graph,
  int  domain_id,
  int* ids)
{
  int n_vertex = 0;
  for (int i = 0; i < graph->n_vertex; ++i) {
    if(graph->vertex_domain_id[i] == domain_id){
      ids[n_vertex] = graph->vertex_id[i];
      n_vertex = n_vertex + 1;
    }
  }
}

/* 領域番号 domain_id に属するグラフのエッジを取得 */
void gedatsu_graph_get_edge_in_internal_region(
  GEDATSU_GRAPH* graph,
  int* domain_id,
  int** edge)
{
  int n_vertex, n_edge, idx;
  int* ids;
  int* perm;

  gedatsu_graph_get_n_vertex_in_internal_region(graph, *domain_id, &n_vertex);

  ids = monolis_alloc_I_1d(ids, n_vertex);

  gedatsu_graph_get_vertex_id_in_internal_region(graph, *domain_id, ids);

  perm = monolis_alloc_I_1d(perm, n_vertex);

  monolis_get_sequence_array_I(perm, n_vertex, 1, 1);

  monolis_qsort_I_2d(ids, perm, 1, n_vertex);

  n_edge = 0;

  for (int i = 0; i < graph->n_vertex; ++i) {
    if(graph->vertex_domain_id[i] != *domain_id) continue;
    int jS = graph->index[i];
    int jE = graph->index[i + 1];
    for (int j = jS; j < jE; ++j) {
      int nid = graph->item[j];
      if(graph->vertex_domain_id[nid] == *domain_id){
        int n1 = graph->vertex_id[i];
        monolis_bsearch_I(ids, 0, n_vertex - 1, n1, &idx);
        //if(idx == -1) 
        int e1 = perm[idx];

        int n2 = graph->vertex_id[nid];
        monolis_bsearch_I(ids, 0, n_vertex - 1, n2, &idx);
        //if(idx == -1) 
        int e2 = perm[idx];

        edge[n_edge][0] = e1;
        edge[n_edge][1] = e2;
        n_edge = n_edge + 1;
      }
    }
  }
}

/* グラフのエッジを定義 */
void gedatsu_graph_set_edge(
  GEDATSU_GRAPH* graph,
  int   n_edge,
  int** edge,
  bool  is_sort)
{
  int** temp; 

  if(n_edge < 1){
    monolis_std_log_string("gedatsu_graph_add_edge");
    monolis_std_log_string("n_edge is less than 1");
  }

  temp = monolis_alloc_I_2d(temp, n_edge, 2);

  for (int i = 0; i < n_edge; ++i) {
    temp[i][0] = edge[i][0];
    temp[i][1] = edge[i][1];
  }

  for (int i = 0; i < n_edge; ++i) {
    int e1 = temp[i][0];
    graph->index[e1] = graph->index[e1] + 1;
  }

  for (int i = 0; i < graph->n_vertex; ++i) {
    graph->index[i + 1] = graph->index[i + 1] + graph->index[i];
  }

  int in = graph->index[graph->n_vertex];

  graph->item = monolis_alloc_I_1d(graph->item, in);

  for (int i = 0; i < n_edge; ++i) {
    int e2 = temp[i][1];
    graph->item[i] = e2;
  }

  if (is_sort) {
    for (int i = 0; i < graph->n_vertex; ++i) {
      int jS = graph->index[i];
      int jE = graph->index[i + 1];
      monolis_qsort_I_1d(graph->item, jS, jE);
    }
  }
}

/* グラフのエッジを追加 */
void gedatsu_graph_add_edge(
  GEDATSU_GRAPH* graph,
  int   n_edge,
  int** edge,
  bool  is_sort)
{
  int n_edge_all;
  int n_edge_cur;
  int** edge_all;

  if(n_edge < 1){
    monolis_std_log_string("gedatsu_graph_add_edge");
    monolis_std_log_string("n_edge is less than 1");
  }

  n_edge_cur = graph->index[graph->n_vertex];
  n_edge_all = n_edge_cur + n_edge;

  edge_all = monolis_alloc_I_2d(edge_all, n_edge_all, 2);

  for (int i = 0; i < graph->n_vertex; ++i) {
    int jS = graph->index[i];
    int jE = graph->index[i + 1];
    for (int j = jS; j < jE; ++j) {
      edge_all[i][0] = i;
      edge_all[i][1] = graph->item[j];
    }
  }

  monolis_dealloc_I_1d(&graph->item);
  graph->item = monolis_alloc_I_1d(graph->item, n_edge_all);

  for (int i = 0; i < n_edge; ++i) {
    edge_all[n_edge_cur + i][0] = edge[i][0];
    edge_all[n_edge_cur + i][1] = edge[i][1];
  }

  gedatsu_graph_set_edge(graph, n_edge_all, edge_all, is_sort);
}

/* グラフの重複したエッジを削除 */
void gedatsu_graph_delete_dupulicate_edge(
  GEDATSU_GRAPH* graph)
{

}
