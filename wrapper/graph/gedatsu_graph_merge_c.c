#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"
#include "gedatsu_def_graph_c.h"
#include "gedatsu_graph_merge_c.h"

/** 計算点グラフの結合 */
void gedatsu_merge_nodal_subgraphs(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  const int order_type)
{}

/** コネクティビティグラフの結合 */
void gedatsu_merge_connectivity_subgraphs(
  const int n_nodal_graphs,
  const GEDATSU_GRAPH* nodal_graphs,
  const GEDATSU_GRAPH* merged_nodal_graph,
  const MONOLIS_COM* merged_nodal_monoCOM,
  const int n_conn_graphs,
  const GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph)
{}

/** 物理量の結合 (実数配列) */
void gedatsu_merge_distval_R(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_R* list_struct_R,
  int** merged_n_dof_list,
  double** merged_array_R)
{
  int* n_vertex;
  int* n_internal_vertex;
  int* vertex_id;
  int* vertex_domain_id;
  int* index;
  int* item;
  int* n_dof_list_n;
  int* list_struct_R_n;
  int* n_dof_list_array;
  double* list_struct_R_array;

  int sum_n_vertex, sum_index, sum_item;
  int iE_n_vertex, iE_index, iE_item;

  n_vertex = (int *)calloc(n_graphs, sizeof(int));
  n_internal_vertex = (int *)calloc(n_graphs, sizeof(int));
  n_dof_list_n = (int *)calloc(n_graphs, sizeof(int));
  list_struct_R_n = (int *)calloc(n_graphs, sizeof(int));

  // graphs構造体とlist構造体を一次元配列に変換

  // allocのサイズを計算
  sum_n_vertex = 0;
  sum_index = 0;
  sum_item = 0;
  for (int i = 0; i < n_graphs; i++)
  {
    sum_n_vertex += graphs[i].n_vertex;
    sum_index += graphs[i].n_vertex + 1;
    sum_item += graphs[i].index[graphs[i].n_vertex];
  }

  // alloc
  vertex_id = (int *)calloc(sum_n_vertex, sizeof(int));
  vertex_domain_id = (int *)calloc(sum_n_vertex, sizeof(int));
  index = (int *)calloc(sum_index, sizeof(int));
  item = (int *)calloc(sum_item, sizeof(int));
  n_dof_list_array = (int *)calloc(sum_n_vertex, sizeof(int));
  list_struct_R_array = (double *)calloc(sum_n_vertex, sizeof(double));
  *merged_n_dof_list = (int *)calloc(merged_graph->n_vertex, sizeof(int));
  *merged_array_R = (double *)calloc(merged_graph->n_vertex, sizeof(double));

  // 一次元配列を作成
  iE_n_vertex = 0;
  iE_index = 0;
  iE_item = 0;
  for (int i = 0; i < n_graphs; i++)
  {
    n_vertex[i] = graphs[i].n_vertex;
    n_internal_vertex[i] = graphs[i].n_internal_vertex;
    n_dof_list_n[i] = n_dof_list[i].n;
    list_struct_R_n[i] = list_struct_R[i].n;
    for (int j = 0; j < n_vertex[i]; j++)
    {
      vertex_id[iE_n_vertex+j] = graphs[i].vertex_id[j];
      vertex_domain_id[iE_n_vertex+j] = graphs[i].vertex_domain_id[j];
      n_dof_list_array[iE_n_vertex+j] = n_dof_list[i].array[j];
      list_struct_R_array[iE_n_vertex+j] = list_struct_R[i].array[j];
    }
    for (int j = 0; j < n_vertex[i]+1; j++)
    {
      index[iE_index+j] = graphs[i].index[j];
    }
    for (int j = 0; j < graphs[i].index[n_vertex[i]]; j++)
    {
      item[iE_item+j] = graphs[i].item[j];
    }
    iE_n_vertex += n_vertex[i];
    iE_index += n_vertex[i] + 1;
    iE_item += graphs[i].index[n_vertex[i]];
  }

  // 結合関数（中間層関数）を呼ぶ
  gedatsu_merge_distval_R_c(sum_n_vertex, sum_index, sum_item,
  n_graphs, n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item,
  merged_graph->n_vertex, merged_graph->n_internal_vertex,
  merged_graph->vertex_id, merged_graph->vertex_domain_id, merged_graph->index, merged_graph->item,
  n_dof_list_n, n_dof_list_array, list_struct_R_n, list_struct_R_array,
  *merged_n_dof_list, *merged_array_R);

  free(n_vertex);
  free(n_internal_vertex);
  free(vertex_id);
  free(vertex_domain_id);
  free(index);
  free(item);
  free(n_dof_list_n);
  free(list_struct_R_n);
  free(n_dof_list_array);
  free(list_struct_R_array);
}

/** 物理量の結合 (整数配列) */
void gedatsu_merge_distval_I(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_I* list_struct_I,
  int* merged_n_dof_list,
  int* merged_array_R)
{}

/** 物理量の結合 (複素数配列) */
void gedatsu_merge_distval_C(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_C* list_struct_C,
  int* merged_n_dof_list,
  int* merged_array_C)
{}

void gedatsu_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n)
{
  for (int i = 0; i < n; i++)
  {
    list_struct_R[i].n = 0;
    monolis_dealloc_R_1d(&list_struct_R[i].array);
  }
}

void gedatsu_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n)
{
    for (int i = 0; i < n; i++)
  {
    list_struct_I[i].n = 0;
    monolis_dealloc_I_1d(&list_struct_I[i].array);
  }
}

void gedatsu_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n)
{
    for (int i = 0; i < n; i++)
  {
    list_struct_C[i].n = 0;
    monolis_dealloc_C_1d(&list_struct_C[i].array);
  }
}

void gedatsu_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R,
  const int n)
{
    for (int i = 0; i < n; i++)
  {
    list_struct_R[i].n = 0;
    monolis_dealloc_R_1d(&list_struct_R[i].array);
  }
}

void gedatsu_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I,
  const int n)
{
    for (int i = 0; i < n; i++)
  {
    list_struct_I[i].n = 0;
    monolis_dealloc_I_1d(&list_struct_I[i].array);
  }
}

void gedatsu_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C,
  const int n)
{
    for (int i = 0; i < n; i++)
  {
    list_struct_C[i].n = 0;
    monolis_dealloc_C_1d(&list_struct_C[i].array);
  }
}

void gedatsu_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  const int id,
  const int n,
  const double* array)
{
  list_struct_R[id].n = n;
  monolis_dealloc_R_1d(&list_struct_R[id].array);
  list_struct_R[id].array = monolis_alloc_R_1d(list_struct_R[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_R[id].array[i] = array[i];
  }
}

void gedatsu_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  const int id,
  const int n,
  const int* array)
{
  list_struct_I[id].n = n;
  monolis_dealloc_I_1d(&list_struct_I[id].array);
  list_struct_I[id].array = monolis_alloc_I_1d(list_struct_I[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_I[id].array[i] = array[i];
  }
}

void gedatsu_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  const int id,
  const int n,
  const double complex* array)
{
  list_struct_C[id].n = n;
  monolis_dealloc_C_1d(&list_struct_C[id].array);
  list_struct_C[id].array = monolis_alloc_C_1d(list_struct_C[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_C[id].array[i] = array[i];
  }
}

void gedatsu_list_get_R(
  const MONOLIS_LIST_R* list_struct_R,
  const int id,
  double** array)
{
  monolis_dealloc_R_1d(array);
  *array = monolis_alloc_R_1d(*array, list_struct_R[id].n);
  for (int i = 0; i < list_struct_R[id].n; i++)
  {
    *array[i] = list_struct_R[id].array[i];
  }
}

void gedatsu_list_get_I(
  const MONOLIS_LIST_I* list_struct_I,
  const int id,
  int** array)
{
  monolis_dealloc_I_1d(array);
  *array = monolis_alloc_I_1d(*array, list_struct_I[id].n);
  for (int i = 0; i < list_struct_I[id].n; i++)
  {
    *array[i] = list_struct_I[id].array[i];
  }
}

void gedatsu_list_get_C(
  const MONOLIS_LIST_C* list_struct_C,
  const int id,
  double complex** array)
{
  monolis_dealloc_C_1d(array);
  *array = monolis_alloc_C_1d(*array, list_struct_C[id].n);
  for (int i = 0; i < list_struct_C[id].n; i++)
  {
    *array[i] = list_struct_C[id].array[i];
  }
}
