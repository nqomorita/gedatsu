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
  int* merged_n_dof_list,
  int* merged_array_R)
{}

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
