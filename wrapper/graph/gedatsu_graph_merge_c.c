#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"
#include "gedatsu_def_graph_c.h"
#include "gedatsu_graph_merge_c.h"

/** 計算点グラフの結合 */
void gedatsu_merge_nodal_subgraphs(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  int order_type)
{}

/** コネクティビティグラフの結合 */
void gedatsu_merge_connectivity_subgraphs(
  int n_nodal_graphs,
  GEDATSU_GRAPH* nodal_graphs,
  GEDATSU_GRAPH* merged_nodal_graph,
  MONOLIS_COM* merged_nodal_monoCOM,
  int n_conn_graphs,
  GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph)
{}

/** 物理量の結合 (実数配列) */
void gedatsu_merge_distval_R(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_R* list_struct_R,
  int* merged_n_dof_list,
  int* merged_array_R)
{}

/** 物理量の結合 (整数配列) */
void gedatsu_merge_distval_I(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_I* list_struct_I,
  int* merged_n_dof_list,
  int* merged_array_R)
{}

/** 物理量の結合 (複素数配列) */
void gedatsu_merge_distval_C(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_C* list_struct_C,
  int* merged_n_dof_list,
  int* merged_array_C)
{}

void gedatsu_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  int n)
{
  for (int i = 0; i < n+1; i++)
  {
    list_struct_R[i].n = 0;
    monolis_dealloc_R_1d(&list_struct_R[i].array);
  }
}

void gedatsu_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  int n)
{
    for (int i = 0; i < n+1; i++)
  {
    list_struct_I[i].n = 0;
    monolis_dealloc_I_1d(&list_struct_I[i].array);
  }
}

void gedatsu_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  int n)
{
    for (int i = 0; i < n+1; i++)
  {
    list_struct_C[i].n = 0;
    monolis_dealloc_C_1d(&list_struct_C[i].array);
  }
}

void gedatsu_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R,
  int n)
{
    for (int i = 0; i < n+1; i++)
  {
    list_struct_R[i].n = 0;
    monolis_dealloc_R_1d(&list_struct_R[i].array);
  }
}

void gedatsu_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I,
  int n)
{
    for (int i = 0; i < n+1; i++)
  {
    list_struct_I[i].n = 0;
    monolis_dealloc_I_1d(&list_struct_I[i].array);
  }
}

void gedatsu_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C,
  int n)
{
    for (int i = 0; i < n+1; i++)
  {
    list_struct_C[i].n = 0;
    monolis_dealloc_C_1d(&list_struct_C[i].array);
  }
}

void gedatsu_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  int n,
  double* array)
{
  list_struct_R[id].n = n;
  monolis_dealloc_R_1d(&list_struct_R[id].array);
  monolis_alloc_R_1d(list_struct_R[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_R[id].array[i] = array[i];
  }
}

void gedatsu_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  int id,
  int n,
  int* array)
{
  list_struct_I[id].n = n;
  monolis_dealloc_I_1d(&list_struct_I[id].array);
  monolis_alloc_I_1d(list_struct_I[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_I[id].array[i] = array[i];
  }
}

void gedatsu_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  int id,
  int n,
  double complex* array)
{
  list_struct_C[id].n = n;
  monolis_dealloc_C_1d(&list_struct_C[id].array);
  monolis_alloc_C_1d(list_struct_C[id].array, n);
  for (int i = 0; i < n; i++)
  {
    list_struct_C[id].array[i] = array[i];
  }
}

void gedatsu_list_get_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  double* array)
{
  monolis_dealloc_R_1d(&array);
  monolis_alloc_R_1d(array, list_struct_R[id].n);
  for (int i = 0; i < list_struct_R[id].n; i++)
  {
    array[i] = list_struct_R[id].array[i];
  }
}

void gedatsu_list_get_I(
    MONOLIS_LIST_I* list_struct_I,
  int id,
  int* array)
{
  monolis_dealloc_I_1d(&array);
  monolis_alloc_I_1d(array, list_struct_I[id].n);
  for (int i = 0; i < list_struct_I[id].n; i++)
  {
    array[i] = list_struct_I[id].array[i];
  }
}

void gedatsu_list_get_C(
MONOLIS_LIST_C* list_struct_C,
  int id,
  double complex* array)
{
  monolis_dealloc_C_1d(&array);
  monolis_alloc_C_1d(array, list_struct_C[id].n);
  for (int i = 0; i < list_struct_C[id].n; i++)
  {
    array[i] = list_struct_C[id].array[i];
  }
}
