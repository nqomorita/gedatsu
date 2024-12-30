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
{}

void gedatsu_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R)
{}

void gedatsu_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  int n,
  double* array)
{}

void gedatsu_list_get_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  double* array)
{}

void gedatsu_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  int n)
{}

void gedatsu_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I)
{}

void gedatsu_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  int id,
  int n,
  int* array)
{}

void gedatsu_list_get_I(
    MONOLIS_LIST_I* list_struct_I,
  int id,
  int* array)
{}

void gedatsu_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  int n)
{}

void gedatsu_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C)
{}

void gedatsu_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  int id,
  int n,
  double complex* array)
{}

void gedatsu_list_get_C(
MONOLIS_LIST_C* list_struct_C,
  int id,
  double complex* array)
{}
