/* graph_convert_c.h */
#include<complex.h>
#include "monolis_utils.h"
#include "gedatsu_def_graph_c.h"
#ifndef GEDATSU_GRAPH_MERGE_C_H
#define GEDATSU_GRAPH_MERGE_C_H

#ifdef __cplusplus
extern "C" {
#endif

// merge_nodal_subgraphsフラグ（部分領域毎に並べる）
#define ORDER_DOMAIN_ID 1
// merge_nodal_subgraphsフラグ（グローバル計算点順に並べる）
#define ORDER_NODAL_ID 2

typedef struct{
  int n;
  double* array;
}MONOLIS_LIST_R;

typedef struct{
  int n;
  int* array;
}MONOLIS_LIST_I;

typedef struct{
  int n;
  double complex* array;
}MONOLIS_LIST_C;

/**
 * @brief 計算点グラフの結合
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[in] monoCOMs 通信テーブルの配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[inout] merged_monoCOM 統合された通信テーブル
 * @param[in] order_type 部分領域ごとに並べるか、グローバル計算点番号順に並べるかを決めるフラグ [ORDER_DOMAIN_ID, ORDER_NODAL_ID]
 */
void gedatsu_merge_nodal_subgraphs(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  int order_type);

void gedatsu_merge_nodal_subgraphs_c_main(
  int n_graphs,
  int* n_vertex,
  int* n_internal_vertex,
  int** vertex_id,
  int** vertex_domain_id,
  int** index,
  int** item,
  int* my_rank,
  int* comm,
  int* comm_size,
  int* recv_n_neib,
  int** recv_neib_pe,
  int** recv_index,
  int** recv_item,
  int* send_n_neib,
  int** send_neib_pe,
  int** send_index,
  int** send_item,
  int merged_n_vertex,
  int merged_n_internal_vertex,
  int* merged_vertex_id,
  int* merged_vertex_domain_id,
  int* merged_index,
  int* merged_item,
  int merged_my_rank,
  int merged_comm,
  int merged_comm_size,
  int merged_recv_n_neib,
  int* merged_recv_neib_pe,
  int* merged_recv_index,
  int* merged_recv_item,
  int merged_send_n_neib,
  int* merged_send_neib_pe,
  int* merged_send_index,
  int* merged_send_item,
  int order_type
);

/**
 * @brief コネクティビティグラフの結合
 * @param[in] n_nodal_graphs 統合したい計算点グラフ構造の個数
 * @param[in] nodal_graphs 計算点グラフ構造の配列（配列長 n_nodal_graphs）
 * @param[inout] merged_nodal_monoCOM 統合された通信テーブル
 * @param[in] n_conn_graphs 統合したいコネクティビティグラフ構造の個数
 * @param[in] conn_graphs コネクティビティグラフ構造の配列（配列長 n_conn_graphs）
 * @param[inout] merged_conn_graph 結合されたコネクティビティグラフ
 */
void gedatsu_merge_connectivity_subgraphs(
  int n_nodal_graphs,
  GEDATSU_GRAPH* nodal_graphs,
  GEDATSU_GRAPH* merged_nodal_graph,
  MONOLIS_COM* merged_nodal_monoCOM,
  int n_conn_graphs,
  GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph);

void gedatsu_merge_connectivity_subgraphs_c_main(
  int n_nodal_graphs,
  int* nodal_n_vertex,
  int* nodal_n_internal_vertex,
  int** nodal_vertex_id,
  int** nodal_vertex_domain_id,
  int** nodal_index,
  int** nodal_item,
  int merged_nodal_n_vertex,
  int merged_nodal_n_internal_vertex,
  int* merged_nodal_vertex_id,
  int* merged_nodal_vertex_domain_id,
  int* merged_nodal_index,
  int* merged_nodal_item,
  int merged_nodal_my_rank,
  int merged_nodal_comm,
  int merged_nodal_comm_size,
  int merged_nodal_recv_n_neib,
  int* merged_nodal_recv_neib_pe,
  int* merged_nodal_recv_index,
  int* merged_nodal_recv_item,
  int merged_nodal_send_n_neib,
  int* merged_nodal_send_neib_pe,
  int* merged_nodal_send_index,
  int* merged_nodal_send_item,
  int n_conn_graphs,
  int* conn_n_vertex,
  int* conn_n_internal_vertex,
  int** conn_vertex_id,
  int** conn_vertex_domain_id,
  int** conn_index,
  int** conn_item,
  int merged_conn_n_vertex,
  int merged_conn_n_internal_vertex,
  int* merged_conn_vertex_id,
  int* merged_conn_vertex_domain_id,
  int* merged_conn_index,
  int* merged_conn_item
);

/**
 * @brief 物理量の結合 (実数配列)
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[in] n_dof_list 計算点が持つ物理量の個数
 * @param[in] list_struct_R リスト構造体
 * @param[inout] merged_n_dof_list 結合後の計算点が持つ物理量の個数
 * @param[inout] merged_array_R 統合された実数配列
 */
void gedatsu_merge_distval_R(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_R* list_struct_R,
  int* merged_n_dof_list,
  int* merged_array_R
);

void gedatsu_merge_distval_R_c_main();

/**
 * @brief 物理量の結合 (整数配列)
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[in] n_dof_list 計算点が持つ物理量の個数
 * @param[in] list_struct_I リスト構造体
 * @param[inout] merged_n_dof_list 結合後の計算点が持つ物理量の個数
 * @param[inout] merged_array_I 統合された実数配列
 */
void gedatsu_merge_distval_I(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_I* list_struct_I,
  int* merged_n_dof_list,
  int* merged_array_R
);

/**
 * @brief 物理量の結合 (複素数配列)
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[in] n_dof_list 計算点が持つ物理量の個数
 * @param[in] list_struct_C リスト構造体
 * @param[inout] merged_n_dof_list 結合後の計算点が持つ物理量の個数
 * @param[inout] merged_array_C 統合された実数配列
 */
void gedatsu_merge_distval_C(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_C* list_struct_C,
  int* merged_n_dof_list,
  int* merged_array_C
);

void gedatsu_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  int n
);

void gedatsu_list_initialize_R_c_main(
  int* list_struct_R_n,
  double** list_struct_R_array,
  int n
);

void gedatsu_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R
);

void gedatsu_list_finalize_R_c_main(
  int* list_struct_R_n,
  double** list_struct_R_array
);

void gedatsu_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  int n,
  double* array
);

void gedatsu_list_set_R_c_main(
  int* list_struct_R_n,
  double** list_struct_R_array,
  int id,
  int n,
  double* array
);

void gedatsu_list_get_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  double* array
);

void gedatsu_list_get_R_c_main(
  int* list_struct_R_n,
  double** list_struct_R_array,
  int id,
  double* array
);

void gedatsu_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  int n
);

void gedatsu_list_initialize_I_c_main(
  int* list_struct_I_n,
  int** list_struct_I_array,
  int n
);

void gedatsu_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I
);

void gedatsu_list_finalize_I_c_main(
  int* list_struct_I_n,
  int** list_struct_I_array
);

void gedatsu_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  int id,
  int n,
  int* array
);

void gedatsu_list_set_I_c_main(
  int* list_struct_I_n,
  int** list_struct_I_array,
  int id,
  int n,
  int* array
);

void gedatsu_list_get_I(
    MONOLIS_LIST_I* list_struct_I,
  int id,
  int* array
);

void gedatsu_list_get_I_c_main(
  int* list_struct_I_n,
  int** list_struct_I_array,
  int id,
  int* array
);

void gedatsu_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  int n
);

void gedatsu_list_initialize_C_c_main(
  int* list_struct_C_n,
  double complex** list_struct_C_array,
  int n
);

void gedatsu_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C
);

void gedatsu_list_finalize_C_c_main(
  int* list_struct_C_n,
  double complex** list_struct_C_array
);

void gedatsu_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  int id,
  int n,
  double complex* array
);

void gedatsu_list_set_C_c_main(
  int* list_struct_C_n,
  int** list_struct_C_array,
  int id,
  int n,
  double complex* array
);

void gedatsu_list_get_C(
MONOLIS_LIST_C* list_struct_C,
  int id,
  double complex* array
);

void gedatsu_list_get_C_c_main(
  int* list_struct_C_n,
  int** list_struct_C_array,
  int id,
  double complex* array
);

#ifdef __cplusplus
}
#endif

#endif
