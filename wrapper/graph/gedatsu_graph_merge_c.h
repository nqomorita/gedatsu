/* graph_merge_c.h */
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

/**
 * @brief 計算点グラフの結合
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[in] monoCOMs 通信テーブルの配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[inout] merged_monoCOM 統合された通信テーブル
 * @param[in] order_type 部分領域ごとに並べるか、グローバル計算点番号順に並べるかを決めるフラグ [ORDER_DOMAIN_ID, ORDER_NODAL_ID]
 * @ingroup graph_merge
 */
void gedatsu_merge_nodal_subgraphs(
  const int n_graphs,
  GEDATSU_GRAPH* graphs,
  const MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  const int order_type);

/**
 * @brief コネクティビティグラフの結合
 * @param[in] n_nodal_graphs 統合したい計算点グラフ構造の個数
 * @param[in] nodal_graphs 計算点グラフ構造の配列（配列長 n_nodal_graphs）
 * @param[in] merged_nodal_graph 結合された計算点グラフ
 * @param[in] merged_nodal_monoCOM 統合された通信テーブル
 * @param[in] n_conn_graphs 統合したいコネクティビティグラフ構造の個数
 * @param[in] conn_graphs コネクティビティグラフ構造の配列（配列長 n_conn_graphs）
 * @param[inout] merged_conn_graph 結合されたコネクティビティグラフ
 * @ingroup graph_merge
 */
void gedatsu_merge_connectivity_subgraphs(
  const int n_nodal_graphs,
  const GEDATSU_GRAPH* nodal_graphs,
  GEDATSU_GRAPH* merged_nodal_graph,
  const MONOLIS_COM* merged_nodal_monoCOM,
  const int n_conn_graphs,
  const GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph);

/**
 * @brief 物理量の結合 (実数配列)
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[in] n_dof_list 計算点が持つ物理量の個数
 * @param[in] list_struct_R リスト構造体
 * @param[inout] merged_n_dof_list 結合後の計算点が持つ物理量の個数
 * @param[inout] merged_array_R 統合された実数配列
 * @ingroup graph_merge
 */
void gedatsu_merge_distval_R(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_R* list_struct_R,
  int** merged_n_dof_list,
  double** merged_array_R
);

/**
 * @brief 物理量の結合 (整数配列)
 * @param[in] n_graphs 統合したいグラフ構造の個数
 * @param[in] graphs グラフ構造の配列（配列長 n_graphs）
 * @param[inout] merged_graph 統合されたグラフ構造
 * @param[in] n_dof_list 計算点が持つ物理量の個数
 * @param[in] list_struct_I リスト構造体
 * @param[inout] merged_n_dof_list 結合後の計算点が持つ物理量の個数
 * @param[inout] merged_array_I 統合された実数配列
 * @ingroup graph_merge
 */
void gedatsu_merge_distval_I(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_I* list_struct_I,
  int** merged_n_dof_list,
  int** merged_array_I
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
 * @ingroup graph_merge
 */
void gedatsu_merge_distval_C(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_C* list_struct_C,
  int** merged_n_dof_list,
  double _Complex** merged_array_C
);

#ifdef __cplusplus
}
#endif

#endif
