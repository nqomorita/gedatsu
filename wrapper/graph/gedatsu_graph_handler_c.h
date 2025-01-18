/* graph_handler_c.h */
#ifndef GEDATSU_GRAPH_HANDLER_C_H
#define GEDATSU_GRAPH_HANDLER_C_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

/**
 * @brief グラフのノード数を指定
 * @param[in,out] graph graph 構造体
 * @param[in] n_vertex グラフのノード数
 * @ingroup graph
 */
void gedatsu_graph_set_n_vertex(
  GEDATSU_GRAPH* graph,
  int n_vertex);

/**
 * @brief グラフのノード数を取得
 * @param[in,out] graph graph 構造体
 * @param[in] n_vertex グラフのノード数
 * @ingroup graph
 */
void gedatsu_graph_get_n_vertex(
  GEDATSU_GRAPH* graph,
  int* n_vertex);

/**
 * @brief グラフのエッジ数を取得
 * @param[in,out] graph graph 構造体
 * @param[in] n_edge グラフのノード数
 * @ingroup graph
 */
void gedatsu_graph_get_n_edge(
  GEDATSU_GRAPH* graph,
  int* n_edge);

/**
 * @brief 領域番号 domain_id に属するノード数を取得
 * @param[in,out] graph graph 構造体
 * @param[in] domain_id 領域番号
 * @param[out] n_vertex グラフのノード数
 * @ingroup graph
 */
void gedatsu_graph_get_n_vertex_in_internal_region(
  GEDATSU_GRAPH* graph,
  int  domain_id,
  int* n_vertex);

/**
 * @brief 領域番号 domain_id に属するノード番号を取得
 * @param[in,out] graph graph 構造体
 * @param[in] domain_id 領域番号
 * @param[out] ids グラフのノード番号配列
 * @ingroup graph
 */
void gedatsu_graph_get_vertex_id_in_internal_region(
  GEDATSU_GRAPH* graph,
  int  domain_id,
  int* ids);

/**
 * @brief 領域番号 domain_id に属するグラフのエッジを取得
 * @param[in,out] graph graph 構造体
 * @param[in] domain_id 領域番号
 * @param[in] edge グラフのエッジ配列
 * @details エッジの組はローカル節点番号で表現される
 * @ingroup graph
 */
void gedatsu_graph_get_edge_in_internal_region(
  GEDATSU_GRAPH* graph,
  int* domain_id,
  int** edge);

/**
 * @brief グラフのエッジを定義
 * @param[in,out] graph graph 構造体
 * @param[in] n_edge 領域番号
 * @param[in] edge グラフのエッジ配列
 * @details 既に定義されているエッジ情報は削除される。エッジの重複判定はなされない。ノード数は変化しない。
 * @ingroup graph
 */
void gedatsu_graph_set_edge(
  GEDATSU_GRAPH* graph,
  int   n_edge,
  int** edge,
  bool  is_sort);

/**
 * @brief グラフのエッジを追加
 * @param[in,out] graph graph 構造体
 * @param[in] n_edge 領域番号
 * @param[in] edge グラフのエッジ配列
 * @details 既に定義されているエッジ情報は維持する。エッジの重複判定はなされない。
 * @ingroup graph
 */
void gedatsu_graph_add_edge(
  GEDATSU_GRAPH* graph,
  int   n_edge,
  int** edge,
  bool  is_sort);

/**
 * @brief グラフの重複したエッジを削除
 * @param[in,out] graph graph 構造体
 * @ingroup graph
 */
void gedatsu_graph_delete_dupulicate_edge(
  GEDATSU_GRAPH* graph);

#ifdef __cplusplus
}
#endif

#endif
