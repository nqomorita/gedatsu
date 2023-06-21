/* graph_convert_c.h */
#ifndef GEDATSU_GRAPH_CONVERT_C_H
#define GEDATSU_GRAPH_CONVERT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief 単一メッシュ形式からコネクティビティグラフ形式に変換
 * @param[in] n_elem 要素数
 * @param[in] n_base 要素を構成する形状関数の数
 * @param[in] elem 要素コネクティビティ
 * @param[out] index コネクティビティグラフの index 配列
 * @param[out] item コネクティビティグラフの item 配列
 * @ingroup graph_conv
 */
void gedatsu_convert_simple_mesh_to_connectivity_graph(
  int   n_elem,
  int   n_base,
  int** elem,
  int*  index,
  int*  item);

/**
 * @brief 単一メッシュ形式から節点グラフ形式に変換
 * @param[in] n_node 節点数
 * @param[in] n_elem 要素数
 * @param[in] conn_index コネクティビティグラフの index 配列
 * @param[inout] conn_item コネクティビティグラフの item 配列
 * @param[out] nodal_index 節点グラフの index 配列
 * @param[out] nodal_item 節点グラフの item 配列
 * @ingroup graph_conv
 */
void gedatsu_convert_connectivity_graph_to_nodal_graph(
  int   n_node,
  int   n_elem,
  int*  conn_index,
  int*  conn_item,
  int** nodal_index,
  int** nodal_item);

#ifdef __cplusplus
}
#endif

#endif
