/* graph_convert_c.h */
#ifndef GEDATSU_GRAPH_CONVERT_C_H
#define GEDATSU_GRAPH_CONVERT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief 単一メッシュ形式からコネクティビティグラフ形式に変換
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_vertex 全計算点数
 * @param[in] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] comm_size !
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @param[in] recv_n_neib 隣接する領域数
 * @param[in] is_neib 隣接する領域フラグ（サイズ：[comm_size]）
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
 * @param[in] comm MPI コミュニケータ
 * @param[in] n_vertex 全計算点数
 * @param[in] outer_domain_id_all 全ての外部計算点が属する領域番号
 * @param[in] comm_size !
 * @param[in] displs 全ての外部計算点配列の各領域に属する計算点数
 * @param[in] recv_n_neib 隣接する領域数
 * @param[in] is_neib 隣接する領域フラグ（サイズ：[comm_size]）
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
