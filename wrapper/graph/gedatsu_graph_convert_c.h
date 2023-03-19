/* graph_convert_c.h */
#ifndef GEDATSU_GRAPH_CONVERT_C_H
#define GEDATSU_GRAPH_CONVERT_C_H

#ifdef __cplusplus
extern "C" {
#endif

/** 単一メッシュ形式からコネクティビティグラフ形式に変換 */
void gedatsu_convert_simple_mesh_to_connectivity_graph(
  int   n_elem,
  int   n_base,
  int** elem,
  int*  index,
  int*  item);

/** 単一メッシュ形式から節点グラフ形式に変換 */
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
