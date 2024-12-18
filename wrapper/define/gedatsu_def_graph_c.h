/* graph_convert_c.h */
#ifndef GEDATSU_DEF_GRAPH_C_H
#define GEDATSU_DEF_GRAPH_C_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct{
  /** ノード数 */
  int n_vertex;
  /** 領域分割における内部ノード数 */
  int n_internal_vertex;
  /** ノード id 配列 */
  int* vertex_id;
  /** 領域番号配列 */
  int* vertex_domain_id;
  /** graph の CSR 圧縮形式の index 配列 */
  int* index;
  /** graph の CSR 圧縮形式の item 配列 */
  int* item;
} GEDATSU_GRAPH;

/**
 * @brief graph 構造体の初期化関数
 * @param[in,out] graph graph 構造体
 * @ingroup graph
 */
void gedatsu_graph_initialize(
  GEDATSU_GRAPH* graph);

/**
 * @brief graph 構造体の終了関数
 * @param[in,out] graph graph 構造体
 * @ingroup graph
 */
void gedatsu_graph_finalize(
  GEDATSU_GRAPH* graph);

#ifdef __cplusplus
}
#endif

#endif
