#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <complex.h>
#include <math.h>
#include "gedatsu.h"
#include "monolis_utils.h"
#include "gedatsu_graph_merge_c_test.h"

void gedatsu_graph_merge_c_test()
{
  gedatsu_merge_nodal_subgraphs_c_test();
  gedatsu_merge_connectivity_subgraphs_c_test();
  gedatsu_merge_distval_R_c_test();
  gedatsu_merge_distval_I_c_test();
  gedatsu_merge_distval_C_c_test();
}

void gedatsu_merge_nodal_subgraphs_c_test()
{
  int n_graphs;
  GEDATSU_GRAPH graphs[3];
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_monoCOM;

  int* check_vertex_id = NULL;
  int* check_vertex_domain_id = NULL;
  int* check_index = NULL;
  int* check_item = NULL;
  int** edge = NULL;

  monolis_std_log_string("gedatsu_merge_nodal_subgraphs_c_test");

  n_graphs = 3;

  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_initialize(&graphs[i]);
  }
  gedatsu_graph_set_n_vertex(&graphs[0], 5);
  gedatsu_graph_set_n_vertex(&graphs[1], 5);
  gedatsu_graph_set_n_vertex(&graphs[2], 6);
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id[0] = 1;
  graphs[0].vertex_id[1] = 4;
  graphs[0].vertex_id[2] = 2;
  graphs[0].vertex_id[3] = 5;
  graphs[0].vertex_id[4] = 6;
  graphs[1].vertex_id[0] = 2;
  graphs[1].vertex_id[1] = 3;
  graphs[1].vertex_id[2] = 1;
  graphs[1].vertex_id[3] = 4;
  graphs[1].vertex_id[4] = 5;
  graphs[2].vertex_id[0] = 5;
  graphs[2].vertex_id[1] = 2;
  graphs[2].vertex_id[2] = 3;
  graphs[2].vertex_id[3] = 4;
  graphs[2].vertex_id[4] = 6;
  graphs[2].vertex_id[5] = 7;

  edge = monolis_alloc_I_2d(edge, 14, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 1; edge[2][1] = 0;
  edge[3][0] = 1; edge[3][1] = 2;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 1;
  edge[8][0] = 2; edge[8][1] = 3;
  edge[9][0] = 3; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 2;
  edge[11][0] = 3; edge[11][1] = 4;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[0], 14, edge, true);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 1; edge[4][1] = 0;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 3; edge[8][1] = 0;
  edge[9][0] = 3; edge[9][1] = 2;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 4; edge[11][1] = 0;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[1], 14, edge, true);

  monolis_dealloc_I_2d(&edge, 14, 2);
  edge = monolis_alloc_I_2d(edge, 18, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 0; edge[4][1] = 5;
  edge[5][0] = 1; edge[5][1] = 0;
  edge[6][0] = 1; edge[6][1] = 2;
  edge[7][0] = 1; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 0;
  edge[9][0] = 2; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 0;
  edge[11][0] = 3; edge[11][1] = 1;
  edge[12][0] = 3; edge[12][1] = 4;
  edge[13][0] = 4; edge[13][1] = 0;
  edge[14][0] = 4; edge[14][1] = 3;
  edge[15][0] = 4; edge[15][1] = 5;
  edge[16][0] = 5; edge[16][1] = 0;
  edge[17][0] = 5; edge[17][1] = 4;
  gedatsu_graph_set_edge(&graphs[2], 18, edge, true);

  // 結合前の通信テーブル
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後グラフの模範解答 (ORDER_NODAL_IDで計算点グラフを結合)
  check_vertex_id = monolis_alloc_I_1d(check_vertex_id, 7);
  check_vertex_domain_id = monolis_alloc_I_1d(check_vertex_domain_id, 7);
  check_index = monolis_alloc_I_1d(check_index, 8);
  check_item = monolis_alloc_I_1d(check_item, 22);

  check_vertex_id[0] = 1;
  check_vertex_id[1] = 4;
  check_vertex_id[2] = 2;
  check_vertex_id[3] = 3;
  check_vertex_id[4] = 5;
  check_vertex_id[5] = 6;
  check_vertex_id[6] = 7;

  check_index[0] = 0;
  check_index[1] = 2;
  check_index[2] = 6;
  check_index[3] = 10;
  check_index[4] = 12;
  check_index[5] = 17;
  check_index[6] = 20;
  check_index[7] = 22;

  check_item[0] = 1;
  check_item[1] = 2;
  check_item[2] = 0;
  check_item[3] = 2;
  check_item[4] = 4;
  check_item[5] = 5;
  check_item[6] = 0;
  check_item[7] = 1;
  check_item[8] = 3;
  check_item[9] = 4;
  check_item[10] = 2;
  check_item[11] = 4;
  check_item[12] = 1;
  check_item[13] = 2;
  check_item[14] = 3;
  check_item[15] = 5;
  check_item[16] = 6;
  check_item[17] = 1;
  check_item[18] = 4;
  check_item[19] = 6;
  check_item[20] = 4;
  check_item[21] = 5;

  // 結合
  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_DOMAIN_ID);

  // 確認
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID n_vertex", merged_graph.n_vertex, 7);
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID n_vertex", merged_graph.n_internal_vertex, 5);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID vertex_id", 7, merged_graph.vertex_id, 7, check_vertex_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID vertex_domain_id", 7, merged_graph.vertex_domain_id, 7, check_vertex_domain_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID index", 8, merged_graph.index, 8, check_index);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_DOMAIN_ID item", 22, merged_graph.item, 22, check_item);

  // free
  gedatsu_graph_finalize(&merged_graph);
  monolis_com_finalize(&merged_monoCOM);

  // 結合後グラフの模範解答 (ORDER_DOMAIN_IDで計算点グラフを結合)
  check_vertex_id[0] = 1;
  check_vertex_id[1] = 2;
  check_vertex_id[2] = 3;
  check_vertex_id[3] = 4;
  check_vertex_id[4] = 5;
  check_vertex_id[5] = 6;
  check_vertex_id[6] = 7;

  check_index[0] = 0;
  check_index[1] = 2;
  check_index[2] = 6;
  check_index[3] = 8;
  check_index[4] = 12;
  check_index[5] = 17;
  check_index[6] = 20;
  check_index[7] = 22;

  check_item[0] = 1;
  check_item[1] = 3;
  check_item[2] = 0;
  check_item[3] = 2;
  check_item[4] = 3;
  check_item[5] = 4;
  check_item[6] = 1;
  check_item[7] = 4;
  check_item[8] = 0;
  check_item[9] = 1;
  check_item[10] = 4;
  check_item[11] = 5;
  check_item[12] = 1;
  check_item[13] = 2;
  check_item[14] = 3;
  check_item[15] = 5;
  check_item[16] = 6;
  check_item[17] = 3;
  check_item[18] = 4;
  check_item[19] = 6;
  check_item[20] = 4;
  check_item[21] = 5;

  // 結合
  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_NODAL_ID);

  // 確認
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID n_vertex", merged_graph.n_vertex, 7);
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID n_vertex", merged_graph.n_internal_vertex, 5);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID vertex_id", 7, merged_graph.vertex_id, 7, check_vertex_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID vertex_domain_id", 7, merged_graph.vertex_domain_id, 7, check_vertex_domain_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID index", 8, merged_graph.index, 8, check_index);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c nodal_graph ORDER_NODAL_ID item", 22, merged_graph.item, 22, check_item);

  // free
  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_finalize(&graphs[i]);
    monolis_com_finalize(&monoCOMs[i]);
  }
  gedatsu_graph_finalize(&merged_graph);
  monolis_com_finalize(&merged_monoCOM);
  monolis_dealloc_I_1d(&check_vertex_id);
  monolis_dealloc_I_1d(&check_vertex_domain_id);
  monolis_dealloc_I_1d(&check_index);
  monolis_dealloc_I_1d(&check_item);
}

void gedatsu_merge_connectivity_subgraphs_c_test()
{
  int n_nodal_graphs;
  GEDATSU_GRAPH nodal_graphs[3];
  GEDATSU_GRAPH merged_nodal_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_nodal_monoCOM;
  int n_conn_graphs;
  GEDATSU_GRAPH conn_graphs[3];
  GEDATSU_GRAPH merged_conn_graph;
  int* check_vertex_id = NULL;
  int* check_vertex_domain_id = NULL;
  int* check_index = NULL;
  int* check_item = NULL;
  int** edge = NULL;

  monolis_std_log_string("gedatsu_merge_connectivity_subgraphs_c_test");

  n_nodal_graphs = 3;
  n_conn_graphs = 3;

  // 結合前の計算点グラフ
  for (int i = 0; i < n_nodal_graphs; i++)
  {
    gedatsu_graph_initialize(&nodal_graphs[i]);
    gedatsu_graph_initialize(&conn_graphs[i]);
  }

  gedatsu_graph_set_n_vertex(&nodal_graphs[0], 5);
  gedatsu_graph_set_n_vertex(&nodal_graphs[1], 5);
  gedatsu_graph_set_n_vertex(&nodal_graphs[2], 6);
  nodal_graphs[0].n_internal_vertex = 2;
  nodal_graphs[1].n_internal_vertex = 2;
  nodal_graphs[2].n_internal_vertex = 1;

  nodal_graphs[0].vertex_id[0] = 1;
  nodal_graphs[0].vertex_id[1] = 4;
  nodal_graphs[0].vertex_id[2] = 2;
  nodal_graphs[0].vertex_id[3] = 5;
  nodal_graphs[0].vertex_id[4] = 6;
  nodal_graphs[1].vertex_id[0] = 2;
  nodal_graphs[1].vertex_id[1] = 3;
  nodal_graphs[1].vertex_id[2] = 1;
  nodal_graphs[1].vertex_id[3] = 4;
  nodal_graphs[1].vertex_id[4] = 5;
  nodal_graphs[2].vertex_id[0] = 5;
  nodal_graphs[2].vertex_id[1] = 2;
  nodal_graphs[2].vertex_id[2] = 3;
  nodal_graphs[2].vertex_id[3] = 4;
  nodal_graphs[2].vertex_id[4] = 6;
  nodal_graphs[2].vertex_id[5] = 7;

  edge = monolis_alloc_I_2d(edge, 14, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 1; edge[2][1] = 0;
  edge[3][0] = 1; edge[3][1] = 2;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 1;
  edge[8][0] = 2; edge[8][1] = 3;
  edge[9][0] = 3; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 2;
  edge[11][0] = 3; edge[11][1] = 4;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&nodal_graphs[0], 14, edge, true);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 1; edge[4][1] = 0;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 3; edge[8][1] = 0;
  edge[9][0] = 3; edge[9][1] = 2;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 4; edge[11][1] = 0;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&nodal_graphs[1], 14, edge, true);

  monolis_dealloc_I_2d(&edge, 14, 2);
  edge = monolis_alloc_I_2d(edge, 18, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 0; edge[4][1] = 5;
  edge[5][0] = 1; edge[5][1] = 0;
  edge[6][0] = 1; edge[6][1] = 2;
  edge[7][0] = 1; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 0;
  edge[9][0] = 2; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 0;
  edge[11][0] = 3; edge[11][1] = 1;
  edge[12][0] = 3; edge[12][1] = 4;
  edge[13][0] = 4; edge[13][1] = 0;
  edge[14][0] = 4; edge[14][1] = 3;
  edge[15][0] = 4; edge[15][1] = 5;
  edge[16][0] = 5; edge[16][1] = 0;
  edge[17][0] = 5; edge[17][1] = 4;
  gedatsu_graph_set_edge(&nodal_graphs[2], 18, edge, true);

  // 結合前の通信テーブル
  for (int i = 0; i < n_nodal_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後の計算点グラフ (ORDER_DOMAIN_IDで計算点グラフを結合) → グラフ結合関数使っちゃう
  gedatsu_merge_nodal_subgraphs(n_nodal_graphs, nodal_graphs, monoCOMs, &merged_nodal_graph, &merged_nodal_monoCOM, ORDER_DOMAIN_ID);

  // 結合前のコネクティビティグラフ
  gedatsu_graph_set_n_vertex(&conn_graphs[0], 3);
  gedatsu_graph_set_n_vertex(&conn_graphs[1], 3);
  gedatsu_graph_set_n_vertex(&conn_graphs[2], 4);
  conn_graphs[0].n_internal_vertex = 3;
  conn_graphs[1].n_internal_vertex = 1;
  conn_graphs[2].n_internal_vertex = 1;

  conn_graphs[0].vertex_id[0] = 1;
  conn_graphs[0].vertex_id[1] = 2;
  conn_graphs[0].vertex_id[2] = 4;
  conn_graphs[1].vertex_id[0] = 3;
  conn_graphs[1].vertex_id[1] = 1;
  conn_graphs[1].vertex_id[2] = 2;
  conn_graphs[2].vertex_id[0] = 5;
  conn_graphs[2].vertex_id[1] = 3;
  conn_graphs[2].vertex_id[2] = 2;
  conn_graphs[2].vertex_id[3] = 4;

  monolis_dealloc_I_2d(&edge, 9, 2);
  edge = monolis_alloc_I_2d(edge, 9, 2);
  edge[0][0] = 0; edge[0][1] = 0;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 1;
  edge[3][0] = 1; edge[3][1] = 1;
  edge[4][0] = 1; edge[4][1] = 2;
  edge[5][0] = 1; edge[5][1] = 3;
  edge[6][0] = 2; edge[6][1] = 1;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 4;
  gedatsu_graph_set_edge(&conn_graphs[0], 9, edge, false);

  edge[0][0] = 0; edge[0][1] = 0;
  edge[1][0] = 0; edge[1][1] = 1;
  edge[2][0] = 0; edge[2][1] = 4;
  edge[3][0] = 1; edge[3][1] = 0;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 2;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 4;
  edge[8][0] = 2; edge[8][1] = 3;
  gedatsu_graph_set_edge(&conn_graphs[1], 9, edge, false);

  monolis_dealloc_I_2d(&edge, 9, 2);
  edge = monolis_alloc_I_2d(edge, 12, 2);
  edge[0][0] = 0; edge[0][1] = 0;
  edge[1][0] = 0; edge[1][1] = 5;
  edge[2][0] = 0; edge[2][1] = 4;
  edge[3][0] = 1; edge[3][1] = 0;
  edge[4][0] = 1; edge[4][1] = 1;
  edge[5][0] = 1; edge[5][1] = 2;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 1;
  edge[9][0] = 3; edge[9][1] = 0;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 3; edge[11][1] = 3;
  gedatsu_graph_set_edge(&conn_graphs[2], 12, edge, false);

  // 結合後のコネクティビティグラフの模範解答 (ORDER_NODAL_IDで計算点グラフを結合)
  check_vertex_id = monolis_alloc_I_1d(check_vertex_id, 5);
  check_vertex_domain_id = monolis_alloc_I_1d(check_vertex_domain_id, 5);
  check_index = monolis_alloc_I_1d(check_index, 6);
  check_item = monolis_alloc_I_1d(check_item, 15);

  check_vertex_id[0] = 1;
  check_vertex_id[1] = 2;
  check_vertex_id[2] = 3;
  check_vertex_id[3] = 4;
  check_vertex_id[4] = 5;

  check_index[0] = 0;
  check_index[1] = 3;
  check_index[2] = 6;
  check_index[3] = 9;
  check_index[4] = 12;
  check_index[5] = 15;

  check_item[0] = 0;
  check_item[1] = 2;
  check_item[2] = 1;
  check_item[3] = 1;
  check_item[4] = 2;
  check_item[5] = 4;
  check_item[6] = 2;
  check_item[7] = 3;
  check_item[8] = 4;
  check_item[9] =  1;
  check_item[10] = 4;
  check_item[11] = 5;
  check_item[12] = 4;
  check_item[13] = 6;
  check_item[14] = 5;

  // 結合
  gedatsu_merge_connectivity_subgraphs(n_nodal_graphs, nodal_graphs, &merged_nodal_graph, &merged_nodal_monoCOM,
    n_conn_graphs, conn_graphs, &merged_conn_graph);

  // 確認
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c conn_graph n_vertex", merged_conn_graph.n_vertex, 5);
  monolis_test_check_eq_I1("gedatsu_graph_merge_test_c conn_graph n_vertex", merged_conn_graph.n_internal_vertex, 5);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c conn_graph vertex_id", 5, merged_conn_graph.vertex_id, 5, check_vertex_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c conn_graph vertex_domain_id", 5, merged_conn_graph.vertex_domain_id, 5, check_vertex_domain_id);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c conn_graph index", 6, merged_conn_graph.index, 6, check_index);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c conn_graph item", 15, merged_conn_graph.item, 15, check_item);

  //free
  for (int i = 0; i < n_nodal_graphs; i++)
  {
    gedatsu_graph_finalize(&nodal_graphs[i]);
  }
    for (int i = 0; i < n_conn_graphs; i++)
  {
    gedatsu_graph_finalize(&conn_graphs[i]);
  }
  gedatsu_graph_finalize(&merged_nodal_graph);
  gedatsu_graph_finalize(&merged_conn_graph);
  monolis_com_finalize(&merged_nodal_monoCOM);
  monolis_dealloc_I_1d(&check_vertex_id);
  monolis_dealloc_I_1d(&check_index);
  monolis_dealloc_I_1d(&check_item);
}

void gedatsu_merge_distval_R_c_test()
{
  // 結合前グラフ
  int n_graphs;
  GEDATSU_GRAPH graphs[3];
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_monoCOM;
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_R list_struct_R[3];
  int* merged_n_dof_list = NULL;
  double* merged_array_R = NULL;
  int* check_merged_n_dof_list = NULL;
  double* check_merged_array_R = NULL;
  int** edge = NULL;
  int* array_I = NULL;
  double* array_R = NULL;

  monolis_std_log_string("gedatsu_merge_distval_R_c_test");

  n_graphs = 3;

  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_initialize(&graphs[i]);
  }
  gedatsu_graph_set_n_vertex(&graphs[0], 5);
  gedatsu_graph_set_n_vertex(&graphs[1], 5);
  gedatsu_graph_set_n_vertex(&graphs[2], 6);
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id[0] = 1;
  graphs[0].vertex_id[1] = 4;
  graphs[0].vertex_id[2] = 2;
  graphs[0].vertex_id[3] = 5;
  graphs[0].vertex_id[4] = 6;
  graphs[1].vertex_id[0] = 2;
  graphs[1].vertex_id[1] = 3;
  graphs[1].vertex_id[2] = 1;
  graphs[1].vertex_id[3] = 4;
  graphs[1].vertex_id[4] = 5;
  graphs[2].vertex_id[0] = 5;
  graphs[2].vertex_id[1] = 2;
  graphs[2].vertex_id[2] = 3;
  graphs[2].vertex_id[3] = 4;
  graphs[2].vertex_id[4] = 6;
  graphs[2].vertex_id[5] = 7;

  edge = monolis_alloc_I_2d(edge, 14, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 1; edge[2][1] = 0;
  edge[3][0] = 1; edge[3][1] = 2;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 1;
  edge[8][0] = 2; edge[8][1] = 3;
  edge[9][0] = 3; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 2;
  edge[11][0] = 3; edge[11][1] = 4;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[0], 14, edge, true);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 1; edge[4][1] = 0;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 3; edge[8][1] = 0;
  edge[9][0] = 3; edge[9][1] = 2;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 4; edge[11][1] = 0;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[1], 14, edge, true);

  monolis_dealloc_I_2d(&edge, 14, 2);
  edge = monolis_alloc_I_2d(edge, 18, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 0; edge[4][1] = 5;
  edge[5][0] = 1; edge[5][1] = 0;
  edge[6][0] = 1; edge[6][1] = 2;
  edge[7][0] = 1; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 0;
  edge[9][0] = 2; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 0;
  edge[11][0] = 3; edge[11][1] = 1;
  edge[12][0] = 3; edge[12][1] = 4;
  edge[13][0] = 4; edge[13][1] = 0;
  edge[14][0] = 4; edge[14][1] = 3;
  edge[15][0] = 4; edge[15][1] = 5;
  edge[16][0] = 5; edge[16][1] = 0;
  edge[17][0] = 5; edge[17][1] = 4;
  gedatsu_graph_set_edge(&graphs[2], 18, edge, true);

  // 結合前の通信テーブル
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_NODAL_ID);

  // 結合前の物理量分布
  monolis_list_initialize_I(n_dof_list, n_graphs);
  monolis_list_initialize_R(list_struct_R, n_graphs);

  // n_dof_list
  array_I = monolis_alloc_I_1d(array_I, 5);
  for (int i = 0; i < 5; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 0, 5, array_I);
  monolis_list_set_I(n_dof_list, 1, 5, array_I);
  monolis_dealloc_I_1d(&array_I);
  array_I = monolis_alloc_I_1d(array_I, 6);
  for (int i = 0; i < 6; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 2, 6, array_I);

  // list_struct_R
  array_R = monolis_alloc_R_1d(array_R, 5);
  array_R[0] = 1.0;
  array_R[1] = 4.0;
  array_R[2] = 2.0;
  array_R[3] = 5.0;
  array_R[4] = 6.0;
  monolis_list_set_R(list_struct_R, 0, 5, array_R);
  array_R[0] = 2.0;
  array_R[1] = 3.0;
  array_R[2] = 1.0;
  array_R[3] = 4.0;
  array_R[4] = 5.0;
  monolis_list_set_R(list_struct_R, 1, 5, array_R);
  monolis_dealloc_R_1d(&array_R);
  array_R = monolis_alloc_R_1d(array_R, 6);
  array_R[0] = 5.0;
  array_R[1] = 2.0;
  array_R[2] = 3.0;
  array_R[3] = 4.0;
  array_R[4] = 6.0;
  array_R[5] = 7.0;
  monolis_list_set_R(list_struct_R, 2, 6, array_R);

  // 結合後の物理量分布の確認
  check_merged_n_dof_list = monolis_alloc_I_1d(check_merged_n_dof_list, 7);
  check_merged_array_R = monolis_alloc_R_1d(check_merged_array_R, 7);
  for (int i = 0; i < 7; i++)
  {
    check_merged_n_dof_list[i] = 1;
    check_merged_array_R[i] = (double)(i+1);
  }

  // 結合
  gedatsu_merge_distval_R(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_R,
    &merged_n_dof_list, &merged_array_R);

  // 確認
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c dist_val_R n_dof_list", 7, merged_n_dof_list, 7, check_merged_n_dof_list);
  monolis_test_check_eq_R("gedatsu_graph_merge_test_c dist_val_R array", 7, merged_array_R, 7, check_merged_array_R);

  // free
  monolis_list_finalize_I(n_dof_list, 1);
  monolis_list_finalize_R(list_struct_R, 1);
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_dealloc_I_1d(&graphs[i].vertex_id);
    monolis_dealloc_I_1d(&graphs[i].vertex_domain_id);
    monolis_dealloc_I_1d(&graphs[i].index);
    monolis_dealloc_I_1d(&graphs[i].item);
  }
  monolis_dealloc_I_1d(&merged_graph.vertex_id);
  monolis_dealloc_I_1d(&merged_graph.vertex_domain_id);
  monolis_dealloc_I_1d(&merged_graph.index);
  monolis_dealloc_I_1d(&merged_graph.item);
  monolis_dealloc_I_1d(&merged_n_dof_list);
  monolis_dealloc_R_1d(&merged_array_R);
}

void gedatsu_merge_distval_I_c_test()
{
  // 結合前グラフ
  int n_graphs;
  GEDATSU_GRAPH graphs[3];
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_monoCOM;
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_I list_struct_I[3];
  int* merged_n_dof_list = NULL;
  int* merged_array_I = NULL;
  int* check_merged_n_dof_list = NULL;
  int* check_merged_array_I = NULL;
  int** edge = NULL;
  int* array_I = NULL;

  monolis_std_log_string("gedatsu_merge_distval_I_c_test");

  n_graphs = 3;

  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_initialize(&graphs[i]);
  }
  gedatsu_graph_set_n_vertex(&graphs[0], 5);
  gedatsu_graph_set_n_vertex(&graphs[1], 5);
  gedatsu_graph_set_n_vertex(&graphs[2], 6);
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id[0] = 1;
  graphs[0].vertex_id[1] = 4;
  graphs[0].vertex_id[2] = 2;
  graphs[0].vertex_id[3] = 5;
  graphs[0].vertex_id[4] = 6;
  graphs[1].vertex_id[0] = 2;
  graphs[1].vertex_id[1] = 3;
  graphs[1].vertex_id[2] = 1;
  graphs[1].vertex_id[3] = 4;
  graphs[1].vertex_id[4] = 5;
  graphs[2].vertex_id[0] = 5;
  graphs[2].vertex_id[1] = 2;
  graphs[2].vertex_id[2] = 3;
  graphs[2].vertex_id[3] = 4;
  graphs[2].vertex_id[4] = 6;
  graphs[2].vertex_id[5] = 7;

  edge = monolis_alloc_I_2d(edge, 14, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 1; edge[2][1] = 0;
  edge[3][0] = 1; edge[3][1] = 2;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 1;
  edge[8][0] = 2; edge[8][1] = 3;
  edge[9][0] = 3; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 2;
  edge[11][0] = 3; edge[11][1] = 4;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[0], 14, edge, true);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 1; edge[4][1] = 0;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 3; edge[8][1] = 0;
  edge[9][0] = 3; edge[9][1] = 2;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 4; edge[11][1] = 0;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[1], 14, edge, true);

  monolis_dealloc_I_2d(&edge, 14, 2);
  edge = monolis_alloc_I_2d(edge, 18, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 0; edge[4][1] = 5;
  edge[5][0] = 1; edge[5][1] = 0;
  edge[6][0] = 1; edge[6][1] = 2;
  edge[7][0] = 1; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 0;
  edge[9][0] = 2; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 0;
  edge[11][0] = 3; edge[11][1] = 1;
  edge[12][0] = 3; edge[12][1] = 4;
  edge[13][0] = 4; edge[13][1] = 0;
  edge[14][0] = 4; edge[14][1] = 3;
  edge[15][0] = 4; edge[15][1] = 5;
  edge[16][0] = 5; edge[16][1] = 0;
  edge[17][0] = 5; edge[17][1] = 4;
  gedatsu_graph_set_edge(&graphs[2], 18, edge, true);

  // 結合前の通信テーブル
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_NODAL_ID);

  // 結合前の物理量分布
  monolis_list_initialize_I(n_dof_list, n_graphs);
  monolis_list_initialize_I(list_struct_I, n_graphs);

  // n_dof_list
  array_I = monolis_alloc_I_1d(array_I, 5);
  for (int i = 0; i < 5; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 0, 5, array_I);
  monolis_list_set_I(n_dof_list, 1, 5, array_I);
  monolis_dealloc_I_1d(&array_I);
  array_I = monolis_alloc_I_1d(array_I, 6);
  for (int i = 0; i < 6; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 2, 6, array_I);

  // list_struct_I
  monolis_dealloc_I_1d(&array_I);
  array_I = monolis_alloc_I_1d(array_I, 5);
  array_I[0] = 1;
  array_I[1] = 4;
  array_I[2] = 2;
  array_I[3] = 5;
  array_I[4] = 6;
  monolis_list_set_I(list_struct_I, 0, 5, array_I);
  array_I[0] = 2;
  array_I[1] = 3;
  array_I[2] = 1;
  array_I[3] = 4;
  array_I[4] = 5;
  monolis_list_set_I(list_struct_I, 1, 5, array_I);
  monolis_dealloc_I_1d(&array_I);
  array_I = monolis_alloc_I_1d(array_I, 6);
  array_I[0] = 5;
  array_I[1] = 2;
  array_I[2] = 3;
  array_I[3] = 4;
  array_I[4] = 6;
  array_I[5] = 7;
  monolis_list_set_I(list_struct_I, 2, 6, array_I);

  // 結合後の物理量分布の模範解答
  check_merged_n_dof_list = monolis_alloc_I_1d(check_merged_n_dof_list, 7);
  check_merged_array_I = monolis_alloc_I_1d(check_merged_array_I, 7);
  for (int i = 0; i < 7; i++)
  {
    check_merged_n_dof_list[i] = 1;
    check_merged_array_I[i] = i + 1;
  }

  // 結合
  gedatsu_merge_distval_I(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_I,
  &merged_n_dof_list, &merged_array_I);

  // 確認
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c dist_val_I n_dof_list", 7, merged_n_dof_list, 7, check_merged_n_dof_list);
  monolis_test_check_eq_I("gedatsu_graph_merge_test_c dist_val_I array", 7, merged_array_I, 7, check_merged_array_I);

  // free
  monolis_list_finalize_I(n_dof_list, 1);
  monolis_list_finalize_I(list_struct_I, 1);
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_dealloc_I_1d(&graphs[i].vertex_id);
    monolis_dealloc_I_1d(&graphs[i].vertex_domain_id);
    monolis_dealloc_I_1d(&graphs[i].index);
    monolis_dealloc_I_1d(&graphs[i].item);
  }
  monolis_dealloc_I_1d(&merged_graph.vertex_id);
  monolis_dealloc_I_1d(&merged_graph.vertex_domain_id);
  monolis_dealloc_I_1d(&merged_graph.index);
  monolis_dealloc_I_1d(&merged_graph.item);
  monolis_dealloc_I_1d(&merged_n_dof_list);
  monolis_dealloc_I_1d(&merged_array_I);
}

void gedatsu_merge_distval_C_c_test()
{
  // 結合前グラフ
  int n_graphs;
  GEDATSU_GRAPH graphs[3];
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_monoCOM;
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_C list_struct_C[3];
  int* merged_n_dof_list = NULL;
  double complex* merged_array_C = NULL;
  int* check_merged_n_dof_list = NULL;
  double complex* check_merged_array_C = NULL;
  int** edge = NULL;
  int* array_I = NULL;
  double complex* array_C = NULL;

  monolis_std_log_string("gedatsu_merge_distval_C_c_test");

  n_graphs = 3;

  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_initialize(&graphs[i]);
  }
  gedatsu_graph_set_n_vertex(&graphs[0], 5);
  gedatsu_graph_set_n_vertex(&graphs[1], 5);
  gedatsu_graph_set_n_vertex(&graphs[2], 6);
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id[0] = 1;
  graphs[0].vertex_id[1] = 4;
  graphs[0].vertex_id[2] = 2;
  graphs[0].vertex_id[3] = 5;
  graphs[0].vertex_id[4] = 6;
  graphs[1].vertex_id[0] = 2;
  graphs[1].vertex_id[1] = 3;
  graphs[1].vertex_id[2] = 1;
  graphs[1].vertex_id[3] = 4;
  graphs[1].vertex_id[4] = 5;
  graphs[2].vertex_id[0] = 5;
  graphs[2].vertex_id[1] = 2;
  graphs[2].vertex_id[2] = 3;
  graphs[2].vertex_id[3] = 4;
  graphs[2].vertex_id[4] = 6;
  graphs[2].vertex_id[5] = 7;

  edge = monolis_alloc_I_2d(edge, 14, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 1; edge[2][1] = 0;
  edge[3][0] = 1; edge[3][1] = 2;
  edge[4][0] = 1; edge[4][1] = 3;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 1;
  edge[8][0] = 2; edge[8][1] = 3;
  edge[9][0] = 3; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 2;
  edge[11][0] = 3; edge[11][1] = 4;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[0], 14, edge, true);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 1; edge[4][1] = 0;
  edge[5][0] = 1; edge[5][1] = 4;
  edge[6][0] = 2; edge[6][1] = 0;
  edge[7][0] = 2; edge[7][1] = 3;
  edge[8][0] = 3; edge[8][1] = 0;
  edge[9][0] = 3; edge[9][1] = 2;
  edge[10][0] = 3; edge[10][1] = 4;
  edge[11][0] = 4; edge[11][1] = 0;
  edge[12][0] = 4; edge[12][1] = 1;
  edge[13][0] = 4; edge[13][1] = 3;
  gedatsu_graph_set_edge(&graphs[1], 14, edge, true);

  monolis_dealloc_I_2d(&edge, 14, 2);
  edge = monolis_alloc_I_2d(edge, 18, 2);
  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 0; edge[1][1] = 2;
  edge[2][0] = 0; edge[2][1] = 3;
  edge[3][0] = 0; edge[3][1] = 4;
  edge[4][0] = 0; edge[4][1] = 5;
  edge[5][0] = 1; edge[5][1] = 0;
  edge[6][0] = 1; edge[6][1] = 2;
  edge[7][0] = 1; edge[7][1] = 3;
  edge[8][0] = 2; edge[8][1] = 0;
  edge[9][0] = 2; edge[9][1] = 1;
  edge[10][0] = 3; edge[10][1] = 0;
  edge[11][0] = 3; edge[11][1] = 1;
  edge[12][0] = 3; edge[12][1] = 4;
  edge[13][0] = 4; edge[13][1] = 0;
  edge[14][0] = 4; edge[14][1] = 3;
  edge[15][0] = 4; edge[15][1] = 5;
  edge[16][0] = 5; edge[16][1] = 0;
  edge[17][0] = 5; edge[17][1] = 4;
  gedatsu_graph_set_edge(&graphs[2], 18, edge, true);

  // 結合前の通信テーブル
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_NODAL_ID);

  // 結合前の物理量分布
  monolis_list_initialize_I(n_dof_list, n_graphs);
  monolis_list_initialize_C(list_struct_C, n_graphs);
  // n_dof_list
  array_I = monolis_alloc_I_1d(array_I, 5);
  for (int i = 0; i < 5; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 0, 5, array_I);
  monolis_list_set_I(n_dof_list, 1, 5, array_I);
  monolis_dealloc_I_1d(&array_I);
  array_I = monolis_alloc_I_1d(array_I, 6);
  for (int i = 0; i < 6; i++)
  {
    array_I[i] = 1;
  }
  monolis_list_set_I(n_dof_list, 2, 6, array_I);

  // list_struct_C
  monolis_dealloc_C_1d(&array_C);
  array_C = monolis_alloc_C_1d(array_C, 5);
  array_C[0] = 1 + 1.0*I;
  array_C[1] = 4 + 1.0*I;
  array_C[2] = 2 + 1.0*I;
  array_C[3] = 5 + 1.0*I;
  array_C[4] = 6 + 1.0*I;
  monolis_list_set_C(list_struct_C, 0, 5, array_C);
  array_C[0] = 2 + 1.0*I;
  array_C[1] = 3 + 1.0*I;
  array_C[2] = 1 + 1.0*I;
  array_C[3] = 4 + 1.0*I;
  array_C[4] = 5 + 1.0*I;
  monolis_list_set_C(list_struct_C, 1, 5, array_C);
  monolis_dealloc_C_1d(&array_C);
  array_C = monolis_alloc_C_1d(array_C, 6);
  array_C[0] = 5 + 1.0*I;
  array_C[1] = 2 + 1.0*I;
  array_C[2] = 3 + 1.0*I;
  array_C[3] = 4 + 1.0*I;
  array_C[4] = 6 + 1.0*I;
  array_C[5] = 7 + 1.0*I;
  monolis_list_set_C(list_struct_C, 2, 6, array_C);

  // 結合後の物理量分布の模範解答
  check_merged_n_dof_list = monolis_alloc_I_1d(check_merged_n_dof_list, 7);
  check_merged_array_C = monolis_alloc_C_1d(check_merged_array_C, 7);
  for (int i = 0; i < 7; i++)
  {
    check_merged_n_dof_list[i] = 1;
    check_merged_array_C[i] = (double complex)(i+1) + 1.0*I;
  }

  // 結合
  gedatsu_merge_distval_C(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_C,
  &merged_n_dof_list, &merged_array_C);

  monolis_test_check_eq_I("gedatsu_graph_merge_test_c dist_val_C n_dof_list", 7, merged_n_dof_list, 7, check_merged_n_dof_list);
  monolis_test_check_eq_C("gedatsu_graph_merge_test_c dist_val_C array", 7, merged_array_C, 7, check_merged_array_C);

  // free
  monolis_list_finalize_I(n_dof_list, 1);
  monolis_list_finalize_C(list_struct_C, 1);
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_dealloc_I_1d(&graphs[i].vertex_id);
    monolis_dealloc_I_1d(&graphs[i].vertex_domain_id);
    monolis_dealloc_I_1d(&graphs[i].index);
    monolis_dealloc_I_1d(&graphs[i].item);
  }
  monolis_dealloc_I_1d(&merged_graph.vertex_id);
  monolis_dealloc_I_1d(&merged_graph.vertex_domain_id);
  monolis_dealloc_I_1d(&merged_graph.index);
  monolis_dealloc_I_1d(&merged_graph.item);
  monolis_dealloc_I_1d(&merged_n_dof_list);
  monolis_dealloc_C_1d(&merged_array_C);
}

