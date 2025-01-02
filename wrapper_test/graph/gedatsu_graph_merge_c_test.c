#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "gedatsu.h"
#include "monolis_utils.h"
#include "gedatsu_graph_merge_c_test.h"

void gedatsu_graph_merge_c_test()
{
  gedatsu_list_initialize_R_c_test();
  gedatsu_list_initialize_I_c_test();
  gedatsu_list_initialize_C_c_test();
  gedatsu_list_finalize_R_c_test();
  gedatsu_list_finalize_I_c_test();
  gedatsu_list_finalize_C_c_test();
  gedatsu_list_set_R_c_test();
  gedatsu_list_set_I_c_test();
  gedatsu_list_set_C_c_test();
  gedatsu_list_get_R_c_test();
  gedatsu_list_get_I_c_test();
  gedatsu_list_get_C_c_test();
  gedatsu_merge_distval_R_c_test();
  gedatsu_merge_distval_I_c_test();
  gedatsu_merge_distval_C_c_test();
  gedatsu_merge_nodal_subgraphs_c_test();
  gedatsu_merge_connectivity_subgraphs_c_test();
}

void gedatsu_list_initialize_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];

  monolis_std_log_string("gedatsu_list_initialize_R");

  list_struct_R[0].array = NULL;
  gedatsu_list_initialize_R(list_struct_R, 1);

  monolis_test_check_eq_I1("gedatsu_list_initialize_R", list_struct_R[0].n, 0);
}

void gedatsu_list_initialize_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];

  monolis_std_log_string("gedatsu_list_initialize_I");

  list_struct_I[0].array = NULL;
  gedatsu_list_initialize_I(list_struct_I, 1);

  monolis_test_check_eq_I1("gedatsu_list_initialize_I", list_struct_I[0].n, 0);
}

void gedatsu_list_initialize_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];

  monolis_std_log_string("gedatsu_list_initialize_C");

  list_struct_C[0].array = NULL;
  gedatsu_list_initialize_C(list_struct_C, 1);

  monolis_test_check_eq_I1("gedatsu_list_initialize_C", list_struct_C[0].n, 0);
}

void gedatsu_list_finalize_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];

  monolis_std_log_string("gedatsu_list_finalize_R");

  list_struct_R[0].array = NULL;
  list_struct_R[1].n = 1;
  gedatsu_list_finalize_R(list_struct_R, 1);

  monolis_test_check_eq_I1("gedatsu_list_finalize_R", list_struct_R[0].n, 0);
}

void gedatsu_list_finalize_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];

  monolis_std_log_string("gedatsu_list_finalize_I");

  list_struct_I[0].array = NULL;
  list_struct_I[1].n = 1;
  gedatsu_list_finalize_I(list_struct_I, 1);

  monolis_test_check_eq_I1("gedatsu_list_finalize_I", list_struct_I[0].n, 0);
}

void gedatsu_list_finalize_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];

  monolis_std_log_string("gedatsu_list_finalize_C");

  list_struct_C[0].array = NULL;
  list_struct_C[1].n = 1;
  gedatsu_list_finalize_C(list_struct_C, 1);

  monolis_test_check_eq_I1("gedatsu_list_finalize_C", list_struct_C[0].n, 0);
}

void gedatsu_list_set_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];
  double array[1];

  monolis_std_log_string("gedatsu_list_set_R");

  list_struct_R[0].array = NULL;
  array[0] = 1.0;
  gedatsu_list_initialize_R(list_struct_R, 1);
  gedatsu_list_set_R(list_struct_R, 0, 1, array);

  monolis_test_check_eq_I1("gedatsu_list_set_R n", list_struct_R[0].n, 1);
  monolis_test_check_eq_R1("gedatsu_list_set_R array", list_struct_R[0].array[0], 1.0);
}

void gedatsu_list_set_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];
  int array[1];

  monolis_std_log_string("gedatsu_list_set_I");

  list_struct_I[0].array = NULL;
  array[0] = 1;
  gedatsu_list_initialize_I(list_struct_I, 1);
  gedatsu_list_set_I(list_struct_I, 0, 1, array);

  monolis_test_check_eq_I1("gedatsu_list_set_I n", list_struct_I[0].n, 1);
  monolis_test_check_eq_I1("gedatsu_list_set_I array", list_struct_I[0].array[0], 1);
}

void gedatsu_list_set_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];
  double complex array[1];

  monolis_std_log_string("gedatsu_list_set_C");

  list_struct_C[0].array = NULL;
  array[0] = 1.0 + 1.0*I;
  gedatsu_list_initialize_C(list_struct_C, 1);
  gedatsu_list_set_C(list_struct_C, 0, 1, array);

  monolis_test_check_eq_I1("gedatsu_list_set_C n", list_struct_C[0].n, 1);
  monolis_test_check_eq_C1("gedatsu_list_set_C array", list_struct_C[0].array[0], 1.0 + 1.0*I);
}

void gedatsu_list_get_R_c_test()
{
  MONOLIS_LIST_R list_struct_R[1];
  double array1[1];
  double *array2 = NULL;

  monolis_std_log_string("gedatsu_list_get_R");

  list_struct_R[0].array = NULL;
  array1[0] = 1.0;
  gedatsu_list_initialize_R(list_struct_R, 1);
  gedatsu_list_set_R(list_struct_R, 0, 1, array1);
  gedatsu_list_get_R(list_struct_R, 0, &array2);

  monolis_test_check_eq_R1("gedatsu_list_get_R", list_struct_R[0].array[0], array2[0]);
}

void gedatsu_list_get_I_c_test()
{
  MONOLIS_LIST_I list_struct_I[1];
  int array1[1];
  int *array2 = NULL;

  monolis_std_log_string("gedatsu_list_get_I");

  list_struct_I[0].array = NULL;
  array1[0] = 1.0;
  gedatsu_list_initialize_I(list_struct_I, 1);
  gedatsu_list_set_I(list_struct_I, 0, 1, array1);
  gedatsu_list_get_I(list_struct_I, 0, &array2);

  monolis_test_check_eq_I1("gedatsu_list_get_I", list_struct_I[0].array[0], array2[0]);
}

void gedatsu_list_get_C_c_test()
{
  MONOLIS_LIST_C list_struct_C[1];
  double complex array1[1];
  double complex *array2 = NULL;

  monolis_std_log_string("gedatsu_list_get_C");

  list_struct_C[0].array = NULL;
  array1[0] = 1.0 + 1.0*I;
  gedatsu_list_initialize_C(list_struct_C, 1);
  gedatsu_list_set_C(list_struct_C, 0, 1, array1);
  gedatsu_list_get_C(list_struct_C, 0, &array2);

  monolis_test_check_eq_C1("gedatsu_list_get_C", list_struct_C[0].array[0], array2[0]);
}

void gedatsu_merge_nodal_subgraphs_c_test()
{
  int n_graphs;
  GEDATSU_GRAPH graphs[3];
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM monoCOMs[3];
  MONOLIS_COM merged_monoCOM;

  int* check_vertex_id = NULL;
  int* check_index = NULL;
  int* check_item = NULL;

  monolis_std_log_string("gedatsu_merge_nodal_subgraphs_c_test");

  n_graphs = 3;

  graphs[0].n_vertex = 5;
  graphs[1].n_vertex = 5;
  graphs[2].n_vertex = 6;
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id = monolis_alloc_I_1d(graphs[0].vertex_id, 5);
  graphs[1].vertex_id = monolis_alloc_I_1d(graphs[1].vertex_id, 5);
  graphs[2].vertex_id = monolis_alloc_I_1d(graphs[2].vertex_id, 6);
  graphs[0].vertex_domain_id = monolis_alloc_I_1d(graphs[0].vertex_domain_id, 5);
  graphs[1].vertex_domain_id = monolis_alloc_I_1d(graphs[1].vertex_domain_id, 5);
  graphs[2].vertex_domain_id = monolis_alloc_I_1d(graphs[2].vertex_domain_id, 6);
  graphs[0].index = monolis_alloc_I_1d(graphs[0].index, 6);
  graphs[1].index = monolis_alloc_I_1d(graphs[1].index, 6);
  graphs[2].index = monolis_alloc_I_1d(graphs[2].index, 7);
  graphs[0].item = monolis_alloc_I_1d(graphs[0].item, 14);
  graphs[1].item = monolis_alloc_I_1d(graphs[1].item, 14);
  graphs[2].item = monolis_alloc_I_1d(graphs[2].item, 18);

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

  graphs[0].index[0] = 0;
  graphs[0].index[1] = 2;
  graphs[0].index[2] = 6;
  graphs[0].index[3] = 9;
  graphs[0].index[4] = 12;
  graphs[0].index[5] = 14;
  graphs[1].index[0] = 0;
  graphs[1].index[1] = 4;
  graphs[1].index[2] = 6;
  graphs[1].index[3] = 8;
  graphs[1].index[4] = 11;
  graphs[1].index[5] = 14;
  graphs[2].index[0] = 0;
  graphs[2].index[1] = 5;
  graphs[2].index[2] = 8;
  graphs[2].index[3] = 10;
  graphs[2].index[4] = 13;
  graphs[2].index[5] = 16;
  graphs[2].index[6] = 18;

  graphs[0].item[0] = 1;
  graphs[0].item[1] = 2;
  graphs[0].item[2] = 0;
  graphs[0].item[3] = 2;
  graphs[0].item[4] = 3;
  graphs[0].item[5] = 4;
  graphs[0].item[6] = 0;
  graphs[0].item[7] = 1;
  graphs[0].item[8] = 3;
  graphs[0].item[9] = 1;
  graphs[0].item[10] = 2;
  graphs[0].item[11] = 4;
  graphs[0].item[12] = 1;
  graphs[0].item[13] = 3;
  graphs[1].item[0] = 1;
  graphs[1].item[1] = 2;
  graphs[1].item[2] = 3;
  graphs[1].item[3] = 4;
  graphs[1].item[4] = 0;
  graphs[1].item[5] = 4;
  graphs[1].item[6] = 0;
  graphs[1].item[7] = 3;
  graphs[1].item[8] = 0;
  graphs[1].item[9] = 2;
  graphs[1].item[10] = 4;
  graphs[1].item[11] = 0;
  graphs[1].item[12] = 1;
  graphs[1].item[13] = 3;
  graphs[2].item[0] = 1;
  graphs[2].item[1] = 2;
  graphs[2].item[2] = 3;
  graphs[2].item[3] = 4;
  graphs[2].item[4] = 5;
  graphs[2].item[5] = 0;
  graphs[2].item[6] = 2;
  graphs[2].item[7] = 3;
  graphs[2].item[8] = 0;
  graphs[2].item[9] = 1;
  graphs[2].item[10] = 0;
  graphs[2].item[11] = 1;
  graphs[2].item[12] = 4;
  graphs[2].item[13] = 0;
  graphs[2].item[14] = 3;
  graphs[2].item[15] = 5;
  graphs[2].item[16] = 0;
  graphs[2].item[17] = 4;

  // 結合前の通信テーブル
  for (int i = 0; i < n_graphs; i++)
  {
    monolis_com_initialize_by_self(&monoCOMs[i]);
  }

  // 結合後グラフの模範解答 (ORDER_NODAL_IDで計算点グラフを結合)
  check_vertex_id = monolis_alloc_I_1d(check_vertex_id, 7);
  check_index = monolis_alloc_I_1d(check_index, 8);
  check_item = monolis_alloc_I_1d(check_item, 22);

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

  check_item[0] = 2;
  check_item[1] = 4;
  check_item[2] = 1;
  check_item[3] = 3;
  check_item[4] = 4;
  check_item[5] = 5;
  check_item[6] = 2;
  check_item[7] = 5;
  check_item[8] = 1;
  check_item[9] = 2;
  check_item[10] = 5;
  check_item[11] = 6;
  check_item[12] = 2;
  check_item[13] = 3;
  check_item[14] = 4;
  check_item[15] = 6;
  check_item[16] = 7;
  check_item[17] = 4;
  check_item[18] = 5;
  check_item[19] = 7;
  check_item[20] = 5;
  check_item[21] = 6;

  // 結合


  // 確認




  // 結合後グラフの模範解答 (ORDER_DOMAIN_IDで計算点グラフを結合)
  monolis_dealloc_I_1d(&check_vertex_id);
  monolis_dealloc_I_1d(&check_index);
  monolis_dealloc_I_1d(&check_item);
  check_vertex_id = monolis_alloc_I_1d(check_vertex_id, 7);
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

  check_item[0] = 2;
  check_item[1] = 3;
  check_item[2] = 1;
  check_item[3] = 3;
  check_item[4] = 5;
  check_item[5] = 6;
  check_item[6] = 1;
  check_item[7] = 2;
  check_item[8] = 4;
  check_item[9] = 5;
  check_item[10] = 3;
  check_item[11] = 5;
  check_item[12] = 2;
  check_item[13] = 3;
  check_item[14] = 4;
  check_item[15] = 6;
  check_item[16] = 7;
  check_item[17] = 2;
  check_item[18] = 5;
  check_item[19] = 7;
  check_item[20] = 5;
  check_item[21] = 6;

  // 結合


  // 確認


  // free
  for (int i = 0; i < n_graphs; i++)
  {
    gedatsu_graph_finalize(&graphs[i]);
    monolis_com_finalize(&monoCOMs[i]);
  }
  // gedatsu_graph_finalize(&merged_graph);
  // monolis_com_finalize(&merged_monoCOM);
  monolis_dealloc_I_1d(&check_vertex_id);
  monolis_dealloc_I_1d(&check_index);
  monolis_dealloc_I_1d(&check_item);
}

void gedatsu_merge_connectivity_subgraphs_c_test()
{
  int n_nodal_graphs;
  GEDATSU_GRAPH nodal_graphs[3];
  GEDATSU_GRAPH merged_nodal_graph;
  MONOLIS_COM merged_nodal_monoCOM;
  int n_conn_graphs;
  GEDATSU_GRAPH conn_graphs[3];
  GEDATSU_GRAPH merged_conn_graph;

  int* check_vertex_id = NULL;
  int* check_index = NULL;
  int* check_item = NULL;

  monolis_std_log_string("gedatsu_merge_connectivity_subgraphs_c_test");

  n_nodal_graphs = 3;
  n_conn_graphs = 3;

  // 結合前の計算点グラフ
  nodal_graphs[0].n_vertex = 5;
  nodal_graphs[1].n_vertex = 5;
  nodal_graphs[2].n_vertex = 6;
  nodal_graphs[0].n_internal_vertex = 2;
  nodal_graphs[1].n_internal_vertex = 2;
  nodal_graphs[2].n_internal_vertex = 1;

  nodal_graphs[0].vertex_id = monolis_alloc_I_1d(nodal_graphs[0].vertex_id, 5);
  nodal_graphs[1].vertex_id = monolis_alloc_I_1d(nodal_graphs[1].vertex_id, 5);
  nodal_graphs[2].vertex_id = monolis_alloc_I_1d(nodal_graphs[2].vertex_id, 6);
  nodal_graphs[0].vertex_domain_id = monolis_alloc_I_1d(nodal_graphs[0].vertex_domain_id, 5);
  nodal_graphs[1].vertex_domain_id = monolis_alloc_I_1d(nodal_graphs[1].vertex_domain_id, 5);
  nodal_graphs[2].vertex_domain_id = monolis_alloc_I_1d(nodal_graphs[2].vertex_domain_id, 6);
  nodal_graphs[0].index = monolis_alloc_I_1d(nodal_graphs[0].index, 6);
  nodal_graphs[1].index = monolis_alloc_I_1d(nodal_graphs[1].index, 6);
  nodal_graphs[2].index = monolis_alloc_I_1d(nodal_graphs[2].index, 7);
  nodal_graphs[0].item = monolis_alloc_I_1d(nodal_graphs[0].item, 14);
  nodal_graphs[1].item = monolis_alloc_I_1d(nodal_graphs[1].item, 14);
  nodal_graphs[2].item = monolis_alloc_I_1d(nodal_graphs[2].item, 18);

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

  nodal_graphs[0].index[0] = 0;
  nodal_graphs[0].index[1] = 2;
  nodal_graphs[0].index[2] = 6;
  nodal_graphs[0].index[3] = 9;
  nodal_graphs[0].index[4] = 12;
  nodal_graphs[0].index[5] = 14;
  nodal_graphs[1].index[0] = 0;
  nodal_graphs[1].index[1] = 4;
  nodal_graphs[1].index[2] = 6;
  nodal_graphs[1].index[3] = 8;
  nodal_graphs[1].index[4] = 11;
  nodal_graphs[1].index[5] = 14;
  nodal_graphs[2].index[0] = 0;
  nodal_graphs[2].index[1] = 5;
  nodal_graphs[2].index[2] = 8;
  nodal_graphs[2].index[3] = 10;
  nodal_graphs[2].index[4] = 13;
  nodal_graphs[2].index[5] = 16;
  nodal_graphs[2].index[6] = 18;

  nodal_graphs[0].item[0] = 1;
  nodal_graphs[0].item[1] = 2;
  nodal_graphs[0].item[2] = 0;
  nodal_graphs[0].item[3] = 2;
  nodal_graphs[0].item[4] = 3;
  nodal_graphs[0].item[5] = 4;
  nodal_graphs[0].item[6] = 0;
  nodal_graphs[0].item[7] = 1;
  nodal_graphs[0].item[8] = 3;
  nodal_graphs[0].item[9] = 1;
  nodal_graphs[0].item[10] = 2;
  nodal_graphs[0].item[11] = 4;
  nodal_graphs[0].item[12] = 1;
  nodal_graphs[0].item[13] = 3;
  nodal_graphs[1].item[0] = 1;
  nodal_graphs[1].item[1] = 2;
  nodal_graphs[1].item[2] = 3;
  nodal_graphs[1].item[3] = 4;
  nodal_graphs[1].item[4] = 0;
  nodal_graphs[1].item[5] = 4;
  nodal_graphs[1].item[6] = 0;
  nodal_graphs[1].item[7] = 3;
  nodal_graphs[1].item[8] = 0;
  nodal_graphs[1].item[9] = 2;
  nodal_graphs[1].item[10] = 4;
  nodal_graphs[1].item[11] = 0;
  nodal_graphs[1].item[12] = 1;
  nodal_graphs[1].item[13] = 3;
  nodal_graphs[2].item[0] = 1;
  nodal_graphs[2].item[1] = 2;
  nodal_graphs[2].item[2] = 3;
  nodal_graphs[2].item[3] = 4;
  nodal_graphs[2].item[4] = 5;
  nodal_graphs[2].item[5] = 0;
  nodal_graphs[2].item[6] = 2;
  nodal_graphs[2].item[7] = 3;
  nodal_graphs[2].item[8] = 0;
  nodal_graphs[2].item[9] = 1;
  nodal_graphs[2].item[10] = 0;
  nodal_graphs[2].item[11] = 1;
  nodal_graphs[2].item[12] = 4;
  nodal_graphs[2].item[13] = 0;
  nodal_graphs[2].item[14] = 3;
  nodal_graphs[2].item[15] = 5;
  nodal_graphs[2].item[16] = 0;
  nodal_graphs[2].item[17] = 4;

  // 結合後の計算点グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  merged_nodal_graph.n_vertex = 7;
  merged_nodal_graph.n_internal_vertex = 5;

  merged_nodal_graph.vertex_id = monolis_alloc_I_1d(merged_nodal_graph.vertex_id, 7);
  merged_nodal_graph.vertex_domain_id = monolis_alloc_I_1d(merged_nodal_graph.vertex_domain_id, 7);
  merged_nodal_graph.index = monolis_alloc_I_1d(merged_nodal_graph.index, 8);
  merged_nodal_graph.item = monolis_alloc_I_1d(merged_nodal_graph.item, 22);

  merged_nodal_graph.vertex_id[0] = 1;
  merged_nodal_graph.vertex_id[1] = 2;
  merged_nodal_graph.vertex_id[2] = 3;
  merged_nodal_graph.vertex_id[3] = 4;
  merged_nodal_graph.vertex_id[4] = 5;
  merged_nodal_graph.vertex_id[5] = 6;
  merged_nodal_graph.vertex_id[6] = 7;

  merged_nodal_graph.index[0] = 0;
  merged_nodal_graph.index[1] = 2;
  merged_nodal_graph.index[2] = 6;
  merged_nodal_graph.index[3] = 8;
  merged_nodal_graph.index[4] = 12;
  merged_nodal_graph.index[5] = 17;
  merged_nodal_graph.index[6] = 20;
  merged_nodal_graph.index[7] = 22;

  merged_nodal_graph.item[0] = 2;
  merged_nodal_graph.item[1] = 4;
  merged_nodal_graph.item[2] = 1;
  merged_nodal_graph.item[3] = 3;
  merged_nodal_graph.item[4] = 4;
  merged_nodal_graph.item[5] = 5;
  merged_nodal_graph.item[6] = 2;
  merged_nodal_graph.item[7] = 5;
  merged_nodal_graph.item[8] = 1;
  merged_nodal_graph.item[9] = 2;
  merged_nodal_graph.item[10] = 5;
  merged_nodal_graph.item[11] = 6;
  merged_nodal_graph.item[12] = 2;
  merged_nodal_graph.item[13] = 3;
  merged_nodal_graph.item[14] = 4;
  merged_nodal_graph.item[15] = 6;
  merged_nodal_graph.item[16] = 7;
  merged_nodal_graph.item[17] = 4;
  merged_nodal_graph.item[18] = 5;
  merged_nodal_graph.item[19] = 7;
  merged_nodal_graph.item[20] = 5;
  merged_nodal_graph.item[21] = 6;

  // 結合前のコネクティビティグラフ
  conn_graphs[0].n_vertex = 3;
  conn_graphs[1].n_vertex = 3;
  conn_graphs[2].n_vertex = 4;
  conn_graphs[0].n_internal_vertex = 3;
  conn_graphs[1].n_internal_vertex = 1;
  conn_graphs[2].n_internal_vertex = 1;

  conn_graphs[0].vertex_id = monolis_alloc_I_1d(conn_graphs[0].vertex_id, 3);
  conn_graphs[1].vertex_id = monolis_alloc_I_1d(conn_graphs[1].vertex_id, 3);
  conn_graphs[2].vertex_id = monolis_alloc_I_1d(conn_graphs[2].vertex_id, 4);
  conn_graphs[0].vertex_domain_id = monolis_alloc_I_1d(conn_graphs[0].vertex_domain_id, 3);
  conn_graphs[1].vertex_domain_id = monolis_alloc_I_1d(conn_graphs[1].vertex_domain_id, 3);
  conn_graphs[2].vertex_domain_id = monolis_alloc_I_1d(conn_graphs[2].vertex_domain_id, 4);
  conn_graphs[0].index = monolis_alloc_I_1d(conn_graphs[0].index, 4);
  conn_graphs[1].index = monolis_alloc_I_1d(conn_graphs[1].index, 4);
  conn_graphs[2].index = monolis_alloc_I_1d(conn_graphs[2].index, 5);
  conn_graphs[0].item = monolis_alloc_I_1d(conn_graphs[0].item, 9);
  conn_graphs[1].item = monolis_alloc_I_1d(conn_graphs[1].item, 9);
  conn_graphs[2].item = monolis_alloc_I_1d(conn_graphs[2].item, 12);

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

  conn_graphs[0].index[0] = 0;
  conn_graphs[0].index[1] = 3;
  conn_graphs[0].index[2] = 6;
  conn_graphs[0].index[3] = 9;
  conn_graphs[1].index[0] = 0;
  conn_graphs[1].index[1] = 3;
  conn_graphs[1].index[2] = 6;
  conn_graphs[1].index[3] = 9;
  conn_graphs[2].index[0] = 0;
  conn_graphs[2].index[1] = 3;
  conn_graphs[2].index[2] = 6;
  conn_graphs[2].index[3] = 9;
  conn_graphs[2].index[4] = 12;

  conn_graphs[0].item[0] = 1;
  conn_graphs[0].item[1] = 2;
  conn_graphs[0].item[2] = 4;
  conn_graphs[0].item[3] = 2;
  conn_graphs[0].item[4] = 5;
  conn_graphs[0].item[5] = 4;
  conn_graphs[0].item[6] = 4;
  conn_graphs[0].item[7] = 5;
  conn_graphs[0].item[8] = 6;
  conn_graphs[1].item[0] = 2;
  conn_graphs[1].item[1] = 3;
  conn_graphs[1].item[2] = 5;
  conn_graphs[1].item[3] = 1;
  conn_graphs[1].item[4] = 2;
  conn_graphs[1].item[5] = 4;
  conn_graphs[1].item[6] = 2;
  conn_graphs[1].item[7] = 5;
  conn_graphs[1].item[8] = 4;
  conn_graphs[2].item[0] = 5;
  conn_graphs[2].item[1] = 6;
  conn_graphs[2].item[2] = 7;
  conn_graphs[2].item[3] = 2;
  conn_graphs[2].item[4] = 3;
  conn_graphs[2].item[5] = 5;
  conn_graphs[2].item[6] = 2;
  conn_graphs[2].item[7] = 5;
  conn_graphs[2].item[8] = 4;
  conn_graphs[2].item[9] = 4;
  conn_graphs[2].item[10] = 5;
  conn_graphs[2].item[11] = 6;

  // 結合後の通信テーブル // TODO あってる？
  monolis_com_initialize_by_self(&merged_nodal_monoCOM);

  // 結合後のコネクティビティグラフの模範解答 (ORDER_NODAL_IDで計算点グラフを結合)
  check_vertex_id = monolis_alloc_I_1d(check_vertex_id, 5);
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

  check_item[0] = 1;
  check_item[1] = 2;
  check_item[2] = 4;
  check_item[3] = 2;
  check_item[4] = 5;
  check_item[5] = 4;
  check_item[6] = 2;
  check_item[7] = 3;
  check_item[8] = 5;
  check_item[9] = 4;
  check_item[10] = 5;
  check_item[11] = 6;
  check_item[12] = 5;
  check_item[13] = 6;
  check_item[14] = 7;

  // 結合

  // 確認

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
  // gedatsu_graph_finalize(&merged_conn_graph);
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
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_R list_struct_R[3];
  int* merged_n_dof_list;
  double* merged_array_R;

  monolis_std_log_string("gedatsu_merge_distval_R_c_test");

  n_graphs = 3;

  graphs[0].n_vertex = 5;
  graphs[1].n_vertex = 5;
  graphs[2].n_vertex = 6;
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id = monolis_alloc_I_1d(graphs[0].vertex_id, 5);
  graphs[1].vertex_id = monolis_alloc_I_1d(graphs[1].vertex_id, 5);
  graphs[2].vertex_id = monolis_alloc_I_1d(graphs[2].vertex_id, 6);
  graphs[0].vertex_domain_id = monolis_alloc_I_1d(graphs[0].vertex_domain_id, 5);
  graphs[1].vertex_domain_id = monolis_alloc_I_1d(graphs[1].vertex_domain_id, 5);
  graphs[2].vertex_domain_id = monolis_alloc_I_1d(graphs[2].vertex_domain_id, 6);
  graphs[0].index = monolis_alloc_I_1d(graphs[0].index, 6);
  graphs[1].index = monolis_alloc_I_1d(graphs[1].index, 6);
  graphs[2].index = monolis_alloc_I_1d(graphs[2].index, 7);
  graphs[0].item = monolis_alloc_I_1d(graphs[0].item, 14);
  graphs[1].item = monolis_alloc_I_1d(graphs[1].item, 14);
  graphs[2].item = monolis_alloc_I_1d(graphs[2].item, 18);

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

  graphs[0].index[0] = 0;
  graphs[0].index[1] = 2;
  graphs[0].index[2] = 6;
  graphs[0].index[3] = 9;
  graphs[0].index[4] = 12;
  graphs[0].index[5] = 14;
  graphs[1].index[0] = 0;
  graphs[1].index[1] = 4;
  graphs[1].index[2] = 6;
  graphs[1].index[3] = 8;
  graphs[1].index[4] = 11;
  graphs[1].index[5] = 14;
  graphs[2].index[0] = 0;
  graphs[2].index[1] = 5;
  graphs[2].index[2] = 8;
  graphs[2].index[3] = 10;
  graphs[2].index[4] = 13;
  graphs[2].index[5] = 16;
  graphs[2].index[6] = 18;

  graphs[0].item[0] = 1;
  graphs[0].item[1] = 2;
  graphs[0].item[2] = 0;
  graphs[0].item[3] = 2;
  graphs[0].item[4] = 3;
  graphs[0].item[5] = 4;
  graphs[0].item[6] = 0;
  graphs[0].item[7] = 1;
  graphs[0].item[8] = 3;
  graphs[0].item[9] = 1;
  graphs[0].item[10] = 2;
  graphs[0].item[11] = 4;
  graphs[0].item[12] = 1;
  graphs[0].item[13] = 3;
  graphs[1].item[0] = 1;
  graphs[1].item[1] = 2;
  graphs[1].item[2] = 3;
  graphs[1].item[3] = 4;
  graphs[1].item[4] = 0;
  graphs[1].item[5] = 4;
  graphs[1].item[6] = 0;
  graphs[1].item[7] = 3;
  graphs[1].item[8] = 0;
  graphs[1].item[9] = 2;
  graphs[1].item[10] = 4;
  graphs[1].item[11] = 0;
  graphs[1].item[12] = 1;
  graphs[1].item[13] = 3;
  graphs[2].item[0] = 1;
  graphs[2].item[1] = 2;
  graphs[2].item[2] = 3;
  graphs[2].item[3] = 4;
  graphs[2].item[4] = 5;
  graphs[2].item[5] = 0;
  graphs[2].item[6] = 2;
  graphs[2].item[7] = 3;
  graphs[2].item[8] = 0;
  graphs[2].item[9] = 1;
  graphs[2].item[10] = 0;
  graphs[2].item[11] = 1;
  graphs[2].item[12] = 4;
  graphs[2].item[13] = 0;
  graphs[2].item[14] = 3;
  graphs[2].item[15] = 5;
  graphs[2].item[16] = 0;
  graphs[2].item[17] = 4;

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  merged_graph.n_vertex = 7;
  merged_graph.n_internal_vertex = 5;

  merged_graph.vertex_id = monolis_alloc_I_1d(merged_graph.vertex_id, 7);
  merged_graph.vertex_domain_id = monolis_alloc_I_1d(merged_graph.vertex_domain_id, 7);
  merged_graph.index = monolis_alloc_I_1d(merged_graph.index, 8);
  merged_graph.item = monolis_alloc_I_1d(merged_graph.item, 22);

  merged_graph.vertex_id[0] = 1;
  merged_graph.vertex_id[1] = 2;
  merged_graph.vertex_id[2] = 3;
  merged_graph.vertex_id[3] = 4;
  merged_graph.vertex_id[4] = 5;
  merged_graph.vertex_id[5] = 6;
  merged_graph.vertex_id[6] = 7;

  merged_graph.index[0] = 0;
  merged_graph.index[1] = 2;
  merged_graph.index[2] = 6;
  merged_graph.index[3] = 8;
  merged_graph.index[4] = 12;
  merged_graph.index[5] = 17;
  merged_graph.index[6] = 20;
  merged_graph.index[7] = 22;

  merged_graph.item[0] = 2;
  merged_graph.item[1] = 4;
  merged_graph.item[2] = 1;
  merged_graph.item[3] = 3;
  merged_graph.item[4] = 4;
  merged_graph.item[5] = 5;
  merged_graph.item[6] = 2;
  merged_graph.item[7] = 5;
  merged_graph.item[8] = 1;
  merged_graph.item[9] = 2;
  merged_graph.item[10] = 5;
  merged_graph.item[11] = 6;
  merged_graph.item[12] = 2;
  merged_graph.item[13] = 3;
  merged_graph.item[14] = 4;
  merged_graph.item[15] = 6;
  merged_graph.item[16] = 7;
  merged_graph.item[17] = 4;
  merged_graph.item[18] = 5;
  merged_graph.item[19] = 7;
  merged_graph.item[20] = 5;
  merged_graph.item[21] = 6;

  // 結合前の物理量分布
  for (int i = 0; i < n_graphs; i++)
  {
    n_dof_list[i].array = NULL;
    list_struct_R[i].array = NULL;
  }
  gedatsu_list_initialize_I(n_dof_list, n_graphs);
  gedatsu_list_initialize_R(list_struct_R, n_graphs);
  n_dof_list[0].n = 5;
  n_dof_list[1].n = 5;
  n_dof_list[2].n = 6;
  n_dof_list[0].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[1].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[2].array = monolis_alloc_I_1d(n_dof_list[0].array, 6);
  n_dof_list[0].array[0] = 1;
  n_dof_list[0].array[1] = 1;
  n_dof_list[0].array[2] = 1;
  n_dof_list[0].array[3] = 1;
  n_dof_list[0].array[4] = 1;
  n_dof_list[1].array[0] = 1;
  n_dof_list[1].array[1] = 1;
  n_dof_list[1].array[2] = 1;
  n_dof_list[1].array[3] = 1;
  n_dof_list[1].array[4] = 1;
  n_dof_list[2].array[0] = 1;
  n_dof_list[2].array[1] = 1;
  n_dof_list[2].array[2] = 1;
  n_dof_list[2].array[3] = 1;
  n_dof_list[2].array[4] = 1;
  n_dof_list[2].array[5] = 1;
  list_struct_R[0].n = 5;
  list_struct_R[1].n = 5;
  list_struct_R[2].n = 6;
  list_struct_R[0].array = monolis_alloc_R_1d(list_struct_R[0].array, 5);
  list_struct_R[1].array = monolis_alloc_R_1d(list_struct_R[0].array, 5);
  list_struct_R[2].array = monolis_alloc_R_1d(list_struct_R[0].array, 6);
  list_struct_R[0].array[0] = 1.0;
  list_struct_R[0].array[1] = 4.0;
  list_struct_R[0].array[2] = 2.0;
  list_struct_R[0].array[3] = 5.0;
  list_struct_R[0].array[4] = 6.0;
  list_struct_R[1].array[0] = 2.0;
  list_struct_R[1].array[1] = 3.0;
  list_struct_R[1].array[2] = 1.0;
  list_struct_R[1].array[3] = 4.0;
  list_struct_R[1].array[4] = 5.0;
  list_struct_R[2].array[0] = 5.0;
  list_struct_R[2].array[1] = 2.0;
  list_struct_R[2].array[2] = 3.0;
  list_struct_R[2].array[3] = 4.0;
  list_struct_R[2].array[4] = 6.0;
  list_struct_R[2].array[5] = 7.0;

  // 結合
  gedatsu_merge_distval_R(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_R,
  &merged_n_dof_list, &merged_array_R);

  // 結合後の物理量分布の確認
  for (int i = 0; i < 7; i++)
  {
    monolis_test_check_eq_I1("gedatsu_graph_merge_test_c dist_val_R n_dof_list", merged_n_dof_list[i], 1);
    monolis_test_check_eq_R1("gedatsu_graph_merge_test_c dist_val_R array", merged_array_R[i], (double)i+1);
  }

  // free
  gedatsu_list_finalize_I(n_dof_list, 1);
  gedatsu_list_finalize_R(list_struct_R, 1);
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
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_I list_struct_I[3];
  int* merged_n_dof_list = NULL;
  int* merged_array_I = NULL;

  monolis_std_log_string("gedatsu_merge_distval_I_c_test");

  n_graphs = 3;

  graphs[0].n_vertex = 5;
  graphs[1].n_vertex = 5;
  graphs[2].n_vertex = 6;
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id = (int*)calloc(5, sizeof(int));
  graphs[1].vertex_id = (int*)calloc(5, sizeof(int));
  graphs[2].vertex_id = (int*)calloc(6, sizeof(int));
  graphs[0].vertex_domain_id = (int*)calloc(5, sizeof(int));
  graphs[1].vertex_domain_id = (int*)calloc(5, sizeof(int));
  graphs[2].vertex_domain_id = (int*)calloc(6, sizeof(int));
  graphs[0].index = (int*)calloc(6, sizeof(int));
  graphs[1].index = (int*)calloc(6, sizeof(int));
  graphs[2].index = (int*)calloc(7, sizeof(int));
  graphs[0].item = (int*)calloc(14, sizeof(int));
  graphs[1].item = (int*)calloc(14, sizeof(int));
  graphs[2].item = (int*)calloc(18, sizeof(int));

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

  graphs[0].index[0] = 0;
  graphs[0].index[1] = 2;
  graphs[0].index[2] = 6;
  graphs[0].index[3] = 9;
  graphs[0].index[4] = 12;
  graphs[0].index[5] = 14;
  graphs[1].index[0] = 0;
  graphs[1].index[1] = 4;
  graphs[1].index[2] = 6;
  graphs[1].index[3] = 8;
  graphs[1].index[4] = 11;
  graphs[1].index[5] = 14;
  graphs[2].index[0] = 0;
  graphs[2].index[1] = 5;
  graphs[2].index[2] = 8;
  graphs[2].index[3] = 10;
  graphs[2].index[4] = 13;
  graphs[2].index[5] = 16;
  graphs[2].index[6] = 18;

  graphs[0].item[0] = 1;
  graphs[0].item[1] = 2;
  graphs[0].item[2] = 0;
  graphs[0].item[3] = 2;
  graphs[0].item[4] = 3;
  graphs[0].item[5] = 4;
  graphs[0].item[6] = 0;
  graphs[0].item[7] = 1;
  graphs[0].item[8] = 3;
  graphs[0].item[9] = 1;
  graphs[0].item[10] = 2;
  graphs[0].item[11] = 4;
  graphs[0].item[12] = 1;
  graphs[0].item[13] = 3;
  graphs[1].item[0] = 1;
  graphs[1].item[1] = 2;
  graphs[1].item[2] = 3;
  graphs[1].item[3] = 4;
  graphs[1].item[4] = 0;
  graphs[1].item[5] = 4;
  graphs[1].item[6] = 0;
  graphs[1].item[7] = 3;
  graphs[1].item[8] = 0;
  graphs[1].item[9] = 2;
  graphs[1].item[10] = 4;
  graphs[1].item[11] = 0;
  graphs[1].item[12] = 1;
  graphs[1].item[13] = 3;
  graphs[2].item[0] = 1;
  graphs[2].item[1] = 2;
  graphs[2].item[2] = 3;
  graphs[2].item[3] = 4;
  graphs[2].item[4] = 5;
  graphs[2].item[5] = 0;
  graphs[2].item[6] = 2;
  graphs[2].item[7] = 3;
  graphs[2].item[8] = 0;
  graphs[2].item[9] = 1;
  graphs[2].item[10] = 0;
  graphs[2].item[11] = 1;
  graphs[2].item[12] = 4;
  graphs[2].item[13] = 0;
  graphs[2].item[14] = 3;
  graphs[2].item[15] = 5;
  graphs[2].item[16] = 0;
  graphs[2].item[17] = 4;

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  merged_graph.n_vertex = 7;
  merged_graph.n_internal_vertex = 5;

  merged_graph.vertex_id = (int*)calloc(7, sizeof(int));
  merged_graph.vertex_domain_id = (int*)calloc(7, sizeof(int));
  merged_graph.index = (int*)calloc(8, sizeof(int));
  merged_graph.item = (int*)calloc(22, sizeof(int));

  merged_graph.vertex_id[0] = 1;
  merged_graph.vertex_id[1] = 2;
  merged_graph.vertex_id[2] = 3;
  merged_graph.vertex_id[3] = 4;
  merged_graph.vertex_id[4] = 5;
  merged_graph.vertex_id[5] = 6;
  merged_graph.vertex_id[6] = 7;

  merged_graph.index[0] = 0;
  merged_graph.index[1] = 2;
  merged_graph.index[2] = 6;
  merged_graph.index[3] = 8;
  merged_graph.index[4] = 12;
  merged_graph.index[5] = 17;
  merged_graph.index[6] = 20;
  merged_graph.index[7] = 22;

  merged_graph.item[0] = 2;
  merged_graph.item[1] = 4;
  merged_graph.item[2] = 1;
  merged_graph.item[3] = 3;
  merged_graph.item[4] = 4;
  merged_graph.item[5] = 5;
  merged_graph.item[6] = 2;
  merged_graph.item[7] = 5;
  merged_graph.item[8] = 1;
  merged_graph.item[9] = 2;
  merged_graph.item[10] = 5;
  merged_graph.item[11] = 6;
  merged_graph.item[12] = 2;
  merged_graph.item[13] = 3;
  merged_graph.item[14] = 4;
  merged_graph.item[15] = 6;
  merged_graph.item[16] = 7;
  merged_graph.item[17] = 4;
  merged_graph.item[18] = 5;
  merged_graph.item[19] = 7;
  merged_graph.item[20] = 5;
  merged_graph.item[21] = 6;

  // 結合前の物理量分布
  for (int i = 0; i < n_graphs; i++)
  {
    n_dof_list[i].array = NULL;
    list_struct_I[i].array = NULL;
  }
  gedatsu_list_initialize_I(n_dof_list, n_graphs);
  gedatsu_list_initialize_I(list_struct_I, n_graphs);
  n_dof_list[0].n = 5;
  n_dof_list[1].n = 5;
  n_dof_list[2].n = 6;
  n_dof_list[0].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[1].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[2].array = monolis_alloc_I_1d(n_dof_list[0].array, 6);
  n_dof_list[0].array[0] = 1;
  n_dof_list[0].array[1] = 1;
  n_dof_list[0].array[2] = 1;
  n_dof_list[0].array[3] = 1;
  n_dof_list[0].array[4] = 1;
  n_dof_list[1].array[0] = 1;
  n_dof_list[1].array[1] = 1;
  n_dof_list[1].array[2] = 1;
  n_dof_list[1].array[3] = 1;
  n_dof_list[1].array[4] = 1;
  n_dof_list[2].array[0] = 1;
  n_dof_list[2].array[1] = 1;
  n_dof_list[2].array[2] = 1;
  n_dof_list[2].array[3] = 1;
  n_dof_list[2].array[4] = 1;
  n_dof_list[2].array[5] = 1;
  list_struct_I[0].n = 5;
  list_struct_I[1].n = 5;
  list_struct_I[2].n = 6;
  list_struct_I[0].array = monolis_alloc_I_1d(list_struct_I[0].array, 5);
  list_struct_I[1].array = monolis_alloc_I_1d(list_struct_I[0].array, 5);
  list_struct_I[2].array = monolis_alloc_I_1d(list_struct_I[0].array, 6);
  list_struct_I[0].array[0] = 1;
  list_struct_I[0].array[1] = 4;
  list_struct_I[0].array[2] = 2;
  list_struct_I[0].array[3] = 5;
  list_struct_I[0].array[4] = 6;
  list_struct_I[1].array[0] = 2;
  list_struct_I[1].array[1] = 3;
  list_struct_I[1].array[2] = 1;
  list_struct_I[1].array[3] = 4;
  list_struct_I[1].array[4] = 5;
  list_struct_I[2].array[0] = 5;
  list_struct_I[2].array[1] = 2;
  list_struct_I[2].array[2] = 3;
  list_struct_I[2].array[3] = 4;
  list_struct_I[2].array[4] = 6;
  list_struct_I[2].array[5] = 7;

  // 結合
  gedatsu_merge_distval_I(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_I,
  &merged_n_dof_list, &merged_array_I);

  // 結合後の物理量分布の確認
  for (int i = 0; i < 7; i++)
  {
    monolis_test_check_eq_I1("gedatsu_graph_merge_test_c dist_val_I n_dof_list", merged_n_dof_list[i], 1);
    monolis_test_check_eq_R1("gedatsu_graph_merge_test_c dist_val_I array", merged_array_I[i], i+1);
  }

  // free
  gedatsu_list_finalize_I(n_dof_list, 1);
  gedatsu_list_finalize_I(list_struct_I, 1);
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
  MONOLIS_LIST_I n_dof_list[3];
  MONOLIS_LIST_C list_struct_C[3];
  int* merged_n_dof_list = NULL;
  double complex* merged_array_C = NULL;

  monolis_std_log_string("gedatsu_merge_distval_C_c_test");

  n_graphs = 3;

  graphs[0].n_vertex = 5;
  graphs[1].n_vertex = 5;
  graphs[2].n_vertex = 6;
  graphs[0].n_internal_vertex = 2;
  graphs[1].n_internal_vertex = 2;
  graphs[2].n_internal_vertex = 1;

  graphs[0].vertex_id = (int*)calloc(5, sizeof(int));
  graphs[1].vertex_id = (int*)calloc(5, sizeof(int));
  graphs[2].vertex_id = (int*)calloc(6, sizeof(int));
  graphs[0].vertex_domain_id = (int*)calloc(5, sizeof(int));
  graphs[1].vertex_domain_id = (int*)calloc(5, sizeof(int));
  graphs[2].vertex_domain_id = (int*)calloc(6, sizeof(int));
  graphs[0].index = (int*)calloc(6, sizeof(int));
  graphs[1].index = (int*)calloc(6, sizeof(int));
  graphs[2].index = (int*)calloc(7, sizeof(int));
  graphs[0].item = (int*)calloc(14, sizeof(int));
  graphs[1].item = (int*)calloc(14, sizeof(int));
  graphs[2].item = (int*)calloc(18, sizeof(int));

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

  graphs[0].index[0] = 0;
  graphs[0].index[1] = 2;
  graphs[0].index[2] = 6;
  graphs[0].index[3] = 9;
  graphs[0].index[4] = 12;
  graphs[0].index[5] = 14;
  graphs[1].index[0] = 0;
  graphs[1].index[1] = 4;
  graphs[1].index[2] = 6;
  graphs[1].index[3] = 8;
  graphs[1].index[4] = 11;
  graphs[1].index[5] = 14;
  graphs[2].index[0] = 0;
  graphs[2].index[1] = 5;
  graphs[2].index[2] = 8;
  graphs[2].index[3] = 10;
  graphs[2].index[4] = 13;
  graphs[2].index[5] = 16;
  graphs[2].index[6] = 18;

  graphs[0].item[0] = 1;
  graphs[0].item[1] = 2;
  graphs[0].item[2] = 0;
  graphs[0].item[3] = 2;
  graphs[0].item[4] = 3;
  graphs[0].item[5] = 4;
  graphs[0].item[6] = 0;
  graphs[0].item[7] = 1;
  graphs[0].item[8] = 3;
  graphs[0].item[9] = 1;
  graphs[0].item[10] = 2;
  graphs[0].item[11] = 4;
  graphs[0].item[12] = 1;
  graphs[0].item[13] = 3;
  graphs[1].item[0] = 1;
  graphs[1].item[1] = 2;
  graphs[1].item[2] = 3;
  graphs[1].item[3] = 4;
  graphs[1].item[4] = 0;
  graphs[1].item[5] = 4;
  graphs[1].item[6] = 0;
  graphs[1].item[7] = 3;
  graphs[1].item[8] = 0;
  graphs[1].item[9] = 2;
  graphs[1].item[10] = 4;
  graphs[1].item[11] = 0;
  graphs[1].item[12] = 1;
  graphs[1].item[13] = 3;
  graphs[2].item[0] = 1;
  graphs[2].item[1] = 2;
  graphs[2].item[2] = 3;
  graphs[2].item[3] = 4;
  graphs[2].item[4] = 5;
  graphs[2].item[5] = 0;
  graphs[2].item[6] = 2;
  graphs[2].item[7] = 3;
  graphs[2].item[8] = 0;
  graphs[2].item[9] = 1;
  graphs[2].item[10] = 0;
  graphs[2].item[11] = 1;
  graphs[2].item[12] = 4;
  graphs[2].item[13] = 0;
  graphs[2].item[14] = 3;
  graphs[2].item[15] = 5;
  graphs[2].item[16] = 0;
  graphs[2].item[17] = 4;

  // 結合後グラフ (ORDER_NODAL_IDで計算点グラフを結合)
  merged_graph.n_vertex = 7;
  merged_graph.n_internal_vertex = 5;

  merged_graph.vertex_id = (int*)calloc(7, sizeof(int));
  merged_graph.vertex_domain_id = (int*)calloc(7, sizeof(int));
  merged_graph.index = (int*)calloc(8, sizeof(int));
  merged_graph.item = (int*)calloc(22, sizeof(int));

  merged_graph.vertex_id[0] = 1;
  merged_graph.vertex_id[1] = 2;
  merged_graph.vertex_id[2] = 3;
  merged_graph.vertex_id[3] = 4;
  merged_graph.vertex_id[4] = 5;
  merged_graph.vertex_id[5] = 6;
  merged_graph.vertex_id[6] = 7;

  merged_graph.index[0] = 0;
  merged_graph.index[1] = 2;
  merged_graph.index[2] = 6;
  merged_graph.index[3] = 8;
  merged_graph.index[4] = 12;
  merged_graph.index[5] = 17;
  merged_graph.index[6] = 20;
  merged_graph.index[7] = 22;

  merged_graph.item[0] = 2;
  merged_graph.item[1] = 4;
  merged_graph.item[2] = 1;
  merged_graph.item[3] = 3;
  merged_graph.item[4] = 4;
  merged_graph.item[5] = 5;
  merged_graph.item[6] = 2;
  merged_graph.item[7] = 5;
  merged_graph.item[8] = 1;
  merged_graph.item[9] = 2;
  merged_graph.item[10] = 5;
  merged_graph.item[11] = 6;
  merged_graph.item[12] = 2;
  merged_graph.item[13] = 3;
  merged_graph.item[14] = 4;
  merged_graph.item[15] = 6;
  merged_graph.item[16] = 7;
  merged_graph.item[17] = 4;
  merged_graph.item[18] = 5;
  merged_graph.item[19] = 7;
  merged_graph.item[20] = 5;
  merged_graph.item[21] = 6;

  // 結合前の物理量分布
  for (int i = 0; i < n_graphs; i++)
  {
    n_dof_list[i].array = NULL;
    list_struct_C[i].array = NULL;
  }
  gedatsu_list_initialize_I(n_dof_list, n_graphs);
  gedatsu_list_initialize_C(list_struct_C, n_graphs);
  n_dof_list[0].n = 5;
  n_dof_list[1].n = 5;
  n_dof_list[2].n = 6;
  n_dof_list[0].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[1].array = monolis_alloc_I_1d(n_dof_list[0].array, 5);
  n_dof_list[2].array = monolis_alloc_I_1d(n_dof_list[0].array, 6);
  n_dof_list[0].array[0] = 1;
  n_dof_list[0].array[1] = 1;
  n_dof_list[0].array[2] = 1;
  n_dof_list[0].array[3] = 1;
  n_dof_list[0].array[4] = 1;
  n_dof_list[1].array[0] = 1;
  n_dof_list[1].array[1] = 1;
  n_dof_list[1].array[2] = 1;
  n_dof_list[1].array[3] = 1;
  n_dof_list[1].array[4] = 1;
  n_dof_list[2].array[0] = 1;
  n_dof_list[2].array[1] = 1;
  n_dof_list[2].array[2] = 1;
  n_dof_list[2].array[3] = 1;
  n_dof_list[2].array[4] = 1;
  n_dof_list[2].array[5] = 1;
  list_struct_C[0].n = 5;
  list_struct_C[1].n = 5;
  list_struct_C[2].n = 6;
  list_struct_C[0].array = monolis_alloc_C_1d(list_struct_C[0].array, 5);
  list_struct_C[1].array = monolis_alloc_C_1d(list_struct_C[0].array, 5);
  list_struct_C[2].array = monolis_alloc_C_1d(list_struct_C[0].array, 6);
  list_struct_C[0].array[0] = 1.0 + 1.0*I;
  list_struct_C[0].array[1] = 4.0 + 1.0*I;
  list_struct_C[0].array[2] = 2.0 + 1.0*I;
  list_struct_C[0].array[3] = 5.0 + 1.0*I;
  list_struct_C[0].array[4] = 6.0 + 1.0*I;
  list_struct_C[1].array[0] = 2.0 + 1.0*I;
  list_struct_C[1].array[1] = 3.0 + 1.0*I;
  list_struct_C[1].array[2] = 1.0 + 1.0*I;
  list_struct_C[1].array[3] = 4.0 + 1.0*I;
  list_struct_C[1].array[4] = 5.0 + 1.0*I;
  list_struct_C[2].array[0] = 5.0 + 1.0*I;
  list_struct_C[2].array[1] = 2.0 + 1.0*I;
  list_struct_C[2].array[2] = 3.0 + 1.0*I;
  list_struct_C[2].array[3] = 4.0 + 1.0*I;
  list_struct_C[2].array[4] = 6.0 + 1.0*I;
  list_struct_C[2].array[5] = 7.0 + 1.0*I;

  // 結合
  gedatsu_merge_distval_C(n_graphs, graphs, &merged_graph, n_dof_list, list_struct_C,
  &merged_n_dof_list, &merged_array_C);

  // 結合後の物理量分布の確認
  for (int i = 0; i < 7; i++)
  {
    monolis_test_check_eq_I1("gedatsu_graph_merge_test_c dist_val_C n_dof_list", merged_n_dof_list[i], 1);
  }
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[0], 1.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[1], 2.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[2], 3.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[3], 4.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[4], 5.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[5], 6.0 + 1.0*I);
  monolis_test_check_eq_C1("gedatsu_graph_merge_test_c dist_val_C array", merged_array_C[6], 7.0 + 1.0*I);

  // free
  gedatsu_list_finalize_I(n_dof_list, 1);
  gedatsu_list_finalize_C(list_struct_C, 1);
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
