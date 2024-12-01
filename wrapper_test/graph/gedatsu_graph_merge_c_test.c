#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "gedatsu.h"
#include "monolis_utils.h"
#include "gedatsu_graph_merge_c_test.h"

void gedatsu_graph_merge_c_test()
{
  gedatsu_list_initialize_R_test();
  gedatsu_list_initialize_I_test();
  gedatsu_list_initialize_C_test();
  gedatsu_list_finalize_R_test();
  gedatsu_list_finalize_I_test();
  gedatsu_list_finalize_C_test();
  gedatsu_list_set_R_test();
  gedatsu_list_set_I_test();
  gedatsu_list_set_C_test();
  gedatsu_list_get_R_test();
  gedatsu_list_get_I_test();
  gedatsu_list_get_C_test();
  gedatsu_merge_nodal_subgraphs_c_test();
  gedatsu_merge_connectivity_subgraphs_c_test();
  gedatsu_merge_distval_R_c_test();
}

void gedatsu_list_initialize_R_test(){}

void gedatsu_list_initialize_I_test(){}

void gedatsu_list_initialize_C_test(){}

void gedatsu_list_finalize_R_test(){}

void gedatsu_list_finalize_I_test(){}

void gedatsu_list_finalize_C_test(){}

void gedatsu_list_set_R_test(){}

void gedatsu_list_set_I_test(){}

void gedatsu_list_set_C_test(){}

void gedatsu_list_get_R_test(){}

void gedatsu_list_get_I_test(){}

void gedatsu_list_get_C_test(){}

void gedatsu_merge_nodal_subgraphs_c_test()
{
  int n_graphs;
  GEDATSU_GRAPH* graphs;
  GEDATSU_GRAPH merged_graph;
  MONOLIS_COM* monoCOMs;
  MONOLIS_COM merged_monoCOM;
  int **edge, *check_vertex_id, *check_vertex_domain_id, *check_index, *check_item;

  monolis_std_log_string("gedatsu_graph_merge_c_test");

  n_graphs = 3;

  graphs = (GEDATSU_GRAPH*)malloc(n_graphs*sizeof(GEDATSU_GRAPH));
  monoCOMs = (MONOLIS_COM*)malloc(n_graphs*sizeof(MONOLIS_COM));

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

  graphs[0].vertex_id[0] = 0;
  graphs[0].vertex_id[1] = 3;
  graphs[0].vertex_id[2] = 1;
  graphs[0].vertex_id[3] = 4;
  graphs[0].vertex_id[4] = 5;
  graphs[1].vertex_id[0] = 1;
  graphs[1].vertex_id[1] = 2;
  graphs[1].vertex_id[2] = 0;
  graphs[1].vertex_id[3] = 3;
  graphs[1].vertex_id[4] = 4;
  graphs[2].vertex_id[0] = 4;
  graphs[2].vertex_id[1] = 1;
  graphs[2].vertex_id[2] = 2;
  graphs[2].vertex_id[3] = 3;
  graphs[2].vertex_id[4] = 5;
  graphs[2].vertex_id[5] = 6;

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

  for(int i=0; i<3; i++){
    monoCOMs[i].my_rank = 0;
    monoCOMs[i].comm = 0;
    monoCOMs[i].comm_size = 0;
    monoCOMs[i].n_internal_vertex = 0;
    monoCOMs[i].recv_n_neib = 0;
    monoCOMs[i].send_n_neib = 0;
  }

  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_DOMAIN_ID);

  monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_vertex", \
  merged_graph.n_vertex, 7);
  monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_internal_vertex", \
  merged_graph.n_internal_vertex, 5);
  check_vertex_domain_id = (int*)calloc(7, sizeof(int));
  for(int i=0; i<7; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_domain_id", \
    merged_graph.vertex_domain_id[i], check_vertex_domain_id[i]);
  }
  check_vertex_id = (int*)calloc(7,sizeof(int));
  check_vertex_id[0] = 0;
  check_vertex_id[1] = 3;
  check_vertex_id[2] = 1;
  check_vertex_id[3] = 2;
  check_vertex_id[4] = 4;
  check_vertex_id[5] = 5;
  check_vertex_id[6] = 6;
  for(int i=0; i<7; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_id", \
    merged_graph.vertex_id[i], check_vertex_id[i]);
  }
  check_index = (int*)calloc(8, sizeof(int));
  check_index[1] = 0;
  check_index[2] = 2;
  check_index[3] = 6;
  check_index[4] = 10;
  check_index[5] = 12;
  check_index[6] = 17;
  check_index[7] = 20;
  check_index[8] = 22;
  for(int i=0; i<8; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID item", \
    merged_graph.index[i], check_index[i]);
  }
  check_item = (int*)calloc(22, sizeof(int));
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
  for(int i=0; i<22; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID item", \
    merged_graph.item[i], check_item[i]);
  }

  gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, &merged_graph, &merged_monoCOM, ORDER_NODAL_ID);

  monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_vertex", \
  merged_graph.n_vertex, 7);
  monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_internal_vertex", \
  merged_graph.n_internal_vertex, 5);
  for(int i=0; i<7; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_domain_id", \
    merged_graph.vertex_domain_id[i], check_vertex_domain_id[i]);
  }
  check_vertex_id[0] = 0;
  check_vertex_id[1] = 1;
  check_vertex_id[2] = 2;
  check_vertex_id[3] = 3;
  check_vertex_id[4] = 4;
  check_vertex_id[5] = 5;
  check_vertex_id[6] = 6;
  for(int i=0; i<7; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_id", \
    merged_graph.vertex_id[i], check_vertex_id[i]);
  }
  check_index[1] = 0;
  check_index[2] = 2;
  check_index[3] = 6;
  check_index[4] = 8;
  check_index[5] = 12;
  check_index[6] = 17;
  check_index[7] = 20;
  check_index[8] = 22;
  for(int i=0; i<8; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID item", \
    merged_graph.index[i], check_index[i]);
  }
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
  for(int i=0; i<22; i++){
    monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID item", \
    merged_graph.item[i], check_item[i]);
  }

  // TODO mallocで確保した配列をfreeで解放
  free(graphs);
  free(monoCOMs);
}

void gedatsu_merge_connectivity_subgraphs_c_test()
{}

void gedatsu_merge_distval_R_c_test()
{}
