#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "gedatsu.h"
#include "monolis_utils.h"

//void gedatsu_graph_get_edge_in_internal_region(
//void gedatsu_graph_delete_dupulicate_edge(

void gedatsu_graph_set_vertex_test()
{
  GEDATSU_GRAPH graph;

  monolis_std_log_string("gedatsu_graph_set_vertex_test");

  int n_vertex = 3;

  gedatsu_graph_initialize(&graph);

  gedatsu_graph_set_n_vertex(&graph, n_vertex);

  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 1", graph.n_vertex, 3);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 2", graph.vertex_id[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 2", graph.vertex_id[1], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 2", graph.vertex_id[2], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 3", graph.vertex_domain_id[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 3", graph.vertex_domain_id[1], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 3", graph.vertex_domain_id[2], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 4", graph.index[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 4", graph.index[1], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 4", graph.index[2], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex 4", graph.index[3], 0);

  monolis_std_log_string("gedatsu_graph_get_n_vertex");

  n_vertex = 0;

  gedatsu_graph_get_n_vertex(&graph, &n_vertex);

  monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex 1", n_vertex, 3);

  graph.vertex_id[0] = 0;
  graph.vertex_id[1] = 1;
  graph.vertex_id[2] = 2;

  graph.vertex_domain_id[0] = 0;
  graph.vertex_domain_id[1] = 0;
  graph.vertex_domain_id[2] = 1;

  gedatsu_graph_get_n_vertex_in_internal_region(&graph, 0, &n_vertex);

  monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_in_internal_region 1", n_vertex, 2);

  int ids[2];
  gedatsu_graph_get_vertex_id_in_internal_region(&graph, 0, &ids[0]);

  monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region 1", ids[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region 1", ids[1], 1);

  gedatsu_graph_get_vertex_id_in_internal_region(&graph, 1, &ids[0]);

  monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region 2", ids[0], 2);
}

void gedatsu_graph_set_edge_test()
{
  GEDATSU_GRAPH graph;

  monolis_std_log_string("gedatsu_graph_set_edge_test");

  int n_vertex = 5;

  gedatsu_graph_initialize(&graph);

  gedatsu_graph_set_n_vertex(&graph, n_vertex);

  int n_edge = 8;
  int** edge;

  edge = monolis_alloc_I_2d(edge, n_edge, 2);

  edge[0][0] = 0; edge[0][1] = 1;
  edge[1][0] = 1; edge[1][1] = 0;
  edge[2][0] = 1; edge[2][1] = 2;
  edge[3][0] = 2; edge[3][1] = 1;
  edge[4][0] = 2; edge[4][1] = 3;
  edge[5][0] = 3; edge[5][1] = 4;
  edge[6][0] = 3; edge[6][1] = 2;
  edge[7][0] = 4; edge[7][1] = 3;

  gedatsu_graph_set_edge(&graph, n_edge, edge, true);

  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[1], 1);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[2], 3);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[3], 5);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[4], 7);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 1", graph.index[5], 8);

  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[1], 0);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[2], 2);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[3], 1);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[4], 3);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[5], 2);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[6], 4);
  monolis_test_check_eq_I1("gedatsu_graph_set_edge 2", graph.item[7], 3);

  n_edge = 0;

  gedatsu_graph_get_n_edge(&graph, &n_edge);

  monolis_test_check_eq_I1("gedatsu_graph_get_n_edge 1", n_edge, 8);

  n_edge = 2;
  int** edge_add;
  edge_add = monolis_alloc_I_2d(edge_add, n_edge, 2);

  edge_add[0][0] = 0; edge_add[0][1] = 2;
  edge_add[1][0] = 1; edge_add[1][1] = 3;

  gedatsu_graph_add_edge(&graph, n_edge, edge_add, true);

  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[2], 5);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[3], 7);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[4], 9);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 1", graph.index[5], 10);

  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[2], 0);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[3], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[4], 3);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[5], 1);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[6], 3);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[7], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[8], 4);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 2", graph.item[9], 3);

  graph.vertex_id[0] = 0;
  graph.vertex_id[1] = 1;
  graph.vertex_id[2] = 2;
  graph.vertex_id[3] = 3;
  graph.vertex_id[4] = 4;

  graph.vertex_domain_id[0] = 0;
  graph.vertex_domain_id[1] = 0;
  graph.vertex_domain_id[2] = 0;
  graph.vertex_domain_id[3] = 1;
  graph.vertex_domain_id[4] = 1;

  n_edge = 0;

  gedatsu_graph_get_n_edge_in_internal_region(&graph, 0, &n_edge);

  monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_in_internal_region 1", n_edge, 5);

  int** edge_get;
  edge_get = monolis_alloc_I_2d(edge_get, n_edge, 2);

  gedatsu_graph_get_edge_in_internal_region(&graph, 0, edge_get);

  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 1", edge_get[0][0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 1", edge_get[0][1], 1);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 2", edge_get[1][0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 2", edge_get[1][1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[2][0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[2][1], 0);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[3][0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[3][1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[4][0], 2);
  monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region 3", edge_get[4][1], 1);

  n_edge = 2;
  edge_add[0][0] = 0; edge_add[0][1] = 1;
  edge_add[1][0] = 1; edge_add[1][1] = 2;

  gedatsu_graph_add_edge(&graph, n_edge, edge_add, true);

  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[1], 1);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[2], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[3], 0);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[4], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[5], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[6], 3);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[7], 1);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[8], 3);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[9], 2);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[10], 4);
  monolis_test_check_eq_I1("gedatsu_graph_add_edge 3", graph.item[11], 3);

  gedatsu_graph_delete_dupulicate_edge(&graph);

  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[0], 0);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[2], 5);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[3], 7);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[4], 9);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 1", graph.index[5], 10);

  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[0], 1);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[1], 2);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[2], 0);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[3], 2);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[4], 3);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[5], 1);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[6], 3);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[7], 2);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[8], 4);
  monolis_test_check_eq_I1("gedatsu_graph_delete_dupulicate_edge 2", graph.item[9], 3);
}

void gedatsu_graph_handler_c_test()
{
  gedatsu_graph_set_vertex_test();
  gedatsu_graph_set_edge_test();
}
