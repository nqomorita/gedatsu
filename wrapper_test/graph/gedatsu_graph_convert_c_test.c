#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "gedatsu.h"
#include "monolis_utils.h"

void gedatsu_graph_convert_c_test()
{
  int n_node;
  int n_elem;
  int n_base;
  int** elem;
  int* index;
  int* item;
  int* node_index;
  int* node_item;

  monolis_std_log_string("gedatsu_graph_convert_c_test");

  n_node = 4;

  n_elem = 3;

  n_base = 2;

  elem = monolis_alloc_I_2d(elem, 3, 2);

  elem[0][0] = 0; elem[0][1] = 1;
  elem[1][0] = 1; elem[1][1] = 2;
  elem[2][0] = 2; elem[2][1] = 3;

  index = monolis_alloc_I_1d(index, n_elem + 1);

  item = monolis_alloc_I_1d(item, n_elem*n_base);

  gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, index, item);

  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 1", index[0], 0);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 2", index[1], 2);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 3", index[2], 4);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 4", index[3], 6);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 5", item[0], 0);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 5", item[1], 1);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 6", item[2], 1);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 7", item[3], 2);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 8", item[4], 2);
  monolis_test_check_eq_I1("gedatsu_convert_simple_mesh_to_connectivity_graph 9", item[5], 3);

  gedatsu_convert_connectivity_graph_to_nodal_graph(n_node, n_elem, index, item, &node_index, &node_item);

  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 1", node_index[0], 0);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 2", node_index[1], 1);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 3", node_index[2], 3);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 4", node_index[3], 5);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 5", node_index[4], 6);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 6", node_item[0], 1);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 7", node_item[1], 0);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 8", node_item[2], 2);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 9", node_item[3], 1);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 10",node_item[4], 3);
  monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph 11",node_item[5], 2);
}
