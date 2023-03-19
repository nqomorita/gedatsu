#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include "gedatsu.h"
#include "monolis_utils.h"

void gedatsu_graph_convert_c_test()
{
  int n_elem;
  int n_base;
  int** elem;
  int* index;
  int* item;

  monolis_std_log_string("gedatsu_graph_convert_c_test");

  n_elem = 3;

  n_base = 2;

  elem = monolis_alloc_I_2d(elem, 2, 3);

  elem[0][0] = 0; elem[0][1] = 1;
  elem[1][0] = 1; elem[1][1] = 2;
  elem[2][0] = 2; elem[2][1] = 3;

  index = monolis_alloc_I_1d(index, n_elem + 1);

  item = monolis_alloc_I_1d(item, n_elem*n_base);

  gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, index, item);

}
