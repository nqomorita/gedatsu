#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "monolis_utils.h"
#include "./define/gedatsu_def_graph_c_test.h"
#include "./graph/gedatsu_graph_convert_c_test.h"
#include "./graph/gedatsu_graph_merge_c_test.h"

int main()
{
  monolis_std_log_string("gedatsu_c_test");

  monolis_mpi_initialize();

  gedatsu_def_graph_c_test();
  gedatsu_graph_convert_c_test();
  gedatsu_graph_merge_c_test();

  monolis_mpi_finalize();
}
