#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gedatsu_graph_convert_c.h"
#include "metis.h"

/** 単一メッシュ形式からコネクティビティグラフ形式に変換 */
void gedatsu_convert_simple_mesh_to_connectivity_graph(
  int   n_elem,
  int   n_base,
  int** elem,
  int*  index,
  int*  item)
{
  int i, j;
  for(i = 0; i < n_elem + 1; i++) {
    index[i] = i*n_base;
  }

  for(i = 0; i < n_elem; i++) {
    for(j = 0; j < n_base; j++) {
      item[n_base*i+j] = elem[i][j];
    }
  }
}

/** 単一メッシュ形式から節点グラフ形式に変換 */
void gedatsu_convert_connectivity_graph_to_nodal_graph(
  int   nnode,
  int   n_elem,
  int*  conn_index,
  int*  conn_item,
  int** nodal_index,
  int** nodal_item)
{
  idx_t numflag = 0;

  int ierr = METIS_MeshToNodal(
    &n_elem,
    &nnode,
    conn_index,
    conn_item,
    &numflag,
    nodal_index,
    nodal_item);
}
