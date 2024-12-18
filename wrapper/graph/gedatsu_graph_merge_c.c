#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_utils.h"
#include "gedatsu_def_graph_c.h"
#include "gedatsu_graph_merge_c.h"

/** 計算点グラフの結合 */
void gedatsu_merge_nodal_subgraphs(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  int order_type)
{
  int* n_vertex;
  int* n_internal_vertex;
  int** vertex_id;
  int** vertex_domain_id;
  int** index;
  int** item;
  int* my_rank;
  int* comm;
  int* comm_size;
  int* recv_n_neib;
  int** recv_neib_pe;
  int** recv_index;
  int** recv_item;
  int* send_n_neib;
  int** send_neib_pe;
  int** send_index;
  int** send_item;
  int merged_n_vertex;
  int merged_n_internal_vertex;
  int* merged_vertex_id;
  int* merged_vertex_domain_id;
  int* merged_index;
  int* merged_item;
  int merged_my_rank;
  int merged_comm;
  int merged_comm_size;
  int merged_recv_n_neib;
  int* merged_recv_neib_pe;
  int* merged_recv_index;
  int* merged_recv_item;
  int merged_send_n_neib;
  int* merged_send_neib_pe;
  int* merged_send_index;
  int* merged_send_item;

  // graphs, monoCOMs を分解
  n_vertex = (int*)calloc(n_graphs, sizeof(int));
  n_internal_vertex = (int*)calloc(n_graphs, sizeof(int));
  vertex_id = (int**)calloc(n_graphs, sizeof(int*));
  vertex_domain_id = (int**)calloc(n_graphs, sizeof(int*));
  index = (int**)calloc(n_graphs, sizeof(int*));
  item = (int**)calloc(n_graphs, sizeof(int*));
  my_rank = (int*)calloc(n_graphs, sizeof(int));
  comm = (int*)calloc(n_graphs, sizeof(int));
  comm_size = (int*)calloc(n_graphs, sizeof(int));
  recv_n_neib = (int*)calloc(n_graphs, sizeof(int));
  recv_neib_pe = (int**)calloc(n_graphs, sizeof(int*));
  recv_index = (int**)calloc(n_graphs, sizeof(int*));
  recv_item = (int**)calloc(n_graphs, sizeof(int*));
  send_n_neib = (int*)calloc(n_graphs, sizeof(int));
  send_neib_pe = (int**)calloc(n_graphs, sizeof(int*));
  send_index = (int**)calloc(n_graphs, sizeof(int*));
  send_item = (int**)calloc(n_graphs, sizeof(int*));

  for(int i = 0; i < n_graphs; i++)
  {
    vertex_id[i] = (int*)calloc(graphs[i].n_vertex, sizeof(int));
    vertex_domain_id[i] = (int*)calloc(graphs[i].n_vertex, sizeof(int));
    index[i] = (int*)calloc(sizeof(graphs[i].index), sizeof(int));
    item[i] = (int*)calloc(sizeof(graphs[i].item), sizeof(int));

    n_vertex[i] = graphs[i].n_vertex;
    n_internal_vertex[i] = graphs[i].n_internal_vertex;
    vertex_id[i] = graphs[i].vertex_id;
    vertex_domain_id[i] = graphs[i].vertex_domain_id;
    index[i] = graphs[i].index;
    item[i] = graphs[i].item;

    recv_neib_pe[i] = (int*)calloc(sizeof(monoCOMs[i].recv_neib_pe), sizeof(int));
    recv_index[i] = (int*)calloc(sizeof(monoCOMs[i].recv_index), sizeof(int));
    recv_item[i] = (int*)calloc(sizeof(monoCOMs[i].recv_item), sizeof(int));
    send_neib_pe[i] = (int*)calloc(sizeof(monoCOMs[i].send_neib_pe), sizeof(int));
    send_index[i] = (int*)calloc(sizeof(monoCOMs[i].send_index), sizeof(int));
    send_item[i] = (int*)calloc(sizeof(monoCOMs[i].send_item), sizeof(int));

    my_rank[i] = monoCOMs[i].my_rank;
    comm[i] = monoCOMs[i].comm;
    comm_size[i] = monoCOMs[i].comm_size;
    recv_n_neib[i] = monoCOMs[i].recv_n_neib;
    recv_neib_pe[i] = monoCOMs[i].recv_neib_pe;
    recv_index[i] = monoCOMs[i].recv_index;
    recv_item[i] = monoCOMs[i].recv_item;
    send_n_neib[i] = monoCOMs[i].send_n_neib;
    send_neib_pe[i] = monoCOMs[i].send_neib_pe;
    send_index[i] = monoCOMs[i].send_index;
    send_item[i] = monoCOMs[i].send_item;
  }

  // Fortranラッパー関数
  gedatsu_merge_nodal_subgraphs_c_main(
    n_graphs, n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, \
    my_rank, comm, comm_size, \
    recv_n_neib, recv_neib_pe, recv_index, recv_item, \
    send_n_neib, send_neib_pe, send_index, send_item, \
    merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, \
    merged_index, merged_item, merged_my_rank, merged_comm, merged_comm_size, \
    merged_recv_n_neib, merged_recv_neib_pe, merged_recv_index, merged_recv_item, \
    merged_send_n_neib, merged_send_neib_pe, merged_send_index, merged_send_item, \
    order_type);

  // merged_graph, merged_monoCOM の作成
  merged_graph->n_vertex = merged_n_vertex;
  merged_graph->n_internal_vertex = merged_n_internal_vertex;
  merged_graph->vertex_id = merged_vertex_id;
  merged_graph->vertex_domain_id = merged_vertex_domain_id;
  merged_graph->index = merged_index;
  merged_graph->item = merged_item;

  merged_monoCOM->n_internal_vertex = merged_n_internal_vertex;
  merged_monoCOM->my_rank = merged_my_rank;
  merged_monoCOM->comm = merged_comm;
  merged_monoCOM->comm_size = merged_comm_size;
  merged_monoCOM->recv_n_neib = merged_recv_n_neib;
  merged_monoCOM->recv_neib_pe = merged_recv_neib_pe;
  merged_monoCOM->recv_index = merged_recv_index;
  merged_monoCOM->recv_item = merged_recv_item;
  merged_monoCOM->send_n_neib = merged_send_n_neib;
  merged_monoCOM->send_neib_pe = merged_send_neib_pe;
  merged_monoCOM->send_index = merged_send_index;
  merged_monoCOM->send_item = merged_send_item;

  // callocで確保したメモリをfreeで解放
  free(n_vertex);
  free(n_internal_vertex);
  free(n_internal_vertex);
  free(vertex_id);
  free(vertex_domain_id);
  free(index);
  free(item);
  free(my_rank);
  free(comm);
  free(comm_size);
  free(recv_n_neib);
  free(recv_neib_pe);
  free(recv_index);
  free(recv_item);
  free(send_n_neib);
  free(send_neib_pe);
  free(send_index);
  free(send_item);
  free(merged_vertex_id);
  free(merged_vertex_domain_id);
  free(merged_index);
  free(merged_item);
  free(merged_recv_neib_pe);
  free(merged_recv_index);
  free(merged_recv_item);
  free(merged_send_neib_pe);
  free(merged_send_index);
  free(merged_send_item);
}

/** コネクティビティグラフの結合 */
void gedatsu_merge_connectivity_subgraphs(
  int n_nodal_graphs,
  GEDATSU_GRAPH* nodal_graphs,
  GEDATSU_GRAPH* merged_nodal_graph,
  MONOLIS_COM* merged_nodal_monoCOM,
  int n_conn_graphs,
  GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph)
{
  int* nodal_n_vertex;
  int* nodal_n_internal_vertex;
  int** nodal_vertex_id;
  int** nodal_vertex_domain_id;
  int** nodal_index;
  int** nodal_item;
  int merged_nodal_n_vertex;
  int merged_nodal_n_internal_vertex;
  int* merged_nodal_vertex_id;
  int* merged_nodal_vertex_domain_id;
  int* merged_nodal_index;
  int* merged_nodal_item;
  int merged_nodal_my_rank;
  int merged_nodal_comm;
  int merged_nodal_comm_size;
  int merged_nodal_recv_n_neib;
  int* merged_nodal_recv_neib_pe;
  int* merged_nodal_recv_index;
  int* merged_nodal_recv_item;
  int merged_nodal_send_n_neib;
  int* merged_nodal_send_neib_pe;
  int* merged_nodal_send_index;
  int* merged_nodal_send_item;

  int* conn_n_vertex;
  int* conn_n_internal_vertex;
  int** conn_vertex_id;
  int** conn_vertex_domain_id;
  int** conn_index;
  int** conn_item;
  int merged_conn_n_vertex;
  int merged_conn_n_internal_vertex;
  int* merged_conn_vertex_id;
  int* merged_conn_vertex_domain_id;
  int* merged_conn_index;
  int* merged_conn_item;

  // nodal_graphs, conn_graphs, merged_nodal_monoCOM を分解
  nodal_n_vertex = (int*)calloc(n_nodal_graphs, sizeof(int));
  nodal_n_internal_vertex = (int*)calloc(n_nodal_graphs, sizeof(int));
  nodal_vertex_id = (int**)calloc(n_nodal_graphs, sizeof(int*));
  nodal_vertex_domain_id = (int**)calloc(n_nodal_graphs, sizeof(int*));
  nodal_index = (int**)calloc(n_nodal_graphs, sizeof(int*));
  nodal_item = (int**)calloc(n_nodal_graphs, sizeof(int*));
  merged_nodal_recv_neib_pe = (int*)calloc(n_nodal_graphs, sizeof(int));
  merged_nodal_recv_index = (int*)calloc(n_nodal_graphs, sizeof(int));
  merged_nodal_recv_item = (int*)calloc(n_nodal_graphs, sizeof(int));
  merged_nodal_send_neib_pe = (int*)calloc(n_nodal_graphs, sizeof(int));
  merged_nodal_send_index = (int*)calloc(n_nodal_graphs, sizeof(int));
  merged_nodal_send_item = (int*)calloc(n_nodal_graphs, sizeof(int));
  conn_n_vertex = (int*)calloc(n_conn_graphs, sizeof(int));
  conn_n_internal_vertex = (int*)calloc(n_conn_graphs, sizeof(int));
  conn_vertex_id = (int**)calloc(n_conn_graphs, sizeof(int*));
  conn_vertex_domain_id = (int**)calloc(n_conn_graphs, sizeof(int*));
  conn_index = (int**)calloc(n_conn_graphs, sizeof(int*));
  conn_item = (int**)calloc(n_conn_graphs, sizeof(int*));

  for(int i = 0; i < n_nodal_graphs; i++)
  {
    nodal_vertex_id[i] = (int*)calloc(nodal_graphs[i].n_vertex, sizeof(int));
    nodal_vertex_domain_id[i] = (int*)calloc(nodal_graphs[i].n_vertex, sizeof(int));
    nodal_index[i] = (int*)calloc(sizeof(nodal_graphs[i].index), sizeof(int));
    nodal_item[i] = (int*)calloc(sizeof(nodal_graphs[i].item), sizeof(int));

    nodal_n_vertex[i] = nodal_graphs[i].n_vertex;
    nodal_n_internal_vertex[i] = nodal_graphs[i].n_internal_vertex;
    nodal_vertex_id[i] = nodal_graphs[i].vertex_id;
    nodal_vertex_domain_id[i] = nodal_graphs[i].vertex_domain_id;
    nodal_index[i] = nodal_graphs[i].index;
    nodal_item[i] = nodal_graphs[i].item;
  }

  for(int i = 0; i < n_conn_graphs; i++)
  {
    conn_vertex_id[i] = (int*)calloc(conn_graphs[i].n_vertex, sizeof(int));
    conn_vertex_domain_id[i] = (int*)calloc(conn_graphs[i].n_vertex, sizeof(int));
    conn_index[i] = (int*)calloc(sizeof(conn_graphs[i].index), sizeof(int));
    conn_item[i] = (int*)calloc(sizeof(conn_graphs[i].item), sizeof(int));

    conn_n_vertex[i] = conn_graphs[i].n_vertex;
    conn_n_internal_vertex[i] = conn_graphs[i].n_internal_vertex;
    conn_vertex_id[i] = conn_graphs[i].vertex_id;
    conn_vertex_domain_id[i] = conn_graphs[i].vertex_domain_id;
    conn_index[i] = conn_graphs[i].index;
    conn_item[i] = conn_graphs[i].item;
  }

  merged_nodal_my_rank = merged_nodal_monoCOM->my_rank;
  merged_nodal_comm = merged_nodal_monoCOM->comm;
  merged_nodal_comm_size = merged_nodal_monoCOM->comm_size;
  merged_nodal_recv_n_neib = merged_nodal_monoCOM->recv_n_neib;
  merged_nodal_recv_neib_pe = merged_nodal_monoCOM->recv_neib_pe;
  merged_nodal_recv_index = merged_nodal_monoCOM->recv_index;
  merged_nodal_recv_item = merged_nodal_monoCOM->recv_item;
  merged_nodal_send_n_neib = merged_nodal_monoCOM->send_n_neib;
  merged_nodal_send_neib_pe = merged_nodal_monoCOM->send_neib_pe;
  merged_nodal_send_index = merged_nodal_monoCOM->send_index;
  merged_nodal_send_item = merged_nodal_monoCOM->send_item;

  // Fortranラッパー関数
  gedatsu_merge_connectivity_subgraphs_c_main(
    n_nodal_graphs, \
    nodal_n_vertex, nodal_n_internal_vertex, nodal_vertex_id, nodal_vertex_domain_id, nodal_index, nodal_item, \
    merged_nodal_n_vertex, merged_nodal_n_internal_vertex, merged_nodal_vertex_id, merged_nodal_vertex_domain_id, \
    merged_nodal_index, merged_nodal_item, merged_nodal_my_rank, merged_nodal_comm, merged_nodal_comm_size, \
    merged_nodal_recv_n_neib, merged_nodal_recv_neib_pe, merged_nodal_recv_index, merged_nodal_recv_item, \
    merged_nodal_send_n_neib, merged_nodal_send_neib_pe, merged_nodal_send_index, merged_nodal_send_item, \
    n_conn_graphs, \
    conn_n_vertex, conn_n_internal_vertex, conn_vertex_id, conn_vertex_domain_id, conn_index, conn_item, \
    merged_conn_n_vertex, merged_conn_n_internal_vertex, merged_conn_vertex_id, merged_conn_vertex_domain_id, \
    merged_conn_index, merged_conn_item
    );

  // merged_conn_graph の作成
  merged_conn_graph->n_vertex = merged_conn_n_vertex;
  merged_conn_graph->n_internal_vertex = merged_conn_n_internal_vertex;
  merged_conn_graph->vertex_id = merged_conn_vertex_id;
  merged_conn_graph->vertex_domain_id = merged_conn_vertex_domain_id;
  merged_conn_graph->index = merged_conn_index;
  merged_conn_graph->item = merged_conn_item;

  // callocで確保したメモリをfreeで解放
  free(nodal_n_vertex);
  free(nodal_n_internal_vertex);
  free(nodal_n_internal_vertex);
  free(nodal_vertex_id);
  free(nodal_vertex_domain_id);
  free(nodal_index);
  free(nodal_item);
  free(merged_nodal_vertex_id);
  free(merged_nodal_vertex_domain_id);
  free(merged_nodal_index);
  free(merged_nodal_item);
  free(merged_nodal_recv_neib_pe);
  free(merged_nodal_recv_index);
  free(merged_nodal_recv_item);
  free(merged_nodal_send_neib_pe);
  free(merged_nodal_send_index);
  free(merged_nodal_send_item);
  free(conn_n_vertex);
  free(conn_n_internal_vertex);
  free(conn_n_internal_vertex);
  free(conn_vertex_id);
  free(conn_vertex_domain_id);
  free(conn_index);
  free(conn_item);
  free(merged_conn_vertex_id);
  free(merged_conn_vertex_domain_id);
  free(merged_conn_index);
  free(merged_conn_item);
}

/** 物理量の結合 (実数配列) */
void gedatsu_merge_distval_R(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_R* list_struct_R,
  int* merged_n_dof_list,
  int* merged_array_R)
{}

/** 物理量の結合 (整数配列) */
void gedatsu_merge_distval_I(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_I* list_struct_I,
  int* merged_n_dof_list,
  int* merged_array_R)
{}

/** 物理量の結合 (複素数配列) */
void gedatsu_merge_distval_C(
  int n_graphs,
  GEDATSU_GRAPH* graphs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_LIST_I* n_dof_list,
  MONOLIS_LIST_C* list_struct_C,
  int* merged_n_dof_list,
  int* merged_array_C)
{}

void gedatsu_list_initialize_R(
  MONOLIS_LIST_R* list_struct_R,
  int n)
{}

void gedatsu_list_finalize_R(
  MONOLIS_LIST_R* list_struct_R)
{}

void gedatsu_list_set_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  int n,
  double* array)
{}

void gedatsu_list_get_R(
  MONOLIS_LIST_R* list_struct_R,
  int id,
  double* array)
{}

void gedatsu_list_initialize_I(
  MONOLIS_LIST_I* list_struct_I,
  int n)
{}

void gedatsu_list_finalize_I(
  MONOLIS_LIST_I* list_struct_I)
{}

void gedatsu_list_set_I(
  MONOLIS_LIST_I* list_struct_I,
  int id,
  int n,
  int* array)
{}

void gedatsu_list_get_I(
    MONOLIS_LIST_I* list_struct_I,
  int id,
  int* array)
{}

void gedatsu_list_initialize_C(
  MONOLIS_LIST_C* list_struct_C,
  int n)
{}

void gedatsu_list_finalize_C(
  MONOLIS_LIST_C* list_struct_C)
{}

void gedatsu_list_set_C(
  MONOLIS_LIST_C* list_struct_C,
  int id,
  int n,
  double complex* array)
{}

void gedatsu_list_get_C(
MONOLIS_LIST_C* list_struct_C,
  int id,
  double complex* array)
{}
