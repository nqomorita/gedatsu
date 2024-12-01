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
    int** vertex_id_ptr;
    int** vertex_domain_id_ptr;
    int** index_ptr;
    int** item_ptr;
    int* index_size;
    int* item_size;
    int* my_rank;
    int* comm;
    int* comm_size;
    int* recv_n_neib;
    int** recv_neib_pe_ptr;
    int** recv_index_ptr;
    int** recv_item_ptr;
    int* recv_neib_pe_size;
    int* recv_index_size;
    int* recv_item_size;
    int* send_n_neib;
    int** send_neib_pe_ptr;
    int** send_index_ptr;
    int** send_item_ptr;
    int* send_neib_pe_size;
    int* send_index_size;
    int* send_item_size;
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

    n_vertex = (int*)malloc(n_graphs * sizeof(int));
    n_internal_vertex = (int*)malloc(n_graphs * sizeof(int));
    vertex_id_ptr = (int**)malloc(n_graphs * sizeof(int));
    vertex_domain_id_ptr = (int**)malloc(n_graphs * sizeof(int));
    index_ptr = (int**)malloc(n_graphs * sizeof(int));
    item_ptr = (int**)malloc(n_graphs * sizeof(int));
    index_size = (int*)malloc(n_graphs * sizeof(int));
    item_size = (int*)malloc(n_graphs * sizeof(int));

    my_rank = (int*)malloc(n_graphs * sizeof(int));
    comm = (int*)malloc(n_graphs * sizeof(int));
    comm_size = (int*)malloc(n_graphs * sizeof(int));
    recv_n_neib = (int*)malloc(n_graphs * sizeof(int));
    recv_neib_pe_ptr = (int**)malloc(n_graphs * sizeof(int));
    recv_index_ptr = (int**)malloc(n_graphs * sizeof(int));
    recv_item_ptr = (int**)malloc(n_graphs * sizeof(int));
    recv_neib_pe_size = (int*)malloc(n_graphs * sizeof(int));
    recv_index_size = (int*)malloc(n_graphs * sizeof(int));
    recv_item_size = (int*)malloc(n_graphs * sizeof(int));
    send_n_neib = (int*)malloc(n_graphs * sizeof(int));
    send_neib_pe_ptr = (int**)malloc(n_graphs * sizeof(int));
    send_index_ptr = (int**)malloc(n_graphs * sizeof(int));
    send_item_ptr = (int**)malloc(n_graphs * sizeof(int));
    send_neib_pe_size = (int*)malloc(n_graphs * sizeof(int));
    send_index_size = (int*)malloc(n_graphs * sizeof(int));
    send_item_size = (int*)malloc(n_graphs * sizeof(int));

    for(int i = 0; i < n_graphs; i++){
      n_vertex[i] = graphs[i].n_vertex;
      n_internal_vertex[i] = graphs[i].n_internal_vertex;
      vertex_id_ptr[i] = (int*)malloc(n_vertex[i] * sizeof(int));
      vertex_domain_id_ptr[i] = (int*)malloc(n_vertex[i] * sizeof(int));
      index_size[i] = sizeof(graphs[i].index);
      index_ptr[i] = (int*)malloc(index_size[i] * sizeof(int));
      item_size[i] = sizeof(graphs[i].index);
      item_ptr[i] = (int*)malloc(item_size[i] * sizeof(int));

      my_rank[i] = monoCOMs[i].my_rank;
      comm[i] = monoCOMs[i].comm;
      comm_size[i] = monoCOMs[i].comm_size;
      recv_n_neib[i] = monoCOMs[i].recv_n_neib;
      send_n_neib[i] = monoCOMs[i].send_n_neib;
      recv_neib_pe_size[i] = sizeof(monoCOMs[i].recv_neib_pe);
      recv_neib_pe_ptr[i] = (int*)malloc(recv_neib_pe_size[i] * sizeof(int));
      recv_index_size[i] = sizeof(monoCOMs[i].recv_index);
      recv_index_ptr[i] = (int*)malloc(recv_index_size[i] * sizeof(int));
      recv_item_size[i] = sizeof(monoCOMs[i].recv_item);
      recv_item_ptr[i] = (int*)malloc(recv_item_size[i] * sizeof(int));
      send_neib_pe_size[i] = sizeof(monoCOMs[i].send_neib_pe);
      send_neib_pe_ptr[i] = (int*)malloc(send_neib_pe_size[i] * sizeof(int));
      send_index_size[i] = sizeof(monoCOMs[i].send_index);
      send_index_ptr[i] = (int*)malloc(send_index_size[i] * sizeof(int));
      send_item_size[i] = sizeof(monoCOMs[i].send_item);
      send_item_ptr[i] = (int*)malloc(send_item_size[i] * sizeof(int));
    }


    gedatsu_merge_nodal_subgraphs_c_main(
      n_graphs, n_vertex, n_internal_vertex, vertex_id_ptr, vertex_domain_id_ptr, \
      index_ptr, item_ptr, index_size, item_size, \
      my_rank, comm, comm_size, \
      recv_n_neib, recv_neib_pe_ptr, recv_index_ptr, recv_item_ptr, recv_neib_pe_size, recv_index_size, recv_item_size, \
      send_n_neib, send_neib_pe_ptr, send_index_ptr, send_item_ptr, send_neib_pe_size, send_index_size, send_item_size, \
      merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, \
      merged_index, merged_item, merged_my_rank, merged_comm, merged_comm_size, \
      merged_recv_n_neib, merged_recv_neib_pe, merged_recv_index, merged_recv_item, \
      merged_send_n_neib, merged_send_neib_pe, merged_send_index, merged_send_item, \
      order_type);

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
    free(vertex_id_ptr);
    free(vertex_domain_id_ptr);
    free(index_ptr);
    free(item_ptr);
    free(index_size);
    free(item_size);
    free(my_rank);
    free(comm);
    free(comm_size);
    free(recv_n_neib);
    free(recv_neib_pe_ptr);
    free(recv_index_ptr);
    free(recv_item_ptr);
    free(recv_neib_pe_size);
    free(recv_index_size);
    free(recv_item_size);
    free(send_n_neib);
    free(send_neib_pe_ptr);
    free(send_index_ptr);
    free(send_item_ptr);
    free(send_neib_pe_size);
    free(send_index_size);
    free(send_item_size);
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
  {}

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
