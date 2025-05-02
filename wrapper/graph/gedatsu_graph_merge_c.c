#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include "monolis_utils.h"
#include "gedatsu_def_graph_c.h"
#include "gedatsu_graph_handler_c.h"
#include "gedatsu_graph_merge_c.h"

/** 計算点グラフの結合 */
void gedatsu_merge_nodal_subgraphs(
  const int n_graphs,
  GEDATSU_GRAPH* graphs,
  const MONOLIS_COM* monoCOMs,
  GEDATSU_GRAPH* merged_graph,
  MONOLIS_COM* merged_monoCOM,
  const int order_type)
{
  int iS, iE, val, idx, tmp1, tmp2;
  int n_vertex, n_internal_vertex, n_overlap_vertex, n_vertex_uniq, n_edge;
  int* vertex_id = NULL;
  int* internal_vertex_id = NULL;
  int* overlap_vertex_id = NULL;
  int* vertex_id_notsorted = NULL;
  int** edge = NULL;
  bool* is_internal = NULL;
  bool* is_already_count_overlap = NULL;

  n_vertex = 0;
  n_internal_vertex = 0;
  for (int i = 0; i < n_graphs; i++) {
    n_vertex += graphs[i].n_vertex;
    n_internal_vertex += graphs[i].n_internal_vertex;
  }

  vertex_id = monolis_alloc_I_1d(vertex_id, n_vertex);

  // 全ての graphs の vertex_id をつなげる
  iS = 0;
  for (int i = 0; i < n_graphs; i++) {
    iE = iS + graphs[i].n_vertex;
    tmp1 = 0;
    for (int j = iS; j < iE; j++) {
      vertex_id[j] = graphs[i].vertex_id[tmp1];
      tmp1 += 1;
    }
    iS += graphs[i].n_vertex;
  }

  // つなげた vertex_id を昇順ソート＋重複削除
  monolis_qsort_I_1d(vertex_id, n_vertex, 0, n_vertex-1);
  monolis_get_uniq_array_I(vertex_id, n_vertex, &tmp1);
  // monolis_qsort_I_1d(vertex_id, n_vertex, 0, n_vertex - 1);
  vertex_id = monolis_realloc_I_1d(vertex_id, n_vertex, tmp1);
  n_vertex = tmp1;  // 重複を削除した全計算点数

  // 内部領域か袖領域かの判定
  is_internal = monolis_alloc_L_1d(is_internal, n_vertex);  // 内部領域なら.true.
  for (int i = 0; i < n_graphs; i++) {
    for (int j = 0; j < graphs[i].n_internal_vertex; j++) {
      val = graphs[i].vertex_id[j];
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);
      is_internal[idx] = true;
    }
  }

  // 内部領域と袖領域に分割
  n_overlap_vertex = n_vertex - n_internal_vertex;
  internal_vertex_id = monolis_alloc_I_1d(internal_vertex_id, n_internal_vertex);
  overlap_vertex_id = monolis_alloc_I_1d(overlap_vertex_id, n_overlap_vertex);
  tmp1 = 0;
  tmp2 = 0;
  for (int i = 0; i < n_vertex; i++) {
    if (is_internal[i]) {
      internal_vertex_id[tmp1] = vertex_id[i];
      tmp1 += 1;
    } else {
      overlap_vertex_id[tmp2] = vertex_id[i];
      tmp2 += 1;
    }
  }

  // ここから結合後のグラフ merged_graph の作成
  gedatsu_graph_initialize(merged_graph);

  // // n_vertex と n_internal_vertex の設定
  gedatsu_graph_set_n_vertex(merged_graph, n_vertex);
  merged_graph->n_internal_vertex = n_internal_vertex;

  // vertex_id の作成
  if (order_type == ORDER_NODAL_ID) {
    for (int i = 0; i < n_internal_vertex; i++) {
      merged_graph->vertex_id[i] = internal_vertex_id[i];
    }
    for (int i = 0; i < n_overlap_vertex; i++) {
      merged_graph->vertex_id[n_internal_vertex+i] = overlap_vertex_id[i];
    }
  } else if (order_type == ORDER_DOMAIN_ID) {
    // 内部領域
    iS = 0;
    for (int i = 0; i < n_graphs; i++) {
      iE = iS + graphs[i].n_internal_vertex;
      tmp1 = 0;
      for (int j = iS; j < iE; j++) {
        merged_graph->vertex_id[j] = graphs[i].vertex_id[tmp1];
        tmp1 += 1;
      }
      iS += graphs[i].n_internal_vertex;
    }
    // 袖領域
    is_already_count_overlap = monolis_alloc_L_1d(is_already_count_overlap, n_overlap_vertex); // 袖領域に含まれるとカウントしたら.true.にする
    tmp1 = 0;
    for (int i = 0; i < n_graphs; i++) {
      for (int j = graphs[i].n_internal_vertex; j < graphs[i].n_vertex; j++) {
        val = graphs[i].vertex_id[j];
        monolis_bsearch_I(overlap_vertex_id, n_overlap_vertex, 0, n_overlap_vertex-1, val, &idx);
        if (idx == -1) {
          continue;
        }
        if (is_already_count_overlap[idx] == false) {
          merged_graph->vertex_id[n_internal_vertex+tmp1] = val;
          tmp1 += 1;
          is_already_count_overlap[idx] = true;
        }
      }
    }
  } else {
    monolis_std_log_string("*** error *** Invalid order_type. order_type must be ORDER_DOMAIN_ID or ORDER_NODAL_ID.");
    exit(1);
  }

  // 「ソート後の結合後ローカル番号」＝vertex_idと「ソートしていない本来の結合後ローカル番号」＝ merged_graph%vertex_idの対応関係を保持しておく
  vertex_id_notsorted = monolis_alloc_I_1d(vertex_id_notsorted, n_vertex);
  for (int i = 0; i < n_vertex; i++) {
    val = merged_graph->vertex_id[i];
    monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);
    vertex_id_notsorted[idx] = i;
  }

  // CSR 形式グラフの作成
  for (int i = 0; i < n_graphs; i++) {
    if (i != 0) {
      monolis_dealloc_I_2d(&edge, n_edge, 2); // BUG i==0でこれ呼ぶと Segmentation faultが起きてる気がするけど、違うのか？
    }
    gedatsu_graph_get_n_edge(&graphs[i], &n_edge);
    edge = monolis_alloc_I_2d(edge, n_edge, 2);
    gedatsu_graph_get_edge_in_internal_region(&graphs[i], monolis_mpi_get_global_my_rank(), edge); // domain_id はランク番号

    // edge を「ソートしていない本来の結合後ローカル番号」に変換
    for (int j = 0; j < n_edge; j++) {
      for (int k = 0; k < 2; k++) {
        idx = edge[j][k];  // 結合前グラフにおけるローカル番号
        val = graphs[i].vertex_id[idx]; // グローバル番号
        monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 「ソート後の結合後ローカル番号」
        edge[j][k] = vertex_id_notsorted[idx];  // 「ソートしていない本来の結合後ローカル番号」
      }
    }
    // merged_graph にエッジを追加
    gedatsu_graph_add_edge(merged_graph, n_edge, edge, true);
  }

  // 重複削除
  gedatsu_graph_delete_dupulicate_edge(merged_graph);

  // // 通信テーブルの結合
  monolis_com_initialize_by_global_id(merged_monoCOM, monolis_mpi_get_global_comm(),
  merged_graph->n_internal_vertex, merged_graph->n_vertex, merged_graph->vertex_id);
}

/** コネクティビティグラフの結合 */
void gedatsu_merge_connectivity_subgraphs(
  const int n_nodal_graphs,
  const GEDATSU_GRAPH* nodal_graphs,
  GEDATSU_GRAPH* merged_nodal_graph,
  const MONOLIS_COM* merged_nodal_monoCOM,
  const int n_conn_graphs,
  const GEDATSU_GRAPH* conn_graphs,
  GEDATSU_GRAPH* merged_conn_graph)
{
  int iS, iE, val, idx, tmp1, tmp2, n_edge;
  int n_conn_vertex, n_conn_internal_vertex, n_conn_overlap_vertex, n_nodal_vertex;
  int* conn_vertex_id = NULL;
  int* conn_internal_vertex_id = NULL;
  int* conn_overlap_vertex_id = NULL;
  int* conn_vertex_id_notsorted = NULL;
  int* nodal_vertex_id = NULL;
  int* nodal_vertex_id_notsorted = NULL;
  int* perm = NULL;
  int* temp1 = NULL;
  int* temp2 = NULL;
  int* global_id_in_merged_graph = NULL;
  int* which_conn_graph = NULL;
  int* local_id_in_conn_graph = NULL;
  int** edge = NULL;
  bool* is_conn_internal = NULL;
  MONOLIS_LIST_I* conn_graphs_vertex_id = NULL;
  MONOLIS_LIST_I* conn_graphs_vertex_id_perm = NULL;

  if (n_nodal_graphs != n_conn_graphs) {
    monolis_std_log_string("*** n_nodal_graphs != n_conn_graphs");
    exit(1);
  }

  n_conn_vertex = 0;
  n_conn_internal_vertex = 0;
  for (int i = 0; i < n_conn_graphs; i++) {
    n_conn_vertex += conn_graphs[i].n_vertex;
    n_conn_internal_vertex += conn_graphs[i].n_internal_vertex;
  }
  conn_vertex_id = monolis_alloc_I_1d(conn_vertex_id, n_conn_vertex);

  // 全ての conn_graphs の vertex_id をつなげる
  iS = 0;
  for (int i = 0; i < n_conn_graphs; i++) {
    tmp1 = 0;
    iE = iS + conn_graphs[i].n_vertex;
    for (int j = iS; j < iE; j++) {
      conn_vertex_id[j] = conn_graphs[i].vertex_id[tmp1];
      tmp1 += 1;
    }
    iS += conn_graphs[i].n_vertex;
  }

  // つなげた conn_vertex_id を昇順ソート＋重複削除
  monolis_qsort_I_1d(conn_vertex_id, n_conn_vertex, 0, n_conn_vertex-1);
  monolis_get_uniq_array_I(conn_vertex_id, n_conn_vertex, &tmp1);
  conn_vertex_id = monolis_realloc_I_1d(conn_vertex_id, n_conn_vertex, tmp1);
  n_conn_vertex = tmp1;

  // 内部領域か袖領域かの判定
  is_conn_internal = monolis_alloc_L_1d(is_conn_internal, n_conn_vertex); // 内部領域なら true
  for (int i = 0; i < n_conn_graphs; i++) {
    for (int j = 0; j < conn_graphs[i].n_internal_vertex; j++) {
      val = conn_graphs[i].vertex_id[j];
      monolis_bsearch_I(conn_vertex_id, n_conn_vertex, 0, n_conn_vertex-1, val, &idx);
      is_conn_internal[idx] = true;
    }
  }

  // 内部領域と袖領域に分割
  n_conn_overlap_vertex = n_conn_vertex - n_conn_internal_vertex;
  conn_internal_vertex_id = monolis_alloc_I_1d(conn_internal_vertex_id, n_conn_internal_vertex);
  conn_overlap_vertex_id = monolis_alloc_I_1d(conn_overlap_vertex_id, n_conn_overlap_vertex);
  tmp1 = 0;
  tmp2 = 0;
  for (int i = 0; i < n_conn_vertex; i++) {
    if (is_conn_internal[i] == true) {
      conn_internal_vertex_id[tmp1] = conn_vertex_id[i];
      tmp1 += 1;
    } else {
      conn_overlap_vertex_id[tmp2] = conn_vertex_id[i];
      tmp2 += 1;
    }
  }

  // ここから結合後のグラフ merged_conn_graph の作成
  gedatsu_graph_initialize(merged_conn_graph);
  gedatsu_graph_set_n_vertex(merged_conn_graph, n_conn_vertex);
  merged_conn_graph->n_internal_vertex = n_conn_internal_vertex;

  for (int i = 0; i < n_conn_internal_vertex; i++) {
    merged_conn_graph->vertex_id[i] = conn_internal_vertex_id[i];
  }
  for (int i = 0; i < n_conn_overlap_vertex; i++) {
    merged_conn_graph->vertex_id[n_conn_internal_vertex+i] = conn_overlap_vertex_id[i];
  }

  // 「ソート後の結合後ローカル番号」と「ソートしていない本来の結合後ローカル番号」の対応関係を保持しておく必要がある
  // 要素
  conn_vertex_id_notsorted = monolis_alloc_I_1d(conn_vertex_id_notsorted, n_conn_vertex);
  for (int i = 0; i < n_conn_vertex; i++) {
    val = merged_conn_graph->vertex_id[i];
    monolis_bsearch_I(conn_vertex_id, n_conn_vertex, 0, n_conn_vertex-1, val, &idx);
    conn_vertex_id_notsorted[i] = idx;
  }
  // 計算点
  nodal_vertex_id = monolis_alloc_I_1d(nodal_vertex_id, merged_nodal_graph->n_vertex);
  for (int i = 0; i < merged_nodal_graph->n_vertex; i++) {
    nodal_vertex_id[i] = merged_nodal_graph->vertex_id[i];
  }
  gedatsu_graph_get_n_vertex(merged_nodal_graph, &n_nodal_vertex);   // TODO 304行目の前に呼ぶべき？
  monolis_qsort_I_1d(nodal_vertex_id, n_nodal_vertex, 0, n_nodal_vertex-1);
  nodal_vertex_id_notsorted = monolis_alloc_I_1d(nodal_vertex_id_notsorted, n_nodal_vertex);
  for (int i = 0; i < n_nodal_vertex; i++) {
    val = merged_nodal_graph->vertex_id[i];
    monolis_bsearch_I(nodal_vertex_id, merged_nodal_graph->n_vertex, 0, n_nodal_vertex-1, val, &idx);
    nodal_vertex_id_notsorted[idx] = i;
  }

  // 結合前グラフの、ソート前後のグローバル番号を用意
  conn_graphs_vertex_id = (MONOLIS_LIST_I *)calloc(n_conn_graphs, sizeof(MONOLIS_LIST_I));
  conn_graphs_vertex_id_perm = (MONOLIS_LIST_I *)calloc(n_conn_graphs, sizeof(MONOLIS_LIST_I));
  monolis_list_initialize_I(conn_graphs_vertex_id, n_conn_graphs);
  monolis_list_initialize_I(conn_graphs_vertex_id_perm, n_conn_graphs);
  for (int i = 0; i < n_conn_graphs; i++) {
    monolis_list_set_I(conn_graphs_vertex_id, i, conn_graphs[i].n_vertex, conn_graphs[i].vertex_id);
    conn_graphs_vertex_id_perm[i].n = conn_graphs[i].n_vertex;
    conn_graphs_vertex_id_perm[i].array = monolis_alloc_I_1d(conn_graphs_vertex_id_perm[i].array, conn_graphs[i].n_vertex);
    monolis_get_sequence_array_I(conn_graphs_vertex_id_perm[i].array, conn_graphs[i].n_vertex, 0, 1);
    monolis_qsort_I_2d(conn_graphs_vertex_id[i].array, conn_graphs_vertex_id_perm[i].array, conn_graphs[i].n_vertex, 0, conn_graphs[i].n_vertex-1);
  }

  global_id_in_merged_graph = monolis_alloc_I_1d(global_id_in_merged_graph, n_conn_vertex);
  perm = monolis_alloc_I_1d(perm, n_conn_vertex);
  which_conn_graph = monolis_alloc_I_1d(which_conn_graph, n_conn_vertex);
  local_id_in_conn_graph = monolis_alloc_I_1d(local_id_in_conn_graph, n_conn_vertex);
  temp1 = monolis_alloc_I_1d(temp1, n_conn_vertex);
  temp2 = monolis_alloc_I_1d(temp2, n_conn_vertex);

  for (int i = 0; i < n_conn_vertex; i++) {
    global_id_in_merged_graph[i] = merged_conn_graph->vertex_id[i];
  }

  for (int i = 0; i < n_conn_vertex; i++) {
    val = merged_conn_graph->vertex_id[i];  // グローバル番号
    for (int j = 0; j < n_conn_graphs; j++) {
      monolis_bsearch_I(conn_graphs_vertex_id[j].array, conn_graphs[j].n_vertex, 0, conn_graphs_vertex_id[j].n-1, val, &idx);  // ソート後ローカル番号
      if (idx == -1) {
        continue;
      }
      tmp1 = conn_graphs_vertex_id_perm[j].array[idx]; // ソート前ローカル番号
      which_conn_graph[i] = j;
      local_id_in_conn_graph[i] = tmp1;
      if (idx != -1) {
        break;
      }
    }
  }

  monolis_get_sequence_array_I(perm, n_conn_vertex, 0, 1);
  monolis_qsort_I_2d(global_id_in_merged_graph, perm, n_conn_vertex, 0, n_conn_vertex-1);

  for (int i = 0; i < n_conn_vertex; i++)
  {
    temp1[i] = which_conn_graph[i];
    temp2[i] = local_id_in_conn_graph[i];
  }
  for (int i = 0; i < n_conn_vertex; i++)
  {
    tmp1 = perm[i];
    which_conn_graph[i] = temp1[tmp1];
    local_id_in_conn_graph[i] = temp2[tmp1];
  }

  // CSR 形式グラフの作成
  for (int i = 0; i < merged_conn_graph->n_vertex; i++)  //「抽出して足す」を繰り返す
  {
    // グローバル要素番号取得
    val = merged_conn_graph->vertex_id[i];
    // グローバル要素番号に対して二分探索→iとローカル番号がわかるので、要素を抽出できる
    monolis_bsearch_I(global_id_in_merged_graph, n_conn_vertex, 0, n_conn_vertex-1, val, &idx); // TODO 重複削除していないが、ソートはされているので二分探索使っても問題ない？
    iS = conn_graphs[which_conn_graph[idx]].index[local_id_in_conn_graph[idx]];
    iE = conn_graphs[which_conn_graph[idx]].index[local_id_in_conn_graph[idx] + 1];

    // edgeに追加
    n_edge = iE - iS;
    if(i != 0) {
      monolis_dealloc_I_2d(&edge, n_edge, 2); // i==0でこれ呼ぶとSegmentation fault起きる
    }
    edge = monolis_alloc_I_2d(edge, n_edge, 2);
    for (int j = 0; j < n_edge; j++) {
      edge[j][0] = i;
    }

    tmp1 = 0;
    for (int j = iS; j < iE; j++) {
      edge[tmp1][1] = conn_graphs[which_conn_graph[idx]].item[j];
      tmp1 += 1;
    }

    // edge の計算点番号を「ソートしていない本来の結合後ローカル番号」に変換
    for (int j = 0; j < n_edge; j++) {
      tmp1 = edge[j][1]; // 結合前ローカル番号
      val = nodal_graphs[which_conn_graph[idx]].vertex_id[tmp1];  //グローバル番号
      monolis_bsearch_I(nodal_vertex_id, n_nodal_vertex, 0, n_nodal_vertex-1, val, &tmp2);  // 結合後ソート後ローカル番号
      edge[j][1] = nodal_vertex_id_notsorted[tmp2];  // 結合後ソート前ローカル番号
    }

    gedatsu_graph_add_edge(merged_conn_graph, n_edge, edge, false);
  }
}

/** 物理量分布の結合 (実数型) */
void gedatsu_merge_distval_R(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_R* list_struct_R,
  int** merged_n_dof_list,
  double** merged_array_R)
{
  int i, j, k, n_vertex, val, idx, size_merged_array_R, iS, iE, jS, jE, len_i, len_j;
  int* perm = NULL;
  int* vertex_id = NULL;
  int* index = NULL;
  MONOLIS_LIST_I* merged_index = NULL;

  n_vertex = merged_graph->n_vertex;
  monolis_dealloc_I_1d(merged_n_dof_list);
  *merged_n_dof_list = monolis_alloc_I_1d(*merged_n_dof_list, n_vertex);

  // ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
  vertex_id = monolis_alloc_I_1d(vertex_id, n_vertex);
  for (int i = 0; i < n_vertex; i++)
  {
    vertex_id[i] = merged_graph->vertex_id[i];
  }
  perm = monolis_alloc_I_1d(perm, n_vertex);
  monolis_get_sequence_array_I(perm, n_vertex, 0, 1);
  monolis_qsort_I_2d(vertex_id, perm, n_vertex, 0, n_vertex-1);

  //  結合後グラフの情報を取得
  for (int i = 0; i < n_graphs; i++)
  {
    if (graphs[i].n_vertex != n_dof_list[i].n)
    {
      printf("*** graphs(i).n_vertex and n_dof_list(i).n don't match. ***");
      exit(1);
    }
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];
      (*merged_n_dof_list)[val] = n_dof_list[i].array[j];  // 指定した計算点の物理量の次元を上書き
    }
  }
  size_merged_array_R = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    size_merged_array_R += (*merged_n_dof_list)[i];
  }

  monolis_dealloc_R_1d(merged_array_R);
  *merged_array_R = monolis_alloc_R_1d(*merged_array_R, size_merged_array_R);

  // index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
  // 結合後グラフ
  val = n_vertex + 1;
  index = monolis_alloc_I_1d(index, val);
  j = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    j += (*merged_n_dof_list)[i];
    index[i+1] = j;
  }
  // 結合前グラフ
  merged_index = (MONOLIS_LIST_I *)calloc(n_graphs, sizeof(MONOLIS_LIST_I));
  monolis_list_initialize_I(merged_index, n_graphs);
  for (int i = 0; i < n_graphs; i++)
  {
    merged_index[i].n = graphs[i].n_vertex + 1;
    merged_index[i].array = monolis_alloc_I_1d(merged_index[i].array, merged_index[i].n);
    k = 0;
    for (int j = 0; j < graphs[i].n_vertex; j++)
    {
      k += n_dof_list[i].array[j];
      merged_index[i].array[j+1] = k;
    }
  }

  // index配列を用いて物理量分布の結合
  for (int i = 0; i < n_graphs; i++)
  {
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      if (n_dof_list[i].array[j] == 0) continue;  // TODO 必要ない？
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];  // 結合後ソート前ローカル番号
      iS = index[val];
      iE = index[val+1] - 1;
      jS = merged_index[i].array[j];
      jE = merged_index[i].array[j+1] - 1;
      len_i = iE - iS + 1;
      len_j = jE - jS + 1;
      if (len_i != len_j)
      {
        printf("*** index size and merged_index size don't match. *** \n");
        exit(1);
      }
      for (int k = 0; k < len_i; k++)
      {
        (*merged_array_R)[iS + k] = list_struct_R[i].array[jS + k];
      }
    }
  }

  monolis_dealloc_I_1d(&perm);
  monolis_dealloc_I_1d(&vertex_id);
  monolis_dealloc_I_1d(&index);
  monolis_list_finalize_I(merged_index, n_graphs);
}

/** 物理量分布の結合 (整数型) */
void gedatsu_merge_distval_I(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_I* list_struct_I,
  int** merged_n_dof_list,
  int** merged_array_I)
{
  int i, j, k, n_vertex, val, idx, size_merged_array_I, iS, iE, jS, jE, len_i, len_j;
  int* perm = NULL;
  int* vertex_id = NULL;
  int* index = NULL;
  MONOLIS_LIST_I* merged_index = NULL;

  n_vertex = merged_graph->n_vertex;
  monolis_dealloc_I_1d(merged_n_dof_list);
  *merged_n_dof_list = monolis_alloc_I_1d(*merged_n_dof_list, n_vertex);

  // ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
  vertex_id = monolis_alloc_I_1d(vertex_id, n_vertex);
  for (int i = 0; i < n_vertex; i++)
  {
    vertex_id[i] = merged_graph->vertex_id[i];
  }
  perm = monolis_alloc_I_1d(perm, n_vertex);
  monolis_get_sequence_array_I(perm, n_vertex, 0, 1);
  monolis_qsort_I_2d(vertex_id, perm, n_vertex, 0, n_vertex-1);

  //  結合後グラフの情報を取得
  for (int i = 0; i < n_graphs; i++)
  {
    if (graphs[i].n_vertex != n_dof_list[i].n)
    {
      printf("*** graphs(i).n_vertex and n_dof_list(i).n don't match. ***");
      exit(1);
    }
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];
      (*merged_n_dof_list)[val] = n_dof_list[i].array[j];  // 指定した計算点の物理量の次元を上書き
    }
  }
  size_merged_array_I = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    size_merged_array_I += (*merged_n_dof_list)[i];
  }

  monolis_dealloc_I_1d(merged_array_I);
  *merged_array_I = monolis_alloc_I_1d(*merged_array_I, size_merged_array_I);

  // index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
  // 結合後グラフ
  val = n_vertex + 1;
  index = monolis_alloc_I_1d(index, val);
  j = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    j += (*merged_n_dof_list)[i];
    index[i+1] = j;
  }
  // 結合前グラフ
  merged_index = (MONOLIS_LIST_I *)calloc(n_graphs, sizeof(MONOLIS_LIST_I));
  monolis_list_initialize_I(merged_index, n_graphs);
  for (int i = 0; i < n_graphs; i++)
  {
    merged_index[i].n = graphs[i].n_vertex + 1;
    merged_index[i].array = monolis_alloc_I_1d(merged_index[i].array, merged_index[i].n);
    k = 0;
    for (int j = 0; j < graphs[i].n_vertex; j++)
    {
      k += n_dof_list[i].array[j];
      merged_index[i].array[j+1] = k;
    }
  }

  // index配列を用いて物理量分布の結合
  for (int i = 0; i < n_graphs; i++)
  {
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      if (n_dof_list[i].array[j] == 0) continue;  // TODO 必要ない？
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];  // 結合後ソート前ローカル番号
      iS = index[val];
      iE = index[val+1] - 1;
      jS = merged_index[i].array[j];
      jE = merged_index[i].array[j+1] - 1;
      len_i = iE - iS + 1;
      len_j = jE - jS + 1;
      if (len_i != len_j)
      {
        printf("*** index size and merged_index size don't match. *** \n");
        exit(1);
      }
      for (int k = 0; k < len_i; k++)
      {
        (*merged_array_I)[iS + k] = list_struct_I[i].array[jS + k];
      }
    }
  }

  monolis_dealloc_I_1d(&perm);
  monolis_dealloc_I_1d(&vertex_id);
  monolis_dealloc_I_1d(&index);
  monolis_list_finalize_I(merged_index, n_graphs);
}

/** 物理量分布の結合 (複素数型) */
void gedatsu_merge_distval_C(
  const int n_graphs,
  const GEDATSU_GRAPH* graphs,
  const GEDATSU_GRAPH* merged_graph,
  const MONOLIS_LIST_I* n_dof_list,
  const MONOLIS_LIST_C* list_struct_C,
  int** merged_n_dof_list,
  double complex** merged_array_C)
{
  int i, j, k, n_vertex, val, idx, size_merged_array_C, iS, iE, jS, jE, len_i, len_j;
  int* perm = NULL;
  int* vertex_id = NULL;
  int* index = NULL;
  MONOLIS_LIST_I* merged_index = NULL;

  n_vertex = merged_graph->n_vertex;
  monolis_dealloc_I_1d(merged_n_dof_list);
  *merged_n_dof_list = monolis_alloc_I_1d(*merged_n_dof_list, n_vertex);

  // ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
  vertex_id = monolis_alloc_I_1d(vertex_id, n_vertex);
  for (int i = 0; i < n_vertex; i++)
  {
    vertex_id[i] = merged_graph->vertex_id[i];
  }
  perm = monolis_alloc_I_1d(perm, n_vertex);
  monolis_get_sequence_array_I(perm, n_vertex, 0, 1);
  monolis_qsort_I_2d(vertex_id, perm, n_vertex, 0, n_vertex-1);

  //  結合後グラフの情報を取得
  for (int i = 0; i < n_graphs; i++)
  {
    if (graphs[i].n_vertex != n_dof_list[i].n)
    {
      printf("*** graphs(i).n_vertex and n_dof_list(i).n don't match. ***");
      exit(1);
    }
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];
      (*merged_n_dof_list)[val] = n_dof_list[i].array[j];  // 指定した計算点の物理量の次元を上書き
    }
  }
  size_merged_array_C = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    size_merged_array_C += (*merged_n_dof_list)[i];
  }

  monolis_dealloc_C_1d(merged_array_C);
  *merged_array_C = monolis_alloc_C_1d(*merged_array_C, size_merged_array_C);

  // index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
  // 結合後グラフ
  val = n_vertex + 1;
  index = monolis_alloc_I_1d(index, val);
  j = 0;
  for (int i = 0; i < n_vertex; i++)
  {
    j += (*merged_n_dof_list)[i];
    index[i+1] = j;
  }
  // 結合前グラフ
  merged_index = (MONOLIS_LIST_I *)calloc(n_graphs, sizeof(MONOLIS_LIST_I));
  monolis_list_initialize_I(merged_index, n_graphs);
  for (int i = 0; i < n_graphs; i++)
  {
    merged_index[i].n = graphs[i].n_vertex + 1;
    merged_index[i].array = monolis_alloc_I_1d(merged_index[i].array, merged_index[i].n);
    k = 0;
    for (int j = 0; j < graphs[i].n_vertex; j++)
    {
      k += n_dof_list[i].array[j];
      merged_index[i].array[j+1] = k;
    }
  }

  // index配列を用いて物理量分布の結合
  for (int i = 0; i < n_graphs; i++)
  {
    for (int j = 0; j < graphs[i].n_vertex; j++)  // 結合前ローカル番号
    {
      if (n_dof_list[i].array[j] == 0) continue;  // TODO 必要ない？
      val = graphs[i].vertex_id[j];  // グローバル番号
      monolis_bsearch_I(vertex_id, n_vertex, 0, n_vertex-1, val, &idx);  // 結合後ソート後ローカル番号
      val = perm[idx];  // 結合後ソート前ローカル番号
      iS = index[val];
      iE = index[val+1] - 1;
      jS = merged_index[i].array[j];
      jE = merged_index[i].array[j+1] - 1;
      len_i = iE - iS + 1;
      len_j = jE - jS + 1;
      if (len_i != len_j)
      {
        printf("*** index size and merged_index size don't match. *** \n");
        exit(1);
      }
      for (int k = 0; k < len_i; k++)
      {
        (*merged_array_C)[iS + k] = list_struct_C[i].array[jS + k];
      }
    }
  }

  monolis_dealloc_I_1d(&perm);
  monolis_dealloc_I_1d(&vertex_id);
  monolis_dealloc_I_1d(&index);
  monolis_list_finalize_I(merged_index, n_graphs);
}
