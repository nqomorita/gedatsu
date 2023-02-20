!> グラフ分割モジュール
!# gedatsu_graph_partition(graph, n_domain, subgraphs)
!# gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
!# gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)
!# gedatsu_get_parted_graph(graph, n_domain, subgraphs)
!# gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
!# gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
module mod_gedatsu_graph_part
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_metis
  implicit none

contains

  !> @ingroup graph_part
  !> グラフを分割する（節点重みなし）
  subroutine gedatsu_graph_partition(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(kint) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)

    call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)

    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)

    call gedatsu_check_vertex_domain_id(graph%n_vertex, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition

  !> @ingroup graph_part
  !> グラフを分割する（節点重みあり）
  subroutine gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(kint) :: n_domain
    !> [in] ノード重み
    integer(kint), allocatable :: node_wgt(:,:)
    !> [in] エッジ重み
    integer(kint), allocatable :: edge_wgt(:,:)
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)

    call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)

    call gedatsu_part_graph_metis_with_weight(graph%n_vertex, graph%index, graph%item, &
      & node_wgt, edge_wgt, n_domain, graph%vertex_domain_id)

    call gedatsu_check_vertex_domain_id(graph%n_vertex, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_with_weight

  !> @ingroup dev_graph_part
  !> 領域番号に従ってオーバーラップ領域を含めた分割グラフを取得
  subroutine gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)
    implicit none
    !> [in] ノード数
    integer(kint) :: n_vertex
    !> [in] 分割数
    integer(kint) :: n_domain
    !> [in] 領域分割番号
    integer(kint) :: vertex_domain_id(:)
    integer(kint) :: newlen

    call monolis_qsort_I_1d(vertex_domain_id, 1, n_vertex)

    call monolis_get_uniq_array_I(vertex_domain_id, n_vertex, newlen)

    if(newlen /= n_domain)then
      call monolis_std_error_string("gedatsu_check_vertex_domain_id")
      call monolis_std_error_string("domain which not has the vertex is found")
    endif
  end subroutine gedatsu_check_vertex_domain_id

  !> @ingroup dev_graph_part
  !> 領域番号に従ってオーバーラップ領域を含めた分割グラフを取得
  subroutine gedatsu_get_parted_graph(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(kint) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)
    integer(kint) :: i

    do i = 1, n_domain
      call gedatsu_get_parted_graph_main(graph, i, subgraphs(i))

      call gedatsu_add_overlapping_nodes(graph, i, subgraphs(i))
    enddo
  end subroutine gedatsu_get_parted_graph

  !> @ingroup dev_graph_part
  !> 領域番号 domain_id に属するオーバーラップ領域を含めない分割グラフを取得
  subroutine gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(kint) :: domain_id
    !> [out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph) :: subgraph
    integer(kint) :: n_vertex, n_edge
    integer(kint), allocatable :: edge(:,:)

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    if(n_vertex == 0) call monolis_std_warning_string("gedatsu_get_parted_graph_main")
    if(n_vertex == 0) call monolis_std_warning_string("n_vertex equals zero")

    subgraph%n_internal_vertex = n_vertex

    call gedatsu_graph_set_n_vertex(subgraph, n_vertex)

    !call gedatsu_graph_get_vertex_domain_id_in_internal_region(graph, domain_id, subgraph%vertex_domain_id)

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    if(n_edge == 0) return

    call monolis_alloc_I_1d(subgraph%item, n_edge)

    call monolis_alloc_I_2d(edge, 2, n_edge)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, subgraph%vertex_id)

    call gedatsu_graph_get_edge_in_internal_region(graph, domain_id, edge)

    call gedatsu_graph_set_edge(subgraph, n_edge, edge)
  end subroutine gedatsu_get_parted_graph_main

  !> @ingroup dev_graph_part
  !> 領域番号 domain_id に属するオーバーラップ領域をグラフ構造体に追加
  subroutine gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(kint) :: domain_id
    !> [out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph) :: subgraph
    integer(kint) :: n_vertex, n_edge
    integer(kint), allocatable :: OVL_vertex_id(:), edge(:,:)

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    if(n_vertex == 0) return

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)

    call monolis_alloc_I_1d(OVL_vertex_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, OVL_vertex_id)

    call gedatsu_graph_add_n_vertex_with_vertex_id(subgraph, n_vertex, OVL_vertex_id)

    if(n_edge == 0) return

    call monolis_alloc_I_2d(edge, 2, n_edge)

    call gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)

    call gedatsu_graph_add_edge(subgraph, n_edge, edge)
  end subroutine gedatsu_add_overlapping_nodes

  !> @ingroup dev_graph_part
  !> 通信テーブルを作成（逐次実行版）
  subroutine gedatsu_com_get_comm_table_serial(graph, n_domain, subgraphs, com)
    implicit none
    !> [in] 全体グラフ
    type(gedatsu_graph) :: graph
    !> [in] 領域分割数
    integer(kint) :: n_domain
    !> [in] 分割グラフ
    type(gedatsu_graph) :: subgraphs(:)
    !> [out] 分割領域に対応する com 構造体
    type(monolis_COM) :: com(:)
    !> 全ての外部節点番号
    integer(kint), allocatable :: outer_node_id_all(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable :: displs(:)
    integer(kint) :: i
    type(monolis_comm_node_list) :: recv_list

    call gedatsu_comm_get_all_external_node_serial(subgraphs, n_domain, outer_node_id_all, displs)

    call monolis_comm_get_all_external_node_domain_id_serial(graph%vertex_domain_id, n_domain, &
      & outer_node_id_all, outer_domain_id_all, displs)

!    call monolis_comm_get_recv_parallel(n_vertex, vertex_id, com, &
!      & outer_node_id_all, outer_domain_id_all, displs, recv_list)

!    call monolis_comm_get_send_parallel(n_vertex, vertex_id, com, recv_list)

    do i = 1, n_domain
!      call monolis_comm_get_recv_serial(graph, n_domain, i, subgraphs(i), com(i), recv_list)

!      !call monolis_comm_get_send_serial(graph, n_domain, i, subgraphs(i), com(i), recv_list)
    enddo
  end subroutine gedatsu_com_get_comm_table_serial

  subroutine gedatsu_comm_get_all_external_node_serial(subgraphs, n_domain, outer_node_id_all, displs)
    implicit none
    !> [in] 分割グラフ
    type(gedatsu_graph) :: subgraphs(:)
    !> [in] 領域分割数
    integer(kint) :: n_domain
    !> [in] 全ての外部節点番号
    integer(kint), allocatable :: outer_node_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable :: displs(:)
    integer(kint) :: i, j, in

    call monolis_alloc_I_1d(displs, n_domain + 1)

    do i = 1, n_domain
      displs(i + 1) = displs(i) + subgraphs(i)%n_vertex - subgraphs(i)%n_internal_vertex
    enddo

    call monolis_alloc_I_1d(outer_node_id_all, displs(n_domain + 1))

    in = 0
    do i = 1, n_domain
      do j = subgraphs(i)%n_internal_vertex + 1, subgraphs(i)%n_vertex
        in = in + 1
        outer_node_id_all(in) = subgraphs(i)%vertex_id(j)
      enddo
    enddo
  end subroutine gedatsu_comm_get_all_external_node_serial

  subroutine monolis_comm_get_all_external_node_domain_id_serial(vertex_domain_id, n_domain, &
    & outer_node_id_all, outer_domain_id_all, displs)
    implicit none
    !> [in] 節点の所属する領域番号（全節点数）
    integer(kint) :: vertex_domain_id(:)
    !> [in] 領域分割数
    integer(kint) :: n_domain
    !> [in] 全ての外部節点番号
    integer(kint) :: outer_node_id_all(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(kint) :: displs(:)
    integer(kint) :: i, j, in, id

    call monolis_alloc_I_1d(outer_domain_id_all, displs(n_domain + 1))

    do i = 1, displs(n_domain + 1)
      in = outer_node_id_all(i)
      id = vertex_domain_id(in)
      outer_domain_id_all(i) = id
    enddo
  end subroutine monolis_comm_get_all_external_node_domain_id_serial
end module mod_gedatsu_graph_part
