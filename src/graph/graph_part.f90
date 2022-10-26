!> グラフ分割モジュール
module mod_gedatsu_graph_part
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_metis
  implicit none

contains

  !> @ingroup group_graph_4
  !> グラフを分割する（節点重みなし）
  subroutine gedatsu_graph_partition(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), allocatable :: subgraphs(:)

    call gedatsu_alloc_int_1d(graph%vertex_domain_id, graph%n_vertex)

    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition

  !> @ingroup group_graph_4
  !> グラフを分割する（節点重みあり）
  subroutine gedatsu_graph_partition_with_weight(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), allocatable :: subgraphs(:)

    call gedatsu_alloc_int_1d(graph%vertex_domain_id, graph%n_vertex)

    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)

    allocate(subgraphs(n_domain))

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_with_weight

  !> 領域番号に従ってオーバーラップ領域を含めた分割グラフを取得
  subroutine gedatsu_get_parted_graph(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), allocatable :: subgraphs(:)
    integer(gint) :: i

    do i = 1, n_domain
      call gedatsu_get_parted_graph_main(graph, i-1, subgraphs(i))

      call gedatsu_add_overlapping_nodes(graph, i-1, subgraphs(i))
    enddo
  end subroutine gedatsu_get_parted_graph

  !> 領域番号 domain_id に属するオーバーラップ領域を含めない分割グラフを取得
  subroutine gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph) :: subgraph
    integer(gint) :: n_vertex, n_edge
    integer(gint), allocatable :: edge(:,:)

    call gedatsu_graph_get_n_vertex_in_subdomain(graph, domain_id, n_vertex)

    if(n_vertex == 0) call gedatsu_warning_string("gedatsu_get_parted_graph_main")
    if(n_vertex == 0) call gedatsu_warning_string("n_vertex equals zero")

    call gedatsu_graph_get_n_edge_in_subdomain(graph, domain_id, n_edge)

    call gedatsu_graph_set_n_vertex(subgraph, n_vertex)

    call gedatsu_alloc_int_1d(subgraph%item, n_edge)
    call gedatsu_alloc_int_2d(edge, 2, n_edge)

    call gedatsu_graph_get_vertex_id_in_subdomain(graph, domain_id, subgraph%vertex_id)

    call gedatsu_graph_get_edge_in_subdomain(graph, domain_id, edge)

    call gedatsu_graph_set_edge(subgraph, n_edge, edge)
  end subroutine gedatsu_get_parted_graph_main

  !> 領域番号 domain_id に属するオーバーラップ領域をグラフ構造体に追加
  subroutine gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph) :: subgraph
    integer(gint) :: n_vertex, n_edge
    integer(gint), allocatable :: OVL_vertex_id(:), edge(:,:)

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)

    call gedatsu_alloc_int_1d(OVL_vertex_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, OVL_vertex_id)

    call gedatsu_alloc_int_2d(edge, 2, n_edge)

    call gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)

    call gedatsu_graph_add_n_vertex_with_vertex_id(subgraph, n_vertex, OVL_vertex_id)

    call gedatsu_graph_add_edge(subgraph, n_edge, edge)
  end subroutine gedatsu_add_overlapping_nodes
end module mod_gedatsu_graph_part
