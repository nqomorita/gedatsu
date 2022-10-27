!> グラフ分割モジュール
module mod_gedatsu_graph_repart
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_metis
  implicit none

contains

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みなし）
  subroutine gedatsu_graph_repartition(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !!> [out] 分割後の graph 構造体
    !type(gedatsu_graph), allocatable :: subgraphs(:)

    !call gedatsu_alloc_int_1d(graph%vertex_domain_id, graph%n_vertex)

    !call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)

    !call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_repartition

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みあり）
  subroutine gedatsu_graph_repartition_with_weight(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !!> [out] 分割後の graph 構造体
    !type(gedatsu_graph), allocatable :: subgraphs(:)

!    call gedatsu_alloc_int_1d(graph%vertex_domain_id, graph%n_vertex)
!
!    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)
!
!    allocate(subgraphs(n_domain))
!
!    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_repartition_with_weight
end module mod_gedatsu_graph_repart
