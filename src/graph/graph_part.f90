!> グラフ分割モジュール
module mod_gedatsu_graph_part
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
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

!    allocate(graph%vertex_domain_id(graph%n_vertex), source = 1)
!    allocate(part_id(graph%n_vertex), source = 0)

    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)
!    call gedatsu_get_partitioned_graph(graph%n_vertex, graph%index, graph%item, n_domain, part_id)

!    do i = 1, graph%n_vertex
!      graph%vertex_domain_id(i) = part_id(i) + 1
!    enddo

!    deallocate(part_id)
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

!    allocate(graph%vertex_domain_id(graph%n_vertex), source = 1)
!    allocate(part_id(graph%n_vertex), source = 0)

!    call gedatsu_get_partitioned_graph(graph%n_vertex, graph%index, graph%item, n_domain, part_id)

!    do i = 1, graph%n_vertex
!      graph%vertex_domain_id(i) = part_id(i) + 1
!    enddo

!    deallocate(part_id)
  end subroutine gedatsu_graph_partition_with_weight
end module mod_gedatsu_graph_part
