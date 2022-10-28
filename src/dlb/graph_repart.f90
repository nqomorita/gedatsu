!> グラフ分割モジュール
module mod_gedatsu_graph_repart
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_parmetis
  implicit none

contains

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みなし）
  subroutine gedatsu_graph_repartition(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: n_part
    integer(gint) :: comm
    integer(gint), allocatable :: vtxdist(:)
    integer(gint), allocatable :: vertex_domain_id(:)

    comm = gedatsu_mpi_global_comm()

    n_part = gedatsu_mpi_global_comm_size()

    call gedatsu_alloc_int_1d(vertex_domain_id, graph%n_vertex)

    call gedatsu_alloc_int_1d(vtxdist, n_part + 1)

    call gedatsu_repart_graph_parmetis(graph%n_vertex, graph%vertex_id, &
      & vtxdist, graph%index, graph%item, n_part, vertex_domain_id, comm)
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
