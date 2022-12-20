!> グラフ分割モジュール
module mod_gedatsu_graph_repart
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_graph_handler
  use mod_gedatsu_communicator
  use mod_gedatsu_communicator_parallel_util
  use mod_gedatsu_wrapper_parmetis
  implicit none

contains

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みなし）
  subroutine gedatsu_graph_repartition(graph, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] comm 構造体
    type(gedatsu_comm) :: comm
    integer(gint) :: mpi_comm
    integer(gint) :: n_part
    integer(gint), allocatable :: vtxdist(:)
    integer(gint), allocatable :: vertex_id(:)

    mpi_comm = gedatsu_mpi_global_comm()

    n_part = gedatsu_mpi_global_comm_size()

    call gedatsu_alloc_int_1d(vertex_id, graph%n_vertex)
    call gedatsu_realloc_int_1d(graph%vertex_domain_id, graph%n_vertex)
    call gedatsu_alloc_int_1d(vtxdist, n_part + 1)

    call gedatsu_comm_n_vertex_list(graph%n_internal_vertex, mpi_comm, vtxdist)

    call gedatsu_generate_global_vertex_id(graph%n_vertex, vtxdist, vertex_id, comm)

    call gedatsu_repart_graph_parmetis(graph%n_vertex, vertex_id, &
      & vtxdist, graph%index, graph%item, n_part, graph%vertex_domain_id, mpi_comm)

    call gedatsu_update_vertex_domain_id(graph%vertex_domain_id, comm)
  end subroutine gedatsu_graph_repartition

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みあり）
  subroutine gedatsu_graph_repartition_with_weight(graph, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] comm 構造体
    type(gedatsu_comm) :: comm
    integer(gint) :: mpi_comm
    integer(gint) :: n_part
    integer(gint), allocatable :: vtxdist(:)
    integer(gint), allocatable :: vertex_id(:)
    integer(gint), allocatable :: node_wgt(:,:)
    integer(gint), allocatable :: edge_wgt(:,:)

    mpi_comm = gedatsu_mpi_global_comm()

    n_part = gedatsu_mpi_global_comm_size()

    call gedatsu_alloc_int_1d(vertex_id, graph%n_vertex)
    call gedatsu_realloc_int_1d(graph%vertex_domain_id, graph%n_vertex)
    call gedatsu_alloc_int_1d(vtxdist, n_part + 1)

    call gedatsu_comm_n_vertex_list(graph%n_internal_vertex, mpi_comm, vtxdist)

    call gedatsu_generate_global_vertex_id(graph%n_vertex, vtxdist, vertex_id, comm)

    call gedatsu_repart_graph_parmetis_with_weight(graph%n_vertex, vertex_id, &
      & vtxdist, graph%index, graph%item, node_wgt, edge_wgt, n_part, graph%vertex_domain_id, mpi_comm)
  end subroutine gedatsu_graph_repartition_with_weight
end module mod_gedatsu_graph_repart
