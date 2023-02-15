!> グラフ分割モジュール
module mod_gedatsu_graph_repart
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_parmetis
  implicit none

contains

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みなし）
  subroutine gedatsu_graph_repartition(graph, COM)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    integer(kint) :: n_part
    integer(kint), allocatable :: vtxdist(:)
    integer(kint), allocatable :: vertex_id(:)

    n_part = monolis_mpi_local_comm_size(COM%comm)

    call monolis_alloc_I_1d(vertex_id, graph%n_vertex)

    call monolis_alloc_I_1d(vtxdist, n_part + 1)

    !call gedatsu_comm_n_vertex_list(graph%n_internal_vertex, COM%comm, vtxdist)

    !call gedatsu_generate_global_vertex_id(graph%n_vertex, vtxdist, vertex_id, COM)

    !call gedatsu_repart_graph_parmetis(graph%n_vertex, vertex_id, &
    !  & vtxdist, graph%index, graph%item, n_part, graph%vertex_domain_id, COM%comm)

    !call gedatsu_update_vertex_domain_id(graph%vertex_domain_id, COM)
  end subroutine gedatsu_graph_repartition

  !> @ingroup group_dlb
  !> グラフを再分割する（節点重みあり）
  subroutine gedatsu_graph_repartition_with_weight(graph, COM)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] COM 構造体
    type(monolis_COM) :: COM
    integer(kint) :: n_part
    integer(kint), allocatable :: vtxdist(:)
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: node_wgt(:,:)
    integer(kint), allocatable :: edge_wgt(:,:)

    n_part = monolis_mpi_local_comm_size(COM%comm)

    call monolis_alloc_I_1d(vertex_id, graph%n_vertex)

    call monolis_alloc_I_1d(vtxdist, n_part + 1)

    !call gedatsu_comm_n_vertex_list(graph%n_internal_vertex, COM%comm, vtxdist)

    !call gedatsu_generate_global_vertex_id(graph%n_vertex, vtxdist, vertex_id, COM)

    !call gedatsu_repart_graph_parmetis_with_weight(graph%n_vertex, vertex_id, &
    !  & vtxdist, graph%index, graph%item, node_wgt, edge_wgt, n_part, graph%vertex_domain_id, COM%comm)
  end subroutine gedatsu_graph_repartition_with_weight
end module mod_gedatsu_graph_repart
