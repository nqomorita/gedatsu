!> グラフ分割モジュール
module mod_gedatsu_graph_repart
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_parmetis
  implicit none

contains

  !> @ingroup group_dlb
  !> グラフを再分割して再分割後に属する領域番号を取得（節点重みなし）
  subroutine gedatsu_graph_repartition(graph, COM)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint) :: n_part
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: vtxdist(:)

    n_part = monolis_mpi_get_local_comm_size(COM%comm)

    call monolis_dealloc_I_1d(graph%vertex_domain_id)
    call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)

    call monolis_alloc_I_1d(vtxdist, n_part + 1)
    call monolis_alloc_I_1d(vertex_id, graph%n_vertex)

    call monolis_com_n_vertex_list(graph%n_internal_vertex, COM%comm, vtxdist)

    call monolis_generate_global_vertex_id(graph%n_internal_vertex, graph%n_vertex, vertex_id, COM)

    call gedatsu_repart_graph_parmetis(graph%n_vertex, vertex_id, &
      & vtxdist, graph%index, graph%item, n_part, graph%vertex_domain_id, COM%comm)

    call monolis_mpi_update_I(COM, 1, graph%vertex_domain_id)
  end subroutine gedatsu_graph_repartition

  !> @ingroup group_dlb
  !> グラフを再分割して再分割後に属する領域番号を取得（節点重みあり）
  subroutine gedatsu_graph_repartition_with_weight(graph, COM, node_wgt, edge_wgt)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [in] ノード重み
    integer(kint), allocatable, intent(in) :: node_wgt(:,:)
    !> [in] エッジ重み
    integer(kint), allocatable, intent(in) :: edge_wgt(:,:)
    integer(kint) :: n_part
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: vtxdist(:)

    n_part = monolis_mpi_get_local_comm_size(COM%comm)

    call monolis_dealloc_I_1d(graph%vertex_domain_id)
    call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)

    call monolis_alloc_I_1d(vtxdist, n_part + 1)
    call monolis_alloc_I_1d(vertex_id, graph%n_vertex)

    call monolis_com_n_vertex_list(graph%n_internal_vertex, COM%comm, vtxdist)

    call monolis_generate_global_vertex_id(graph%n_internal_vertex, graph%n_vertex, vertex_id, COM)

    call gedatsu_repart_graph_parmetis_with_weight(graph%n_vertex, vertex_id, &
      & vtxdist, graph%index, graph%item, node_wgt, edge_wgt, n_part, graph%vertex_domain_id, COM%comm)

    call monolis_mpi_update_I(COM, 1, graph%vertex_domain_id)
  end subroutine gedatsu_graph_repartition_with_weight
end module mod_gedatsu_graph_repart
