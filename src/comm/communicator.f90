!> グラフ分割モジュール（通信テーブル作成）
module mod_gedatsu_communicator
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_comm
  use mod_gedatsu_std
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_mpi
  use mod_gedatsu_mpi_util
  use mod_gedatsu_graph_handler
  use mod_gedatsu_communicator_serial_util
  use mod_gedatsu_communicator_parallel_util
  implicit none

  public :: gedatsu_comm_n_vertex_list
  public :: gedatsu_comm_get_comm_table_serial
  public :: gedatsu_comm_get_comm_table_parallel

contains

  !> 各領域の内部節点数リスト vtxdist を作成
  subroutine gedatsu_comm_n_vertex_list(n_internal_vertex, comm, vtxdist)
    implicit none
    !> [in] 分割領域の内部節点数
    integer(gint) :: n_internal_vertex
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    !> [out] 各領域の内部節点数リスト
    integer(gint) :: vtxdist(:)
    integer(gint) :: n_size, i

    n_size = gedatsu_mpi_local_comm_size(comm)

    call gedatsu_allgather_I1(n_internal_vertex, vtxdist(2:n_size + 1), comm)

    do i = 1, n_size
      vtxdist(i + 1) = vtxdist(i + 1) + vtxdist(i)
    enddo
  end subroutine gedatsu_comm_n_vertex_list

  !> グラフの通信テーブルを作成（逐次実行版）
  subroutine gedatsu_comm_get_comm_table_serial(graph, subgraphs, n_domain, comms)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comms(:)
    type(dedatsu_comm_node_list), allocatable :: send_list(:)

    call gedatsu_comm_get_recv_neib_domain_serial(graph, subgraphs, n_domain, comms)

    call gedatsu_comm_get_recv_serial(graph, subgraphs, n_domain, comms)

    call gedatsu_comm_get_send_list_serial(subgraphs, n_domain, comms, send_list)

    call gedatsu_comm_get_send_serial(subgraphs, n_domain, comms, send_list)
  end subroutine gedatsu_comm_get_comm_table_serial

  !> グラフの通信テーブルを作成（並列実行版）
  subroutine gedatsu_comm_get_comm_table_parallel(graph, comm)
    implicit none
    !> [in] 分割領域に対応する graph 構造体
    type(gedatsu_graph) :: graph
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm
    !> 全ての外部節点番号
    integer(gint), allocatable :: outer_node_id_all(:)
    !> 全ての外部節点が属する領域番号
    integer(gint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(gint), allocatable :: displs(:)

    call gedatsu_comm_get_all_external_node(graph, comm, outer_node_id_all, displs)

    call gedatsu_comm_get_all_external_node_domain_id(graph, comm, outer_node_id_all, outer_domain_id_all, displs)

    !call gedatsu_comm_get_recv_neib_domain_parallel(graph, subgraphs, n_domain, comms)

    !call gedatsu_comm_get_recv_parallel(graph, subgraphs, n_domain, comms)

    !call gedatsu_comm_get_send_list_parallel(subgraphs, n_domain, comms, send_list)

    !call gedatsu_comm_get_send_parallel(subgraphs, n_domain, comms, send_list)
  end subroutine gedatsu_comm_get_comm_table_parallel
end module mod_gedatsu_communicator
