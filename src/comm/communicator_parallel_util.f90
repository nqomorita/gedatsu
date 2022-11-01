!> 通信テーブル作成モジュール（並列実行版）
module mod_gedatsu_communicator_parallel_util
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_comm
  use mod_gedatsu_std
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_mpi
  use mod_gedatsu_mpi_util
  use mod_gedatsu_graph_handler
  use mod_gedatsu_std
  implicit none

contains

  subroutine gedatsu_generate_global_vertex_id(n_vertex, vtxdist, vertex_id, comm)
    implicit none
    !> [in] ノード数
    integer(gint) :: n_vertex
    !> [in] 領域ごとのノード数を示す配列
    integer(gint) :: vtxdist(:)
    !> [out] グローバルノード番号配列
    integer(gint) :: vertex_id(:)
    !> [in] comm 構造体
    type(gedatsu_comm) :: comm
    integer(gint) :: my_rank, i, origin

    my_rank = gedatsu_mpi_global_my_rank()
    origin = vtxdist(my_rank + 1)

    do i = 1, n_vertex
      vertex_id(i) = origin + i
    enddo

    call gedatsu_SendRecv_I(comm%send_n_neib, comm%send_neib_pe, comm%recv_n_neib, comm%recv_neib_pe, &
    & comm%send_index, comm%send_item, comm%recv_index, comm%recv_item, &
    & vertex_id, 1, gedatsu_mpi_global_comm())
  end subroutine gedatsu_generate_global_vertex_id
end module mod_gedatsu_communicator_parallel_util
