!> グラフ分割モジュール（通信テーブル作成）
module mod_gedatsu_comm
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_mpi_util
  use mod_gedatsu_graph_handler
  implicit none

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
    integer(gint) :: n_size, i, ierr

    n_size = gedatsu_mpi_local_comm_size(comm)

    call mpi_allgather(n_internal_vertex, 1, MPI_INTEGER, vtxdist(2:n_size + 1), 1, MPI_INTEGER, comm, ierr)

    do i = 1, n_size
      vtxdist(i + 1) = vtxdist(i + 1) + vtxdist(i)
    enddo
  end subroutine gedatsu_comm_n_vertex_list
end module mod_gedatsu_comm
