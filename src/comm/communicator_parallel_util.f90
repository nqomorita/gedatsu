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

  !> 全ての外部節点を取得
  subroutine gedatsu_comm_get_all_external_node(graph, comm, outer_node_id_all, displs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm
    !> [out] 全ての外部節点番号
    integer(gint), allocatable :: outer_node_id_all(:)
    integer(gint) :: N, NP, M, comm_size, n_outer, i
    integer(gint), allocatable :: counts(:)
    integer(gint), allocatable :: displs(:)
    integer(gint), allocatable :: outer_node_id_local(:)

    N = graph%n_internal_vertex
    NP = graph%n_vertex
    M = NP - N
    comm_size = gedatsu_mpi_local_comm_size(comm%comm)

    !> 個数の共有
    call gedatsu_alloc_int_1d(counts, comm_size)
    call gedatsu_allgather_I1(M, counts, comm%comm)

    !> MPI 通信用 displs 配列の作成
    call gedatsu_alloc_int_1d(displs, comm_size + 1)
    call gedatsu_alloc_int_1d(outer_node_id_local, M)

    do i = N + 1, NP
      outer_node_id_local(i - N) = graph%vertex_id(i)
    enddo

    do i = 1, comm_size
      displs(i + 1) = displs(i) + counts(i)
    enddo

    !> 全ての外部節点を取得
    n_outer = displs(comm_size + 1)
    call gedatsu_alloc_int_1d(outer_node_id_all, n_outer)

    call gedatsu_allgatherv_I(M, outer_node_id_local, outer_node_id_all, counts, displs, comm%comm)
  end subroutine gedatsu_comm_get_all_external_node

  !> 全ての外部節点を取得
  subroutine gedatsu_comm_get_all_external_node_domain_id(graph, comm, outer_node_id_all, outer_domain_id_all, displs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm
    !> [in] 全ての外部節点番号
    integer(gint) :: outer_node_id_all(:)
    !> [out] 全ての外部節点が属する領域番号
    integer(gint), allocatable :: outer_domain_id_all(:)
    integer(gint) :: displs(:)
    integer(gint) :: N, comm_size, n_outer, my_rank
    integer(gint) :: i, j, jS, jE, id, idx
    integer(gint), allocatable :: internal_node_id(:)

    N = graph%n_internal_vertex
    my_rank = gedatsu_mpi_global_my_rank()
    comm_size = gedatsu_mpi_local_comm_size(comm%comm)
    n_outer = displs(comm_size + 1)

    !> 外点が属する領域番号を取得
    call gedatsu_alloc_int_1d(internal_node_id, N)

    do i = 1, N
      internal_node_id(i) = graph%vertex_id(i)
    enddo

    call gedatsu_qsort_int_1d(internal_node_id, 1, N)

    call gedatsu_alloc_int_1d(outer_domain_id_all, n_outer)
    outer_domain_id_all(:) = comm_size + 1

    aa:do i = 1, comm_size
      !> 自領域であればスキップ
      if(i == my_rank + 1) cycle
      !> 他領域の外点と自領域の内点が重複するか判定
      jS = displs(i) + 1
      jE = displs(i + 1)
      do j = jS, jE
        id = outer_node_id_all(j)
        call gedatsu_bsearch_int(internal_node_id, 1, N, id, idx)
        if(idx /= -1)then
          outer_domain_id_all(j) = my_rank
        endif
      enddo
    enddo aa

    call gedatsu_allreduce_I(n_outer, outer_domain_id_all, gedatsu_mpi_min, comm%comm)
  end subroutine gedatsu_comm_get_all_external_node_domain_id


end module mod_gedatsu_communicator_parallel_util
