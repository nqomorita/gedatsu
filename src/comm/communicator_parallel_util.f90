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
  use mod_gedatsu_communicator_serial_util
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
    !> 全ての外部節点配列の各領域に属する節点数
    integer(gint), allocatable :: displs(:)
    integer(gint) :: N, NP, M, comm_size, n_outer, i
    integer(gint), allocatable :: counts(:)
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
    !> 全ての外部節点配列の各領域に属する節点数
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

  !> データ通信する recv 隣接領域の取得（並列実行版）
  subroutine gedatsu_comm_get_recv_parallel(graph, comm, outer_node_id_all, outer_domain_id_all, displs, recv_list)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in,out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm
    !> [in] 全ての外部節点番号
    integer(gint) :: outer_node_id_all(:)
    !> [in] 全ての外部節点が属する領域番号
    integer(gint) :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(gint) :: displs(:)
    !> [out] 分割領域に対応する recv list 構造体
    type(dedatsu_comm_node_list), allocatable :: recv_list(:)

    integer(gint) :: i, in, j, jS, jE, id, comm_size, my_rank
    integer(gint) :: NP, n_neib_recv, recv_rank, n_data, global_id, idx
    integer(gint), allocatable :: is_neib(:)
    integer(gint), allocatable :: local_nid(:)
    integer(gint), allocatable :: neib_id(:)
    integer(gint), allocatable :: temp(:)

    !> 隣接領域の取得
    my_rank = gedatsu_mpi_global_my_rank()
    comm_size = gedatsu_mpi_local_comm_size(comm%comm)
    call gedatsu_alloc_int_1d(is_neib, comm_size)

    in = my_rank + 1
    jS = displs(in) + 1
    jE = displs(in + 1)
    do j = jS, jE
      id = outer_domain_id_all(j)
      is_neib(id + 1) = 1
    enddo
    is_neib(my_rank + 1) = 0

    n_neib_recv = 0
    do i = 1, comm_size
      if(is_neib(i) == 1) n_neib_recv = n_neib_recv + 1
    enddo

    call gedatsu_alloc_int_1d(neib_id, n_neib_recv)

    j = 0
    do i = 1, comm_size
      if(is_neib(i) == 1)then
        j = j + 1
        neib_id(j) = i - 1
      endif
    enddo

    !> recv の作成
    NP = graph%n_vertex
    allocate(recv_list(n_neib_recv))
    call gedatsu_alloc_int_1d(local_nid, NP)
    call gedatsu_alloc_int_1d(temp, NP)

    temp(:) = graph%vertex_id(:)
    do i = 1, NP
      local_nid(i) = i
    enddo

    call gedatsu_qsort_int_1d_with_perm(temp, 1, NP, local_nid)

    do i = 1, n_neib_recv
      recv_rank = neib_id(i)
      in = my_rank + 1
      jS = displs(in) + 1
      jE = displs(in + 1)

      n_data = 0
      do j = jS, jE
        id = outer_domain_id_all(j)
        if(recv_rank == id)then
          n_data = n_data + 1
        endif
      enddo

      recv_list(i)%n_node = n_data
      recv_list(i)%domid = recv_rank
      allocate(recv_list(i)%global_id(n_data), source = 0)

      n_data = 0
      do j = jS, jE
        id = outer_domain_id_all(j)
        if(recv_rank == id)then
          n_data = n_data + 1
          global_id = outer_node_id_all(j)
          call gedatsu_bsearch_int(temp, 1, NP, global_id, idx)
          recv_list(i)%global_id(n_data) = local_nid(idx)
        endif
      enddo
    enddo

    !> gedatsu com の構築
    !> recv
    comm%recv_n_neib = n_neib_recv
    call gedatsu_alloc_int_1d(comm%recv_neib_pe, n_neib_recv)

    do i = 1, n_neib_recv
      comm%recv_neib_pe(i) = recv_list(i)%domid(1)
    enddo

    call gedatsu_alloc_int_1d(comm%recv_index, n_neib_recv + 1)

    do i = 1, n_neib_recv
      comm%recv_index(i + 1) = comm%recv_index(i) + recv_list(i)%n_node
    enddo

    in = comm%recv_index(n_neib_recv + 1)

    call gedatsu_alloc_int_1d(comm%recv_item, in)

    in = 0
    do i = 1, n_neib_recv
      jE = recv_list(i)%n_node
      do j = 1, jE
        in = in + 1
        idx = recv_list(i)%global_id(j)
        comm%recv_item(in) = idx
      enddo
    enddo
  end subroutine gedatsu_comm_get_recv_parallel

  !> データ通信する send 隣接領域の取得（並列実行版）
  subroutine gedatsu_comm_get_send_parallel(graph, comm, outer_node_id_all, outer_domain_id_all, displs, recv_list, send_list)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in,out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm
    !> [in] 全ての外部節点番号
    integer(gint) :: outer_node_id_all(:)
    !> [in] 全ての外部節点が属する領域番号
    integer(gint) :: outer_domain_id_all(:)
    !> [in] 全ての外部節点配列の各領域に属する節点数
    integer(gint) :: displs(:)
    !> [out] 分割領域に対応する send list 構造体
    type(dedatsu_comm_node_list) :: recv_list(:)
    !> [out] 分割領域に対応する send list 構造体
    type(dedatsu_comm_node_list), allocatable :: send_list(:)

    integer(gint) :: i, in, j, jS, jE, id, comm_size, my_rank
    integer(gint) :: NP, n_neib_recv, recv_rank, n_data, global_id, idx, ierr
    integer(gint) :: n_neib_send
    integer(gint), allocatable :: send_n_list(:)
    integer(gint), allocatable :: is_neib(:)
    integer(gint), allocatable :: local_nid(:)
    integer(gint), allocatable :: neib_id(:)
    integer(gint), allocatable :: temp(:)
    integer(gint), allocatable :: sta1(:,:)
    integer(gint), allocatable :: sta2(:,:)
    integer(gint), allocatable :: req1(:)
    integer(gint), allocatable :: req2(:)
    integer(gint), allocatable :: wr(:)
    integer(gint), allocatable :: ws(:)

    !> send の作成
    !> slave から master に個数を送信
    allocate(send_n_list(comm_size), source = 0)

    do i = 1, n_neib_recv
      id = recv_list(i)%domid(1)
      in = recv_list(i)%n_node
      send_n_list(id + 1) = in
    enddo

    call mpi_alltoall(send_n_list, 1, MPI_INTEGER, &
      send_n_list, 1, MPI_INTEGER, comm, ierr)

    !> send 個数の確保
    n_neib_send = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0) n_neib_send = n_neib_send + 1
    enddo

    allocate(send_list(n_neib_send))

    n_neib_send = 0
    do i = 1, comm_size
      if(send_n_list(i) > 0)then
        n_neib_send = n_neib_send + 1
        send_list(n_neib_send)%domid = i - 1
        n_data = send_n_list(i)
        send_list(n_neib_send)%n_node = n_data
        allocate(send_list(n_neib_send)%global_id(n_data), source = 0)
      endif
    enddo

    !> send
    comm%send_n_neib = n_neib_send
    allocate(comm%send_neib_pe(n_neib_send), source = 0)
    do i = 1, n_neib_send
      comm%send_neib_pe(i) = send_list(i)%domid(1)
    enddo
    allocate(comm%send_index(0:n_neib_send), source = 0)
    do i = 1, n_neib_send
      comm%send_index(i) = comm%send_index(i-1) + send_list(i)%n_node
    enddo
    in = comm%send_index(n_neib_send)
    allocate(comm%send_item(in), source = 0)

    !> slave から master に global_nid を送信
    allocate(sta1(gedatsu_mpi_status_size,comm%recv_n_neib))
    allocate(sta2(gedatsu_mpi_status_size,comm%send_n_neib))
    allocate(req1(comm%recv_n_neib))
    allocate(req2(comm%send_n_neib))

    in = comm%recv_index(n_neib_recv)
    allocate(ws(in), source = 0)

    do i = 1, comm%recv_n_neib
      id = recv_list(i)%domid(1)
      in = recv_list(i)%n_node
      jS = comm%recv_index(i-1) + 1
      jE = comm%recv_index(i)
      do j = jS, jE
        idx = comm%recv_item(j)
        ws(j) = graph%vertex_id(idx)
      enddo
      call MPI_Isend(ws(jS:jE), in, MPI_INTEGER, id, 0, comm%comm, req1(i), ierr)
    enddo

    in = comm%send_index(n_neib_send)
    allocate(wr(in), source = 0)
    do i = 1, comm%send_n_neib
      id = send_list(i)%domid(1)
      in = send_list(i)%n_node
      jS = comm%send_index(i-1) + 1
      jE = comm%send_index(i)
      call MPI_Irecv(wr(jS:jE), in, MPI_INTEGER, id, 0, comm%comm, req2(i), ierr)
    enddo

    call MPI_waitall(comm%recv_n_neib, req2, sta2, ierr)
    call MPI_waitall(comm%send_n_neib, req1, sta1, ierr)

    !> local_nid に変換
    in = comm%send_index(n_neib_send)
    do i = 1, in
      !call monolis_bsearch_int(temp, 1, NP, wr(i), id)
      comm%send_item(i) = local_nid(id)
    enddo
  end subroutine gedatsu_comm_get_send_parallel
end module mod_gedatsu_communicator_parallel_util
