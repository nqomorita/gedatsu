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
  use mod_gedatsu_communicator_util
  implicit none

  public :: gedatsu_comm_n_vertex_list
  !public :: gedatsu_comm_get_comm_table
  public :: gedatsu_comm_get_comm_table_serial

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

  !> グラフの通信テーブルを作成
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

    call gedatsu_comm_get_recv_neib_domain_serial(graph, subgraphs, n_domain, comms)

    call gedatsu_comm_get_recv_serial(graph, subgraphs, n_domain, comms)

    call gedatsu_comm_get_send_neib_domain_serial(n_domain, comms)

    call gedatsu_comm_get_send_serial(graph, subgraphs, n_domain, comms)
  end subroutine gedatsu_comm_get_comm_table_serial

!  !> グラフの通信テーブルを作成
!  subroutine gedatsu_comm_get_comm_table(graph, n_internal_vertex, comm)
!    implicit none
!    !> [in] graph 構造体
!    type(gedatsu_graph) :: graph
!    !> [in] 分割領域の内部節点数
!    integer(gint) :: n_internal_vertex
!    !> [in] MPI コミュニケータ
!    integer(gint) :: comm
!
!    type(dedatsu_comm_node_list), allocatable :: send_list(:)
!    type(dedatsu_comm_node_list), allocatable :: recv_list(:)
!    integer(gint) :: N, NP
!    integer(gint) :: M, n_outer, myrank, commsize, ierr
!    integer(gint) :: i, in, id, idx, j, jS, jE, recv_rank, ns, nr
!    integer(gint) :: n_neib_recv, n_neib_send, local_id, global_id, n_data
!    integer(gint), allocatable :: counts(:), outer_node_id_local(:), local_nid(:)
!    integer(gint), allocatable :: outer_node_id_all(:), outer_dom_id_all(:), temp(:)
!    integer(gint), allocatable :: displs(:), internal_node_id(:), is_neib(:), neib_id(:)
!    integer(gint), allocatable :: send_n_list(:)
!    integer(gint), allocatable :: ws(:), wr(:)
!    integer(gint), allocatable :: sta1(:,:)
!    integer(gint), allocatable :: sta2(:,:)
!    integer(gint), allocatable :: req1(:)
!    integer(gint), allocatable :: req2(:)
!
!    myrank = gedatsu_mpi_local_my_rank(comm)
!    commsize = gedatsu_mpi_local_comm_size(comm)
!
!    N = graph%n_internal_vertex
!    NP = graph%n_vertex
!    M = NP - N
!
!    !> 外点を全体で共有
!    allocate(counts(commsize), source = 0)
!
!    call mpi_allgather(M, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, comm, ierr)
!
!    allocate(outer_node_id_local(M), source = 0)
!    allocate(displs(commsize + 1), source = 0)
!
!    do i = N + 1, NP
!      outer_node_id_local(i-N) = graph%vertex_id(i)
!    enddo
!
!    do i = 1, commsize
!      displs(i + 1) = displs(i) + counts(i)
!    enddo
!
!    n_outer = displs(commsize + 1)
!    allocate(outer_node_id_all(n_outer), source = 0)
!
!    call mpi_allgatherv(outer_node_id_local, M, MPI_INTEGER, &
!      outer_node_id_all, counts, displs, MPI_INTEGER, comm, ierr)
!
!    !> 外点が属する領域番号を取得
!    allocate(internal_node_id(N), source = 0)
!    do i = 1, N
!      internal_node_id(i) = graph%vertex_id(i)
!    enddo
!    call gedatsu_qsort_int_1d(internal_node_id, 1, N)
!
!    allocate(outer_dom_id_all(n_outer), source = 0)
!    outer_dom_id_all(:) = commsize + 1
!
!    aa:do i = 1, commsize
!      !> 自領域であればスキップ
!      if(i == myrank + 1) cycle
!      !> 他領域の外点と自領域の内点が重複するか判定
!      jS = displs(i) + 1
!      jE = displs(i + 1)
!      do j = jS, jE
!        id = outer_node_id_all(j)
!        call gedatsu_bsearch_int(internal_node_id, 1, N, id, idx)
!        if(idx /= -1)then
!          outer_dom_id_all(j) = myrank
!        endif
!      enddo
!    enddo aa
!
!    !> reduce に修正して効率化可能
!!    call monolis_allreduce_I(n_outer, outer_dom_id_all, monolis_min, monolis%COM%comm)
!
!    !> 隣接領域の取得
!    allocate(is_neib(commsize), source = 0)
!
!    in = myrank + 1
!    jS = displs(in) + 1
!    jE = displs(in + 1)
!    do j = jS, jE
!      id = outer_dom_id_all(j)
!      is_neib(id + 1) = 1
!    enddo
!    is_neib(myrank + 1) = 0
!
!    n_neib_recv = 0
!    do i = 1, commsize
!      if(is_neib(i) == 1) n_neib_recv = n_neib_recv + 1
!    enddo
!
!    allocate(neib_id(n_neib_recv), source = 0)
!
!    j = 0
!    do i = 1, commsize
!      if(is_neib(i) == 1)then
!        j = j + 1
!        neib_id(j) = i - 1
!      endif
!    enddo
!
!    !> recv の作成
!    allocate(recv_list(n_neib_recv))
!    allocate(local_nid(NP), source = 0)
!    allocate(temp(NP), source = 0)
!
!    temp(:) = graph%vertex_id(:)
!    do i = 1, NP
!      local_nid(i) = i
!    enddo
!
!    call gedatsu_qsort_int_1d_with_perm(temp, 1, NP, local_nid)
!
!    do i = 1, n_neib_recv
!      recv_rank = neib_id(i)
!      in = myrank + 1
!      jS = displs(in) + 1
!      jE = displs(in + 1)
!
!      n_data = 0
!      do j = jS, jE
!        id = outer_dom_id_all(j)
!        if(recv_rank == id)then
!          n_data = n_data + 1
!        endif
!      enddo
!
!      recv_list(i)%nnode = n_data
!      recv_list(i)%domid = recv_rank
!      allocate(recv_list(i)%local_nid(n_data), source = 0)
!
!      n_data = 0
!      do j = jS, jE
!        id = outer_dom_id_all(j)
!        if(recv_rank == id)then
!          n_data = n_data + 1
!          global_id = outer_node_id_all(j)
!          call gedatsu_bsearch_int(temp, 1, NP, global_id, idx)
!          recv_list(i)%local_nid(n_data) = local_nid(idx)
!        endif
!      enddo
!    enddo
!
!    !> send の作成
!    !> slave から master に個数を送信
!    allocate(send_n_list(commsize), source = 0)
!
!    do i = 1, n_neib_recv
!      id = recv_list(i)%domid
!      in = recv_list(i)%nnode
!      send_n_list(id + 1) = in
!    enddo
!
!    call mpi_alltoall(send_n_list, 1, MPI_INTEGER, &
!      send_n_list, 1, MPI_INTEGER, comm, ierr)
!
!    !> send 個数の確保
!    n_neib_send = 0
!    do i = 1, commsize
!      if(send_n_list(i) > 0) n_neib_send = n_neib_send + 1
!    enddo
!
!    allocate(send_list(n_neib_send))
!
!    n_neib_send = 0
!    do i = 1, commsize
!      if(send_n_list(i) > 0)then
!        n_neib_send = n_neib_send + 1
!        send_list(n_neib_send)%domid = i - 1
!        n_data = send_n_list(i)
!        send_list(n_neib_send)%nnode = n_data
!        allocate(send_list(n_neib_send)%local_nid(n_data), source = 0)
!      endif
!    enddo
!
!    !> gedatsu com の構築
!    !> recv
!!    monolis%COM%recv_n_neib = n_neib_recv
!!    allocate(monolis%COM%recv_neib_pe(n_neib_recv), source = 0)
!!    do i = 1, n_neib_recv
!!      monolis%COM%recv_neib_pe(i) = recv_list(i)%domid
!!    enddo
!!    allocate(monolis%COM%recv_index(0:n_neib_recv), source = 0)
!!    do i = 1, n_neib_recv
!!      monolis%COM%recv_index(i) = monolis%COM%recv_index(i-1) + recv_list(i)%nnode
!!    enddo
!!    in = monolis%COM%recv_index(n_neib_recv)
!!    allocate(monolis%COM%recv_item(in), source = 0)
!!    in = 0
!!    do i = 1, n_neib_recv
!!      jE = recv_list(i)%nnode
!!      do j = 1, jE
!!        in = in + 1
!!        idx = recv_list(i)%local_nid(j)
!!        monolis%COM%recv_item(in) = idx
!!      enddo
!!    enddo
!!
!!    !> send
!!    monolis%COM%send_n_neib = n_neib_send
!!    allocate(monolis%COM%send_neib_pe(n_neib_send), source = 0)
!!    do i = 1, n_neib_send
!!      monolis%COM%send_neib_pe(i) = send_list(i)%domid
!!    enddo
!!    allocate(monolis%COM%send_index(0:n_neib_send), source = 0)
!!    do i = 1, n_neib_send
!!      monolis%COM%send_index(i) = monolis%COM%send_index(i-1) + send_list(i)%nnode
!!    enddo
!!    in = monolis%COM%send_index(n_neib_send)
!!    allocate(monolis%COM%send_item(in), source = 0)
!!
!!    !> slave から master に global_nid を送信
!!    allocate(sta1(monolis_status_size,monolis%COM%recv_n_neib))
!!    allocate(sta2(monolis_status_size,monolis%COM%send_n_neib))
!!    allocate(req1(monolis%COM%recv_n_neib))
!!    allocate(req2(monolis%COM%send_n_neib))
!!
!!    in = monolis%COM%recv_index(n_neib_recv)
!!    allocate(ws(in), source = 0)
!!
!!    do i = 1, monolis%COM%recv_n_neib
!!      id = recv_list(i)%domid
!!      in = recv_list(i)%nnode
!!      jS = monolis%COM%recv_index(i-1) + 1
!!      jE = monolis%COM%recv_index(i)
!!      do j = jS, jE
!!        idx = monolis%COM%recv_item(j)
!!        ws(j) = nid(idx)
!!      enddo
!!      call MPI_Isend(ws(jS:jE), in, MPI_INTEGER, id, 0, monolis%COM%comm, req1(i), ierr)
!!    enddo
!!
!!    in = monolis%COM%send_index(n_neib_send)
!!    allocate(wr(in), source = 0)
!!    do i = 1, monolis%COM%send_n_neib
!!      id = send_list(i)%domid
!!      in = send_list(i)%nnode
!!      jS = monolis%COM%send_index(i-1) + 1
!!      jE = monolis%COM%send_index(i)
!!      call MPI_Irecv(wr(jS:jE), in, MPI_INTEGER, id, 0, monolis%COM%comm, req2(i), ierr)
!!    enddo
!!
!!    call MPI_waitall(monolis%COM%recv_n_neib, req2, sta2, ierr)
!!    call MPI_waitall(monolis%COM%send_n_neib, req1, sta1, ierr)
!!
!!    !> local_nid に変換
!!    in = monolis%COM%send_index(n_neib_send)
!!    do i = 1, in
!!      call monolis_bsearch_int(temp, 1, NP, wr(i), id)
!!      monolis%COM%send_item(i) = local_nid(id)
!!    enddo
!  end subroutine gedatsu_comm_get_comm_table
end module mod_gedatsu_communicator
