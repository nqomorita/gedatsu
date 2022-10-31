!> グラフ分割モジュール（通信テーブル作成）
module mod_gedatsu_communicator_util
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

  type dedatsu_comm_node_list
    integer(gint) :: n_node = 0
    integer(gint), allocatable :: global_id(:)
  end type dedatsu_comm_node_list

contains

  !> データ通信する send 隣接領域の取得（逐次実行版）
  subroutine gedatsu_comm_get_send_neib_domain_serial(n_domain, comms)
    implicit none
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comms(:)
    integer(gint) :: i, j, did, n_neib, add(1)
    type(dedatsu_comm_node_list), allocatable :: send_list(:)
    integer(gint), allocatable :: domain(:)

    allocate(send_list(n_domain))
    call gedatsu_alloc_int_1d(domain, n_domain)

    do i = 1, n_domain
      do j = 1, comms(i)%recv_n_neib
        did = comms(i)%recv_neib_pe(j)
        send_list(did)%n_node = send_list(did)%n_node + 1
        add = i
        call gedatsu_append_int_1d(send_list(did)%global_id, 1, add)
      enddo
    enddo

    do i = 1, n_domain
      domain = 0
      do j = 1, send_list(i)%n_node
        did = send_list(i)%global_id(j)
        domain(did) = domain(did) + 1
      enddo

      call gedatsu_get_uniq_int(send_list(i)%global_id, send_list(i)%n_node, n_neib)
      call gedatsu_comm_alloc_comm_index(n_neib, comms(i)%send_n_neib, &
        & comms(i)%send_neib_pe, comms(i)%send_index)
      call gedatsu_dealloc_int_1d(send_list(i)%global_id)

      did = 0
      do j = 1, n_domain
        if(domain(j) /= 0)then
          did = did + 1
          comms(i)%send_neib_pe(did) = j
          comms(i)%send_index(did + 1) = comms(i)%send_index(did) + domain(j)
        endif
      enddo
    enddo
  end subroutine gedatsu_comm_get_send_neib_domain_serial

  !> データ通信する recv 隣接領域の取得（逐次実行版）
  subroutine gedatsu_comm_get_recv_neib_domain_serial(graph, subgraphs, n_domain, comms)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comms(:)
    integer(gint) :: i, j, nid, did, n_neib
    integer(gint), allocatable :: domain(:)

    call gedatsu_alloc_int_1d(domain, n_domain)

    do i = 1, n_domain
      domain = 0
      do j = 1, subgraphs(i)%n_vertex
        nid = subgraphs(i)%vertex_id(j)
        did = graph%vertex_domain_id(nid) + 1
        if(did /= i)then
          domain(did) = domain(did) + 1
        endif
      enddo

      n_neib = 0
      do j = 1, n_domain
        if(domain(j) /= 0) n_neib = n_neib + 1
      enddo

      call gedatsu_comm_alloc_comm_index(n_neib, comms(i)%recv_n_neib, &
        & comms(i)%recv_neib_pe, comms(i)%recv_index)

      did = 0
      do j = 1, n_domain
        if(domain(j) /= 0)then
          did = did + 1
          comms(i)%recv_neib_pe(did) = j
          comms(i)%recv_index(did + 1) = comms(i)%recv_index(did) + domain(j)
        endif
      enddo
    enddo
  end subroutine gedatsu_comm_get_recv_neib_domain_serial

  !> データ通信する recv 隣接領域の取得（逐次実行版）
  subroutine gedatsu_comm_alloc_comm_index(n_neib_in, n_neib, neib_pe, index)
    implicit none
    !> [in] 隣接領域数
    integer(gint) :: n_neib_in
    !> [out] comm 構造体の隣接領域数
    integer(gint) :: n_neib
    !> [out] comm 構造体の隣接領域 id
    integer(gint), allocatable :: neib_pe(:)
    !> [out] comm 構造体の index 配列
    integer(gint), allocatable :: index(:)

    n_neib = n_neib_in

    if(n_neib == 0)then
      call gedatsu_alloc_int_1d(neib_pe, 1)
      call gedatsu_alloc_int_1d(index, 2)
    else
      call gedatsu_alloc_int_1d(neib_pe, n_neib)
      call gedatsu_alloc_int_1d(index, n_neib + 1)
    endif
  end subroutine gedatsu_comm_alloc_comm_index

  !> send 通信テーブルの取得（逐次実行版）
  subroutine gedatsu_comm_get_send_serial(graph, subgraphs, n_domain, comms)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comms(:)
    integer(gint) :: i, j, jS, jE, k, did
    !type(dedatsu_comm_node_list), allocatable :: send_list(:)
    integer(gint), allocatable :: global_id(:)

    !allocate(send_list(n_domain))

    do i = 1, n_domain
      do j = 1, comms(i)%recv_n_neib
        did = comms(i)%recv_neib_pe(j)
        jS = comms(i)%recv_index(j) + 1
        jE = comms(i)%recv_index(j + 1)
        call gedatsu_alloc_int_1d(global_id, jE - jS + 1)
        do k = jS, jE
          global_id(k - jS + 1) = subgraphs(i)%vertex_id(comms(i)%recv_item(k))
        enddo
        !call append_send_node(send_list(did), i, jE - jS + 1, global_id)
        call gedatsu_dealloc_int_1d(global_id)
      enddo
    enddo
  end subroutine gedatsu_comm_get_send_serial

!  !> send 通信テーブル情報の一次保存
!  subroutine append_send_node(send_list, domid, n_node, global_id)
!    use mod_monolis_stdlib
!    implicit none
!    type(dedatsu_comm_node_list) :: send_list
!    integer(gint) :: domid, n_node, iS
!    integer(gint) :: global_id(:)
!
!    iS = send_list%n_node
!    call gedatsu_realloc_int_1d(send_list%domid, n_node)
!    call gedatsu_realloc_int_1d(send_list%global_id, n_node)
!
!    !call monolis_reallocate_integer(send_list%domid    , send_list%nnode, nnode, temp)
!    !call monolis_reallocate_integer(send_list%global_id, send_list%nnode, nnode, global_id)
!    !send_list%nnode = send_list%nnode + nnode
!  end subroutine append_send_node

  !> recv 通信テーブルの取得（逐次実行版）
  subroutine gedatsu_comm_get_recv_serial(graph, subgraphs, n_domain, comms)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 分割後の graph 構造体
    type(gedatsu_graph) :: subgraphs(:)
    !> [in] 分割数
    integer(gint) :: n_domain
    !> [out] 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comms(:)
    integer(gint) :: i, j, nid, did, idx, n_recv, n_neib, id, jS, jE
    integer(gint), allocatable :: add_count(:)
    integer(gint), allocatable :: local_node_id_on_each_domain(:)

    call gedatsu_alloc_int_1d(local_node_id_on_each_domain, graph%n_vertex)

    do i = 1, n_domain
      do j = 1, subgraphs(i)%n_internal_vertex
        nid = subgraphs(i)%vertex_id(j)
        local_node_id_on_each_domain(nid) = j
      enddo
    enddo

    do i = 1, graph%n_vertex
      if(local_node_id_on_each_domain(i) == 0)then
        call gedatsu_error_string("gedatsu_comm_get_recv_serial")
        call gedatsu_error_string("local node id is not set")
        call gedatsu_error_stop()
      endif
    enddo

    do i = 1, n_domain
      n_neib = comms(i)%recv_n_neib
      n_recv = comms(i)%recv_index(n_neib + 1)
      call gedatsu_alloc_int_1d(comms(i)%recv_item, n_recv)
      call gedatsu_alloc_int_1d(add_count, comms(i)%recv_n_neib)

      do j = 1, n_neib
        add_count(j) = comms(i)%recv_index(j)
      enddo

      do j = subgraphs(i)%n_internal_vertex + 1, subgraphs(i)%n_vertex
        nid = subgraphs(i)%vertex_id(j)
        did = graph%vertex_domain_id(nid) + 1
        call gedatsu_bsearch_int(comms(i)%recv_neib_pe, 1, n_neib, did, idx)
        id = add_count(idx) + 1
        comms(i)%recv_item(id) = local_node_id_on_each_domain(nid)
        add_count(idx) = add_count(idx) + 1
      enddo

      do j = 1, n_neib
        jS = comms(i)%recv_index(j) + 1
        jE = comms(i)%recv_index(j + 1)
        n_recv = jE - jS + 1
        call gedatsu_qsort_int_1d(comms(i)%recv_item(jS:jE), 1, n_recv)
      enddo

      call gedatsu_dealloc_int_1d(add_count)
    enddo
  end subroutine gedatsu_comm_get_recv_serial
end module mod_gedatsu_communicator_util
