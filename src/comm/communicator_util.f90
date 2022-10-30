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
    integer(gint) :: nnode = 0
    integer(gint) :: domid = -1
    integer(gint), allocatable :: local_nid(:)
  end type dedatsu_comm_node_list

contains

  !> データ通信する隣接領域の取得（逐次実行版）
  subroutine gedatsu_comm_get_neib_domain_serial(graph, subgraphs, n_domain, comms)
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

      comms(i)%recv_n_neib = n_neib

      if(n_neib == 0)then
        call gedatsu_alloc_int_1d(comms(i)%recv_neib_pe, 1)
        call gedatsu_alloc_int_1d(comms(i)%recv_index, 2)
      else
        call gedatsu_alloc_int_1d(comms(i)%recv_neib_pe, n_neib)
        call gedatsu_alloc_int_1d(comms(i)%recv_index, n_neib + 1)
      endif

      did = 0
      do j = 1, n_domain
        if(domain(j) /= 0)then
          did = did + 1
          comms(i)%recv_neib_pe(did) = j
          comms(i)%recv_index(did + 1) = comms(i)%recv_index(did) + domain(j)
        endif
      enddo
    enddo
  end subroutine gedatsu_comm_get_neib_domain_serial

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

  end subroutine gedatsu_comm_get_send_serial

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
    integer(gint) :: i, j, nid, did, n_recv, n_neib, id, jS, jE
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
      n_recv = comms(i)%recv_index(n_neib)
      call gedatsu_alloc_int_1d(comms(i)%recv_item, n_recv)
      call gedatsu_alloc_int_1d(add_count, comms(i)%recv_n_neib)

      do j = 1, n_neib
        add_count(j) = comms(i)%recv_index(j)
      enddo

      do j = subgraphs(i)%n_internal_vertex + 1, subgraphs(i)%n_vertex
        nid = subgraphs(i)%vertex_id(j)
        did = graph%vertex_domain_id(nid) + 1

        id = add_count(did) + 1
        comms(i)%recv_item(id) = local_node_id_on_each_domain(nid)
        add_count(did) = add_count(did) + 1
      enddo

      do j = 1, n_neib
        jS = comms(i)%recv_index(i) + 1
        jE = comms(i)%recv_index(i + 1)
        n_recv = jE - jS + 1
        call gedatsu_qsort_int_1d(comms(i)%recv_item(jS:jE), 1, n_recv)
      enddo

      call gedatsu_dealloc_int_1d(add_count)
    enddo
  end subroutine gedatsu_comm_get_recv_serial
end module mod_gedatsu_communicator_util
