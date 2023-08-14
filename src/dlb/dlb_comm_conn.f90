!> 動的負荷分散モジュール
module mod_gedatsu_dlb_comm_conn
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_dlb_comm_nodal
  use mod_gedatsu_graph_repart
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための通信テーブル作成（付随グラフ）
  subroutine gedatsu_dlb_get_conn_graph_comm_table(dlb, nodal_graph, conn_graph, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: nodal_graph
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: conn_graph
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    type(gedatsu_update_db), allocatable :: update_db(:)
    integer(kint) :: i, comm_size, my_rank, no_use
    integer(kint), allocatable :: is_send_nodal_vertex(:)
    integer(kint), allocatable :: domain_id_org(:)
    integer(kint), allocatable :: send_n_node_list(:)
    integer(kint), allocatable :: recv_n_node_list(:)
    integer(kint), allocatable :: send_n_edge_list(:)
    integer(kint), allocatable :: recv_n_edge_list(:)

    !# 送信計算点の情報取得
    my_rank = monolis_mpi_get_local_my_rank(COM%comm)
    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    call gedatsu_dlb_get_domain_id_org(nodal_graph, COM, domain_id_org)

    allocate(update_db(comm_size))

    do i = 1, comm_size
      if(my_rank == i - 1) cycle
      !# 各領域番号に送る要素とエッジ数を格納
      call monolis_alloc_I_1d(is_send_nodal_vertex, nodal_graph%n_vertex)
      call gedatsu_dlb_get_n_move_vertex(nodal_graph, no_use, &
        & is_send_nodal_vertex, domain_id_org, i - 1)

      call monolis_alloc_I_1d(update_db(i)%is_send_node, conn_graph%n_vertex)
      call monolis_alloc_I_1d(update_db(i)%is_send_edge, conn_graph%index(conn_graph%n_vertex + 1))
      call gedatsu_dlb_get_n_move_connectivity(conn_graph, is_send_nodal_vertex, &
        & update_db(i)%n_send_node, update_db(i)%is_send_node, &
        & update_db(i)%n_send_edge, update_db(i)%is_send_edge)
    enddo

    !# 送信計算点（要素）の全体情報取得
    call monolis_alloc_I_1d(send_n_node_list, comm_size)
    call monolis_alloc_I_1d(recv_n_node_list, comm_size)

    do i = 1, comm_size
      send_n_node_list(i) = update_db(i)%n_send_node
    enddo

    recv_n_node_list = send_n_node_list
    call monolis_alltoall_I1(comm_size, recv_n_node_list, COM%comm)

    !# エッジ（コネクティビティ）の全体情報取得
    call monolis_alloc_I_1d(send_n_edge_list, comm_size)
    call monolis_alloc_I_1d(recv_n_edge_list, comm_size)

    do i = 1, comm_size
      send_n_edge_list(i) = update_db(i)%n_send_edge
    enddo

    recv_n_edge_list = send_n_edge_list
    call monolis_alltoall_I1(comm_size, recv_n_edge_list, COM%comm)

    call gedatsu_dlb_generate_nodal_graph_comm_table(dlb, conn_graph, update_db, &
      & send_n_node_list, recv_n_node_list, &
      & send_n_edge_list, recv_n_edge_list, COM)
  end subroutine gedatsu_dlb_get_conn_graph_comm_table

  !> @ingroup group_dlb
  !> 動的負荷分散のためのグラフ構造アップデート
  subroutine gedatsu_dlb_get_temporary_conn_graph(dlb, graph_org, graph_tmp, recv_global_id, recv_domain_org, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph_org
    !> [in] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph_tmp
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint) :: n_recv_conn, n_recv_edge, n_send_edge, n_vertex
    integer(kint) :: n_merge_node, n_merge_edge, n_my_edge
    integer(kint) :: i, j, jS, jE, in, i1, i2, my_rank, id, id1, id2
    integer(kint), allocatable :: domain_id_org(:)
    integer(kint), allocatable :: recv_n_vertex_list(:)
    integer(kint), allocatable :: recv_global_id(:)
    integer(kint), allocatable :: recv_domain_new(:)
    integer(kint), allocatable :: recv_domain_org(:)
    integer(kint), allocatable :: send_n_vertex_list(:)
    integer(kint), allocatable :: send_edge(:)
    integer(kint), allocatable :: recv_edge(:)
    integer(kint), allocatable :: is_merge_node(:)
    integer(kint), allocatable :: is_merge_edge(:)
    integer(kint), allocatable :: global_id_tmp(:)
    integer(kint), allocatable :: my_edge(:,:)
    integer(kint), allocatable :: ids(:)
    integer(kint), allocatable :: perm(:)

    my_rank = monolis_mpi_get_local_my_rank(COM%comm)

    !# 計算点の送受信
    n_recv_conn = dlb%COM_node%recv_index(dlb%COM_node%recv_n_neib + 1)

    call monolis_alloc_I_1d(recv_global_id,  n_recv_conn)
    call monolis_alloc_I_1d(recv_domain_new, n_recv_conn)
    call monolis_alloc_I_1d(recv_domain_org, n_recv_conn)
    call monolis_alloc_I_1d(recv_n_vertex_list, n_recv_conn)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & graph_org%vertex_id, recv_global_id, 1, dlb%COM_node%comm)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & graph_org%vertex_domain_id, recv_domain_new, 1, dlb%COM_node%comm)

    call gedatsu_dlb_get_domain_id_org(graph_org, COM, domain_id_org)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & domain_id_org, recv_domain_org, 1, dlb%COM_node%comm)

    call monolis_alloc_I_1d(send_n_vertex_list, graph_org%n_vertex)
    do i = 1, graph_org%n_vertex
      send_n_vertex_list(i) = graph_org%index(i + 1) - graph_org%index(i)
    enddo

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & send_n_vertex_list, recv_n_vertex_list, 1, dlb%COM_node%comm)

!write(100+monolis_mpi_get_global_my_rank(),*)"vertex_domain_id", graph_org%vertex_domain_id
!write(100+monolis_mpi_get_global_my_rank(),*)"domain_id_org   ", domain_id_org
!write(100+monolis_mpi_get_global_my_rank(),*)"vertex_id       ", graph_org%vertex_id
!write(100+monolis_mpi_get_global_my_rank(),*)"send_n_vertex_list       ", graph_org%vertex_id

!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_node", n_recv_node
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_global_id", recv_global_id
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_new", recv_domain_new
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_org", recv_domain_org
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_n_vertex_list", recv_domain_org

    !# コネクティビティの送受信
    n_send_edge = graph_org%index(graph_org%n_vertex + 1)
    n_recv_edge = dlb%COM_edge%recv_index(dlb%COM_edge%recv_n_neib + 1)

    call monolis_alloc_I_1d(send_edge, 2*n_send_edge)
    call monolis_alloc_I_1d(recv_edge, 2*n_recv_edge)

    do i = 1, graph_org%n_vertex
      jS = graph_org%index(i) + 1
      jE = graph_org%index(i + 1)
      do j = jS, jE
        in = graph_org%item(j)
        send_edge(2*j-1) = graph_org%vertex_id(i)
        send_edge(2*j  ) = graph_org%vertex_id(in)
      enddo
    enddo

    call monolis_SendRecv_I(dlb%COM_edge%send_n_neib, dlb%COM_edge%send_neib_pe, &
       & dlb%COM_edge%recv_n_neib, dlb%COM_edge%recv_neib_pe, &
       & dlb%COM_edge%send_index, dlb%COM_edge%send_item, &
       & dlb%COM_edge%recv_index, dlb%COM_edge%recv_item, &
       & send_edge, recv_edge, 2, dlb%COM_edge%comm)

!!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_edge", n_recv_edge
!!write(100+monolis_mpi_get_global_my_rank(),*)"recv_edge", recv_edge

    !# 検索用配列の作成
    call monolis_alloc_I_1d(global_id_tmp, graph_org%n_vertex)
    global_id_tmp = graph_org%vertex_id
    call monolis_qsort_I_1d(global_id_tmp, 1, graph_org%n_vertex)

!    !# 計算点のマージ（自領域 + 受信領域）
!    call monolis_alloc_I_1d(is_merge_node, n_recv_node)
!    n_merge_node = 0
!    do i = 1, n_recv_node
!      in = recv_global_id(i)
!      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, in, id)
!      if(id == -1)then
!        is_merge_node(i) = 1
!        n_merge_node = n_merge_node + 1
!      endif
!    enddo
!
!!write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_node", n_merge_node
!!write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_node", is_merge_node
!
!    !# エッジのマージ（自領域 + 受信領域）
!    call monolis_alloc_I_1d(is_merge_edge, n_recv_edge)
!    n_merge_edge = 0
!    do i = 1, n_recv_edge
!      i1 = recv_edge(2*i-1)
!      i2 = recv_edge(2*i  )
!      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, i1, id1)
!      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, i2, id2)
!      if(.not.(id1 /= -1 .and. id2 /= -1))then
!        is_merge_edge(i) = 1
!        n_merge_edge = n_merge_edge + 1
!      endif
!    enddo
!
!!write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_edge", n_merge_edge
!!write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_edge", is_merge_edge
!
!    !# アップデートされたグラフの作成
!    call gedatsu_graph_set_n_vertex(graph_tmp, graph_org%n_vertex + n_merge_node)
!
!    do i = 1, graph_org%n_vertex
!      graph_tmp%vertex_id(i) = graph_org%vertex_id(i)
!      graph_tmp%vertex_domain_id(i) = graph_org%vertex_domain_id(i)
!    enddo
!
!!write(100+monolis_mpi_get_global_my_rank(),*)"graph_tmp%vertex_id A", graph_tmp%vertex_id
!!write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_node", n_merge_node
!!write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_node", is_merge_node
!
!    in = graph_org%n_vertex
!    do i = 1, n_recv_node
!      if(is_merge_node(i) == 1)then
!        in = in + 1
!        graph_tmp%vertex_id(in) = recv_global_id(i)
!        graph_tmp%vertex_domain_id(in) = recv_domain_new(i)
!      endif
!    enddo
!
!!write(100+monolis_mpi_get_global_my_rank(),*)"graph_tmp%vertex_id B", graph_tmp%vertex_id
!
!    n_my_edge = n_send_edge + n_merge_edge
!    call monolis_alloc_I_2d(my_edge, 2,n_my_edge)
!
!    do i = 1, n_send_edge
!      my_edge(1,i) = send_edge(2*i-1)
!      my_edge(2,i) = send_edge(2*i  )
!    enddo
!
!    in = n_send_edge
!    do i = 1, n_recv_edge
!      if(is_merge_edge(i) == 1)then
!        in = in + 1
!        my_edge(1,in) = recv_edge(2*i-1)
!        my_edge(2,in) = recv_edge(2*i  )
!      endif
!    enddo
!
!    call monolis_alloc_I_1d(ids, graph_tmp%n_vertex)
!
!    call monolis_alloc_I_1d(perm, graph_tmp%n_vertex)
!
!    call monolis_get_sequence_array_I(perm, graph_tmp%n_vertex, 1, 1)
!
!!write(100+monolis_mpi_get_global_my_rank(),*)"graph_tmp%vertex_id C", graph_tmp%vertex_id
!
!    ids = graph_tmp%vertex_id
!
!    call monolis_qsort_I_2d(ids, perm, 1, graph_tmp%n_vertex)
!
!    do i = 1, n_my_edge
!      call monolis_bsearch_I(ids, 1, graph_tmp%n_vertex, my_edge(1,i), in)
!      my_edge(1,i) = perm(in)
!      call monolis_bsearch_I(ids, 1, graph_tmp%n_vertex, my_edge(2,i), in)
!      my_edge(2,i) = perm(in)
!    enddo
!
!    call gedatsu_graph_set_edge(graph_tmp, n_my_edge, my_edge)
  end subroutine gedatsu_dlb_get_temporary_conn_graph

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信するエッジ数の取得
  subroutine gedatsu_dlb_get_n_move_connectivity(graph, is_send_nodal_vertex, &
    & n_send_node, is_send_node, n_send_edge, is_send_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する節点のフラグ
    integer(kint) :: is_send_nodal_vertex(:)
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: n_send_node
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: is_send_node(:)
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: n_send_edge
    !> [in] オーバーラップ計算点を含む通信するエッジのフラグ
    integer(kint) :: is_send_edge(:)
    integer(kint) :: i, j, jS, jE, jn
    logical :: is_move_tmp

    is_send_node = 0

    do i = 1, graph%n_vertex
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      is_move_tmp = .true.
      do j = jS, jE
        jn = graph%item(j)
        if(is_send_nodal_vertex(jn) == 0)then
          is_move_tmp = .false.
        endif
      enddo
      if(is_move_tmp) is_send_node(i) = 1
    enddo

    n_send_node = 0
    do i = 1, graph%n_vertex
      if(is_send_node(i) == 1) n_send_node = n_send_node + 1
    enddo

    n_send_edge = 0
    is_send_edge = 0
    do i = 1, graph%n_vertex
      if(is_send_node(i) == 0) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        n_send_edge = n_send_edge + 1
        is_send_edge(j) = 1
      enddo
    enddo
  end subroutine gedatsu_dlb_get_n_move_connectivity

end module mod_gedatsu_dlb_comm_conn
