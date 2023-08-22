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
        & is_send_nodal_vertex, domain_id_org, my_rank, i - 1)

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
  subroutine gedatsu_dlb_get_temporary_conn_graph(dlb, nodal_graph_org, nodal_graph_new, &
    & conn_graph_org, conn_graph_new, recv_global_id, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(inout) :: nodal_graph_org
    type(gedatsu_graph), intent(inout) :: nodal_graph_new
    type(gedatsu_graph), intent(inout) :: conn_graph_org
    type(gedatsu_graph), intent(inout) :: conn_graph_new
    type(gedatsu_graph) :: graph_temp
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint) :: n_recv_node, n_recv_edge, n_send_edge, n_recv_conn
    integer(kint) :: n_merge_edge, n_item
    integer(kint) :: i, j, jS, jE, in, jn, i1, my_rank, id1
    !integer(kint), allocatable :: domain_id_org(:)
    integer(kint), allocatable :: recv_global_id(:)
    integer(kint), allocatable :: send_edge(:)
    integer(kint), allocatable :: recv_edge(:)
    !integer(kint), allocatable :: is_merge_node(:)
    integer(kint), allocatable :: is_merge_edge(:)
    !integer(kint), allocatable :: global_node_id_tmp(:)
    integer(kint), allocatable :: global_conn_id_tmp(:)
    !integer(kint), allocatable :: my_edge(:,:)
    integer(kint), allocatable :: recv_conn_index(:)
    integer(kint), allocatable :: recv_conn_item(:)

    my_rank = monolis_mpi_get_local_my_rank(COM%comm)

    !# 計算点の送受信
    n_recv_node = dlb%COM_node%recv_index(dlb%COM_node%recv_n_neib + 1)

    call monolis_alloc_I_1d(recv_global_id, n_recv_node)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & conn_graph_org%vertex_id, recv_global_id, 1, dlb%COM_node%comm)

!write(100+monolis_mpi_get_global_my_rank(),*)"vertex_domain_id", graph_org%vertex_domain_id
!write(100+monolis_mpi_get_global_my_rank(),*)"domain_id_org   ", domain_id_org
!write(100+monolis_mpi_get_global_my_rank(),*)"vertex_id       ", graph_org%vertex_id

!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_node", n_recv_node
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_global_id", recv_global_id
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_new", recv_domain_new
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_org", recv_domain_org

    !# エッジの送受信
    n_send_edge = conn_graph_org%index(conn_graph_org%n_vertex + 1)
    n_recv_edge = dlb%COM_edge%recv_index(dlb%COM_edge%recv_n_neib + 1)

!write(100+monolis_mpi_get_global_my_rank(),*)"n_send_edge", n_send_edge
!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_edge", n_recv_edge

    call monolis_alloc_I_1d(send_edge, 2*n_send_edge)
    call monolis_alloc_I_1d(recv_edge, 2*n_recv_edge)

    do i = 1, conn_graph_org%n_vertex
      jS = conn_graph_org%index(i) + 1
      jE = conn_graph_org%index(i + 1)
      do j = jS, jE
        in = conn_graph_org%item(j)
        send_edge(2*j-1) = conn_graph_org%vertex_id(i)
        send_edge(2*j  ) = nodal_graph_org%vertex_id(in)
      enddo
    enddo

    call monolis_SendRecv_I(dlb%COM_edge%send_n_neib, dlb%COM_edge%send_neib_pe, &
       & dlb%COM_edge%recv_n_neib, dlb%COM_edge%recv_neib_pe, &
       & dlb%COM_edge%send_index, dlb%COM_edge%send_item, &
       & dlb%COM_edge%recv_index, dlb%COM_edge%recv_item, &
       & send_edge, recv_edge, 2, dlb%COM_edge%comm)

    !# 2 次元エッジ配列を conn 情報に変換
    if(n_recv_edge > 0)then
      n_recv_conn = 1
      do i = 1, n_recv_edge - 1
        if(recv_edge(2*i-1) /= recv_edge(2*i+1)) n_recv_conn = n_recv_conn + 1
      enddo

      call monolis_alloc_I_1d(recv_conn_index, n_recv_conn + 1)
      call monolis_alloc_I_1d(recv_conn_item, n_recv_edge)

      n_recv_conn = 1
      recv_conn_index(2) = 1
      do i = 1, n_recv_edge - 1
        if(recv_edge(2*i-1) /= recv_edge(2*i+1))then
          n_recv_conn = n_recv_conn + 1
          recv_conn_index(n_recv_conn + 1) = recv_conn_index(n_recv_conn) + 1
        else
          recv_conn_index(n_recv_conn + 1) = recv_conn_index(n_recv_conn + 1) + 1
        endif
      enddo

      do i = 1, n_recv_edge
        recv_conn_item(i) = recv_edge(2*i)
      enddo
    else
      n_recv_conn = 0
      call monolis_alloc_I_1d(recv_conn_index, 1)
      call monolis_alloc_I_1d(recv_conn_item, 1)
    endif

!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_conn", n_recv_conn
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_edge", recv_edge
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_conn_index", recv_conn_index
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_conn_item", recv_conn_item

    !# graph_temp に global ID のまま結合
    graph_temp%n_vertex = conn_graph_org%n_vertex + n_recv_conn

    call monolis_alloc_I_1d(graph_temp%vertex_id, graph_temp%n_vertex)
    call monolis_alloc_I_1d(graph_temp%index, graph_temp%n_vertex + 1)
    call monolis_alloc_I_1d(graph_temp%item, n_send_edge + n_recv_edge)

    !# conn_graph_org section
    do i = 1, conn_graph_org%n_vertex
      graph_temp%vertex_id(i) = conn_graph_org%vertex_id(i)
    enddo

    do i = 1, conn_graph_org%n_vertex + 1
      graph_temp%index(i) = conn_graph_org%index(i)
    enddo

    do i = 1, n_send_edge
      in = conn_graph_org%item(i)
      graph_temp%item(i) = nodal_graph_org%vertex_id(in)
    enddo

    !# recv_conn section
    do i = conn_graph_org%n_vertex + 1, graph_temp%n_vertex
      graph_temp%vertex_id(i) = recv_global_id(i - conn_graph_org%n_vertex)
    enddo

    do i = conn_graph_org%n_vertex + 1, graph_temp%n_vertex
      graph_temp%index(i + 1) = recv_conn_index(i - conn_graph_org%n_vertex + 1) + graph_temp%index(conn_graph_org%n_vertex + 1)
    enddo

    do i = n_send_edge + 1, n_send_edge + n_recv_edge
      graph_temp%item(i) = recv_conn_item(i - n_send_edge)
    enddo

!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%n_vertex", conn_graph_org%n_vertex
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%index", conn_graph_org%index
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%item", conn_graph_org%item

!write(100+monolis_mpi_get_global_my_rank(),*)"graph_temp%n_vertex", graph_temp%n_vertex
!write(100+monolis_mpi_get_global_my_rank(),*)"graph_temp%index", graph_temp%index
!write(100+monolis_mpi_get_global_my_rank(),*)"graph_temp%item", graph_temp%item

!write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_edge", n_recv_edge
!write(100+monolis_mpi_get_global_my_rank(),*)"recv_edge", recv_edge

!write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_node", n_merge_node
!write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_node", is_merge_node

    !# 検索用配列の作成（nodal_graph_new のグローバル節点 id）
    call monolis_alloc_I_1d(global_conn_id_tmp, nodal_graph_new%n_vertex)
    global_conn_id_tmp = nodal_graph_new%vertex_id
    call monolis_qsort_I_1d(global_conn_id_tmp, 1, nodal_graph_new%n_vertex)

    !# マージする conn 数の取得
    call monolis_alloc_I_1d(is_merge_edge, graph_temp%n_vertex)

    aa:do i = 1, graph_temp%n_vertex
      jS = graph_temp%index(i) + 1
      jE = graph_temp%index(i + 1)
      is_merge_edge(i) = 1
      do j = jS, jE
        i1 = graph_temp%item(j)
        call monolis_bsearch_I(global_conn_id_tmp, 1, nodal_graph_new%n_vertex, i1, id1)
        if(id1 == - 1)then
          is_merge_edge(i) = 0
          cycle aa
        endif
      enddo
    enddo aa

    n_merge_edge = 0
    n_item = 0
    do i = 1, graph_temp%n_vertex
      if(is_merge_edge(i) == 1)then
        n_merge_edge = n_merge_edge + 1
        n_item = n_item + graph_temp%index(i + 1) - graph_temp%index(i)
      endif
    enddo

!write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_edge", n_merge_edge
!write(100+monolis_mpi_get_global_my_rank(),*)"graph_tmp%vertex_id", nodal_graph_new%vertex_id

    !# conn_graph_new の作成
    conn_graph_new%n_vertex = n_merge_edge
    call monolis_alloc_I_1d(conn_graph_new%vertex_id, conn_graph_new%n_vertex)
    call monolis_alloc_I_1d(conn_graph_new%index, conn_graph_new%n_vertex + 1)
    call monolis_alloc_I_1d(conn_graph_new%item, n_item)

    in = 0
    jn = 0
    do i = 1, graph_temp%n_vertex
      jS = graph_temp%index(i) + 1
      jE = graph_temp%index(i + 1)
      if(is_merge_edge(i) == 1)then
        in = in + 1
        conn_graph_new%vertex_id(in) = graph_temp%vertex_id(i)
        conn_graph_new%index(in + 1) = conn_graph_new%index(in) + jE - jS + 1
        do j = jS, jE
          jn = jn + 1
          conn_graph_new%item(jn) = graph_temp%item(j)
        enddo
      endif
    enddo

    !# ローカル node ID への変換
    do i = 1, n_item
      i1 = conn_graph_new%item(i)
      call monolis_bsearch_I(global_conn_id_tmp, 1, nodal_graph_new%n_vertex, i1, id1)
      if(id1 == -1) stop "gedatsu_dlb_get_temporary_conn_graph"
      conn_graph_new%item(i) = id1
    enddo

!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%n_vertex", conn_graph_new%n_vertex
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%index", conn_graph_new%index
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%item", conn_graph_new%item
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

  !> @ingroup group_dlb
  !> 更新後のグラフの取得
  subroutine gedatsu_dlb_get_conn_graph_comm_table_modify(dlb, graph_new, recv_global_id, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph_new
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [in] recv 計算点のグローバル id
    integer(kint) :: recv_global_id(:)
    integer(kint) :: i, n_recv_node, id, pos, my_rank
    integer(kint), allocatable :: ids(:), perm(:)

    my_rank = monolis_mpi_get_local_my_rank(COM%comm)

    n_recv_node = dlb%COM_node%recv_index(dlb%COM_node%recv_n_neib + 1)

    call monolis_alloc_I_1d(ids, graph_new%n_vertex)

    call monolis_alloc_I_1d(perm, graph_new%n_vertex)

    call monolis_get_sequence_array_I(perm, graph_new%n_vertex, 1, 1)

    ids = graph_new%vertex_id

    call monolis_qsort_I_2d(ids, perm, 1, graph_new%n_vertex)

    do i = 1, n_recv_node
      id = recv_global_id(i)
      call monolis_bsearch_I(ids, 1, graph_new%n_vertex, id, pos)

      if(pos == -1)then
        dlb%COM_node%recv_item(i) = -1
      else
        dlb%COM_node%recv_item(i) = perm(pos)
      endif
    enddo
  end subroutine gedatsu_dlb_get_conn_graph_comm_table_modify
end module mod_gedatsu_dlb_comm_conn
