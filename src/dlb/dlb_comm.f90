!> 動的負荷分散モジュール
module mod_gedatsu_dlb_comm
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_graph_repart
  !use mod_gedatsu_comm
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための通信テーブル作成（節点グラフ）
  subroutine gedatsu_dlb_get_comm_table_main(dlb, graph, update_db, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] データベース 構造体
    type(gedatsu_update_db) :: update_db(:)
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint) :: i, comm_size, my_rank
    integer(kint), allocatable :: send_n_node_list(:)
    integer(kint), allocatable :: recv_n_node_list(:)
    integer(kint), allocatable :: send_n_edge_list(:)
    integer(kint), allocatable :: recv_n_edge_list(:)

    !# 送信計算点の情報取得
    my_rank = monolis_mpi_get_local_my_rank(COM%comm)
    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    do i = 1, comm_size
      if(my_rank == i - 1) cycle
      call monolis_alloc_I_1d(update_db(i)%is_send_node, graph%n_vertex)
      call gedatsu_dlb_get_n_move_vertex(graph, update_db(i)%n_send_node, &
        & update_db(i)%is_send_node, i - 1)

      call monolis_alloc_I_1d(update_db(i)%is_send_edge, graph%index(graph%n_vertex + 1))
      call gedatsu_dlb_get_n_move_edge(graph, update_db(i)%n_send_edge, &
        & update_db(i)%is_send_node, update_db(i)%is_send_edge)
    enddo

    !# 送信計算点の全体情報取得
    call monolis_alloc_I_1d(send_n_node_list, comm_size)
    call monolis_alloc_I_1d(recv_n_node_list, comm_size)

    do i = 1, comm_size
      send_n_node_list(i) = update_db(i)%n_send_node
    enddo

    recv_n_node_list = send_n_node_list
    call monolis_alltoall_I1(comm_size, recv_n_node_list, COM%comm)

    !# エッジの全体情報取得
    call monolis_alloc_I_1d(send_n_edge_list, comm_size)
    call monolis_alloc_I_1d(recv_n_edge_list, comm_size)

    do i = 1, comm_size
      send_n_edge_list(i) = update_db(i)%n_send_edge
    enddo

    recv_n_edge_list = send_n_edge_list
    call monolis_alltoall_I1(comm_size, recv_n_edge_list, COM%comm)

write(100+monolis_mpi_get_global_my_rank(),*)"send_n_node_list", send_n_node_list
write(100+monolis_mpi_get_global_my_rank(),*)"recv_n_node_list", recv_n_node_list
write(100+monolis_mpi_get_global_my_rank(),*)"send_n_edge_list", send_n_edge_list
write(100+monolis_mpi_get_global_my_rank(),*)"recv_n_edge_list", recv_n_edge_list

    call gedatsu_dlb_generate_comm_table(dlb, graph, update_db, &
      & send_n_node_list, recv_n_node_list, &
      & send_n_edge_list, recv_n_edge_list, COM)
  end subroutine gedatsu_dlb_get_comm_table_main

  !> @ingroup group_dlb
  !> データ通信のための通信テーブルの作成
  subroutine gedatsu_dlb_generate_comm_table(dlb, graph, update_db, &
    & send_n_node_list, recv_n_node_list, &
    & send_n_edge_list, recv_n_edge_list, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] データベース 構造体
    type(gedatsu_update_db) :: update_db(:)
    !> [in] dlb 構造体
    integer(kint) :: send_n_node_list(:)
    integer(kint) :: recv_n_node_list(:)
    integer(kint) :: send_n_edge_list(:)
    integer(kint) :: recv_n_edge_list(:)
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint) :: i, in, j, n_item
    integer(kint) :: my_rank, comm_size

    call monolis_com_finalize(dlb%COM_node)
    call monolis_com_finalize(dlb%COM_edge)

    my_rank = monolis_mpi_get_local_my_rank(COM%comm)
    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    dlb%COM_node%comm = COM%comm
    dlb%COM_node%my_rank = COM%my_rank
    dlb%COM_node%comm_size = COM%comm_size

    dlb%COM_edge%comm = COM%comm
    dlb%COM_edge%my_rank = COM%my_rank
    dlb%COM_edge%comm_size = COM%comm_size

    !# node セクション
    !# send table の作成
    n_item = 0
    do i = 1, comm_size
      if(send_n_node_list(i) /= 0)then
        dlb%COM_node%send_n_neib = dlb%COM_node%send_n_neib + 1
        n_item = n_item + send_n_node_list(i)
      endif
    enddo

    if(dlb%COM_node%send_n_neib == 0)then
      call monolis_palloc_I_1d(dlb%COM_node%send_neib_pe, 1)
      call monolis_palloc_I_1d(dlb%COM_node%send_index, 2)
      call monolis_palloc_I_1d(dlb%COM_node%send_item, 1)
    else
      call monolis_palloc_I_1d(dlb%COM_node%send_neib_pe, dlb%COM_node%send_n_neib)
      call monolis_palloc_I_1d(dlb%COM_node%send_index, dlb%COM_node%send_n_neib + 1)
      call monolis_palloc_I_1d(dlb%COM_node%send_item, n_item)
    endif

    in = 0
    do i = 1, comm_size
      if(send_n_node_list(i) /= 0)then
        in = in + 1
        dlb%COM_node%send_neib_pe(in) = i - 1
        dlb%COM_node%send_index(in + 1) = dlb%COM_node%send_index(in) + send_n_node_list(i)
      endif
    enddo

    in = 0
    do i = 1, comm_size
      do j = 1, graph%n_vertex
        if(update_db(i)%n_send_node == 0) cycle
        if(update_db(i)%is_send_node(j) == 1)then
          in = in + 1
          dlb%COM_node%send_item(in) = j
        endif
      enddo
    enddo

    !# recv table の作成
    n_item = 0
    do i = 1, comm_size
      if(recv_n_node_list(i) /= 0)then
        dlb%COM_node%recv_n_neib = dlb%COM_node%recv_n_neib + 1
        n_item = n_item + recv_n_node_list(i)
      endif
    enddo

    if(dlb%COM_node%recv_n_neib == 0)then
      call monolis_palloc_I_1d(dlb%COM_node%recv_neib_pe, 1)
      call monolis_palloc_I_1d(dlb%COM_node%recv_index, 2)
      call monolis_palloc_I_1d(dlb%COM_node%recv_item, 1)
    else
      call monolis_palloc_I_1d(dlb%COM_node%recv_neib_pe, dlb%COM_node%recv_n_neib)
      call monolis_palloc_I_1d(dlb%COM_node%recv_index, dlb%COM_node%recv_n_neib + 1)
      call monolis_palloc_I_1d(dlb%COM_node%recv_item, n_item)
    endif

    in = 0
    do i = 1, comm_size
      if(recv_n_node_list(i) /= 0)then
        in = in + 1
        dlb%COM_node%recv_neib_pe(in) = i - 1
        dlb%COM_node%recv_index(in + 1) = dlb%COM_node%recv_index(in) + recv_n_node_list(i)
      endif
    enddo

    call monolis_get_sequence_array_I(dlb%COM_node%recv_item, n_item, 1, 1)

write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%send_n_neib", dlb%COM_node%send_n_neib
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%send_neib_pe", dlb%COM_node%send_neib_pe
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%send_index", dlb%COM_node%send_index
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%send_item", dlb%COM_node%send_item
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%recv_n_neib", dlb%COM_node%recv_n_neib
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%recv_neib_pe", dlb%COM_node%recv_neib_pe
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%recv_index", dlb%COM_node%recv_index
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_node%recv_item", dlb%COM_node%recv_item

    !# edge セクション
    !# send table の作成
    n_item = 0
    do i = 1, comm_size
      if(send_n_edge_list(i) /= 0)then
        dlb%COM_edge%send_n_neib = dlb%COM_edge%send_n_neib + 1
        n_item = n_item + send_n_edge_list(i)
      endif
    enddo

    if(dlb%COM_edge%send_n_neib == 0)then
      call monolis_palloc_I_1d(dlb%COM_edge%send_neib_pe, 1)
      call monolis_palloc_I_1d(dlb%COM_edge%send_index, 2)
      call monolis_palloc_I_1d(dlb%COM_edge%send_item, 1)
    else
      call monolis_palloc_I_1d(dlb%COM_edge%send_neib_pe, dlb%COM_edge%send_n_neib)
      call monolis_palloc_I_1d(dlb%COM_edge%send_index, dlb%COM_edge%send_n_neib + 1)
      call monolis_palloc_I_1d(dlb%COM_edge%send_item, n_item)
    endif

    in = 0
    do i = 1, comm_size
      if(send_n_edge_list(i) /= 0)then
        in = in + 1
        dlb%COM_edge%send_neib_pe(in) = i - 1
        dlb%COM_edge%send_index(in + 1) = dlb%COM_edge%send_index(in) + send_n_edge_list(i)
      endif
    enddo

    in = 0
    do i = 1, comm_size
      do j = 1, graph%index(graph%n_vertex + 1)
        if(update_db(i)%n_send_edge == 0) cycle
        if(update_db(i)%is_send_edge(j) == 1)then
          in = in + 1
          dlb%COM_edge%send_item(in) = j
        endif
      enddo
    enddo

    !# recv table の作成
    n_item = 0
    do i = 1, comm_size
      if(recv_n_edge_list(i) /= 0)then
        dlb%COM_edge%recv_n_neib = dlb%COM_edge%recv_n_neib + 1
        n_item = n_item + recv_n_edge_list(i)
      endif
    enddo

    if(dlb%COM_edge%recv_n_neib == 0)then
      call monolis_palloc_I_1d(dlb%COM_edge%recv_neib_pe, 1)
      call monolis_palloc_I_1d(dlb%COM_edge%recv_index, 2)
      call monolis_palloc_I_1d(dlb%COM_edge%recv_item, 1)
    else
      call monolis_palloc_I_1d(dlb%COM_edge%recv_neib_pe, dlb%COM_edge%recv_n_neib)
      call monolis_palloc_I_1d(dlb%COM_edge%recv_index, dlb%COM_edge%recv_n_neib + 1)
      call monolis_palloc_I_1d(dlb%COM_edge%recv_item, n_item)
    endif

    in = 0
    do i = 1, comm_size
      if(recv_n_edge_list(i) /= 0)then
        in = in + 1
        dlb%COM_edge%recv_neib_pe(in) = i - 1
        dlb%COM_edge%recv_index(in + 1) = dlb%COM_edge%recv_index(in) + recv_n_edge_list(i)
      endif
    enddo

    call monolis_get_sequence_array_I(dlb%COM_edge%recv_item, n_item, 1, 1)

write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%send_n_neib", dlb%COM_edge%send_n_neib
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%send_neib_pe", dlb%COM_edge%send_neib_pe
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%send_index", dlb%COM_edge%send_index
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%send_item", dlb%COM_edge%send_item
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%recv_n_neib", dlb%COM_edge%recv_n_neib
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%recv_neib_pe", dlb%COM_edge%recv_neib_pe
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%recv_index", dlb%COM_edge%recv_index
write(100+monolis_mpi_get_global_my_rank(),*)"dlb%COM_edge%recv_item", dlb%COM_edge%recv_item
  end subroutine gedatsu_dlb_generate_comm_table

  !> @ingroup group_dlb
  !> 動的負荷分散のためのグラフ構造アップデート
  subroutine gedatsu_dlb_update_nodal_graph_main(dlb, graph_org, COM, &
    & n_my_node, n_my_edge, my_edge, my_global_id, my_domain_id)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph_org
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [in] 自領域候補の計算点数
    integer(kint) :: n_my_node
    !> [in] 自領域候補のエッジ数
    integer(kint) :: n_my_edge
    !> [in] 自領域候補のエッジ
    integer(kint), allocatable :: my_edge(:)
    !> [in] 自領域候補の global id
    integer(kint), allocatable :: my_global_id(:)
    !> [in] 自領域候補の領域番号
    integer(kint), allocatable :: my_domain_id(:)
    integer(kint) :: n_recv_node, n_recv_edge, n_send_edge
    integer(kint) :: n_merge_node, n_merge_edge
    integer(kint) :: i, j, jS, jE, in, i1, i2, my_rank, id, id1, id2
    real(kdouble) :: tcomm
    integer(kint), allocatable :: domain_id_org(:)
    integer(kint), allocatable :: recv_global_id(:)
    integer(kint), allocatable :: recv_domain_new(:)
    integer(kint), allocatable :: recv_domain_org(:)
    integer(kint), allocatable :: send_edge(:)
    integer(kint), allocatable :: recv_edge(:)
    integer(kint), allocatable :: is_merge_node(:)
    integer(kint), allocatable :: is_merge_edge(:)
    integer(kint), allocatable :: global_id_tmp(:)

    my_rank = monolis_mpi_get_local_my_rank(COM%comm)

    !# 計算点の送受信
    n_recv_node = dlb%COM_node%recv_index(dlb%COM_node%recv_n_neib + 1)

    call monolis_alloc_I_1d(recv_global_id,  n_recv_node)
    call monolis_alloc_I_1d(recv_domain_new, n_recv_node)
    call monolis_alloc_I_1d(recv_domain_org, n_recv_node)

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

    call monolis_alloc_I_1d(domain_id_org, graph_org%n_vertex)
    domain_id_org = my_rank
    call monolis_mpi_update_I(COM, 1, domain_id_org, tcomm)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & domain_id_org, recv_domain_org, 1, dlb%COM_node%comm)

write(100+monolis_mpi_get_global_my_rank(),*)"vertex_domain_id", graph_org%vertex_domain_id
write(100+monolis_mpi_get_global_my_rank(),*)"domain_id_org   ", domain_id_org
write(100+monolis_mpi_get_global_my_rank(),*)"vertex_id       ", graph_org%vertex_id

write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_node", n_recv_node
write(100+monolis_mpi_get_global_my_rank(),*)"recv_global_id", recv_global_id
write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_new", recv_domain_new
write(100+monolis_mpi_get_global_my_rank(),*)"recv_domain_org", recv_domain_org

    !# エッジの送受信
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

write(100+monolis_mpi_get_global_my_rank(),*)"n_recv_edge", n_recv_edge
write(100+monolis_mpi_get_global_my_rank(),*)"recv_edge", recv_edge

    !# 検索用配列の作成
    call monolis_alloc_I_1d(global_id_tmp, graph_org%n_vertex)
    global_id_tmp = graph_org%vertex_id
    call monolis_qsort_I_1d(global_id_tmp, 1, graph_org%n_vertex)

    !# 計算点のマージ（自領域 + 受信領域）
    call monolis_alloc_I_1d(is_merge_node, n_recv_node)
    n_merge_node = 0
    do i = 1, n_recv_node
      in = recv_global_id(i)
      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, in, id)
      if(id == -1)then
        is_merge_node(i) = 1
        n_merge_node = n_merge_node + 1
      endif
    enddo

write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_node", n_merge_node
write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_node", is_merge_node

    !# エッジのマージ（自領域 + 受信領域）
    call monolis_alloc_I_1d(is_merge_edge, n_recv_edge)
    n_merge_edge = 0
    do i = 1, n_recv_edge
      i1 = recv_edge(2*i-1)
      i2 = recv_edge(2*i  )
      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, i1, id1)
      call monolis_bsearch_I(global_id_tmp, 1, graph_org%n_vertex, i2, id2)
      if(.not.(id1 /= -1 .and. id2 /= -1))then
        is_merge_edge(i) = 1
        n_merge_edge = n_merge_edge + 1
      endif
    enddo

write(100+monolis_mpi_get_global_my_rank(),*)"n_merge_edge", n_merge_edge
write(100+monolis_mpi_get_global_my_rank(),*)"is_merge_edge", is_merge_edge

    !# アップデートされたグラフの作成
    n_my_node = graph_org%n_vertex + n_merge_node
    n_my_edge = n_send_edge + n_merge_edge

    call monolis_alloc_I_1d(my_global_id, n_my_node)
    call monolis_alloc_I_1d(my_domain_id, n_my_node)
    call monolis_alloc_I_1d(my_edge, 2*n_my_edge)

    do i = 1, graph_org%n_vertex
      my_global_id(i) = graph_org%vertex_id(i)
      my_domain_id(i) = graph_org%vertex_domain_id(i)
    enddo

    in = graph_org%n_vertex
    do i = 1, n_merge_node
      if(is_merge_node(i) == 1)then
        in = in + 1
        my_global_id(in) = recv_global_id(i)
        my_domain_id(in) = recv_domain_new(i)
      endif
    enddo

    do i = 1, n_send_edge
      my_edge(2*i-1) = send_edge(2*i-1)
      my_edge(2*i  ) = send_edge(2*i  )
    enddo

    in = n_send_edge
    do i = 1, n_merge_edge
      if(is_merge_edge(i) == 1)then
        in = in + 1
        my_edge(2*in-1) = recv_edge(2*i-1)
        my_edge(2*in  ) = recv_edge(2*i  )
      endif
    enddo

write(100+monolis_mpi_get_global_my_rank(),*)"n_my_node", n_my_node
write(100+monolis_mpi_get_global_my_rank(),*)"n_my_edge", n_my_edge
write(100+monolis_mpi_get_global_my_rank(),*)"my_global_id", my_global_id
write(100+monolis_mpi_get_global_my_rank(),*)"my_domain_id", my_domain_id
write(100+monolis_mpi_get_global_my_rank(),*)"my_edge"
write(100+monolis_mpi_get_global_my_rank(),"(4i4)")my_edge
  end subroutine gedatsu_dlb_update_nodal_graph_main

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する計算点数の取得
  subroutine gedatsu_dlb_get_n_move_vertex(graph, n_move_vertex, is_move, did)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] オーバーラップ計算点を含む通信する計算点のフラグ
    integer(kint) :: is_move(:)
    !> [in] 領域番号
    integer(kint), intent(in) :: did
    integer(kint) :: i, j, jS, jE, jn

    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == did)then
        is_move(i) = 1
      else
        cycle
      endif
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        jn = graph%item(j)
        is_move(jn) = 1
      enddo
    enddo

    n_move_vertex = 0
    do i = 1, graph%n_vertex
      if(is_move(i) == 1) n_move_vertex = n_move_vertex + 1
    enddo
  end subroutine gedatsu_dlb_get_n_move_vertex

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信するエッジ数の取得
  subroutine gedatsu_dlb_get_n_move_edge(graph, n_move_edge, is_move_node, is_move)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: n_move_edge
    !> [in] オーバーラップ計算点を含む通信する節点のフラグ
    integer(kint) :: is_move_node(:)
    !> [in] オーバーラップ計算点を含む通信するエッジのフラグ
    integer(kint) :: is_move(:)
    integer(kint) :: i, j, jS, jE, jn

    do i = 1, graph%n_vertex
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        jn = graph%item(j)
        if(is_move_node(i) == 1 .and. is_move_node(jn) == 1)then
          is_move(j) = 1
        endif
      enddo
    enddo

    n_move_edge = 0
    do i = 1, graph%index(graph%n_vertex + 1)
      if(is_move(i) == 1) n_move_edge = n_move_edge + 1
    enddo
  end subroutine gedatsu_dlb_get_n_move_edge
end module mod_gedatsu_dlb_comm
