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
