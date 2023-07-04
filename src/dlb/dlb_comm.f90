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
  subroutine gedatsu_dlb_update_nodal_graph_main(dlb, graph, graph_new, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] graph 構造体
    type(gedatsu_graph), intent(out) :: graph_new
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !# オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: comm_size, my_rank, i, j, in, jS, jE, jn, i1, i2
    integer(kint) :: n_send_vertex, n_send_edge
!    integer(kint) :: n_move_edge, n_move_edge_all
!    integer(kint), allocatable :: move_global_id(:)
!    integer(kint), allocatable :: move_global_id_all(:)
!    integer(kint), allocatable :: move_domain_id_new(:)
!    integer(kint), allocatable :: move_domain_id_new_all(:)
!    integer(kint), allocatable :: domain_id_org(:)
!    integer(kint), allocatable :: move_domain_id_org(:)
!    integer(kint), allocatable :: move_domain_id_org_all(:)
!    integer(kint), allocatable :: move_global_edge_node(:)
!    integer(kint), allocatable :: move_global_edge_node_all(:)
    integer(kint), allocatable :: is_send_node(:)
    integer(kint), allocatable :: is_send_edge(:)
    integer(kint), allocatable :: send_n_node_list(:)
    integer(kint), allocatable :: recv_n_node_list(:)
    integer(kint), allocatable :: send_n_edge_list(:)
    integer(kint), allocatable :: recv_n_edge_list(:)

    !# 送信計算点の情報取得
    my_rank = monolis_mpi_get_local_my_rank(COM%comm)
    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    call monolis_alloc_I_1d(is_send_node, graph%n_vertex)
    call gedatsu_dlb_get_n_move_vertex(graph, n_send_vertex, is_send_node, COM%comm)

    !# 送信エッジの情報取得
    call monolis_alloc_I_1d(is_send_edge, graph%index(graph%n_vertex + 1))
    call gedatsu_dlb_get_n_move_edge(graph, n_send_edge, is_send_node, is_send_edge, COM%comm)

    !# 送信計算点の全体情報取得
    call monolis_alloc_I_1d(send_n_node_list, comm_size)
    call monolis_alloc_I_1d(recv_n_node_list, comm_size)

    !> call gedatsu_dlb_get_n_node_list()
    do i = 1, graph%n_vertex
      if(is_send_node(i) == 1)then
        in = graph%vertex_domain_id(i)
        send_n_node_list(in + 1) = send_n_node_list(in + 1) + 1
      endif
    enddo

    recv_n_node_list = send_n_node_list
    call monolis_alltoall_I1(comm_size, recv_n_node_list, COM%comm)

    !# エッジの全体情報取得
    call monolis_alloc_I_1d(send_n_edge_list, comm_size)
    call monolis_alloc_I_1d(recv_n_edge_list, comm_size)

    !> call gedatsu_dlb_get_n_edge_list()
    do i = 1, graph%n_vertex
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        jn = graph%item(j)
        i1 = graph%vertex_domain_id(i)
        i2 = graph%vertex_domain_id(jn)
        send_n_edge_list(i1 + 1) = send_n_edge_list(i1 + 1) + 1
        send_n_edge_list(i2 + 1) = send_n_edge_list(i2 + 1) + 1
      enddo
    enddo

    recv_n_edge_list = send_n_edge_list
    call monolis_alltoall_I1(comm_size, recv_n_edge_list, COM%comm)

write(100+monolis_mpi_get_global_my_rank(),*)"send_n_node_list", send_n_node_list
write(100+monolis_mpi_get_global_my_rank(),*)"recv_n_node_list", recv_n_node_list
write(100+monolis_mpi_get_global_my_rank(),*)"send_n_edge_list", send_n_edge_list
write(100+monolis_mpi_get_global_my_rank(),*)"recv_n_edge_list", recv_n_edge_list

    !# send table の作成
    !# recv table の作成
    call gedatsu_dlb_get_comm_table_main(dlb, graph, &
      & is_send_node, is_send_edge, &
      & send_n_node_list, recv_n_node_list, &
      & send_n_edge_list, recv_n_edge_list, &
      & COM)

    !# n_vertex の取得
    !# n_internal_vertex の取得
    !# n_edge の取得
    !# graph index / item の取得

  end subroutine gedatsu_dlb_update_nodal_graph_main

  !> @ingroup group_dlb
  !> データ通信のための通信テーブルの作成
  subroutine gedatsu_dlb_get_comm_table_main(dlb, graph, &
    & is_send_node, is_send_edge, &
    & send_n_node_list, recv_n_node_list, &
    & send_n_edge_list, recv_n_edge_list, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(inout) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] dlb 構造体
    integer(kint) :: is_send_node(:)
    integer(kint) :: is_send_edge(:)
    integer(kint) :: send_n_node_list(:)
    integer(kint) :: recv_n_node_list(:)
    integer(kint) :: send_n_edge_list(:)
    integer(kint) :: recv_n_edge_list(:)
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
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

    !# send table の作成

    !# recv table の作成
  end subroutine gedatsu_dlb_get_comm_table_main

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する計算点数の取得
  subroutine gedatsu_dlb_get_n_move_vertex(graph, n_move_vertex, is_move, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] オーバーラップ計算点を含む通信する計算点のフラグ
    integer(kint) :: is_move(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, j, jS, jE, jn, my_rank

    my_rank = monolis_mpi_get_local_my_rank(comm)

    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= my_rank)then
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
  subroutine gedatsu_dlb_get_n_move_edge(graph, n_move_edge, is_move_node, is_move, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: n_move_edge
    !> [in] オーバーラップ計算点を含む通信する節点のフラグ
    integer(kint) :: is_move_node(:)
    !> [in] オーバーラップ計算点を含む通信するエッジのフラグ
    integer(kint) :: is_move(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: i, j, jS, jE, jn, my_rank

    my_rank = monolis_mpi_get_local_my_rank(comm)

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
