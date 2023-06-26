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
  subroutine gedatsu_dlb_get_comm_table(dlb, graph, comm)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    !# オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: comm_size
    integer(kint) :: n_move_vertex, n_move_vertex_all
    integer(kint) :: n_move_edge, n_move_edge_all
    integer(kint), allocatable :: move_global_id(:)
    integer(kint), allocatable :: move_global_id_all(:)
    integer(kint), allocatable :: move_domain_id_new(:)
    integer(kint), allocatable :: move_domain_id_new_all(:)
    integer(kint), allocatable :: move_domain_id_org(:)
    integer(kint), allocatable :: move_domain_id_org_all(:)
    integer(kint), allocatable :: move_global_edge_node(:)
    integer(kint), allocatable :: move_global_edge_node_all(:)
    integer(kint), allocatable :: is_move_node(:)
    integer(kint), allocatable :: is_move_edge(:)
    integer(kint), allocatable :: counts_node(:)
    integer(kint), allocatable :: counts_edge(:)

write(100+monolis_mpi_get_global_my_rank(),*)"graph    ", graph%vertex_domain_id
write(100+monolis_mpi_get_global_my_rank(),*)"vertex_id", graph%vertex_id

    !# 送信計算点の全体情報取得
    comm_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(is_move_node, graph%n_vertex)

    call gedatsu_dlb_get_n_move_vertex(graph, n_move_vertex, is_move_node, comm)

    call monolis_alloc_I_1d(move_global_id, n_move_vertex)

    call gedatsu_dlb_get_move_vertex_global_id(graph, n_move_vertex, is_move_node, move_global_id)

    call monolis_alloc_I_1d(move_domain_id_new, n_move_vertex)

    call gedatsu_dlb_get_move_vertex_domain_id(graph, n_move_vertex, is_move_node, move_domain_id_new)

    call monolis_alloc_I_1d(move_domain_id_org, n_move_vertex)

    move_domain_id_org = monolis_mpi_get_local_my_rank(comm)

    n_move_vertex_all = n_move_vertex
    call monolis_allreduce_I1(n_move_vertex_all, monolis_mpi_sum, comm)

    call monolis_alloc_I_1d(counts_node, comm_size + 1)
    call monolis_allgather_I1(n_move_vertex, counts_node, comm)

    call monolis_alloc_I_1d(move_global_id_all, n_move_vertex_all)
    call monolis_alloc_I_1d(move_domain_id_new_all, n_move_vertex_all)
    call monolis_alloc_I_1d(move_domain_id_org_all, n_move_vertex_all)

    call gedatsu_dlb_get_move_vertex_global_id_all(graph, n_move_vertex, counts_node, &
      & move_global_id, move_global_id_all, comm)

    call gedatsu_dlb_get_move_vertex_domain_id_all(graph, n_move_vertex, counts_node, &
      & move_domain_id_new, move_domain_id_new_all, comm)

    call gedatsu_dlb_get_move_vertex_domain_id_all(graph, n_move_vertex, counts_node, &
      & move_domain_id_org, move_domain_id_org_all, comm)

    !# 送信エッジの全体情報取得
    call monolis_alloc_I_1d(is_move_edge, graph%index(graph%n_vertex + 1))

    call gedatsu_dlb_get_n_move_edge(graph, n_move_edge, is_move_node, is_move_edge, comm)

    call monolis_alloc_I_1d(move_global_edge_node, 2*n_move_edge)

    call gedatsu_dlb_get_move_global_edge_node(graph, n_move_edge, is_move_edge, move_global_edge_node)

    n_move_edge_all = n_move_edge
    call monolis_allreduce_I1(n_move_edge_all, monolis_mpi_sum, comm)

    call monolis_alloc_I_1d(counts_edge, comm_size + 1)
    call monolis_allgather_I1(n_move_edge, counts_edge, comm)

    call monolis_alloc_I_1d(move_global_edge_node_all, 2*n_move_edge_all)

    call gedatsu_dlb_get_move_global_edge_node_all(graph, n_move_edge, counts_edge, &
      & move_global_edge_node, move_global_edge_node_all, comm)

write(100+monolis_mpi_get_global_my_rank(),*)"n_move_vertex", n_move_vertex
write(100+monolis_mpi_get_global_my_rank(),*)"n_move_vertex_all", n_move_vertex_all
write(100+monolis_mpi_get_global_my_rank(),*)"move_global_id_all    ", move_global_id_all
write(100+monolis_mpi_get_global_my_rank(),*)"move_domain_id_new_all", move_domain_id_new_all
write(100+monolis_mpi_get_global_my_rank(),*)"move_domain_id_org_all", move_domain_id_org_all

write(100+monolis_mpi_get_global_my_rank(),*)"n_move_edge", n_move_edge
write(100+monolis_mpi_get_global_my_rank(),*)"n_move_edge_all", n_move_edge_all
write(100+monolis_mpi_get_global_my_rank(),*)"move_global_edge_node_all", move_global_edge_node_all

    !# n_vertex の取得
    !# n_internal_vertex の取得
    !# n_edge の取得
    !# graph index / item の取得
    call gedatsu_dlb_get_new_graph(graph, n_move_vertex_all, &
    & move_global_id_all, move_domain_id_new_all, move_domain_id_org_all, &
    & n_move_edge_all, move_global_edge_node_all, comm)

    !# send table の作成
    !# recv table の作成
    !call gedatsu_dlb_get_comm_table_main()
  end subroutine gedatsu_dlb_get_comm_table

  !> @ingroup group_dlb
  !> 更新後のグラフの取得
  subroutine gedatsu_dlb_get_new_graph(graph, n_move_vertex_all, &
    & move_global_id_all, move_domain_id_new_all, move_domain_id_org_all, &
    & n_move_edge_all, move_global_edge_node_all, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] graph 構造体
    integer(kint) :: n_move_vertex_all
    !> [in] graph 構造体
    integer(kint) :: n_move_edge_all
    !> [in] graph 構造体
    integer(kint), allocatable :: move_global_id_all(:)
    !> [in] graph 構造体
    integer(kint), allocatable :: move_domain_id_new_all(:)
    !> [in] graph 構造体
    integer(kint), allocatable :: move_domain_id_org_all(:)
    !> [in] graph 構造体
    integer(kint), allocatable :: move_global_edge_node_all(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: n_my_global_id
    integer(kint) :: n_my_edge
    integer(kint) :: i, my_rank, itmp
    integer(kint), allocatable :: my_global_id(:)
    integer(kint), allocatable :: my_global_id_perm(:)

    !# 更新後の節点の取得
    my_rank = monolis_mpi_get_local_my_rank(comm)

    n_my_global_id = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == my_rank)then
        n_my_global_id = n_my_global_id + 1
      endif
    enddo

    do i = 1, n_move_vertex_all
      if(move_domain_id_new_all(i) /= my_rank)then
        n_my_global_id = n_my_global_id + 1
      endif
    enddo

write(100+monolis_mpi_get_global_my_rank(),*)"n_my_global_id", n_my_global_id

    call monolis_alloc_I_1d(my_global_id, n_my_global_id)

    n_my_global_id = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == my_rank)then
        n_my_global_id = n_my_global_id + 1
        my_global_id(n_my_global_id) = graph%vertex_id(i)
      endif
    enddo

    do i = 1, n_move_vertex_all
      if(move_domain_id_new_all(i) /= my_rank)then
        n_my_global_id = n_my_global_id + 1
        my_global_id(n_my_global_id) = move_global_id_all(i)
      endif
    enddo

write(100+monolis_mpi_get_global_my_rank(),*)"my_global_id", my_global_id

    call monolis_alloc_I_1d(my_global_id_perm, n_my_global_id)

    call monolis_get_sequence_array_I(my_global_id_perm, n_my_global_id, 1, 1)

    call monolis_qsort_I_2d(my_global_id, my_global_id_perm, 1, n_my_global_id)

    !# 更新後のエッジの取得
    n_my_edge = 0
  end subroutine gedatsu_dlb_get_new_graph

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
  !> オーバーラップ計算点を含む通信する計算点のグローバル id の取得
  subroutine gedatsu_dlb_get_move_vertex_global_id(graph, n_move_vertex, is_move, move_global_id)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] オーバーラップ計算点を含む通信する計算点のフラグ
    integer(kint) :: is_move(:)
    !> [in] オーバーラップ計算点を含む通信する計算点のグローバル id
    integer(kint) :: move_global_id(:)
    integer(kint) :: i, in

    in = 0
    do i = 1, graph%n_vertex
      if(is_move(i) == 1)then
        in = in + 1
        move_global_id(in) = graph%vertex_id(i)
      endif
    enddo
  end subroutine gedatsu_dlb_get_move_vertex_global_id

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する計算点の領域 id の取得
  subroutine gedatsu_dlb_get_move_vertex_domain_id(graph, n_move_vertex, is_move, move_domain_id)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] オーバーラップ計算点を含む通信する計算点のフラグ
    integer(kint) :: is_move(:)
    !> [in] オーバーラップ計算点を含む通信する計算点の領域 id
    integer(kint) :: move_domain_id(:)
    integer(kint) :: i, in

    in = 0
    do i = 1, graph%n_vertex
      if(is_move(i) == 1)then
        in = in + 1
        move_domain_id(in) = graph%vertex_domain_id(i)
      endif
    enddo
  end subroutine gedatsu_dlb_get_move_vertex_domain_id

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する全ての計算点のグローバル計算点 id の取得
  subroutine gedatsu_dlb_get_move_vertex_global_id_all(graph, n_move_vertex, counts, move_global_id, move_global_id_all, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] 各領域の計算点数配列
    integer(kint) :: counts(:)
    !> [in] オーバーラップ計算点を含む通信する計算点のグローバル id
    integer(kint) :: move_global_id(:)
    !> [in] オーバーラップ計算点を含む通信する全ての計算点のグローバル id
    integer(kint) :: move_global_id_all(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: comm_size, i
    integer(kint), allocatable :: displs(:)

    comm_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(displs, comm_size + 1)

    do i = 1, comm_size
      displs(i + 1) = displs(i) + counts(i)
    enddo

    call monolis_allgatherv_I(n_move_vertex, move_global_id, move_global_id_all, counts, displs, comm)
  end subroutine gedatsu_dlb_get_move_vertex_global_id_all

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する全ての計算点のグローバル領域 id の取得
  subroutine gedatsu_dlb_get_move_vertex_domain_id_all(graph, n_move_vertex, counts, move_domain_id, move_domain_id_all, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_vertex
    !> [in] 各領域の計算点数配列
    integer(kint) :: counts(:)
    !> [in] オーバーラップ計算点を含む通信する計算点のグローバル id
    integer(kint) :: move_domain_id(:)
    !> [in] オーバーラップ計算点を含む通信する全ての計算点のグローバル id
    integer(kint) :: move_domain_id_all(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: comm_size, i
    integer(kint), allocatable :: displs(:)

    comm_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(displs, comm_size + 1)

    do i = 1, comm_size
      displs(i + 1) = displs(i) + counts(i)
    enddo

    call monolis_allgatherv_I(n_move_vertex, move_domain_id, move_domain_id_all, counts, displs, comm)
  end subroutine gedatsu_dlb_get_move_vertex_domain_id_all

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

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信するエッジの取得
  subroutine gedatsu_dlb_get_move_global_edge_node(graph, n_move_edge, is_move, move_global_edge_node)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信する計算点数
    integer(kint) :: n_move_edge
    !> [in] オーバーラップ計算点を含む通信する計算点のフラグ
    integer(kint) :: is_move(:)
    !> [in] オーバーラップ計算点を含む通信する計算点のグローバル id
    integer(kint) :: move_global_edge_node(:)
    integer(kint) :: i, in, j, jn, jS, jE, id1, id2

    in = 0
    do i = 1, graph%n_vertex
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        jn = graph%item(j)
        if(is_move(j) == 1)then
          id1 = graph%vertex_id(i)
          id2 = graph%vertex_id(jn)
          in = in + 1
          move_global_edge_node(2*in-1) = id1
          move_global_edge_node(2*in  ) = id2
        endif
      enddo
    enddo
  end subroutine gedatsu_dlb_get_move_global_edge_node

  !> @ingroup group_dlb
  !> オーバーラップ計算点を含む通信する全ての計算点のグローバル計算点 id の取得
  subroutine gedatsu_dlb_get_move_global_edge_node_all(graph, n_move_edge, counts, &
    & move_global_edge_node, move_global_edge_node_all, comm)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] オーバーラップ計算点を含む通信するエッジ数
    integer(kint) :: n_move_edge
    !> [in] 各領域の計算点数配列
    integer(kint) :: counts(:)
    !> [in] オーバーラップ計算点を含む通信するエッジのグローバル id
    integer(kint) :: move_global_edge_node(:)
    !> [in] オーバーラップ計算点を含む通信する全てのエッジのグローバル id
    integer(kint) :: move_global_edge_node_all(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint) :: comm_size, i
    integer(kint), allocatable :: displs(:)

    comm_size = monolis_mpi_get_local_comm_size(comm)

    call monolis_alloc_I_1d(displs, comm_size + 1)

    do i = 1, comm_size
      displs(i + 1) = displs(i) + 2*counts(i)
    enddo

    call monolis_allgatherv_I(2*n_move_edge, move_global_edge_node, move_global_edge_node_all, 2*counts, displs, comm)
  end subroutine gedatsu_dlb_get_move_global_edge_node_all
end module mod_gedatsu_dlb_comm
