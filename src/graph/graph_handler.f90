!> グラフ操作モジュール
module mod_gedatsu_graph_handler
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_alloc
  use mod_gedatsu_std

  implicit none

contains

  !> @ingroup group_graph_1
  !> グラフのノード数を指定
  !> @details 既にノードが設定されている場合はエラー終了する。
  subroutine gedatsu_graph_set_n_vertex(graph, n_vertex)
    implicit none
    !> [inout] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] グラフのノード数
    integer(gint) :: n_vertex
    integer(gint) :: i

    if(graph%n_vertex > 0)then
      call gedatsu_error_string("gedatsu_graph_set_n_vertex")
      call gedatsu_error_string("graph nodes are already defined")
      call gedatsu_error_stop()
    endif

    call gedatsu_alloc_int_1d(graph%vertex_id, n_vertex)
    call gedatsu_alloc_int_1d(graph%vertex_domain_id, n_vertex)
    call gedatsu_alloc_int_1d(graph%index, n_vertex + 1)
  end subroutine gedatsu_graph_set_n_vertex

!!  !> @ingroup group_graph_1
!!  !> グラフにノードを追加
!!  subroutine gedatsu_graph_add_n_vertex(graph, n_vertex_add)
!!    implicit none
!!    !> [inout] graph 構造体
!!    type(gedatsu_graph) :: graph
!!    !> [in] グラフに追加するノード数
!!    integer(gint) :: n_vertex_add
!!    integer(gint) :: n_vertex_all, i
!!
!!    n_vertex_all = graph%n_vertex + n_vertex_add
!!    !call gedatsu_realloc_int_1d(graph%vertex_id, n_vertex_all)
!!    !call gedatsu_realloc_int_1d(graph%vertex_domain_id, n_vertex_all)
!!    call gedatsu_realloc_int_1d(graph%index, n_vertex_all + 1)
!!    graph%n_vertex = n_vertex_all
!!
!!    !do i = graph%n_vertex + 1, n_vertex_add
!!    !  graph%vertex_domain_id(i) = 1
!!    !enddo
!!  end subroutine gedatsu_graph_add_n_vertex

  !> @ingroup group_graph_1
  !> グラフのノード数を取得
  subroutine gedatsu_graph_get_n_vertex(graph, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [out] グラフのノード数
    integer(gint) :: n_vertex
    n_vertex = graph%n_vertex
  end subroutine gedatsu_graph_get_n_vertex

  !> @ingroup group_graph_1
  !> 領域番号 domain_id に属するノード数を取得
  subroutine gedatsu_graph_get_n_vertex_in_subdomain(graph, domain_id, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフのノード数
    integer(gint) :: n_vertex
    integer(gint) :: i

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == domain_id) n_vertex = n_vertex + 1
    enddo
  end subroutine gedatsu_graph_get_n_vertex_in_subdomain

  !> @ingroup group_graph_1
  !> 領域番号 domain_id のオーバーラッピング領域に属するノード数を取得
  subroutine gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフのノード数
    integer(gint) :: n_vertex
    integer(gint) :: i, j, jS, jE, nid

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) /= domain_id) n_vertex = n_vertex + 1
      enddo
    enddo
  end subroutine gedatsu_graph_get_n_vertex_in_overlap_region

  !> @ingroup group_graph_1
  !> 領域番号 domain_id に属するノード番号を取得
  subroutine gedatsu_graph_get_vertex_id_in_subdomain(graph, domain_id, ids)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] 領域番号 domain_id に属する節点番号
    integer(gint) :: ids(:)
    integer(gint) :: i, n_vertex

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == domain_id)then
        n_vertex = n_vertex + 1
        ids(n_vertex) = graph%vertex_id(i)
      endif
    enddo
  end subroutine gedatsu_graph_get_vertex_id_in_subdomain

  !> @ingroup group_graph_1
  !> 領域番号 domain_id のオーバーラッピング領域に属するノード番号を取得
  subroutine gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] 領域番号 domain_id に属する節点番号
    integer(gint) :: ids(:)
    integer(gint) :: n_vertex
    integer(gint) :: i, j, jS, jE, nid

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) /= domain_id)then
          n_vertex = n_vertex + 1
          ids(n_vertex) = graph%vertex_id(i)
        endif
      enddo
    enddo
  end subroutine gedatsu_graph_get_vertex_id_in_overlap_region

!!  !> @ingroup group_graph_1
!!  !> グラフの i 番目のノードを削除
!!  !> @details i 番目のノードに関連するエッジも削除される。
!!  subroutine gedatsu_graph_delete_vertex(graph, vertex_id)
!!    implicit none
!!    !> [in] graph 構造体
!!    type(gedatsu_graph) :: graph
!!    !> [in] ノード番号 i
!!    integer(gint) :: vertex_id
!!  end subroutine gedatsu_graph_delete_vertex

  !> @ingroup group_graph_1
  !> グラフのエッジ数を取得
  subroutine gedatsu_graph_get_n_edge(graph, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [out] グラフのエッジ数
    integer(gint) :: n_edge
    n_edge = graph%index(graph%n_vertex + 1)
  end subroutine gedatsu_graph_get_n_edge

  !> @ingroup group_graph_1
  !> 領域番号 domain_id に属するグラフのエッジ数を取得
  subroutine gedatsu_graph_get_n_edge_in_subdomain(graph, domain_id, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフのエッジ数
    integer(gint) :: n_edge
    integer(gint) :: i, j, jS, jE, nid

    n_edge = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) == domain_id) n_edge = n_edge + 1
      enddo
    enddo
  end subroutine gedatsu_graph_get_n_edge_in_subdomain

  !> @ingroup group_graph_1
  !> 領域番号 domain_id のオーバーラッピング領域に属するエッジ数を取得
  subroutine gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフのエッジ数
    integer(gint) :: n_edge
    integer(gint) :: i, j, jS, jE, nid

    n_edge = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        !> 無向グラフのため 2 を加算
        if(graph%vertex_domain_id(nid) /= domain_id) n_edge = n_edge + 2
      enddo
    enddo
  end subroutine gedatsu_graph_get_n_edge_in_overlap_region

  !> @ingroup group_graph_1
  !> 領域番号 domain_id に属するグラフのエッジを取得
  !> @details エッジの組はローカル節点番号で表現される
  subroutine gedatsu_graph_get_edge_in_subdomain(graph, domain_id, index, item)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフの index 配列
    integer(gint) :: index(:)
    !> [out] グラフの item 配列
    integer(gint) :: item(:)

    integer(gint) :: i, nid, local_id, idx, j, jS, jE
    integer(gint) :: n_vertex, n_edge, n_edge_all, n_node_cur
    integer(gint), allocatable :: ids(:)
    integer(gint), allocatable :: perm(:)

    call gedatsu_graph_get_n_vertex_in_subdomain(graph, domain_id, n_vertex)

    call gedatsu_alloc_int_1d(ids, n_vertex)

    call gedatsu_graph_get_vertex_id_in_subdomain(graph, domain_id, ids)

    call gedatsu_alloc_int_1d(perm, n_vertex)

    call gedatsu_get_sequence_array_int(perm, n_vertex, 1, 1)

    call gedatsu_qsort_int_with_perm(ids, 1, n_vertex, perm)

    n_edge_all = 0
    n_node_cur = 0
    index = 0
    item = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      n_node_cur = n_node_cur + 1
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      n_edge = 0
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) == domain_id)then
          n_edge_all = n_edge_all + 1
          n_edge = n_edge + 1
          call gedatsu_bsearch_int(ids, 1, n_vertex, nid, idx)
          local_id = perm(idx)
          item(n_edge_all) = local_id
        endif
      enddo
      index(n_node_cur) = n_edge
    enddo

    do i = 1, n_vertex
      index(i+1) = index(i+1) + index(i)
    enddo
  end subroutine gedatsu_graph_get_edge_in_subdomain

  !> @ingroup group_graph_1
  !> 領域番号 domain_id のオーバーラッピング領域に属するエッジを取得
  subroutine gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] 領域番号
    integer(gint) :: domain_id
    !> [out] グラフエッジ
    integer(gint) :: edge(:,:)
    integer(gint) :: i, j, jS, jE, nid
    integer(gint) :: n_edge

    n_edge = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) /= domain_id)then
          n_edge = n_edge + 1
          edge(1,n_edge) = i
          edge(2,n_edge) = nid
          n_edge = n_edge + 1
          edge(1,n_edge) = nid
          edge(2,n_edge) = i
        endif
      enddo
    enddo
  end subroutine gedatsu_graph_get_edge_in_overlap_region

  !> @ingroup group_graph_1
  !> グラフのエッジを追加
  subroutine gedatsu_graph_add_edge(graph, n_edge, edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] グラフのエッジ数
    integer(gint) :: n_edge
    !> [in] グラフエッジ
    integer(gint) :: edge(:,:)
  end subroutine gedatsu_graph_add_edge

end module mod_gedatsu_graph_handler
