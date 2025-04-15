!> グラフ操作モジュール
!> ノードの組を渡してエッジ番号を得る関数
!# gedatsu_graph_set_n_vertex(graph, n_vertex)
!# gedatsu_graph_add_n_vertex(graph, n_vertex_add)
!# gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)
!# gedatsu_graph_get_n_vertex(graph, n_vertex)
!# gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)
!# gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)
!# gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids)
!# gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)
!# gedatsu_graph_get_n_edge(graph, n_edge)
!# gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)
!# gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)
!# gedatsu_graph_get_edge_in_internal_region(graph, domain_id, edge)
!# gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)
!# gedatsu_graph_set_edge(graph, n_edge, edge)
!# gedatsu_graph_add_edge(graph, n_edge, edge)
!# gedatsu_graph_delete_dupulicate_edge(graph)
!# gedatsu_graph_get_edge_in_internal_region_conn_graph(graph, n_edge, edge)
module mod_gedatsu_graph_handler
  use mod_monolis_utils
  use mod_gedatsu_graph

  implicit none

contains

  !> @ingroup graph_basic
  !> グラフのノード数を指定
  !> @details 既にノードが設定されている場合はエラー終了する。
  subroutine gedatsu_graph_set_n_vertex(graph, n_vertex)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] グラフのノード数
    integer(kint), intent(in) :: n_vertex

    if(graph%n_vertex > 0)then
      call monolis_std_error_string("gedatsu_graph_set_n_vertex")
      call monolis_std_error_string("graph nodes are already defined")
      call monolis_std_error_stop()
    endif

    if(n_vertex < 1)then
      call monolis_std_warning_string("gedatsu_graph_set_n_vertex")
      call monolis_std_warning_string("n_vertex is less than 1")
      return
    endif

    call monolis_alloc_I_1d(graph%vertex_id, n_vertex)
    call monolis_alloc_I_1d(graph%vertex_domain_id, n_vertex)
    call monolis_alloc_I_1d(graph%index, n_vertex + 1)

    graph%n_vertex = n_vertex
  end subroutine gedatsu_graph_set_n_vertex

  !> @ingroup graph_basic
  !> グラフにノードを追加
  !> @details 追加されたノードに対応する節点番号、領域番号は 0 初期化される。
  subroutine gedatsu_graph_add_n_vertex(graph, n_vertex_add)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] グラフに追加するノード数
    integer(kint), intent(in) :: n_vertex_add
    integer(kint) :: n_vertex_all

    if(n_vertex_add < 1)then
      call monolis_std_warning_string("gedatsu_graph_add_n_vertex")
      call monolis_std_warning_string("n_vertex is less than 1")
      return
    endif

    n_vertex_all = graph%n_vertex + n_vertex_add

    call monolis_realloc_I_1d(graph%vertex_id, n_vertex_all)
    call monolis_realloc_I_1d(graph%vertex_domain_id, n_vertex_all)
    call monolis_realloc_I_1d(graph%index, n_vertex_all + 1)

    graph%n_vertex = n_vertex_all
  end subroutine gedatsu_graph_add_n_vertex

  !> @ingroup graph_basic
  !> グラフにノードを追加
  !> @details 追加されたノードに対応する節点番号は引数で初期化され、領域番号は 0 初期化される。
  subroutine gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] グラフに追加するノード数
    integer(kint), intent(in) :: n_vertex_add
    !> [in] グラフに追加するノードに対応する節点番号
    integer(kint), intent(in) :: vertex_id(:)
    integer(kint) :: n_vertex_all, i

    if(n_vertex_add < 1)then
      call monolis_std_warning_string("gedatsu_graph_add_n_vertex_with_vertex_id")
      call monolis_std_warning_string("n_vertex is less than 1")
      return
    endif

    if(size(vertex_id) /= n_vertex_add)then
      call monolis_std_warning_string("gedatsu_graph_add_n_vertex_with_vertex_id")
      call monolis_std_warning_string("size of vertex_id is not equal to n_vertex_add")
      return
    endif

    n_vertex_all = graph%n_vertex + n_vertex_add

    call monolis_realloc_I_1d(graph%vertex_id, n_vertex_all)
    call monolis_realloc_I_1d(graph%vertex_domain_id, n_vertex_all)
    call monolis_realloc_I_1d(graph%index, n_vertex_all + 1)

    do i = 1, n_vertex_add
      graph%vertex_id(graph%n_vertex + i) = vertex_id(i)
      graph%index(graph%n_vertex + i + 1) = graph%index(graph%n_vertex + i + 1) + graph%index(graph%n_vertex + i)
    enddo

    graph%n_vertex = n_vertex_all
  end subroutine gedatsu_graph_add_n_vertex_with_vertex_id

  !> @ingroup graph_basic
  !> グラフのノード数を取得
  subroutine gedatsu_graph_get_n_vertex(graph, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [out] グラフのノード数
    integer(kint), intent(out) :: n_vertex
    n_vertex = graph%n_vertex
  end subroutine gedatsu_graph_get_n_vertex

  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するノード数を取得
  subroutine gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのノード数
    integer(kint), intent(out) :: n_vertex
    integer(kint) :: i

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == domain_id) n_vertex = n_vertex + 1
    enddo
  end subroutine gedatsu_graph_get_n_vertex_in_internal_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id のオーバーラッピング領域に属するノード数を取得
  subroutine gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのノード数
    integer(kint), intent(out) :: n_vertex
    integer(kint) :: i, j, jS, jE, nid
    integer(kint), allocatable :: is_used(:)

    call monolis_alloc_I_1d(is_used, graph%n_vertex)

    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) /= domain_id)then
          is_used(nid) = 1
        endif
      enddo
    enddo

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(is_used(i) == 1) n_vertex = n_vertex + 1
    enddo
  end subroutine gedatsu_graph_get_n_vertex_in_overlap_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するノード番号を取得
  subroutine gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] 領域番号 domain_id に属する節点番号
    integer(kint), intent(out) :: ids(:)
    integer(kint) :: i, n_vertex

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) == domain_id)then
        n_vertex = n_vertex + 1
        ids(n_vertex) = graph%vertex_id(i)
      endif
    enddo
  end subroutine gedatsu_graph_get_vertex_id_in_internal_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id のオーバーラッピング領域に属するノード番号を取得
  subroutine gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] 領域番号 domain_id に属する節点番号
    integer(kint), intent(out) :: ids(:)
    integer(kint) :: n_vertex
    integer(kint) :: i, j, jS, jE, nid
    integer(kint), allocatable :: is_used(:)

    call monolis_alloc_I_1d(is_used, graph%n_vertex)

    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) /= domain_id)then
          is_used(nid) = 1
        endif
      enddo
    enddo

    n_vertex = 0
    do i = 1, graph%n_vertex
      if(is_used(i) == 1)then
        n_vertex = n_vertex + 1
        ids(n_vertex) = graph%vertex_id(i)
      endif
    enddo
  end subroutine gedatsu_graph_get_vertex_id_in_overlap_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するノードのフラグ配列を取得
  subroutine gedatsu_graph_get_flag_in_subdomain(graph, domain_id, is_in, is_border)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] オーバーラッピング領域に属するノードのフラグ
    logical, intent(out) :: is_in(:)
    !> [out] オーバーラッピング領域に接するノードのフラグ
    logical, intent(out) :: is_border(:)
    integer(kint) :: i, j, jS, jE, nid
    logical :: is_find

    is_in = .false.
    is_border = .false.
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      is_find = .false.
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) == domain_id) cycle
        is_in(nid) = .true.
        is_find = .true.
      enddo
      if(is_find) is_border(i) = .true.
    enddo
  end subroutine gedatsu_graph_get_flag_in_subdomain

  !> @ingroup graph_basic
  !> グラフのエッジ数を取得
  subroutine gedatsu_graph_get_n_edge(graph, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [out] グラフのエッジ数
    integer(kint), intent(out) :: n_edge
    n_edge = graph%index(graph%n_vertex + 1)
  end subroutine gedatsu_graph_get_n_edge

  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するグラフのエッジ数を取得
  subroutine gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのエッジ数
    integer(kint), intent(out) :: n_edge
    integer(kint) :: i, j, jS, jE, nid

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
  end subroutine gedatsu_graph_get_n_edge_in_internal_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id のオーバーラッピング領域に属するエッジ数を取得
  subroutine gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのエッジ数
    integer(kint), intent(out) :: n_edge
    integer(kint) :: i, j, jS, jE, nid
    logical, allocatable :: is_in(:)
    logical, allocatable :: is_border(:)

    call monolis_alloc_L_1d(is_in, graph%n_vertex)
    call monolis_alloc_L_1d(is_border, graph%n_vertex)

    call gedatsu_graph_get_flag_in_subdomain(graph, domain_id, is_in, is_border)

    n_edge = 0
    do i = 1, graph%n_vertex
      if(.not. is_border(i)) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(is_in(nid)) n_edge = n_edge + 1
      enddo
    enddo

    do i = 1, graph%n_vertex
      if(.not. is_in(i)) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(is_in(nid) .or. is_border(nid)) n_edge = n_edge + 1
      enddo
    enddo
  end subroutine gedatsu_graph_get_n_edge_in_overlap_region

  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するグラフのエッジを取得
  !> @details エッジの組はローカル節点番号で表現される
  subroutine gedatsu_graph_get_edge_in_internal_region(graph, domain_id, edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのエッジ配列
    integer(kint), intent(out) :: edge(:,:)

    integer(kint) :: i, nid, idx, j, jS, jE
    integer(kint) :: n_vertex, n_edge, e1, e2, n1, n2
    integer(kint), allocatable :: ids(:)
    integer(kint), allocatable :: perm(:)

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    call monolis_alloc_I_1d(ids, n_vertex)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids)

    call monolis_alloc_I_1d(perm, n_vertex)

    call monolis_get_sequence_array_I(perm, n_vertex, 1, 1)

    call monolis_qsort_I_2d(ids, perm, 1, n_vertex)

    n_edge = 0
    do i = 1, graph%n_vertex
      if(graph%vertex_domain_id(i) /= domain_id) cycle
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        nid = graph%item(j)
        if(graph%vertex_domain_id(nid) == domain_id)then

          n1 = graph%vertex_id(i)
          call monolis_bsearch_I(ids, 1, n_vertex, n1, idx)
          if(idx == -1) stop "gedatsu_graph_get_edge_in_internal_region 1"
          e1 = perm(idx)

          n2 = graph%vertex_id(nid)
          call monolis_bsearch_I(ids, 1, n_vertex, n2, idx)
          if(idx == -1) stop "gedatsu_graph_get_edge_in_internal_region 2"
          e2 = perm(idx)

          n_edge = n_edge + 1
          edge(1,n_edge) = e1
          edge(2,n_edge) = e2
        endif
      enddo
    enddo
  end subroutine gedatsu_graph_get_edge_in_internal_region

  !> *** コネクティビティグラフ結合時はこの関数を使う ***
  !> *** 理由：nodal_graph%vertex_id は計算点番号、conn_graph%vertex_id は要素番号が記述されているから。 ***
  !> @ingroup graph_basic
  !> 領域番号 domain_id に属するグラフのエッジを取得
  !> @details エッジの組はローカル節点番号で表現される
  subroutine gedatsu_graph_get_edge_in_internal_region_conn_graph(nodal_graph, conn_graph, domain_id, edge)
    implicit none
    !> [in] graph 構造体（計算点グラフ）
    type(gedatsu_graph), intent(in) :: nodal_graph
    !> [in] graph 構造体（コネクティビティグラフ）
    type(gedatsu_graph), intent(in) :: conn_graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフのエッジ配列
    integer(kint), intent(out) :: edge(:,:)

    integer(kint) :: i, nid, idx, j, jS, jE
    integer(kint) :: n_nodal_vertex, n_conn_vertex, n_edge, e1, e2, n1, n2
    integer(kint), allocatable :: nodal_ids(:), conn_ids(:)
    integer(kint), allocatable :: nodal_perm(:), conn_perm(:)

    call gedatsu_graph_get_n_vertex_in_internal_region(nodal_graph, domain_id, n_nodal_vertex)
    call gedatsu_graph_get_n_vertex_in_internal_region(conn_graph, domain_id, n_conn_vertex)

    call monolis_alloc_I_1d(nodal_ids, n_nodal_vertex)
    call monolis_alloc_I_1d(conn_ids, n_conn_vertex)

    call gedatsu_graph_get_vertex_id_in_internal_region(nodal_graph, domain_id, nodal_ids)
    call gedatsu_graph_get_vertex_id_in_internal_region(conn_graph, domain_id, conn_ids)

    call monolis_alloc_I_1d(nodal_perm, n_nodal_vertex)
    call monolis_alloc_I_1d(conn_perm, n_conn_vertex)

    call monolis_get_sequence_array_I(nodal_perm, n_nodal_vertex, 1, 1)
    call monolis_get_sequence_array_I(conn_perm, n_conn_vertex, 1, 1)

    call monolis_qsort_I_2d(nodal_ids, nodal_perm, 1, n_nodal_vertex)
    call monolis_qsort_I_2d(conn_ids, conn_perm, 1, n_conn_vertex)

    n_edge = 0
    do i = 1, conn_graph%n_vertex
      if(conn_graph%vertex_domain_id(i) /= domain_id) cycle
      jS = conn_graph%index(i) + 1
      jE = conn_graph%index(i + 1)
      do j = jS, jE
        nid = conn_graph%item(j)
        if(nodal_graph%vertex_domain_id(nid) == domain_id)then

          n1 = conn_graph%vertex_id(i)
          call monolis_bsearch_I(conn_ids, 1, n_conn_vertex, n1, idx)
          if(idx == -1) stop "gedatsu_graph_get_edge_in_internal_region 1"
          e1 = conn_perm(idx)

          n2 = nodal_graph%vertex_id(nid)
          call monolis_bsearch_I(nodal_ids, 1, n_nodal_vertex, n2, idx)
          if(idx == -1) stop "gedatsu_graph_get_edge_in_internal_region 2"
          e2 = nodal_perm(idx)

          n_edge = n_edge + 1
          edge(1,n_edge) = e1
          edge(2,n_edge) = e2
        endif
      enddo
    enddo
  end subroutine gedatsu_graph_get_edge_in_internal_region_conn_graph

  !> @ingroup graph_basic
  !> 領域番号 domain_id のオーバーラッピング領域に属するエッジを取得
  !> @details エッジの組はローカル節点番号で表現される
  subroutine gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [out] グラフエッジ
    integer(kint), intent(out) :: edge(:,:)
    integer(kint) :: i, j, jS, jE, gid, idx1, idx2, lid1, lid2
    integer(kint) :: n_edge, n1, n2, n_vertex
    integer(kint), allocatable :: ids(:)
    integer(kint), allocatable :: perm(:)
    logical, allocatable :: is_in(:)
    logical, allocatable :: is_border(:)

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n1)

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n2)

    n_vertex = n1 + n2

    call monolis_alloc_I_1d(ids, n_vertex)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids(1:n1))

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids(n1+1:n_vertex))

    call monolis_alloc_I_1d(perm, n_vertex)

    call monolis_get_sequence_array_I(perm, n_vertex, 1, 1)

    call monolis_qsort_I_2d(ids, perm, 1, n_vertex)

    call monolis_alloc_L_1d(is_in, graph%n_vertex)
    call monolis_alloc_L_1d(is_border, graph%n_vertex)

    call gedatsu_graph_get_flag_in_subdomain(graph, domain_id, is_in, is_border)

    n_edge = 0
    do i = 1, graph%n_vertex
      if(.not. is_border(i)) cycle

      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        gid = graph%item(j)
        if(is_in(gid))then
          n1 = graph%vertex_id(i)
          n2 = graph%vertex_id(gid)

          call monolis_bsearch_I(ids, 1, n_vertex, n1, idx1)
          call monolis_bsearch_I(ids, 1, n_vertex, n2, idx2)

          if(idx1 == -1) stop "gedatsu_graph_get_edge_in_overlap_region 1"
          if(idx2 == -1) stop "gedatsu_graph_get_edge_in_overlap_region 2"

          lid1 = perm(idx1)
          lid2 = perm(idx2)

          n_edge = n_edge + 1
          edge(1,n_edge) = lid1
          edge(2,n_edge) = lid2
        endif
      enddo
    enddo

    do i = 1, graph%n_vertex
      if(.not. is_in(i)) cycle

      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        gid = graph%item(j)
        if(is_in(gid) .or. is_border(gid))then
          n1 = graph%vertex_id(i)
          n2 = graph%vertex_id(gid)

          call monolis_bsearch_I(ids, 1, n_vertex, n1, idx1)
          call monolis_bsearch_I(ids, 1, n_vertex, n2, idx2)

          if(idx1 == -1) stop "gedatsu_graph_get_edge_in_overlap_region 1"
          if(idx2 == -1) stop "gedatsu_graph_get_edge_in_overlap_region 2"

          lid1 = perm(idx1)
          lid2 = perm(idx2)

          n_edge = n_edge + 1
          edge(1,n_edge) = lid1
          edge(2,n_edge) = lid2
        endif
      enddo
    enddo
  end subroutine gedatsu_graph_get_edge_in_overlap_region

  !> @ingroup graph_basic
  !> グラフのエッジを設定
  !> @details 既に定義されているエッジ情報は削除される。エッジの重複判定はなされない。ノード数は変化しない。
  subroutine gedatsu_graph_set_edge(graph, n_edge, edge, is_sort)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] グラフのエッジ数
    integer(kint), intent(in) :: n_edge
    !> [in] グラフエッジ
    integer(kint), intent(in) :: edge(:,:)
    !> [in] グラフの隣接ノード情報を昇順ソートするフラグ
    logical :: is_sort
    integer(kint) :: i, e1, e2, jS, jE, in
    integer(kint), allocatable :: temp(:,:)

    if(n_edge < 1)then
      call monolis_std_error_string("gedatsu_graph_set_edge")
      call monolis_std_error_string("n_edge is less than 1")
      call monolis_std_error_stop()
    endif

    call monolis_alloc_I_2d(temp, 2, n_edge)

    temp = edge

    call monolis_qsort_I_2d(temp(1,:), temp(2,:), 1, n_edge)

    graph%index = 0
    do i = 1, n_edge
      e1 = temp(1,i)
      e2 = temp(2,i)
      graph%index(e1 + 1) = graph%index(e1 + 1) + 1
    enddo

    do i = 1, graph%n_vertex
      graph%index(i + 1) = graph%index(i + 1) + graph%index(i)
    enddo

    in = graph%index(graph%n_vertex + 1)

    if(allocated(graph%item)) call monolis_dealloc_I_1d(graph%item)

    call monolis_alloc_I_1d(graph%item, in)

    do i = 1, n_edge
      e2 = temp(2,i)
      graph%item(i) = e2
    enddo

    if(is_sort)then
      do i = 1, graph%n_vertex
        jS = graph%index(i) + 1
        jE = graph%index(i + 1)
        call monolis_qsort_I_1d(graph%item, jS, jE)
      enddo
    endif
  end subroutine gedatsu_graph_set_edge

  !> @ingroup graph_basic
  !> グラフのエッジを追加
  !> @details 既に定義されているエッジ情報は維持する。エッジの重複判定はなされない。
  subroutine gedatsu_graph_add_edge(graph, n_edge, edge, is_sort)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] グラフのエッジ数
    integer(kint), intent(in) :: n_edge
    !> [in] グラフエッジ
    integer(kint), intent(in) :: edge(:,:)
    !> [in] グラフの隣接ノード情報を昇順ソートするフラグ
    logical :: is_sort
    integer(kint) :: n_edge_all, n_edge_cur, i, j, jS, jE
    integer(kint), allocatable :: edge_all(:,:)

    if(n_edge < 1)then
      call monolis_std_error_string("gedatsu_graph_add_edge")
      call monolis_std_error_string("n_edge is less than 1")
      call monolis_std_error_stop()
    endif

    n_edge_cur = graph%index(graph%n_vertex + 1)

    n_edge_all = n_edge_cur + n_edge

    call monolis_alloc_I_2d(edge_all, 2, n_edge_all)

    do i = 1, graph%n_vertex
      jS = graph%index(i) + 1
      jE = graph%index(i + 1)
      do j = jS, jE
        edge_all(1,j) = i
        edge_all(2,j) = graph%item(j)
      enddo
    enddo

    do i = 1, n_edge
      edge_all(1,n_edge_cur + i) = edge(1,i)
      edge_all(2,n_edge_cur + i) = edge(2,i)
    enddo

    call gedatsu_graph_set_edge(graph, n_edge_all, edge_all, is_sort)
  end subroutine gedatsu_graph_add_edge

  !> @ingroup graph_basic
  !> グラフのエッジを追加
  !> @details 既に定義されているエッジ情報は維持する。エッジの重複判定はなされない。
  subroutine gedatsu_graph_check_edge(graph, edge, is_exist)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] グラフエッジ
    integer(kint), intent(in) :: edge(:)
    !> [in] グラフエッジ
    logical, intent(out) :: is_exist
    integer(kint) :: i, j, jS, jE, e1, e2

    is_exist = .false.

    e1 = edge(1)
    e2 = edge(2)

    jS = graph%index(e1) + 1
    jE = graph%index(e1 + 1)
    do j = jS, jE
      i = graph%item(j)
      if(i == e2)then
        is_exist = .true.
        return
      endif
    enddo
  end subroutine gedatsu_graph_check_edge

  !> @ingroup graph_basic
  !> グラフの重複したエッジを削除
  subroutine gedatsu_graph_delete_dupulicate_edge(graph)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    integer(kint) :: i, iS, iE, len, len_uniq
    integer(kint), allocatable :: index_(:), item(:), item_tmp(:)

    call monolis_alloc_I_1d(index_, graph%n_vertex+1)
    call monolis_alloc_I_1d(item, graph%index(graph%n_vertex+1))

    index_(:) = graph%index(:)
    item(:) = graph%item(:)

    call monolis_dealloc_I_1d(graph%item)

    do i = 1, graph%n_vertex
      iS = index_(i)+1
      iE = index_(i+1)
      len = iE - iS + 1

      call monolis_dealloc_I_1d(item_tmp)
      call monolis_alloc_I_1d(item_tmp, len)
      item_tmp(1:len) = item(iS:iE)

      call monolis_qsort_I_1d(item_tmp, 1, len)
      call monolis_get_uniq_array_I(item_tmp, len, len_uniq)

      call monolis_append_I_1d(graph%item, len_uniq, item_tmp)
      graph%index(i+1) = graph%index(i) + len_uniq
    enddo

  end subroutine gedatsu_graph_delete_dupulicate_edge

end module mod_gedatsu_graph_handler
