!> グラフ結合モジュール
!# gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)
!# gedatsu_merge_connectivity_subgraphs(merged_nodal_graph, merged_nodal_monoCOM, n_conn_graphs, conn_graphs, merged_conn_graph)
!# gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, list_struct_R, merged_array_R)
!# gedatsu_merge_distval_I(n_graphs, graphs, merged_graph, list_struct_I, merged_array_I)
!# gedatsu_merge_distval_C(n_graphs, graphs, merged_graph, list_struct_C, merged_array_C)
!# gedatsu_list_initialize_R(list_struct_R, n)
!# gedatsu_list_initialize_I(list_struct_R, n)
!# gedatsu_list_initialize_C(list_struct_R, n)
!# gedatsu_list_finalize_R(list_struct_R)
!# gedatsu_list_finalize_I(list_struct_I)
!# gedatsu_list_finalize_C(list_struct_C)
!# gedstsu_list_set_R(list_struct_R, id, n, array)
!# gedstsu_list_set_I(list_struct_I, id, n, array)
!# gedstsu_list_set_C(list_struct_C, id, n, array)
!# gedatsu_list_get_R(list_struct_R, id, n, array)
!# gedatsu_list_get_I(list_struct_I, id, n, array)
!# gedatsu_list_get_C(list_struct_C, id, n, array)

module mod_gedatsu_graph_merge
  use mod_monolis_utils
  use mod_gedatsu_graph
  implicit none

  !> merge_nodal_subgraphsフラグ（部分領域毎に並べる）
  integer(kint), parameter :: ORDER_DOMAIN_ID = 1
  !> merge_nodal_subgraphsフラグ（グローバル計算点順に並べる）
  integer(kint), parameter :: ORDER_NODAL_ID = 2

  !> リスト構造体配列（実数型）
  type monolis_list_R
    integer(kint) :: n
    real(kdouble), allocatable :: array(:)
  end type monolis_list_R

  !> リスト構造体配列（実数型）
  type monolis_list_I
  integer(kint) :: n
  integer(kint), allocatable :: array(:)
  end type monolis_list_I

  !> リスト構造体配列（複素数型）
  type monolis_list_C
  integer(kint) :: n
  complex(kdouble), allocatable :: array(:)
  end type monolis_list_C

contains

  subroutine gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 通信テーブルの配列（配列長 n_graphs）
    type(monolis_COM), intent(in) :: monoCOMs
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(inout) :: merged_graph
    !> 統合された通信テーブル
    type(monolis_COM), intent(inout) :: merged_monoCOM
    !> 部分領域ごとに並べるか、グローバル計算点番号順に並べるかを決めるフラグ [ORDER_DOMAIN_ID, ORDER_NODAL_ID]
    integer(kint), intent(in) :: order_type

    integer(kint) :: i, j, k, iS, iE, val, idx
    integer(kint) :: n_vertex, n_internal_vertex, n_overlap_vertex, n_vertex_uniq
    integer(kint), allocatable :: vertex_id(:), internal_vertex_id(:), overlap_vertex_id(:), iS_vertex_id(:)
    logical, allocatable :: is_internal(:), is_already_count_overlap(:)
    type(monolis_list_I), allocatable :: adj_list(:)  !> list of lists 計算点同士の隣接関係

    !> 全ての graphs の vertex_id をつなげる
    n_vertex = 0
    n_internal_vertex = 0
    do i = 1, n_graphs
      n_vertex = n_vertex + graphs(i)%n_vertex
      n_internal_vertex = n_internal_vertex + graphs(i)%n_internal_vertex
    enddo
    call monolis_alloc_I_1d(vertex_id, n_vertex)

    iS = 1
    do i = 1, n_graphs
      iE = iS + graphs(i)%n_vertex
      vertex_id(iS:iE) = graphs(i)%vertex_id(1:iE)
      iS = iS + iE
    enddo

    !> つなげた vertex_id を昇順ソート＋重複削除
    call monolis_qsort_I_1d(vertex_id, 1, n_vertex)
    call monolis_get_uniq_array_I(vertex_id, n_vertex, n_vertex_uniq)

    !> 内部領域か袖領域かの判定
    call monolis_alloc_L_1d(is_internal, n_vertex_uniq) ! 内部領域なら.true.
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_internal_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_vertex_uniq, val, idx)
        is_internal(idx) = .true.
      enddo
    enddo
    !> 内部領域と袖領域に分割
    n_overlap_vertex = n_vertex_uniq - n_internal_vertex
    call monolis_alloc_I_1d(internal_vertex_id, n_internal_vertex)
    call monolis_alloc_I_1d(overlap_vertex_id, n_overlap_vertex)
    j = 1
    k = 1
    do i = 1, n_vertex_uniq
      if(is_internal(i))then
        internal_vertex_id(j) = vertex_id(i)
        j = j + 1
      else
        overlap_vertex_id(k) = vertex_id(i)
        k = k + 1
      endif
    enddo

    !> ここから結合後のグラフ merged_graph の作成
    call monolis_graph_initialize(merged_graph)

    merged_graph%n_vertex = n_vertex_uniq
    merged_graph%n_internal_vertex = n_internal_vertex

    call monolis_alloc_I_1d(merged_graph%vertex_id, n_vertex_uniq)
    if(order_type == ORDER_DOMAIN_ID)then
      !> 内部領域
      iS = 1
      do i = 1, n_graphs
        iE = graphs(i)%n_internal_vertex
        merged_graph%vertex_id(iS:iS+iE) = graphs(i)%vertex_id(1:iE)
        iS = iS + iE
      enddo
      !> 袖領域
      call monolis_alloc_L_1d(is_already_count_overlap, n_overlap_vertex) ! 袖領域に含まれるとカウントしたら.true.にする
      k = 1
      do i = 1, n_graphs
        do j = graphs(i)%n_internal_vertex+1, graphs(i)%n_vertex
          val = graphs(i)%vertex_id(j)
          call monolis_bsearch_I(overlap_vertex_id, 1, n_overlap_vertex, val, idx)
          if(idx /= -1 .and. .not.is_already_count_overlap(idx))then
            merged_graph%vertex_id(n_internal_vertex+k) = val
            k = k + 1
            is_already_count_overlap(idx) = .true.
          endif
        enddo
      enddo
    elseif(order_type == ORDER_NODAL_ID)then
      merged_graph%vertex_id(1:n_internal_vertex) = internal_vertex_id(1:n_internal_vertex)
      merged_graph%vertex_id(n_internal_vertex+1:n_vertex) = overlap_vertex_id(1:n_overlap_vertex)
    else
      stop "*** error *** Invalid order_type. order_type must be ORDER_DOMAIN_ID or ORDER_NODAL_ID."
    endif

    !> merged_graph%vertex_id の並び方が order_type フラグによって変わるため、これを vertex_id にコピーして使う
    call monolis_dealloc_I_1d(vertex_id)
    call monolis_alloc_I_1d(vertex_id, n_vertex_uniq)
    vertex_id(:) = merged_graph%vertex_id(:)

    call monolis_alloc_I_1d(merged_graph%vertex_domain_id, n_vertex_uniq)
    merged_graph%vertex_domain_id(:) = monolis_mpi_get_global_my_rank()

    !> 隣接計算点数のカウント
    allocate(adj_list(n_vertex_uniq))
    call gedatsu_list_initialize_I(adj_list, n_vertex_uniq)
    do i = 1, n_graphs
      !> 内部領域
      do j = 1, graphs(i)%n_internal_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_internal_vertex, val, idx)
        if(idx == -1 )stop "*** can't find val in vertex_id."
        adj_list(idx)%n = graphs(i)%index(j+1) - graphs(i)%index(j) - 1  !> 隣接計算点数
      enddo
      !> 袖領域
      do j = graphs(i)%n_internal_vertex+1, graphs(i)%n_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_internal_vertex, val, idx)
        !> 結合後グラフでは内部領域だったらすでにカウントされているのでスキップ
        !> 結合後グラフでも袖領域だったら隣接計算点数は各領域における隣接計算点数の和になる
        if(idx == -1)then
          call monolis_bsearch_I(vertex_id, n_internal_vertex+1, n_vertex_uniq, val, idx)
          adj_list(idx)%n = adj_list(idx)%n + graphs(i)%index(j+1) - graphs(i)%index(j) - 1  !> 隣接計算点数
        endif
      enddo
    enddo

    !> 隣接計算点番号リストを作成
    do i = 1, n_vertex_uniq
      call monolis_alloc_I_1d(adj_list(i)%array, adj_list(i)%n)
    enddo
    call monolis_alloc_I_1d(iS_vertex_id, n_vertex_uniq)
    iS_vertex_id(:) = 1
    do i = 1, n_graphs
      !> 内部領域
      do j = 1, graphs(i)%n_internal_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_vertex_uniq, val, idx)
        adj_list(idx)%array(1:adj_list(idx)%n) = graphs(i)%item(graphs(i)%index(j)+1:graphs(i)%index(j+1))  !> 隣接計算点番号
      enddo
      !> 袖領域
      do j = graphs(i)%n_internal_vertex+1, graphs(i)%n_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_internal_vertex, val, idx)
        !> 結合後グラフでは内部領域だったらすでにカウントされているのでスキップ
        !> 結合後グラフでも袖領域だったら隣接計算点数は各領域における隣接計算点数の和になる
        if(idx == -1)then
          call monolis_bsearch_I(vertex_id, n_internal_vertex+1, n_vertex_uniq, val, idx)
          iE = iS_vertex_id(idx) + graphs(i)%index(j+1) - graphs(i)%index(j) - 1
          adj_list(idx)%array(iS_vertex_id(idx):iE) = graphs(i)%item(graphs(i)%index(j)+1:graphs(i)%index(j+1))  !> 隣接計算点番号
          iS_vertex_id(idx) = iS_vertex_id(idx) + iE
        endif
      enddo
    enddo

    !> CSR 形式グラフ構造体の作成
    j = 0
    do i = 1, n_vertex_uniq
      j = j + adj_list(i)%n
    enddo
    call monolis_alloc_I_1d(merged_graph%index, n_vertex_uniq+1)
    call monolis_alloc_I_1d(merged_graph%item, j)
    merged_graph%index(1) = 0
    iS = 1
    do i = 1, n_vertex_uniq
      merged_graph%index(i+1) = merged_graph%index(i) + 1 + adj_list(i)%n
      iE = iS + adj_list(i)%n
      merged_graph%item(iS:iE) = adj_list(i)%array(1:adj_list(i)%n)
      iS = iS + iE
    enddo

    !> 通信テーブルの結合
    call monolis_com_get_comm_table_parallel(merged_graph%n_internal_vertex, merged_graph%n_vertex, merged_graph%vertex_id, &
    & merged_monoCOM)
  end subroutine gedatsu_merge_nodal_subgraphs

  subroutine gedatsu_merge_connectivity_subgraphs(merged_nodal_graph, merged_nodal_monoCOM, &
    & n_conn_graphs, conn_graphs, merged_conn_graph)
    implicit none
    !> 統合された計算点グラフ構造
    type(gedatsu_graph), intent(in) :: merged_nodal_graph
    !> 統合された計算点グラフ構造の通信テーブル
    type(monolis_COM), intent(in) ::  merged_nodal_monoCOM
    !> 統合したいコネクティビティグラフ構造の個数
    integer(kint), intent(in) :: n_conn_graphs
    !> コネクティビティグラフ構造の配列（配列長 n_conn_graphs）
    type(gedatsu_graph), intent(in) :: conn_graphs(:)
    !> 統合されたコネクティビティグラフ構造
    type(gedatsu_graph), intent(inout) :: merged_conn_graph

    integer(kint) :: i, j, k, iS, iE, val, idx
    integer(kint) :: n_nodal_vertex, n_conn_vertex, n_conn_internal_vertex, n_conn_overlap_vertex, n_conn_vertex_uniq
    integer(kint), allocatable :: nodal_vertex_id(:), conn_vertex_id(:), conn_internal_vertex_id(:), conn_overlap_vertex_id(:)
    integer(kint), allocatable :: nodal_vertex_local_id(:)  !> ソート前のローカル計算点番号
    logical, allocatable :: is_conn_internal(:)
    type(monolis_list_I), allocatable :: adj_list(:)  !> list of lists 計算点同士の隣接関係

    !> #####
    !> 要素のグローバル番号はわかるが、要素を構成する節点のグローバル番号はわからないから翻訳が必要
    !> グローバル番号の検索は nodal_vertex で行うが、結合後のローカル番号の検索は mergedmerged_nodal_graph%n_vertex で行う必要がある
    !> merged_nodal_graph%n_vertex の検索は二分探索が使えない
    !> → ソート前のローカルIDーグローバルID対応 → ソート後のローカルIDーグローバルID対応への変換が必要（余分な配列が必要）
    !> 【これは計算点グラフに関する話。コネクティビティグラフは単純に結合できる気がする】
    !> #####

    !> グローバル計算点番号を探索するのに使う配列
    allocate(nodal_vertex_id, source = merged_nodal_graph%vertex_id)
    n_nodal_vertex = merged_nodal_graph%n_vertex
    call monolis_qsort_I_1d(nodal_vertex_id, 1, n_nodal_vertex)
    !> ソート後のローカル計算点番号を求める
    call monolis_alloc_I_1d(nodal_vertex_local_id, n_nodal_vertex)
    do i = 1, n_nodal_vertex
      val = merged_nodal_graph%vertex_id(i)
      call monolis_bsearch_I(nodal_vertex_id, 1, n_nodal_vertex, val, idx)
      nodal_vertex_local_id(i) = idx
    enddo

    !> 全ての conn_graphs の vertex_id をつなげる
    n_conn_vertex = 0
    n_conn_internal_vertex = 0
    do i = 1, n_conn_graphs
      n_conn_vertex = n_conn_vertex + conn_graphs(i)%n_vertex
      n_conn_internal_vertex = n_conn_internal_vertex + conn_graphs(i)%n_internal_vertex
    enddo
    call monolis_alloc_I_1d(conn_vertex_id, n_conn_vertex)

    iS = 1
    do i = 1, n_conn_graphs
      iE = iS + conn_graphs(i)%n_vertex
      conn_vertex_id(iS:iE) = conn_graphs(i)%vertex_id(1:iE)
      iS = iS + iE
    enddo

    !> つなげた conn_vertex_id を昇順ソート＋重複削除
    call monolis_qsort_I_1d(conn_vertex_id, 1, n_conn_vertex)
    call monolis_get_uniq_array_I(conn_vertex_id, n_conn_vertex, n_conn_vertex_uniq)

    !> 内部領域か袖領域かの判定
    call monolis_alloc_L_1d(is_conn_internal, n_conn_vertex_uniq) ! 内部領域なら.true.
    do i = 1, n_conn_graphs
      do j = 1, conn_graphs(i)%n_internal_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_vertex_uniq, val, idx)
        is_conn_internal(idx) = .true.
      enddo
    enddo
    !> 内部領域と袖領域に分割
    n_conn_overlap_vertex = n_conn_vertex_uniq - n_conn_internal_vertex
    call monolis_alloc_I_1d(conn_internal_vertex_id, n_conn_internal_vertex)
    call monolis_alloc_I_1d(conn_overlap_vertex_id, n_conn_overlap_vertex)
    j = 1
    k = 1
    do i = 1, n_conn_vertex_uniq
      if(is_conn_internal(i))then
        conn_internal_vertex_id(j) = conn_vertex_id(i)
        j = j + 1
      else
        conn_overlap_vertex_id(k) = conn_vertex_id(i)
        k = k + 1
      endif
    enddo

    !> ここから結合後のグラフ merged_conn_graph の作成
    call gedatsu_graph_initialize(merged_conn_graph)

    merged_conn_graph%n_vertex = n_conn_vertex_uniq
    merged_conn_graph%n_internal_vertex = n_conn_internal_vertex

    call monolis_alloc_I_1d(merged_conn_graph%vertex_id, n_conn_vertex_uniq)
    merged_conn_graph%vertex_id(1:n_conn_internal_vertex) = conn_internal_vertex_id(1:n_conn_internal_vertex)
    merged_conn_graph%vertex_id(n_conn_internal_vertex+1:n_conn_vertex) = conn_overlap_vertex_id(1:n_conn_overlap_vertex)

    call monolis_alloc_I_1d(merged_conn_graph%vertex_domain_id, merged_conn_graph%n_vertex)
    merged_conn_graph%vertex_domain_id(:) = monolis_mpi_get_global_my_rank()

    !> conn_vertex_id の並び方を merged_conn_graph%vertex_id にそろえる
    call monolis_dealloc_I_1d(conn_vertex_id)
    call monolis_alloc_I_1d(conn_vertex_id, n_conn_vertex_uniq)
    conn_vertex_id(:) = merged_conn_graph%vertex_id(:)

    call monolis_alloc_I_1d(merged_conn_graph%vertex_domain_id, n_conn_vertex_uniq)
    merged_conn_graph%vertex_domain_id(:) = monolis_mpi_get_global_my_rank()

    !> 隣接計算点数のカウント
    allocate(adj_list(n_conn_vertex_uniq))
    call gedatsu_list_initialize_I(adj_list, n_conn_vertex_uniq)
    do i = 1, n_conn_graphs
      !> 内部領域
      do j = 1, conn_graphs(i)%n_internal_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_internal_vertex, val, idx)
        if(idx == -1 )stop "*** can't find val in vertex_id."
        adj_list(idx)%n = conn_graphs(i)%index(j+1) - conn_graphs(i)%index(j) - 1  !> 隣接計算点数
      enddo
      !> 袖領域
      do j = conn_graphs(i)%n_internal_vertex+1, conn_graphs(i)%n_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_internal_vertex, val, idx)
        !> 結合後グラフでは内部領域だったらすでにカウントされているのでスキップ
        if(idx == -1)then
          call monolis_bsearch_I(conn_vertex_id, n_conn_internal_vertex+1, n_conn_vertex_uniq, val, idx)
          adj_list(idx)%n = conn_graphs(i)%index(j+1) - conn_graphs(i)%index(j) - 1  !> 隣接計算点数
        endif
      enddo
    enddo

    !> 隣接計算点番号リストを作成
    do i = 1, n_conn_vertex_uniq
      call monolis_alloc_I_1d(adj_list(i)%array, adj_list(i)%n)
    enddo
    do i = 1, n_conn_graphs
      !> 内部領域
      do j = 1, conn_graphs(i)%n_internal_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_vertex_uniq, val, idx)
        adj_list(idx)%array(1:adj_list(idx)%n) = conn_graphs(i)%item(conn_graphs(i)%index(j)+1:conn_graphs(i)%index(j+1))  !> 隣接計算点番号
      enddo
      !> 袖領域
      do j = conn_graphs(i)%n_internal_vertex+1, conn_graphs(i)%n_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_internal_vertex, val, idx)
        !> 結合後グラフでは内部領域だったらすでにカウントされているのでスキップ
        if(idx == -1)then
          call monolis_bsearch_I(conn_vertex_id, n_conn_internal_vertex+1, n_conn_vertex_uniq, val, idx)
          adj_list(idx)%array(1:adj_list(idx)%n) = conn_graphs(i)%item(conn_graphs(i)%index(j)+1:conn_graphs(i)%index(j+1))  !> 隣接計算点番号
        endif
      enddo
    enddo

    !> CSR 形式グラフ構造体の作成
    j = 0
    do i = 1, n_conn_vertex_uniq
      j = j + adj_list(i)%n
    enddo
    call monolis_alloc_I_1d(merged_conn_graph%index, n_conn_vertex_uniq+1)
    call monolis_alloc_I_1d(merged_conn_graph%item, j)
    merged_conn_graph%index(1) = 0
    iS = 1
    do i = 1, n_conn_vertex_uniq
      merged_conn_graph%index(i+1) = merged_conn_graph%index(i) + 1 + adj_list(i)%n
      iE = iS + adj_list(i)%n
      merged_conn_graph%item(iS:iE) = adj_list(i)%array(1:adj_list(i)%n)
      iS = iS + iE
    enddo
  end subroutine gedatsu_merge_connectivity_subgraphs

  subroutine gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, list_struct_R, merged_array_R)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !>  リスト構造体
    type(monolis_list_R), intent(in) :: list_struct_R(:)
    !> 統合された実数配列
    real(kdouble), allocatable, intent(inout) :: merged_array_R(:) !> 結合後グラフのローカル計算点番号で記述

    integer(kint) :: i, j, k, val, idx, local_idx, n_vertex !> local_idx：ソート前のローカル計算点番号を代入するのに使う
    integer(kint), allocatable :: vertex_id(:), vertex_local_id(:)  !> vertex_local_id：ソート前のローカル計算点番号

    if(n_graphs /= size(list_struct_R)) stop "*** n_graphs and size(list_struct_R) don't match."

    !> グローバル計算点番号を探索するのに使う配列
    allocate(vertex_id, source = merged_graph%vertex_id)
    n_vertex = merged_graph%n_vertex
    call monolis_qsort_I_1d(vertex_id, 1, n_vertex)
    !> ソート後のローカル計算点番号を求める
    call monolis_alloc_I_1d(vertex_local_id, n_vertex)
    do i = 1, n_vertex
      val = merged_graph%vertex_id(i)
      call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)
      vertex_local_id(i) = idx
    enddo

    !> 結合後グラフのローカル計算点番号に変換
    n_vertex = merged_graph%n_vertex
    call monolis_alloc_R_1d(merged_array_R, n_vertex)
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_vertex  !> j : 結合前グラフにおけるローカル計算点番号
        val = graphs(i)%vertex_id(j)  !> val：グローバル計算点番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx) !> idx : "ソートした" 結合後グラフにおけるローカル計算点番号
        local_idx = vertex_local_id(idx)  !> local_id : "ソート前の" 結合後グラフにおけるローカル計算点番号
        merged_array_R(local_idx) = list_struct_R(i)%array(j)
      enddo
    enddo
  end subroutine gedatsu_merge_distval_R

  subroutine gedatsu_merge_distval_I(n_graphs, graphs, merged_graph, list_struct_I, merged_array_I)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !> リスト構造体
    type(monolis_list_I), intent(in) :: list_struct_I(:)
    !>  統合された整数配列
    integer(kint), allocatable, intent(inout) :: merged_array_I(:)
  end subroutine gedatsu_merge_distval_I

  subroutine gedatsu_merge_distval_C(n_graphs, graphs, merged_graph, list_struct_C, merged_array_C)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !> リスト構造体
    type(monolis_list_C), intent(in) :: list_struct_C(:)
    !> 統合された複素数配列
    real(kdouble), allocatable, intent(inout) :: merged_array_C(:)
  end subroutine gedatsu_merge_distval_C

  subroutine gedatsu_list_initialize_R(list_struct_R, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_R(i)%n = 0
      call monolis_dealloc_R_1d(list_struct_R(i)%array)
    enddo
  end subroutine gedatsu_list_initialize_R

  subroutine gedatsu_list_initialize_I(list_struct_I, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_I(i)%n = 0
      call monolis_dealloc_I_1d(list_struct_I(i)%array)
    enddo
  end subroutine gedatsu_list_initialize_I

  subroutine gedatsu_list_initialize_C(list_struct_C, n)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    !> リスト構造体配列の長さ
    integer(kint), intent(in) :: n
    integer(kint) :: i

    do i = 1, n
      list_struct_C(i)%n = 0
      call monolis_dealloc_C_1d(list_struct_C(i)%array)
    enddo
  end subroutine gedatsu_list_initialize_C

  subroutine gedatsu_list_finalize_R(list_struct_R)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    integer(kint) :: i

    do i = 1, size(list_struct_R)
      list_struct_R(i)%n = 0
      call monolis_dealloc_R_1d(list_struct_R(i)%array)
    enddo
  end subroutine gedatsu_list_finalize_R

  subroutine gedatsu_list_finalize_I(list_struct_I)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    integer(kint) :: i

    do i = 1, size(list_struct_I)
      list_struct_I(i)%n = 0
      call monolis_dealloc_I_1d(list_struct_I(i)%array)
    enddo
  end subroutine gedatsu_list_finalize_I

  subroutine gedatsu_list_finalize_C(list_struct_C)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    integer(kint) :: i

    do i = 1, size(list_struct_C)
      list_struct_C(i)%n = 0
      call monolis_dealloc_C_1d(list_struct_C(i)%array)
    enddo
  end subroutine gedatsu_list_finalize_C

  subroutine gedatsu_list_set_R(list_struct_R, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(inout) :: list_struct_R(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    real(kdouble), allocatable, intent(in) :: array(:)

    if(allocated(list_struct_R(id)%array)) call monolis_dealloc_R_1d(list_struct_R(id)%array)
    call monolis_alloc_R_1d(list_struct_R(id)%array, n)
    list_struct_R(id)%array = array(:)
  end subroutine gedatsu_list_set_R

  subroutine gedatsu_list_set_I(list_struct_I, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(inout) :: list_struct_I(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    integer(kint), allocatable, intent(in) :: array(:)

    if(allocated(list_struct_I(id)%array)) call monolis_dealloc_I_1d(list_struct_I(id)%array)
    call monolis_alloc_I_1d(list_struct_I(id)%array, n)
    list_struct_I(id)%array = array(:)
  end subroutine gedatsu_list_set_I

  subroutine gedatsu_list_set_C(list_struct_C, id, n, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(inout) :: list_struct_C(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 配列番号 id に登録する配列サイズ
    integer(kint), intent(in) :: n
    !> 登録する配列
    complex(kdouble), allocatable, intent(in) :: array(:)

    if(allocated(list_struct_C(id)%array)) call monolis_dealloc_C_1d(list_struct_C(id)%array)
    call monolis_alloc_C_1d(list_struct_C(id)%array, n)
    list_struct_C(id)%array = array(:)
  end subroutine gedatsu_list_set_C

  subroutine gedatsu_list_get_R(list_struct_R, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_R), intent(in) :: list_struct_R(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    real(kdouble), allocatable, intent(inout) :: array(:)
    integer(kint) :: n

    if(allocated(array)) call monolis_dealloc_R_1d(array)
    n = size(list_struct_R(id)%array)
    call monolis_alloc_R_1d(array, n)
    array(:) = list_struct_R(id)%array
  end subroutine gedatsu_list_get_R

  subroutine gedatsu_list_get_I(list_struct_I, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_I), intent(in) :: list_struct_I(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    integer(kint), allocatable, intent(inout) :: array(:)
    integer(kint) :: n

    if(allocated(array)) call monolis_dealloc_I_1d(array)
    n = size(list_struct_I(id)%array)
    call monolis_alloc_I_1d(array, n)
    array(:) = list_struct_I(id)%array
  end subroutine gedatsu_list_get_I

  subroutine gedatsu_list_get_C(list_struct_C, id, array)
    implicit none
    !> リスト構造体配列
    type(monolis_list_C), intent(in) :: list_struct_C(:)
    !> 配列番号
    integer(kint), intent(in) :: id
    !> 参照する配列
    complex(kdouble), allocatable, intent(inout) :: array(:)
    integer(kint) :: n

    if(allocated(array)) call monolis_dealloc_C_1d(array)
    n = size(list_struct_C(id)%array)
    call monolis_alloc_C_1d(array, n)
    array(:) = list_struct_C(id)%array
  end subroutine gedatsu_list_get_C

end module mod_gedatsu_graph_merge
