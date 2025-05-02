!> グラフ結合モジュール
!# gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)
!# gedatsu_merge_connectivity_subgraphs(n_nodal_graphs, nodal_graphs, merged_nodal_graph, merged_nodal_monoCOM, n_conn_graphs, conn_graphs, merged_conn_graph)
!# gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, n_dof_list, list_struct_R, merged_n_dof_list, merged_array_R)
!# gedatsu_merge_distval_I(n_graphs, graphs, merged_graph, n_dof_list, list_struct_I, merged_n_dof_list, merged_array_I)
!# gedatsu_merge_distval_C(n_graphs, graphs, merged_graph, n_dof_list, list_struct_C, merged_n_dof_list, merged_array_C)

module mod_gedatsu_graph_merge
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_graph_handler
  implicit none

  !> merge_nodal_subgraphsフラグ（部分領域毎に並べる）
  integer(kint), parameter :: ORDER_DOMAIN_ID = 1
  !> merge_nodal_subgraphsフラグ（グローバル計算点順に並べる）
  integer(kint), parameter :: ORDER_NODAL_ID = 2

contains

  !> @ingroup graph_merge
  !> 計算点グラフを結合する
  subroutine gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)
    implicit none
    !> [in] 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> [in] グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> [in] 通信テーブルの配列（配列長 n_graphs）
    type(monolis_COM), intent(in) :: monoCOMs(:)
    !> [inout] 統合されたグラフ構造
    type(gedatsu_graph), intent(inout) :: merged_graph
    !> [inout] 統合された通信テーブル
    type(monolis_COM), intent(inout) :: merged_monoCOM
    !> [in] 部分領域ごとに並べるか、グローバル計算点番号順に並べるかを決めるフラグ [ORDER_DOMAIN_ID, ORDER_NODAL_ID]
    integer(kint), intent(in) :: order_type

    integer(kint) :: i, j, k, iS, iE, val, idx, tmp
    integer(kint) :: n_vertex, n_internal_vertex, n_overlap_vertex, n_vertex_uniq, n_edge
    integer(kint), allocatable :: vertex_id(:), internal_vertex_id(:), overlap_vertex_id(:), vertex_id_notsorted(:), edge(:,:)
    logical, allocatable :: is_internal(:), is_already_count_overlap(:)

    n_vertex = 0
    n_internal_vertex = 0
    do i = 1, n_graphs
      n_vertex = n_vertex + graphs(i)%n_vertex
      n_internal_vertex = n_internal_vertex + graphs(i)%n_internal_vertex
    enddo

    call monolis_alloc_I_1d(vertex_id, n_vertex)

    !> 全ての graphs の vertex_id をつなげる
    iS = 1
    do i = 1, n_graphs
      iE = iS + graphs(i)%n_vertex - 1
      vertex_id(iS:iE) = graphs(i)%vertex_id(1:graphs(i)%n_vertex)
      iS = iS + graphs(i)%n_vertex
    enddo

    !> つなげた vertex_id を昇順ソート＋重複削除
    call monolis_qsort_I_1d(vertex_id, 1, n_vertex)
    call monolis_get_uniq_array_I(vertex_id, n_vertex, tmp)
    n_vertex = tmp  !> 重複を削除した全計算点数
    call monolis_realloc_I_1d(vertex_id, n_vertex)

    !> 内部領域か袖領域かの判定
    call monolis_alloc_L_1d(is_internal, n_vertex) ! 内部領域なら.true.
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_internal_vertex
        val = graphs(i)%vertex_id(j)
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)
        is_internal(idx) = .true.
      enddo
    enddo

    !> 内部領域と袖領域に分割
    n_overlap_vertex = n_vertex - n_internal_vertex
    call monolis_alloc_I_1d(internal_vertex_id, n_internal_vertex)
    call monolis_alloc_I_1d(overlap_vertex_id, n_overlap_vertex)
    j = 1
    k = 1
    do i = 1, n_vertex
      if(is_internal(i))then
        internal_vertex_id(j) = vertex_id(i)
        j = j + 1
      else
        overlap_vertex_id(k) = vertex_id(i)
        k = k + 1
      endif
    enddo

    !> ここから結合後のグラフ merged_graph の作成
    call gedatsu_graph_initialize(merged_graph)

    !> n_vertex と n_internal_vertex の設定
    call gedatsu_graph_set_n_vertex(merged_graph, n_vertex)
    merged_graph%n_internal_vertex = n_internal_vertex

    !> vertex_id の作成
    if(order_type == ORDER_NODAL_ID)then
      merged_graph%vertex_id(1:n_internal_vertex) = internal_vertex_id(1:n_internal_vertex)
      merged_graph%vertex_id(n_internal_vertex+1:n_vertex) = overlap_vertex_id(1:n_overlap_vertex)
    elseif(order_type == ORDER_DOMAIN_ID)then
      !> 内部領域
      iS = 1
      do i = 1, n_graphs
        iE = iS + graphs(i)%n_internal_vertex - 1
        merged_graph%vertex_id(iS:iE) = graphs(i)%vertex_id(1:graphs(i)%n_internal_vertex)
        iS = iS + graphs(i)%n_internal_vertex
      enddo
      !> 袖領域
      call monolis_alloc_L_1d(is_already_count_overlap, n_overlap_vertex) ! 袖領域に含まれるとカウントしたら.true.にする
      k = 1
      do i = 1, n_graphs
        do j = graphs(i)%n_internal_vertex+1, graphs(i)%n_vertex
          val = graphs(i)%vertex_id(j)
          call monolis_bsearch_I(overlap_vertex_id, 1, n_overlap_vertex, val, idx)
          if(idx == -1) cycle
          if(.not. is_already_count_overlap(idx))then
            merged_graph%vertex_id(n_internal_vertex+k) = val
            k = k + 1
            is_already_count_overlap(idx) = .true.
          endif
        enddo
      enddo
    else
      stop "*** error *** Invalid order_type. order_type must be ORDER_DOMAIN_ID or ORDER_NODAL_ID."
    endif

    !> 「ソート後の結合後ローカル番号」＝vertex_idと「ソートしていない本来の結合後ローカル番号」＝merged_graph%vertex_idの対応関係を保持しておく
    call monolis_alloc_I_1d(vertex_id_notsorted, n_vertex)
    do i = 1, n_vertex  !> i が「ソートしていない本来の結合後ローカル番号」
      val = merged_graph%vertex_id(i)
      call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)
      vertex_id_notsorted(idx) = i
    enddo

    !> CSR 形式グラフの作成
    do i = 1, n_graphs
      call monolis_dealloc_I_2d(edge)
      call gedatsu_graph_get_n_edge(graphs(i), n_edge)
      call monolis_alloc_I_2d(edge, 2, n_edge)
      call gedatsu_graph_get_edge_in_internal_region(graphs(i), monolis_mpi_get_global_my_rank(), edge) !> domain_id はランク番号

      !> edge を「ソートしていない本来の結合後ローカル番号」に変換
      do j = 1, n_edge
        do k = 1, 2
          idx = edge(k,j) !> 結合前グラフにおけるローカル番号
          val = graphs(i)%vertex_id(idx)  !> グローバル番号
          call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx) !> 「ソート後の結合後ローカル番号」
          edge(k,j) = vertex_id_notsorted(idx)  !> 「ソートしていない本来の結合後ローカル番号」
        enddo
      enddo

      !> merged_graph にエッジを追加
      call gedatsu_graph_add_edge(merged_graph, n_edge, edge, .true.)
    enddo

    !> 重複削除
    call gedatsu_graph_delete_dupulicate_edge(merged_graph)

    !> 通信テーブルの結合
    call monolis_com_initialize_by_global_id(merged_monoCOM, monolis_mpi_get_global_comm(), &
    & merged_graph%n_internal_vertex, merged_graph%n_vertex, merged_graph%vertex_id)
  end subroutine gedatsu_merge_nodal_subgraphs

  !> @ingroup graph_merge
  !> コネクティビティグラフを結合する
  subroutine gedatsu_merge_connectivity_subgraphs(n_nodal_graphs, nodal_graphs, merged_nodal_graph, merged_nodal_monoCOM, &
  & n_conn_graphs, conn_graphs, merged_conn_graph)
    implicit none
    !> [in] 統合したい節点グラフ構造の個数
    integer(kint), intent(in) :: n_nodal_graphs
    !> [in] グラフ構造の配列
    type(gedatsu_graph), intent(in) :: nodal_graphs(:)
    !> [in] 統合された計算点グラフ構造
    type(gedatsu_graph), intent(in) :: merged_nodal_graph
    !> [in] 統合された計算点グラフ構造の通信テーブル
    type(monolis_COM), intent(in) ::  merged_nodal_monoCOM
    !> [in] 統合したいコネクティビティグラフ構造の個数
    integer(kint), intent(in) :: n_conn_graphs
    !> [in] コネクティビティグラフ構造の配列（配列長 n_conn_graphs）
    type(gedatsu_graph), intent(in) :: conn_graphs(:)
    !> [inout] 統合されたコネクティビティグラフ構造
    type(gedatsu_graph), intent(inout) :: merged_conn_graph

    integer(kint) :: i, j, k, iS, iE, val, idx, tmp, n_edge
    integer(kint) :: n_conn_vertex, n_conn_internal_vertex, n_conn_overlap_vertex, n_nodal_vertex
    integer(kint), allocatable :: conn_vertex_id(:), conn_internal_vertex_id(:), conn_overlap_vertex_id(:), &
    & conn_vertex_id_notsorted(:)
    integer(kint), allocatable :: nodal_vertex_id(:), nodal_vertex_id_notsorted(:), edge(:,:), &
    & perm(:), global_id_in_merged_graph(:), which_conn_graph(:), local_id_in_conn_graph(:)
    integer(kint), allocatable :: temp1(:), temp2(:)
    logical, allocatable :: is_conn_internal(:)
    type(monolis_list_I), allocatable :: conn_graphs_vertex_id(:), conn_graphs_vertex_id_perm(:)

    if(n_nodal_graphs /= n_conn_graphs) stop "*** n_nodal_graphs /= n_conn_graphs"

    n_conn_vertex = 0
    n_conn_internal_vertex = 0
    do i = 1, n_conn_graphs
      n_conn_vertex = n_conn_vertex + conn_graphs(i)%n_vertex
      n_conn_internal_vertex = n_conn_internal_vertex + conn_graphs(i)%n_internal_vertex
    enddo
    call monolis_alloc_I_1d(conn_vertex_id, n_conn_vertex)

    !> 全ての conn_graphs の vertex_id をつなげる
    iS = 1
    do i = 1, n_conn_graphs
      iE = iS + conn_graphs(i)%n_vertex - 1
      conn_vertex_id(iS:iE) = conn_graphs(i)%vertex_id(1:conn_graphs(i)%n_vertex)
      iS = iS + conn_graphs(i)%n_vertex
    enddo

    !> つなげた conn_vertex_id を昇順ソート＋重複削除
    call monolis_qsort_I_1d(conn_vertex_id, 1, n_conn_vertex)
    call monolis_get_uniq_array_I(conn_vertex_id, n_conn_vertex, tmp)
    n_conn_vertex = tmp
    call monolis_realloc_I_1d(conn_vertex_id, n_conn_vertex)

    !> 内部領域か袖領域かの判定
    call monolis_alloc_L_1d(is_conn_internal, n_conn_vertex) ! 内部領域なら.true.
    do i = 1, n_conn_graphs
      do j = 1, conn_graphs(i)%n_internal_vertex
        val = conn_graphs(i)%vertex_id(j)
        call monolis_bsearch_I(conn_vertex_id, 1, n_conn_vertex, val, idx)
        is_conn_internal(idx) = .true.
      enddo
    enddo
    !> 内部領域と袖領域に分割
    n_conn_overlap_vertex = n_conn_vertex - n_conn_internal_vertex
    call monolis_alloc_I_1d(conn_internal_vertex_id, n_conn_internal_vertex)
    call monolis_alloc_I_1d(conn_overlap_vertex_id, n_conn_overlap_vertex)
    j = 1
    k = 1
    do i = 1, n_conn_vertex
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

    call gedatsu_graph_set_n_vertex(merged_conn_graph, n_conn_vertex)
    merged_conn_graph%n_internal_vertex = n_conn_internal_vertex

    merged_conn_graph%vertex_id(1:n_conn_internal_vertex) = conn_internal_vertex_id(1:n_conn_internal_vertex)
    merged_conn_graph%vertex_id(n_conn_internal_vertex+1:n_conn_vertex) = conn_overlap_vertex_id(1:n_conn_overlap_vertex)

    !> 「ソート後の結合後ローカル番号」と「ソートしていない本来の結合後ローカル番号」の対応関係を保持しておく必要がある
    !> 要素
    call monolis_alloc_I_1d(conn_vertex_id_notsorted, n_conn_vertex)
    do i = 1, n_conn_vertex
      val = merged_conn_graph%vertex_id(i)
      call monolis_bsearch_I(conn_vertex_id, 1, n_conn_vertex, val, idx)
      conn_vertex_id_notsorted(i) = idx
    enddo
    !> 計算点
    call monolis_alloc_I_1d(nodal_vertex_id, merged_nodal_graph%n_vertex)
    nodal_vertex_id(:) = merged_nodal_graph%vertex_id(:)
    call gedatsu_graph_get_n_vertex(merged_nodal_graph, n_nodal_vertex)   !> // TODO 304行目の前に呼ぶべき？
    call monolis_qsort_I_1d(nodal_vertex_id, 1, n_nodal_vertex)
    call monolis_alloc_I_1d(nodal_vertex_id_notsorted, n_nodal_vertex)
    do i = 1, n_nodal_vertex
      val = merged_nodal_graph%vertex_id(i)
      call monolis_bsearch_I(nodal_vertex_id, 1, n_nodal_vertex, val, idx)
      nodal_vertex_id_notsorted(idx) = i
    enddo

    !> 結合前グラフの、ソート前後のグローバル番号を用意
    allocate(conn_graphs_vertex_id(n_conn_graphs))
    allocate(conn_graphs_vertex_id_perm(n_conn_graphs))
    call monolis_list_initialize_I(conn_graphs_vertex_id, n_conn_graphs)
    call monolis_list_initialize_I(conn_graphs_vertex_id_perm, n_conn_graphs)
    do i = 1, n_conn_graphs
      call monolis_list_set_I(conn_graphs_vertex_id, i, conn_graphs(i)%n_vertex, conn_graphs(i)%vertex_id)
      conn_graphs_vertex_id_perm(i)%n = conn_graphs(i)%n_vertex
      call monolis_alloc_I_1d(conn_graphs_vertex_id_perm(i)%array, conn_graphs(i)%n_vertex)
      call monolis_get_sequence_array_I(conn_graphs_vertex_id_perm(i)%array, conn_graphs(i)%n_vertex, 1, 1)
      call monolis_qsort_I_2d(conn_graphs_vertex_id(i)%array, conn_graphs_vertex_id_perm(i)%array, 1, conn_graphs(i)%n_vertex)
    enddo

    call monolis_alloc_I_1d(global_id_in_merged_graph, n_conn_vertex)
    call monolis_alloc_I_1d(perm, n_conn_vertex)
    call monolis_alloc_I_1d(which_conn_graph, n_conn_vertex)
    call monolis_alloc_I_1d(local_id_in_conn_graph, n_conn_vertex)
    call monolis_alloc_I_1d(temp1, n_conn_vertex)
    call monolis_alloc_I_1d(temp2, n_conn_vertex)

    global_id_in_merged_graph(:) = merged_conn_graph%vertex_id(:)
    do i = 1, n_conn_vertex
      val = merged_conn_graph%vertex_id(i)  !> グローバル番号
      do j = 1, n_conn_graphs
        call monolis_bsearch_I(conn_graphs_vertex_id(j)%array, 1, conn_graphs_vertex_id(j)%n, val, idx)  !> ソート後ローカル番号
        if(idx == -1) cycle
        tmp = conn_graphs_vertex_id_perm(j)%array(idx)  !> ソート前ローカル番号
        which_conn_graph(i) = j
        local_id_in_conn_graph(i) = tmp
        if(idx /= -1) exit
      enddo
    enddo

    call monolis_get_sequence_array_I(perm, n_conn_vertex, 1, 1)
    call monolis_qsort_I_2d(global_id_in_merged_graph, perm, 1, n_conn_vertex)
    ! call monolis_qsort_I_2d(perm, which_conn_graph, 1, n_conn_vertex)
    ! call monolis_qsort_I_2d(perm, local_id_in_conn_graph, 1, n_conn_vertex)

    temp1(:) = which_conn_graph(:)
    temp2(:) = local_id_in_conn_graph(:)
    do i = 1, n_conn_vertex
      tmp = perm(i)
      which_conn_graph(i) = temp1(tmp)
      local_id_in_conn_graph(i) = temp2(tmp)
    enddo

    !> CSR 形式グラフの作成
    do i = 1, merged_conn_graph%n_vertex  !>「抽出して足す」を繰り返す
      !グローバル要素番号取得
      val = merged_conn_graph%vertex_id(i)
      ! グローバル要素番号に対して二分探索→iとローカル番号がわかるので、要素を抽出できる
      call monolis_bsearch_I(global_id_in_merged_graph, 1, n_conn_vertex, val, idx) !> // TODO 重複削除していないが、ソートはされているので二分探索使っても問題ない？

      iS = conn_graphs(which_conn_graph(idx))%index(local_id_in_conn_graph(idx)) + 1
      iE = conn_graphs(which_conn_graph(idx))%index(local_id_in_conn_graph(idx) + 1)

      !> edgeに追加
      n_edge = iE - iS + 1
      call monolis_dealloc_I_2d(edge)
      call monolis_alloc_I_2d(edge, 2, n_edge)
      edge(1,:) = i
      edge(2,1:n_edge) = conn_graphs(which_conn_graph(idx))%item(iS:iE)

      !> edge の計算点番号を「ソートしていない本来の結合後ローカル番号」に変換
      do j = 1, n_edge
        tmp = edge(2,j)     !> 結合前ローカル番号
        val = nodal_graphs(which_conn_graph(idx))%vertex_id(tmp)    !> グローバル番号
        call monolis_bsearch_I(nodal_vertex_id, 1, n_nodal_vertex, val, tmp)  !> 結合後ソート後ローカル番号
        edge(2,j) = nodal_vertex_id_notsorted(tmp)  !> 結合後ソート前ローカル番号
      enddo

      call gedatsu_graph_add_edge(merged_conn_graph, n_edge, edge, .false.)
    enddo
  end subroutine gedatsu_merge_connectivity_subgraphs

  !> @ingroup graph_merge
  !> 物理量分布を結合する（実数型）
  subroutine gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, n_dof_list, list_struct_R, merged_n_dof_list, merged_array_R)
    implicit none
    !> [in] 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> [in] グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> [in] 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !> [in] 計算点が持つ物理量の個数
    type(monolis_list_I), intent(in) :: n_dof_list(:)
    !> [in] リスト構造体
    type(monolis_list_R), intent(in) :: list_struct_R(:)
    !> [inout] 結合後の計算点が持つ物理量の個数
    integer(kint), allocatable, intent(inout) :: merged_n_dof_list(:)
    !> [inout] 統合された実数配列
    real(kdouble), allocatable, intent(inout) :: merged_array_R(:)

    integer(kint) :: i, j, k, n_vertex, val, idx, size_merged_array_R, iS, iE, jS, jE
    integer(kint), allocatable :: perm(:), vertex_id(:), index(:)
    type(monolis_list_I), allocatable :: merged_index(:)

    if(n_graphs /= size(n_dof_list)) stop "*** n_graphs and size(n_dof_list) don't match."
    if(n_graphs /= size(list_struct_R)) stop "*** n_graphs and size(list_struct_R) don't match."

    n_vertex = merged_graph%n_vertex
    call monolis_dealloc_I_1d(merged_n_dof_list)
    call monolis_alloc_I_1d(merged_n_dof_list, n_vertex)

    !> ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
    call monolis_alloc_I_1d(vertex_id, n_vertex)
    vertex_id(:) = merged_graph%vertex_id(:)
    call monolis_alloc_I_1d(perm, n_vertex)
    call monolis_get_sequence_array_I(perm, n_vertex, 1, 1)
    call monolis_qsort_I_2d(vertex_id, perm, 1, n_vertex)

    !> 結合後グラフの情報を取得
    do i = 1, n_graphs
      if(graphs(i)%n_vertex /= n_dof_list(i)%n) stop "*** graphs(i)%n_vertex and n_dof_list(i)%n don't match. ***"
      do j = 1, graphs(i)%n_vertex !> 結合前ローカル番号
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        merged_n_dof_list(val) = n_dof_list(i)%array(j)  !> 指定した計算点の物理量の次元を上書き
      enddo
    enddo
    size_merged_array_R = sum(merged_n_dof_list)

    call monolis_dealloc_R_1d(merged_array_R)
    call monolis_alloc_R_1d(merged_array_R, size_merged_array_R)

    !> index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
    !> 結合後グラフ
    val = n_vertex + 1
    call monolis_alloc_I_1d(index, val)
    j = 0
    do i = 1, n_vertex
      j = j + merged_n_dof_list(i)
      index(i+1) = j
    enddo
    !> 結合前グラフ
    allocate(merged_index(n_graphs))
    call monolis_list_initialize_I(merged_index, n_graphs)
    do i = 1, n_graphs
      merged_index(i)%n = graphs(i)%n_vertex + 1
      call monolis_alloc_I_1d(merged_index(i)%array, merged_index(i)%n)
      k = 0
      do j = 1, graphs(i)%n_vertex
        k = k + n_dof_list(i)%array(j)
        merged_index(i)%array(j+1) = k
      enddo
    enddo

    !> index配列を用いて物理量分布の結合
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_vertex  !> 結合前ローカル番号
        if(n_dof_list(i)%array(j) == 0) cycle     !> // TODO 必要ない？
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        iS = index(val) + 1
        iE = index(val+1)
        jS = merged_index(i)%array(j) + 1
        jE = merged_index(i)%array(j+1)
        merged_array_R(iS:iE) = list_struct_R(i)%array(jS:jE)
      enddo
    enddo
  end subroutine gedatsu_merge_distval_R

  !> @ingroup graph_merge
  !> 物理量分布を結合する（整数型）
  subroutine gedatsu_merge_distval_I(n_graphs, graphs, merged_graph, n_dof_list, list_struct_I, merged_n_dof_list, merged_array_I)
    implicit none
    !> [in] 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> [in] グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> [in] 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !> [in] 計算点が持つ物理量の個数
    type(monolis_list_I), intent(in) :: n_dof_list(:)
    !> [in] リスト構造体
    type(monolis_list_I), intent(in) :: list_struct_I(:)
    !> [inout] 結合後の計算点が持つ物理量の個数
    integer(kint), allocatable, intent(inout) :: merged_n_dof_list(:)
    !> [inout] 統合された整数配列
    integer(kint), allocatable, intent(inout) :: merged_array_I(:)

    integer(kint) :: i, j, k, n_vertex, val, idx, size_merged_array_I, iS, iE, jS, jE
    integer(kint), allocatable :: perm(:), vertex_id(:), index(:)
    type(monolis_list_I), allocatable :: merged_index(:)

    if(n_graphs /= size(n_dof_list)) stop "*** n_graphs and size(n_dof_list) don't match."
    if(n_graphs /= size(list_struct_I)) stop "*** n_graphs and size(list_struct_I) don't match."

    n_vertex = merged_graph%n_vertex
    call monolis_dealloc_I_1d(merged_n_dof_list)
    call monolis_alloc_I_1d(merged_n_dof_list, n_vertex)

    !> ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
    call monolis_alloc_I_1d(vertex_id, merged_graph%n_vertex)
    vertex_id(:) = merged_graph%vertex_id(:)
    call monolis_alloc_I_1d(perm, n_vertex)
    call monolis_get_sequence_array_I(perm, n_vertex, 1, 1)
    call monolis_qsort_I_2d(vertex_id, perm, 1, n_vertex)

    !> 結合後グラフの情報を取得
    do i = 1, n_graphs
      if(graphs(i)%n_vertex /= n_dof_list(i)%n) stop "*** graphs(i)%n_vertex and n_dof_list(i)%n don't match. ***"
      do j = 1, n_dof_list(i)%n !> 結合前ローカル番号
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        merged_n_dof_list(val) = n_dof_list(i)%array(j)  !> 指定した計算点の物理量の次元を上書き
      enddo
    enddo
    size_merged_array_I = sum(merged_n_dof_list)

    call monolis_dealloc_I_1d(merged_array_I)
    call monolis_alloc_I_1d(merged_array_I, size_merged_array_I)

    !> index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
    !> 結合後グラフ
    val = n_vertex + 1
    call monolis_alloc_I_1d(index, val)
    j = 0
    do i = 1, n_vertex
      j = j + merged_n_dof_list(i)
      index(i+1) = j
    enddo
    !> 結合前グラフ
    allocate(merged_index(n_graphs))
    call monolis_list_initialize_I(merged_index, n_graphs)
    do i = 1, n_graphs
      merged_index(i)%n = graphs(i)%n_vertex + 1
      call monolis_alloc_I_1d(merged_index(i)%array, merged_index(i)%n)
      k = 0
      do j = 1, graphs(i)%n_vertex
        k = k + n_dof_list(i)%array(j)
        merged_index(i)%array(j+1) = k
      enddo
    enddo

    !> index配列を用いて物理量分布の結合
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_vertex !> 結合前ローカル番号
        if(n_dof_list(i)%array(j) == 0) cycle     !> // TODO 必要ない？
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        iS = index(val) + 1
        iE = index(val+1)
        jS = merged_index(i)%array(j) + 1
        jE = merged_index(i)%array(j+1)
        merged_array_I(iS:iE) = list_struct_I(i)%array(jS:jE)
      enddo
    enddo
  end subroutine gedatsu_merge_distval_I

  !> @ingroup graph_merge
  !> 物理量分布を結合する（複素数型）
  subroutine gedatsu_merge_distval_C(n_graphs, graphs, merged_graph, n_dof_list, list_struct_C, merged_n_dof_list, merged_array_C)
    implicit none
    !> [in] 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> [in] グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> [in] 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph
    !> [in] 計算点が持つ物理量の個数
    type(monolis_list_I), intent(in) :: n_dof_list(:)
    !> [in] リスト構造体
    type(monolis_list_C), intent(in) :: list_struct_C(:)
    !> [inout] 結合後の計算点が持つ物理量の個数
    integer(kint), allocatable, intent(inout) :: merged_n_dof_list(:)
    !> [inout] 統合された複素数配列
    complex(kdouble), allocatable, intent(inout) :: merged_array_C(:)

    integer(kint) :: i, j, k, n_vertex, val, idx, size_merged_array_C, iS, iE, jS, jE
    integer(kint), allocatable :: perm(:), vertex_id(:), index(:)
    type(monolis_list_I), allocatable :: merged_index(:)

    if(n_graphs /= size(n_dof_list)) stop "*** n_graphs and size(n_dof_list) don't match."
    if(n_graphs /= size(list_struct_C)) stop "*** n_graphs and size(list_struct_C) don't match."

    n_vertex = merged_graph%n_vertex
    call monolis_dealloc_I_1d(merged_n_dof_list)
    call monolis_alloc_I_1d(merged_n_dof_list, n_vertex)

    !> ソート前後の結合後ローカル番号の対応付け（vertex_idでグローバル番号　→　結合後ソート後ローカル番号を検索）
    call monolis_alloc_I_1d(vertex_id, merged_graph%n_vertex)
    vertex_id(:) = merged_graph%vertex_id(:)
    call monolis_alloc_I_1d(perm, n_vertex)
    call monolis_get_sequence_array_I(perm, n_vertex, 1, 1)
    call monolis_qsort_I_2d(vertex_id, perm, 1, n_vertex)

    !> 結合後グラフの情報を取得
    do i = 1, n_graphs
      if(graphs(i)%n_vertex /= n_dof_list(i)%n) stop "*** graphs(i)%n_vertex and n_dof_list(i)%n don't match. ***"
      do j = 1, n_dof_list(i)%n !> 結合前ローカル番号
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        merged_n_dof_list(val) = n_dof_list(i)%array(j)  !> 指定した計算点の物理量の次元を上書き
      enddo
    enddo
    size_merged_array_C = sum(merged_n_dof_list)

    call monolis_dealloc_C_1d(merged_array_C)
    call monolis_alloc_C_1d(merged_array_C, size_merged_array_C)

    !> index配列の作成 : 物理量分布の結合において、インデックス指定を簡単にするためにindex配列を使う
    !> 結合後グラフ
    val = n_vertex + 1
    call monolis_alloc_I_1d(index, val)
    j = 0
    do i = 1, n_vertex
      j = j + merged_n_dof_list(i)
      index(i+1) = j
    enddo
    !> 結合前グラフ
    allocate(merged_index(n_graphs))
    call monolis_list_initialize_I(merged_index, n_graphs)
    do i = 1, n_graphs
      merged_index(i)%n = graphs(i)%n_vertex + 1
      call monolis_alloc_I_1d(merged_index(i)%array, merged_index(i)%n)
      k = 0
      do j = 1, graphs(i)%n_vertex
        k = k + n_dof_list(i)%array(j)
        merged_index(i)%array(j+1) = k
      enddo
    enddo

    !> index配列を用いて物理量分布の結合
    do i = 1, n_graphs
      do j = 1, graphs(i)%n_vertex !> 結合前ローカル番号
        if(n_dof_list(i)%array(j) == 0) cycle     !> // TODO 必要ない？
        val = graphs(i)%vertex_id(j)  !> グローバル番号
        call monolis_bsearch_I(vertex_id, 1, n_vertex, val, idx)  !> 結合後ソート後ローカル番号
        val = perm(idx)  !> 結合後ソート前ローカル番号
        iS = index(val) + 1
        iE = index(val+1)
        jS = merged_index(i)%array(j) + 1
        jE = merged_index(i)%array(j+1)
        merged_array_C(iS:iE) = list_struct_C(i)%array(jS:jE)
      enddo
    enddo
  end subroutine gedatsu_merge_distval_C
end module mod_gedatsu_graph_merge
