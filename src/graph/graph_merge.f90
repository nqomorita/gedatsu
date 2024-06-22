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

  type monolis_list_R
    integer(kint) :: n
    real(kdouble), allocatable :: array(:)
  end type monolis_list_R

  type monolis_list_I
  integer(kint) :: n
  integer(kint), allocatable :: array(:)
  end type monolis_list_I

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
    !> 部分領域ごとに並べるか、グローバル計算点順に並べるかを決めるフラグ [ORDER_DOMAIN_ID, ORDER_NODAL_ID]
    integer(kint), intent(in) :: order_type

    integer(kint) :: i, j, k, minval_1
    integer(kint) :: n_vertex_including_overlap, n_vertex_overlap
    integer(kint), allocatable :: vertex_id_including_overlap(:)

    n_vertex_including_overlap = 0
    do i = 1, n_graphs
      n_vertex_including_overlap = n_vertex_including_overlap + graphs(i)%n_vertex
    enddo
    allocate(vertex_id_including_overlap(n_vertex_including_overlap))

    !> 結合（部分領域ごとに並べるか、グローバル計算点順に並べるか）
    j = 1
    k = 0
    do i = 1, n_graphs
      k = k + graphs(i)%n_vertex
      vertex_id_including_overlap(j:k) = graphs(i)%vertex_id(1:graphs(i)%n_vertex)
      j = j + graphs(i)%n_vertex
    enddo

    if(order_type == ORDER_DOMAIN_ID)then
    elseif(order_type == ORDER_NODAL_ID)then
      !> 昇順ソート（重複した値は配列の先頭に集める）
      minval_1 = minval(vertex_id_including_overlap) - 1
      n_vertex_overlap = 0
      call qsort_I_1d_overlap(vertex_id_including_overlap, 1, size(vertex_id_including_overlap), minval_1, n_vertex_overlap)

      !> 重複を除いたグラフを作成
      call gedatsu_graph_initialize(merged_graph)
      merged_graph%n_vertex = n_vertex_including_overlap - n_vertex_overlap
      do i = 1, n_graphs
        merged_graph%n_internal_vertex = merged_graph%n_internal_vertex + graphs(i)%n_internal_vertex
      enddo
      allocate(merged_graph%vertex_id(merged_graph%n_vertex), source=0)
      allocate(merged_graph%vertex_domain_id(merged_graph%n_vertex), source=0)
      merged_graph%vertex_id(1:) = vertex_id_including_overlap(n_vertex_overlap+1:)

      ! index と item 配列ってどうやって結合する？
      ! →　index は Local_id のはず。グローバル計算点順に並べる場合、順番ごっちゃになるから探索が必要かも
    endif
  end subroutine gedatsu_merge_nodal_subgraphs

  subroutine gedatsu_merge_connectivity_subgraphs(merged_nodal_graph, merged_nodal_monoCOM, &
    & n_conn_graphs, conn_graphs, merged_conn_graph)
    implicit none
    !> 統合された計算点グラフ構造
    type(gedatsu_graph), intent(in) :: merged_nodal_graph
    !>統合された計算点グラフ構造の通信テーブル
    type(monolis_COM), intent(in) ::  merged_nodal_monoCOM
    !> 統合したいコネクティビティグラフ構造の個数
    integer(kint), intent(in) :: n_conn_graphs
    !> コネクティビティグラフ構造の配列（配列長 n_conn_graphs）
    type(gedatsu_graph), intent(in) :: conn_graphs(:)
    !> 統合されたコネクティビティグラフ構造
    type(gedatsu_graph), intent(inout) :: merged_conn_graph
  end subroutine

  subroutine gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, list_struct_R, merged_array_R)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph(:)
    !>  リスト構造体
    type(monolis_list_R), intent(in) :: list_struct_R(:)
    !> 統合された整数配列
    real(kdouble), intent(inout) :: merged_array_R(:)
  end subroutine gedatsu_merge_distval_R

  subroutine gedatsu_merge_distval_I(n_graphs, graphs, merged_graph, list_struct_I, merged_array_I)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph(:)
    !> リスト構造体
    type(monolis_list_I), intent(in) :: list_struct_I(:)
    !>  統合された実数配列
    real(kdouble), intent(inout) :: merged_array_I(:)
  end subroutine gedatsu_merge_distval_I

  subroutine gedatsu_merge_distval_C(n_graphs, graphs, merged_graph, list_struct_C, merged_array_C)
    implicit none
    !> 統合したいグラフ構造の個数
    integer(kint), intent(in) :: n_graphs
    !> グラフ構造の配列（配列長 n_graphs）
    type(gedatsu_graph), intent(in) :: graphs(:)
    !> 統合されたグラフ構造
    type(gedatsu_graph), intent(in) :: merged_graph(:)
    !> リスト構造体
    type(monolis_list_C), intent(in) :: list_struct_C(:)
    !> 統合された複素数配列
    real(kdouble), intent(inout) :: merged_array_C(:)
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

  !> クイックソート（1次元整数配列）：重複した値が存在したら、片方のみ minval で上書きする
  recursive subroutine qsort_I_1d_overlap(array, iS, iE, minval, n_vertex_overlap)
    implicit none
    !> 整数配列
    integer(kint), intent(inout) :: array(:)
    !> ソートする開始位置
    integer(kint), intent(in) :: iS
    !> ソートする終了位置
    integer(kint), intent(in) :: iE
    !> 重複した値を上書きする値
    integer(kint), intent(in) :: minval
    !> 重複した値の数
    integer(kint), intent(inout) :: n_vertex_overlap
    integer(kint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array(center)
    left = iS
    right = iE
    n_vertex_overlap = 0

    do
      do while (array(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array(right))
        right = right - 1
      enddo

      if (left > right) exit
      if(left == right)then
        n_vertex_overlap = n_vertex_overlap + 1
        right = minval
        exit
      endif

      tmp = array(left)
      array(left) = array(right)
      array(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call qsort_I_1d_overlap(array, iS, left - 1, minval, n_vertex_overlap)
    if(right + 1 < iE) call qsort_I_1d_overlap(array, right + 1, iE, minval, n_vertex_overlap)
  end subroutine qsort_I_1d_overlap
end module mod_gedatsu_graph_merge
