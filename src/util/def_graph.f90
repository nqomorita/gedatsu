!> graph モジュール
module mod_gedatsu_graph
  use mod_monolis_utils
  implicit none

  !> @ingroup graph_basic
  !> graph 構造体
  !> @note gedatsu IO では gedatsu グラフ形式フォーマットを入出力するが、
  !> プログラム内部では CSR 圧縮形式によってグラフを保持する。
  type gedatsu_graph
    !> ノード数
    integer(kint) :: n_vertex = 0
    !> 領域分割における内部ノード数
    integer(kint) :: n_internal_vertex = 0
    !> ノード id 配列
    integer(kint), allocatable :: vertex_id(:)
    !> 領域番号配列
    integer(kint), allocatable :: vertex_domain_id(:)
    !> graph の CSR 圧縮形式の index 配列
    integer(kint), allocatable :: index(:)
    !> graph の CSR 圧縮形式の index 配列
    integer(kint), allocatable :: item(:)
  end type gedatsu_graph

contains

  !> @ingroup group_init
  !> graph 構造体の初期化関数
  subroutine gedatsu_graph_initialize(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    graph%n_vertex = 0

    call monolis_dealloc_I_1d(graph%vertex_domain_id)

    call monolis_dealloc_I_1d(graph%index)

    call monolis_dealloc_I_1d(graph%item)
  end subroutine gedatsu_graph_initialize

  !> @ingroup group_init
  !> graph 構造体の終了関数
  subroutine gedatsu_graph_finalize(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    graph%n_vertex = 0

    call monolis_dealloc_I_1d(graph%vertex_domain_id)

    call monolis_dealloc_I_1d(graph%index)

    call monolis_dealloc_I_1d(graph%item)
  end subroutine gedatsu_graph_finalize

  !> graph 構造体のデバッグ用データ書き出し
  subroutine gedatsu_graph_debug_write(graph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    write(*,*)"--- gedatsu_graph_debug_write"
    write(*,*)"graph%n_vertex"
    write(*,*)graph%n_vertex
    write(*,*)"graph%vertex_id"
    write(*,*)graph%vertex_id
    write(*,*)"graph%vertex_domain_id"
    write(*,*)graph%vertex_domain_id
    write(*,*)"graph%index"
    write(*,*)graph%index
    write(*,*)"graph%item"
    write(*,*)graph%item
  end subroutine gedatsu_graph_debug_write
end module mod_gedatsu_graph