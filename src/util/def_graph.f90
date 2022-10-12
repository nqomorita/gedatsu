!> graph モジュール
module mod_gedatsu_graph
  use mod_gedatsu_prm
  use iso_c_binding
  implicit none

  !> graph 構造体
  type gedatsu_graph
    !> ノード数
    integer(gint) :: n_vertex
    !> ノード id 配列
    integer(gint), allocatable :: vertex_id(:)
    !> 領域番号配列
    integer(gint), allocatable :: vertex_domain_id(:)
    !> graph の CSR 圧縮形式の index 配列
    integer(gint), allocatable :: index(:)
    !> graph の CSR 圧縮形式の index 配列
    integer(gint), allocatable :: item(:)
  end type gedatsu_graph

contains

  !> @ingroup group_init
  !> graph 構造体の初期化関数
  subroutine gedatsu_graph_initialize(graph)
    implicit none
    !> graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_graph_initialize

  !> @ingroup group_init
  !> graph 構造体の終了関数
  subroutine gedatsu_graph_finalize(graph)
    implicit none
    !> graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_graph_finalize

end module mod_gedatsu_graph