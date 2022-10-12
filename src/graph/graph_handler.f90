!> グラフ操作モジュール
module mod_gedatsu_graph_handler
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

  implicit none

contains

  !> @ingroup group_graph
  !> グラフのノード数を取得
  subroutine gedatsu_graph_get_n_vertex(graph, n_vertex)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [out] グラフのノード数
    integer(gint) :: n_vertex

    n_vertex = graph%n_vertex
  end subroutine gedatsu_graph_get_n_vertex

  !> @ingroup group_graph
  !> グラフの i 番目のノード id を取得
  subroutine gedatsu_graph_get_ith_vertex_id(graph, i, vertex_id)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] ノード番号 i
    integer(gint) :: i
    !> [out] グラフの i 番目のノード id
    integer(gint) :: vertex_id

    if(graph%n_vertex < i)then
      call gedatsu_error_string("input node number is larger than the number of graph nodes")
      call gedatsu_error_stop()
    endif

    if(i < 1)then
      call gedatsu_error_string("input node number is less than 0")
      call gedatsu_error_stop()
    endif

    vertex_id = graph%vertex_id(i)
  end subroutine gedatsu_graph_get_ith_vertex_id

end module mod_gedatsu_graph_handler
