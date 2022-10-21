!> グラフ操作モジュール
module mod_gedatsu_graph_handler
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

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

!! !> @ingroup group_graph_1
!! !> グラフの i 番目のノード id を取得
!! subroutine gedatsu_graph_get_ith_vertex_id(graph, i, vertex_id)
!!   implicit none
!!   !> [in] graph 構造体
!!   type(gedatsu_graph) :: graph
!!   !> [in] ノード番号 i
!!   integer(gint) :: i
!!   !> [out] グラフの i 番目のノード id
!!   integer(gint) :: vertex_id

!!   if(graph%n_vertex < i)then
!!     call gedatsu_error_string("gedatsu_graph_get_ith_vertex_id")
!!     call gedatsu_error_string("input node number is larger than the number of graph node
!!     call gedatsu_error_stop()
!!   endif

!!   if(i < 1)then
!!     call gedatsu_error_string("gedatsu_graph_get_ith_vertex_id")
!!     call gedatsu_error_string("input node number is less than 0")
!!     call gedatsu_error_stop()
!!   endif

!!   vertex_id = graph%vertex_id(i)
!! end subroutine gedatsu_graph_get_ith_vertex_id

  !> @ingroup group_graph_1
  !> グラフのノード数を取得
  subroutine gedatsu_graph_add_edge(graph, n_edge, edge)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] グラフのエッジ数
    integer(gint) :: n_edge
    !> [in] グラフエッジ
    integer(gint) :: edge(:,:)
  end subroutine gedatsu_graph_add_edge

!gedatsu_graph_delete_edge(graph, n_edge, array)
!gedatsu_graph_set_ith_vertex_id(graph, i, vertex_id)
!gedatsu_graph_set_vertex_id_array(graph, array)
!gedatsu_graph_get_vertex_id_array(graph, array)

end module mod_gedatsu_graph_handler
