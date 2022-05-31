module mod_gedatsu_graph_handler
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  !> getter
  subroutine gedatsu_graph_get_n_vertex(graph, n_vertex)
    implicit none
    type(gedatsu_graph) :: graph
    integer(gint) :: n_vertex
    n_vertex = graph%n_vertex
  end subroutine gedatsu_graph_get_n_vertex

  subroutine gedatsu_graph_get_ith_vertex_id(graph, i, vertex_id)
    implicit none
    type(gedatsu_graph) :: graph
    integer(gint) :: i, vertex_id

    if(graph%n_vertex < i) stop
    if(i < 1) stop

    vertex_id = graph%vertex_id(i)
  end subroutine gedatsu_graph_get_ith_vertex_id

end module mod_gedatsu_graph_handler
