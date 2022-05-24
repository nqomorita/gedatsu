module mod_gedatsu_graph
  use mod_gedatsu_prm
  use iso_c_binding
  implicit none

  type gedatsu_graph
    integer(gint) :: n_vertex
    !integer(gint) :: n_edge

    integer(gint), allocatable :: vertex_id(:)

    integer(c_int), pointer :: index(:) => null()
    integer(c_int), pointer :: item(:)  => null()
  end type gedatsu_graph

contains

  subroutine gedatsu_graph_initialize(graph)
    implicit none
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_graph_initialize

  subroutine gedatsu_graph_finalize(graph)
    implicit none
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_graph_finalize

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

end module mod_gedatsu_graph