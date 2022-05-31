module mod_gedatsu_graph
  use mod_gedatsu_prm
  use iso_c_binding
  implicit none

  type gedatsu_graph
    integer(gint) :: n_vertex

    integer(gint), allocatable :: vertex_id(:)
    integer(gint), allocatable :: vertex_domain_id(:)

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
end module mod_gedatsu_graph