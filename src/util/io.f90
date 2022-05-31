module mod_gedatsu_io
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  implicit none

contains

  subroutine gedatsu_graph_input(fname, graph)
    implicit none
    type(gedatsu_graph) :: graph
    integer(gint) :: i, in, j, tmp, nz
    character :: fname*100

    nz = 0
    open(20, file = fname, status = "old")
      read(20,*) graph%n_vertex
      do i = 1, graph%n_vertex
        read(20,*) tmp, in, (tmp, j = 1, in)
        nz = nz + in
      enddo
    close(20)

    allocate(graph%vertex_id(graph%n_vertex), source = 0)
    allocate(graph%index(graph%n_vertex), source = 0)
    allocate(graph%item(nz), source = 0)

    nz = 0
    open(20, file = fname, status = "old")
      read(20,*) graph%n_vertex
      do i = 1, graph%n_vertex
        read(20,*) graph%vertex_id(i), in, (graph%item(nz+j), j = 1, in)
        graph%index(i) = in
        nz = nz + in
      enddo
    close(20)
  end subroutine gedatsu_graph_input

  subroutine gedatsu_graph_output(fname, graph)
    implicit none
    type(gedatsu_graph) :: graph
    !integer(kint) :: i, in, j, tmp, nz
    character :: fname*100
  end subroutine gedatsu_graph_output

end module mod_gedatsu_io
