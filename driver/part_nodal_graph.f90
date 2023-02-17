program gedatsu_partitioner_nodal_graph
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_vertex
  character(monolis_charlen) :: finame, foname
  logical :: is_get
  integer(kint), allocatable :: vertex_id(:)
  integer(kint), allocatable :: index(:), item(:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_partitioner_nodal_graph {options}"
    write(*,"(a)")""
    write(*,"(a)")"-i {input node filename}: (defualt) graph.dat"
    write(*,"(a)")"-o {output graph filename}: (defualt) graph.dat.{domain_id}"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  finame = "graph.dat"
  call monolis_get_arg_input_i_tag(finame)

  foname = "graph.dat"
  call monolis_get_arg_input_o_tag(foname)

  call monolis_input_graph(finame, n_vertex, vertex_id, index, item)

  call monolis_output_graph(foname, n_vertex, vertex_id, index, item)

  call monolis_mpi_finalize()
end program gedatsu_partitioner_nodal_graph
