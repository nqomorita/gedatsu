program gedatsu_simple_mesh2graph_convertor
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  integer(kint) :: n_node, n_elem, n_base
  character(monolis_charlen) :: finame, foname
  logical :: is_get, is_1_origin
  integer(kint), allocatable :: elem(:,:), vertex_id(:)
  integer(kint), allocatable :: conn_index(:), conn_item(:)
  integer(kint), allocatable :: nodal_index(:), nodal_item(:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_simple_mesh2graph_convertor")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_simple_mesh2graph_convertor {options}"
    write(*,"(a)")""
    write(*,"(a)")"-i {input elem filename}: (default) elem.dat"
    write(*,"(a)")"-o {output graph filename}: (default) graph.dat"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  finame = "elem.dat"

  call monolis_get_arg_input_i_tag(finame, is_get)

  call monolis_input_elem(finame, n_elem, n_base, elem)

  call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

  if(.not. is_1_origin)then
    elem = elem + 1
  endif

  n_node = maxval(elem)

  call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_index, conn_item)

  call gedatsu_convert_connectivity_graph_to_nodal_graph &
    & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)

  call monolis_alloc_I_1d(vertex_id, n_node)

  call monolis_get_sequence_array_I(vertex_id, n_node, 1, 1)

  if(.not. is_1_origin)then
    nodal_item = nodal_item - 1
    vertex_id = vertex_id - 1
  endif

  foname = "graph.dat"

  call monolis_get_arg_input_o_tag(foname, is_get)

  call monolis_output_graph(foname, n_node, vertex_id, nodal_index, nodal_item)

  call monolis_mpi_finalize()
end program gedatsu_simple_mesh2graph_convertor
