program gedatsu_partitioner_simple_mesh
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_node, n_elem, n_base
  character(monolis_charlen) :: finname, fiename, foname, dirname
  logical :: is_get
  integer(kint), allocatable :: elem(:,:), vertex_id(:)
  integer(kint), allocatable :: conn_index(:), conn_item(:)
  integer(kint), allocatable :: nodal_index(:), nodal_item(:)
  real(kdouble), allocatable :: node(:,:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_convertor_simple_mesh2graph {options}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (default) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (default) elem.dat"
    write(*,"(a)")"-o  {output graph filename}: (default) graph.dat"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  finname = "node.dat"
  call monolis_get_arg_input_in_tag(finname, is_get)

  fiename = "elem.dat"
  call monolis_get_arg_input_ie_tag(fiename, is_get)

  foname = "graph.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_node(finname, n_node, node)

  call monolis_input_elem(fiename, n_elem, n_base, elem)

  call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_index, conn_item)

  call gedatsu_convert_connectivity_graph_to_nodal_graph &
    & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)

  call monolis_alloc_I_1d(vertex_id, n_node)

  call monolis_get_sequence_array_I(vertex_id, n_node, 1, 1)

  !> for C binding
  if(minval(elem) == 0)then
    nodal_item = nodal_item - 1
    vertex_id = vertex_id - 1
  endif

  call monolis_output_graph(foname, n_node, vertex_id, nodal_index, nodal_item)

  call monolis_mpi_finalize()
end program gedatsu_partitioner_simple_mesh
