program gedatsu_partitioner_simple_mesh
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_node, n_elem, n_base
  character(monolis_charlen) :: finname, fiename, fonname, foename, dirname
  logical :: is_get, is_1_origin
  integer(kint), allocatable :: elem(:,:), vertex_id(:)
  integer(kint), allocatable :: conn_index(:), conn_item(:)
  integer(kint), allocatable :: nodal_index(:), nodal_item(:)
  real(kdouble), allocatable :: node(:,:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_simple_mesh_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_convertor_simple_mesh2graph {options}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (default) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (default) elem.dat"
    write(*,"(a)")"-on {output node filename}: (default) node.dat"
    write(*,"(a)")"-oe {output elem filename}: (default) elem.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  finname = "node.dat"
  call monolis_get_arg_input_in_tag(finname, is_get)

  fiename = "elem.dat"
  call monolis_get_arg_input_ie_tag(fiename, is_get)

  fonname = "node.dat"
  call monolis_get_arg_input_S("-on", fonname, is_get)

  foename = "elem.dat"
  call monolis_get_arg_input_S("-oe", foename, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_node(finname, n_node, node)

  call monolis_input_elem(fiename, n_elem, n_base, elem)

  call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

  if(.not. is_1_origin) elem = elem + 1

  call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_index, conn_item)

  call gedatsu_convert_connectivity_graph_to_nodal_graph &
    & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)

  call monolis_alloc_I_1d(vertex_id, n_node)

  call monolis_get_sequence_array_I(vertex_id, n_node, 1, 1)

!  call monolis_output_graph(foname, n_node, vertex_id, nodal_index, nodal_item)
!
!  allocate(com(n_domain))
!
!  call gedatsu_com_get_comm_table_serial(graph, n_domain, subgraphs, com)
!
!  dirname = "./parted.0"
!  call monolis_get_arg_input_d_tag(dirname, is_get)
!
!  do i = 1, n_domain
!    !> graph.dat
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname), i - 1)
!    call monolis_alloc_I_1d(id1, subgraphs(i)%n_vertex)
!    call monolis_get_sequence_array_I(id1, subgraphs(i)%n_vertex, 1, 1)
!    call monolis_output_graph(foname_full, subgraphs(i)%n_vertex, id1, subgraphs(i)%index, subgraphs(i)%item)
!    call monolis_dealloc_I_1d(id1)
!
!    !> internal n_vertex
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".n_internal", i - 1)
!    call monolis_output_internal_vertex_number(foname_full, subgraphs(i)%n_internal_vertex)
!
!    !> global vertex_id
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".id", i - 1)
!    call monolis_output_global_id(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id)
!
!    !> send
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".send", i - 1)
!    call monolis_output_send_com_table(foname_full, com(i))
!
!    !> recv
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".recv", i - 1)
!    call monolis_output_recv_com_table(foname_full, com(i))
!  enddo
!
!
!  call gedatsu_check_connectivity_graph(global_node_graph, global_conn_graph, is_valid)
!
!  if(.not. is_valid)then
!    call monolis_std_error_string("input connectivity graph is not valid")
!    stop monolis_fail
!  endif
!
!  call monolis_alloc_I_1d(is_used, global_node_graph%n_vertex)
!
!  do i = 1, n_domain
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".id", i - 1)
!    call monolis_input_global_id(foname_full, local_node_graph%n_vertex, local_node_graph%vertex_id)
!
!    is_used = 0
!    do j = 1, local_node_graph%n_vertex
!      in = local_node_graph%vertex_id(j)
!      is_used(in) = 1
!    enddo
!
!    call gedatsu_get_parted_connectivity_main(is_used, &
!      & global_conn_graph%n_vertex, global_conn_graph%index, global_conn_graph%item, global_conn_graph%vertex_id, &
!      & local_conn_graph%n_vertex, local_conn_graph%index, local_conn_graph%item, local_conn_graph%vertex_id)
!
!    call monolis_alloc_I_1d(id1, local_conn_graph%n_vertex)
!    call monolis_get_sequence_array_I(id1, local_conn_graph%n_vertex, 1, 1)
!
!    !> graph.dat
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname), i - 1)
!    call monolis_output_graph(foname_full, local_conn_graph%n_vertex, id1, &
!      & local_conn_graph%index, local_conn_graph%item)
!
!    call monolis_dealloc_I_1d(id1)
!    call monolis_dealloc_I_1d(local_node_graph%vertex_id)
!
!    !> global vertex_id
!    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".id", i - 1)
!    call monolis_output_global_id(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id)
!
!    call gedatsu_graph_finalize(local_conn_graph)
!  enddo
!
!  call monolis_mpi_finalize()
end program gedatsu_partitioner_simple_mesh
