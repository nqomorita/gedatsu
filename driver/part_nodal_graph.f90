program gedatsu_nodal_graph_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_domain, i
  integer(kint) :: n_nw_dof, n_ew_dof, n_vertex
  character(monolis_charlen) :: finame, dirname, foname, foname_full
  character(monolis_charlen) :: finwname, fiewname, label
  logical :: is_get, is_inw, is_iew
  type(gedatsu_graph), allocatable :: subgraphs(:)
  type(monolis_COM), allocatable :: com(:)
  integer(kint), allocatable :: node_wgt(:,:)
  integer(kint), allocatable :: edge_wgt(:,:)
  integer(kint), allocatable :: id1(:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_nodal_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input node filename}: (default) graph.dat"
    write(*,"(a)")"-inw {input node weight filename}: (default) node_weight.dat"
    write(*,"(a)")"-iew {input edge weight filename}: (default) edge_weight.dat"
    write(*,"(a)")"-o {output graph filename}: (default) graph.dat.{domain_id}"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)") &
    & "./gedatsu_nodal_graph_partitioner {options} -n {number of domains}"
    stop monolis_fail
  endif

  finame = "graph.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  foname = "graph.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  call monolis_input_graph(finame, graph%n_vertex, graph%vertex_id, graph%index, graph%item)

  finwname = "node_weight.dat"
  call monolis_get_arg_input_S("-inw", finwname, is_inw)

  if(is_inw)then
    call monolis_std_log_string2("[input node weight filename]", finwname)
    call monolis_input_distval_i(finwname, label, n_vertex, n_nw_dof, node_wgt)
  endif

  fiewname = "edge_weight.dat"
  call monolis_get_arg_input_S("-iew", fiewname, is_iew)

  if(is_iew)then
    call monolis_std_log_string2("[input edge weight filename]", fiewname)
    call monolis_input_distval_i(fiewname, label, n_vertex, n_ew_dof, edge_wgt)
  endif

  allocate(subgraphs(n_domain))

  call gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)

  allocate(com(n_domain))

  call gedatsu_com_get_comm_table_serial(graph, n_domain, subgraphs, com)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  do i = 1, n_domain
    !> graph.dat
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname), i)
    call monolis_alloc_I_1d(id1, subgraphs(i)%n_vertex)
    call monolis_get_sequence_array_I(id1, subgraphs(i)%n_vertex, 1, 1)
    call monolis_output_graph(foname_full, subgraphs(i)%n_vertex, id1, subgraphs(i)%index, subgraphs(i)%item)
    call monolis_dealloc_I_1d(id1)

    !> internal n_vertex
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".n_internal", i)
    call monolis_output_internal_vertex_number(foname_full, subgraphs(i)%n_internal_vertex)

    !> internal vertex_id
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".id", i)
    call monolis_output_global_id(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id)

    !> send
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".send", i)
    call monolis_output_send_com_table(foname_full, com(i))

    !> recv
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".recv", i)
    call monolis_output_recv_com_table(foname_full, com(i))
  enddo

  call monolis_mpi_finalize()
end program gedatsu_nodal_graph_partitioner
