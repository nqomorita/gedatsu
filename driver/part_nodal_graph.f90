program gedatsu_nodal_graph_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_domain, i
  integer(kint) :: n_nw_dof, n_ew_dof, n_vertex
  character(monolis_charlen) :: finame, dirname, foname_full
  character(monolis_charlen) :: finwname, fiewname, label
  logical :: is_get, is_inw, is_iew, is_1_origin
  type(gedatsu_graph), allocatable :: subgraphs(:)
  type(monolis_COM), allocatable :: com(:)
  integer(kint), allocatable :: node_wgt(:,:)
  integer(kint), allocatable :: edge_wgt(:,:)
  integer(kint), allocatable :: id1(:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_nodal_graph_partitioner")

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
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_nodal_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input node filename}: (default) graph.dat"
    write(*,"(a)")"-inw {input node weight filename}: (default) node_weight.dat"
    write(*,"(a)")"-iew {input edge weight filename}: (default) edge_weight.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_fail
  endif

  if(n_domain <= 1) stop

  finame = "graph.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  call monolis_input_graph(finame, graph%n_vertex, graph%vertex_id, graph%index, graph%item)

  call monolis_check_fortran_1_origin_graph(graph%vertex_id, is_1_origin)

  if(.not. is_1_origin) graph%vertex_id = graph%vertex_id + 1
  if(.not. is_1_origin) graph%item = graph%item + 1

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
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame), i - 1)
    call monolis_alloc_I_1d(id1, subgraphs(i)%n_vertex)
    call monolis_get_sequence_array_I(id1, subgraphs(i)%n_vertex, 1, 1)

    if(.not. is_1_origin) id1 = id1 - 1
    if(.not. is_1_origin) subgraphs(i)%item = subgraphs(i)%item - 1
    if(.not. is_1_origin) subgraphs(i)%vertex_id = subgraphs(i)%vertex_id - 1
    if(.not. is_1_origin) com(i)%send_item = com(i)%send_item - 1
    if(.not. is_1_origin) com(i)%recv_item = com(i)%recv_item - 1

    call monolis_output_graph(foname_full, subgraphs(i)%n_vertex, id1, subgraphs(i)%index, subgraphs(i)%item)
    call monolis_dealloc_I_1d(id1)

    !> internal n_vertex
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".n_internal", i - 1)
    call monolis_output_internal_vertex_number(foname_full, subgraphs(i)%n_internal_vertex)

    !> global vertex_id
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".id", i - 1)
    call monolis_output_global_id(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id)

    !> send
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".send", i - 1)
    call monolis_output_send_com_table(foname_full, com(i))

    !> recv
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".recv", i - 1)
    call monolis_output_recv_com_table(foname_full, com(i))
  enddo

  call monolis_mpi_finalize()
end program gedatsu_nodal_graph_partitioner
