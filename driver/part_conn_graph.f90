program gedatsu_connectivity_graph_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: global_node_graph
  type(gedatsu_graph) :: global_conn_graph
  type(gedatsu_graph) :: local_node_graph
  type(gedatsu_graph) :: local_conn_graph
  integer(kint) :: n_domain, i
  character(monolis_charlen) :: finame, dirname, foname, foname_full, fidname
  character(monolis_charlen) :: finname
  logical :: is_get, is_valid

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_connectivity_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input graph filename}: (default) connectivity.dat"
    write(*,"(a)")"-o {output graph filename}: (default) connectivity.dat.{domain_id}"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)") &
    & "./gedatsu_connectivity_graph_partitioner {options} -n {number of domains}"
    stop monolis_fail
  endif

  finame = "connectivity.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  foname = "connectivity.dat"
  call monolis_get_arg_input_o_tag(foname, is_get)

  finname = "graph.dat"
  call monolis_get_arg_input_S("-in", finname, is_get)

  fidname = "graph.dat.id"
  call monolis_get_arg_input_S("-id", fidname, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_graph(finname, global_node_graph%n_vertex, global_node_graph%vertex_id, &
    & global_node_graph%index, global_node_graph%item)

  call monolis_input_graph(finame, global_conn_graph%n_vertex, global_conn_graph%vertex_id, &
    & global_conn_graph%index, global_conn_graph%item)

  call gedatsu_check_connectivity_graph(global_node_graph, global_conn_graph, is_valid)

  if(.not. is_valid)then
    call monolis_std_error_string("input connectivity graph is not valid")
    stop monolis_fail
  endif

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name(dirname, trim(fidname), i)
    call monolis_input_global_id(foname_full, local_node_graph%n_vertex, local_node_graph%vertex_id)

    !call local_conn_graph

    foname_full = monolis_get_output_file_name(dirname, trim(foname), i)
    call monolis_output_graph(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id, &
      & local_conn_graph%index, local_conn_graph%item)

    call monolis_dealloc_I_1d(local_node_graph%vertex_id)

    call gedatsu_graph_finalize(local_conn_graph)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_connectivity_graph_partitioner
