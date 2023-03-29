program gedatsu_connectivity_graph_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: global_node_graph
  type(gedatsu_graph) :: global_conn_graph
  type(gedatsu_graph) :: local_node_graph
  type(gedatsu_graph) :: local_conn_graph
  integer(kint) :: n_domain, i, j, in
  character(monolis_charlen) :: finame, dirname, foname, foname_full
  character(monolis_charlen) :: finname
  logical :: is_get, is_valid
  integer(kint), allocatable :: is_used(:), id1(:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_connectivity_graph_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_connectivity_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-ig {input nodal graph filename}: (default) graph.dat"
    write(*,"(a)")"-i {input connectivity graph filename}: (default) connectivity.dat"
    write(*,"(a)")"-o {output connectivity graph filename}: (default) connectivity.dat.{domain_id}"
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
  call monolis_get_arg_input_S("-ig", finname, is_get)

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

  call monolis_alloc_I_1d(is_used, global_node_graph%n_vertex)

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".id", i - 1)
    call monolis_input_global_id(foname_full, local_node_graph%n_vertex, local_node_graph%vertex_id)

    is_used = 0
    do j = 1, local_node_graph%n_vertex
      in = local_node_graph%vertex_id(j)
      is_used(in) = 1
    enddo

    call gedatsu_get_parted_connectivity_main(is_used, &
      & global_conn_graph%n_vertex, global_conn_graph%index, global_conn_graph%item, global_conn_graph%vertex_id, &
      & local_conn_graph%n_vertex, local_conn_graph%index, local_conn_graph%item, local_conn_graph%vertex_id)

    call monolis_alloc_I_1d(id1, local_conn_graph%n_vertex)
    call monolis_get_sequence_array_I(id1, local_conn_graph%n_vertex, 1, 1)

    !> graph.dat
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname), i - 1)
    call monolis_output_graph(foname_full, local_conn_graph%n_vertex, id1, &
      & local_conn_graph%index, local_conn_graph%item)

    call monolis_dealloc_I_1d(id1)
    call monolis_dealloc_I_1d(local_node_graph%vertex_id)

    !> global vertex_id
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(foname)//".id", i - 1)
    call monolis_output_global_id(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id)

    call gedatsu_graph_finalize(local_conn_graph)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_connectivity_graph_partitioner
