program gedatsu_connectivity_graph_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: global_node_graph
  type(gedatsu_graph) :: global_conn_graph
  type(gedatsu_graph) :: local_node_graph
  type(gedatsu_graph) :: local_conn_graph
  integer(kint) :: n_domain, i, j, in, id, idx
  character(monolis_charlen) :: finame, dirname, foname_full
  character(monolis_charlen) :: finname
  logical :: is_get, is_valid, is_1_origin
  integer(kint), allocatable :: domain_id(:), id1(:), perm(:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_connectivity_graph_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_connectivity_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input connectivity graph filename}: (default) connectivity.dat"
    write(*,"(a)")"-ig {input nodal graph filename}: (default) graph.dat"
    write(*,"(a)")"-o {output connectivity graph filename}: (default) connectivity.dat.{domain_id}"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_connectivity_graph_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input connectivity graph filename}: (default) connectivity.dat"
    write(*,"(a)")"-ig {input nodal graph filename}: (default) graph.dat"
    write(*,"(a)")"-o {output connectivity graph filename}: (default) connectivity.dat.{domain_id}"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_fail
  endif

  if(n_domain <= 1) stop

  finame = "connectivity.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  finname = "graph.dat"
  call monolis_get_arg_input_S("-ig", finname, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_graph(finname, global_node_graph%n_vertex, global_node_graph%vertex_id, &
    & global_node_graph%index, global_node_graph%item)

  call monolis_input_graph(finame, global_conn_graph%n_vertex, global_conn_graph%vertex_id, &
    & global_conn_graph%index, global_conn_graph%item)

  call monolis_check_fortran_1_origin_graph(global_conn_graph%vertex_id, is_1_origin)

  if(.not. is_1_origin) global_node_graph%vertex_id = global_node_graph%vertex_id + 1
  if(.not. is_1_origin) global_node_graph%item = global_node_graph%item + 1
  if(.not. is_1_origin) global_conn_graph%vertex_id = global_conn_graph%vertex_id + 1
  if(.not. is_1_origin) global_conn_graph%item = global_conn_graph%item + 1

  call gedatsu_check_connectivity_graph(global_node_graph, global_conn_graph, is_valid)

  if(.not. is_valid)then
    call monolis_std_error_string("input connectivity graph is not valid")
    stop monolis_fail
  endif

  call monolis_alloc_I_1d(domain_id, global_node_graph%n_vertex)

  !> get domain id array
  do i = 1, n_domain
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".id", i - 1)
    call monolis_input_global_id(foname_full, local_node_graph%n_vertex, local_node_graph%vertex_id)

    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".n_internal", i - 1)
    call monolis_input_internal_vertex_number(foname_full, local_node_graph%n_internal_vertex)

    if(.not. is_1_origin) local_node_graph%vertex_id = local_node_graph%vertex_id + 1

    do j = 1, local_node_graph%n_internal_vertex
      in = local_node_graph%vertex_id(j)
      domain_id(in) = i
    enddo
    call monolis_dealloc_I_1d(local_node_graph%vertex_id)
  enddo
  domain_id = domain_id - 1

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".id", i - 1)
    call monolis_input_global_id(foname_full, local_node_graph%n_vertex, local_node_graph%vertex_id)

    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".n_internal", i - 1)
    call monolis_input_internal_vertex_number(foname_full, local_node_graph%n_internal_vertex)

    if(.not. is_1_origin) local_node_graph%vertex_id = local_node_graph%vertex_id + 1

    call gedatsu_get_parted_connectivity_main(i - 1, domain_id, &
      & global_conn_graph%n_vertex, global_conn_graph%index, global_conn_graph%item, global_conn_graph%vertex_id, &
      & local_conn_graph%n_vertex, local_conn_graph%n_internal_vertex, &
      & local_conn_graph%index, local_conn_graph%item, local_conn_graph%vertex_id)

    call monolis_alloc_I_1d(id1, local_conn_graph%n_vertex)
    call monolis_get_sequence_array_I(id1, local_conn_graph%n_vertex, 1, 1)

    !> graph.dat
    call monolis_alloc_I_1d(perm, local_node_graph%n_vertex)
    call monolis_get_sequence_array_I(perm, local_node_graph%n_vertex, 1, 1)
    call monolis_qsort_I_2d(local_node_graph%vertex_id, perm, 1, local_node_graph%n_vertex)

    do j = 1, local_conn_graph%index(local_conn_graph%n_vertex + 1)
      id = local_conn_graph%item(j)
      call monolis_bsearch_I(local_node_graph%vertex_id, 1, local_node_graph%n_vertex, id, idx)
      local_conn_graph%item(j) = perm(idx)
    enddo

    call monolis_dealloc_I_1d(perm)

    if(.not. is_1_origin) local_conn_graph%item = local_conn_graph%item - 1
    if(.not. is_1_origin) id1 = id1 - 1

    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame), i - 1)
    call monolis_output_graph(foname_full, local_conn_graph%n_vertex, id1, &
      & local_conn_graph%index, local_conn_graph%item)

    call monolis_dealloc_I_1d(id1)
    call monolis_dealloc_I_1d(local_node_graph%vertex_id)

    !> global vertex_id
    if(.not. is_1_origin) local_conn_graph%vertex_id = local_conn_graph%vertex_id - 1

    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".id", i - 1)
    call monolis_output_global_id(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id)

    !> internal n_vertex
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame)//".n_internal", i - 1)
    call monolis_output_internal_vertex_number(foname_full, local_conn_graph%n_internal_vertex)

    call gedatsu_graph_finalize(local_conn_graph)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_connectivity_graph_partitioner
