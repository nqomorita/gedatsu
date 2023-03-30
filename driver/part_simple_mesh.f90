program gedatsu_partitioner_simple_mesh
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: node_graph
  type(gedatsu_graph) :: conn_graph
  type(gedatsu_graph), allocatable :: subgraphs(:)
  integer(kint) :: n_node, n_elem, n_base, n_domain
  character(monolis_charlen) :: finname, fiename, dirname
  logical :: is_get, is_1_origin
  integer(kint), allocatable :: elem(:,:)
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
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  finname = "node.dat"
  call monolis_get_arg_input_in_tag(finname, is_get)

  fiename = "elem.dat"
  call monolis_get_arg_input_ie_tag(fiename, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_node(finname, n_node, node)

  call monolis_input_elem(fiename, n_elem, n_base, elem)

  call monolis_check_fortran_1_origin_elem(elem, is_1_origin)

  if(.not. is_1_origin) elem = elem + 1

  call node_partition()

  call elem_partition()

  call monolis_mpi_finalize()

contains

  subroutine elem_partition()
    implicit none
    type(gedatsu_graph) :: local_conn_graph
    integer(kint) :: i, j, in
    character(monolis_charlen) :: foname_full
    logical :: is_valid
    integer(kint), allocatable :: id1(:)
    integer(kint), allocatable :: is_used(:)

    call gedatsu_check_connectivity_graph(node_graph, conn_graph, is_valid)

    if(.not. is_valid)then
      call monolis_std_error_string("input connectivity graph is not valid")
      stop monolis_fail
    endif

    call monolis_alloc_I_1d(is_used, node_graph%n_vertex)

    do i = 1, n_domain
      if(.not. is_1_origin) subgraphs(i)%vertex_id = subgraphs(i)%vertex_id + 1

      is_used = 0
      do j = 1, subgraphs(i)%n_vertex
        in = subgraphs(i)%vertex_id(j)
        is_used(in) = 1
      enddo

      call monolis_alloc_I_1d(conn_graph%vertex_id, conn_graph%n_vertex)
      call monolis_get_sequence_array_I(conn_graph%vertex_id, conn_graph%n_vertex, 1, 1)

      call gedatsu_get_parted_connectivity_main(is_used, &
        & conn_graph%n_vertex, conn_graph%index, conn_graph%item, conn_graph%vertex_id, &
        & local_conn_graph%n_vertex, local_conn_graph%index, local_conn_graph%item, local_conn_graph%vertex_id)

      call monolis_alloc_I_1d(id1, local_conn_graph%n_vertex)
      call monolis_get_sequence_array_I(id1, local_conn_graph%n_vertex, 1, 1)

      !> graph.dat
      if(.not. is_1_origin) local_conn_graph%item = local_conn_graph%item - 1
      if(.not. is_1_origin) id1 = id1 - 1

      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(fiename), i - 1)
      call monolis_output_graph(foname_full, local_conn_graph%n_vertex, id1, &
        & local_conn_graph%index, local_conn_graph%item)

      call monolis_dealloc_I_1d(id1)

      !> global vertex_id
      if(.not. is_1_origin) local_conn_graph%vertex_id = local_conn_graph%vertex_id - 1

      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(fiename)//".id", i - 1)
      call monolis_output_global_id(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id)

      call gedatsu_graph_finalize(local_conn_graph)
    enddo
  end subroutine elem_partition

  subroutine node_partition()
    implicit none
    integer(kint) :: i
    character(monolis_charlen) :: foname_full
    type(monolis_COM), allocatable :: com(:)
    integer(kint), allocatable :: id1(:)

    call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_graph%index, conn_graph%item)

    conn_graph%n_vertex = n_elem

    call gedatsu_convert_connectivity_graph_to_nodal_graph &
      & (n_node, n_elem, conn_graph%index, conn_graph%item, node_graph%index, node_graph%item)

    node_graph%n_vertex = n_node

    call monolis_alloc_I_1d(node_graph%vertex_id, n_node)

    call monolis_get_sequence_array_I(node_graph%vertex_id, n_node, 1, 1)

    allocate(subgraphs(n_domain))

    call gedatsu_graph_partition(node_graph, n_domain, subgraphs)

    allocate(com(n_domain))

    call gedatsu_com_get_comm_table_serial(node_graph, n_domain, subgraphs, com)

    do i = 1, n_domain
      !> graph.dat
      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname), i - 1)
      call monolis_alloc_I_1d(id1, subgraphs(i)%n_vertex)
      call monolis_get_sequence_array_I(id1, subgraphs(i)%n_vertex, 1, 1)

      if(.not. is_1_origin) id1 = id1 - 1
      if(.not. is_1_origin) subgraphs(i)%item = subgraphs(i)%item - 1
      if(.not. is_1_origin) subgraphs(i)%vertex_id = subgraphs(i)%vertex_id - 1

      call monolis_output_graph(foname_full, subgraphs(i)%n_vertex, id1, subgraphs(i)%index, subgraphs(i)%item)
      call monolis_dealloc_I_1d(id1)

      !> internal n_vertex
      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".n_internal", i - 1)
      call monolis_output_internal_vertex_number(foname_full, subgraphs(i)%n_internal_vertex)

      !> global vertex_id
      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".id", i - 1)
      call monolis_output_global_id(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id)

      !> send
      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".send", i - 1)
      call monolis_output_send_com_table(foname_full, com(i))

      !> recv
      foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finname)//".recv", i - 1)
      call monolis_output_recv_com_table(foname_full, com(i))

      call monolis_dealloc_I_1d(id1)
    enddo
  end subroutine node_partition

end program gedatsu_partitioner_simple_mesh
