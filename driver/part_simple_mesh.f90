program gedatsu_partitioner_simple_mesh
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: node_graph
  type(gedatsu_graph) :: conn_graph
  type(gedatsu_graph), allocatable :: subgraphs(:)
  integer(kint) :: n_node, n_elem, n_base, n_domain
  integer(kint) :: n_nw_dof, n_ew_dof, n_edge
  character(monolis_charlen) :: finname, fiename, dirname
  character(monolis_charlen) :: finwname, fiewname, fgsname, label
  logical :: is_get, is_inw, is_iew, is_1_origin, is_given_subdomain
  integer(kint), allocatable :: elem(:,:)
  integer(kint), allocatable :: node_wgt(:,:)
  integer(kint), allocatable :: edge_wgt(:,:)
  integer(kint), allocatable :: given_domain_id(:)
  real(kdouble), allocatable :: node(:,:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_simple_mesh_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_convertor_simple_mesh2graph {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (default) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (default) elem.dat"
    write(*,"(a)")"-inw {input node weight filename}: (default) node_weight.dat"
    write(*,"(a)")"-iew {input edge weight filename}: (default) edge_weight.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"--given_subdomain {input subdomain filename}"
    write(*,"(a)")"-h  : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_convertor_simple_mesh2graph {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-in {input node filename}: (default) node.dat"
    write(*,"(a)")"-ie {input elem filename}: (default) elem.dat"
    write(*,"(a)")"-inw {input node weight filename}: (default) node_weight.dat"
    write(*,"(a)")"-iew {input edge weight filename}: (default) edge_weight.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"--given_subdomain {input subdomain filename}"
    write(*,"(a)")"-h  : help"
  endif

  if(n_domain <= 1) stop

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

  finwname = "node_weight.dat"
  call monolis_get_arg_input_S("-inw", finwname, is_inw)

  if(is_inw)then
    call monolis_std_log_string2("[input node weight filename]", finwname)
    call monolis_input_distval_i(finwname, label, n_node, n_nw_dof, node_wgt)
  endif

  fiewname = "edge_weight.dat"
  call monolis_get_arg_input_S("-iew", fiewname, is_iew)

  if(is_iew)then
    call monolis_std_log_string2("[input edge weight filename]", fiewname)
    call monolis_input_distval_i(fiewname, label, n_edge, n_ew_dof, edge_wgt)
  endif

  fgsname = ""
  call monolis_get_arg_input_S("--given_subdomain", fgsname, is_given_subdomain)

  if(is_given_subdomain)then
    call monolis_std_log_string2("[input given subdomain filename]", fgsname)
    call gedatsu_get_given_subdomain(fgsname, n_node, is_1_origin, given_domain_id)
  endif

  call node_partition()

  call elem_partition()

  !call visual_parted_mesh(n_node, node, n_elem, n_base, elem, node_graph%vertex_domain_id)

  call monolis_mpi_finalize()

contains

  subroutine elem_partition()
    implicit none
    type(gedatsu_graph) :: local_conn_graph
    integer(kint) :: i, j, k, in, id, idx
    character(monolis_charlen) :: foname_full
    logical :: is_valid
    integer(kint), allocatable :: id1(:)
    integer(kint), allocatable :: domain_id(:)
    integer(kint), allocatable :: perm(:)
    integer(kint), allocatable :: local_elem(:,:)
    logical, allocatable :: is_used(:)

    call gedatsu_check_connectivity_graph(node_graph, conn_graph, is_valid)

    if(.not. is_valid)then
      call monolis_std_error_string("input connectivity graph is not valid")
      stop monolis_fail
    endif

    call monolis_alloc_I_1d(domain_id, node_graph%n_vertex)
    call monolis_alloc_L_1d(is_used, node_graph%n_vertex)

    !> get domain id array
    do i = 1, n_domain
      do j = 1, subgraphs(i)%n_internal_vertex
        in = subgraphs(i)%vertex_id(j)
        if(.not. is_1_origin) in = in + 1
        if(is_used(in)) stop "flag: already used"
        domain_id(in) = i - 1
        is_used(in) = .true.
      enddo
    enddo

    do i = 1, n_domain
      if(.not. is_1_origin) subgraphs(i)%vertex_id = subgraphs(i)%vertex_id + 1

      call monolis_alloc_I_1d(conn_graph%vertex_id, conn_graph%n_vertex)
      call monolis_get_sequence_array_I(conn_graph%vertex_id, conn_graph%n_vertex, 1, 1)

      call gedatsu_get_parted_connectivity_main(i - 1, domain_id, &
        & node_graph, conn_graph, local_conn_graph)

      !> graph.dat
      call monolis_alloc_I_1d(perm, subgraphs(i)%n_vertex)
      call monolis_get_sequence_array_I(perm, subgraphs(i)%n_vertex, 1, 1)
      call monolis_qsort_I_2d(subgraphs(i)%vertex_id, perm, 1, subgraphs(i)%n_vertex)

      do j = 1, local_conn_graph%index(local_conn_graph%n_vertex + 1)
        id = local_conn_graph%item(j)
        call monolis_bsearch_I(subgraphs(i)%vertex_id, 1, subgraphs(i)%n_vertex, id, idx)
        local_conn_graph%item(j) = perm(idx)
      enddo

      call monolis_dealloc_I_1d(perm)
      call monolis_dealloc_I_1d(conn_graph%vertex_id)

      if(.not. is_1_origin) local_conn_graph%item = local_conn_graph%item - 1

      call monolis_alloc_I_2d(local_elem, n_base, local_conn_graph%n_vertex)

      do j = 1, local_conn_graph%n_vertex
        do k = 1, n_base
          local_elem(k,j) = local_conn_graph%item(n_base*(j - 1) + k)
        enddo
      enddo

      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(fiename), i - 1)
      call monolis_output_elem(foname_full, local_conn_graph%n_vertex, n_base, local_elem)

      call monolis_dealloc_I_1d(id1)
      call monolis_dealloc_I_2d(local_elem)

      !> global vertex_id
      if(.not. is_1_origin) local_conn_graph%vertex_id = local_conn_graph%vertex_id - 1

      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(fiename)//".id", i - 1)
      call monolis_output_global_id(foname_full, local_conn_graph%n_vertex, local_conn_graph%vertex_id)

      !> internal n_vertex
      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(fiename)//".n_internal", i - 1)
      call monolis_output_internal_vertex_number(foname_full, local_conn_graph%n_internal_vertex)

      call gedatsu_graph_finalize(local_conn_graph)
    enddo
  end subroutine elem_partition

  subroutine node_partition()
    implicit none
    type(gedatsu_graph) :: metagraph
    integer(kint) :: i, j, in
    character(monolis_charlen) :: foname_full
    type(monolis_COM), allocatable :: com(:)
    integer(kint), allocatable :: id1(:)
    real(kdouble), allocatable :: local_node(:,:)

    call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_graph%index, conn_graph%item)

    conn_graph%n_vertex = n_elem

    call gedatsu_convert_connectivity_graph_to_nodal_graph &
      & (n_node, n_elem, conn_graph%index, conn_graph%item, node_graph%index, node_graph%item)

    node_graph%n_vertex = n_node

    call monolis_alloc_I_1d(node_graph%vertex_id, n_node)

    call monolis_get_sequence_array_I(node_graph%vertex_id, n_node, 1, 1)

    allocate(subgraphs(n_domain))

    if(is_given_subdomain)then
      call gedatsu_graph_partition_given_subdomain(node_graph, n_domain, given_domain_id, subgraphs)
    else
      call gedatsu_graph_partition_METIS_with_weight(node_graph, n_domain, node_wgt, edge_wgt, subgraphs)
    endif

    allocate(com(n_domain))

    call gedatsu_com_get_comm_table_serial(node_graph, n_domain, subgraphs, com)

    foname_full = trim(dirname)//"/metagraph.dat"
    call gedatsu_get_metagraph(com, n_domain, metagraph)
    if(.not. is_1_origin) metagraph%vertex_id = metagraph%vertex_id - 1
    if(.not. is_1_origin) metagraph%item = metagraph%item - 1
    call monolis_output_graph(foname_full, metagraph%n_vertex, metagraph%vertex_id, metagraph%index, metagraph%item)

    do i = 1, n_domain
      !> node.dat
      call monolis_alloc_R_2d(local_node, 3, subgraphs(i)%n_vertex)

      do j = 1, subgraphs(i)%n_vertex
        in = subgraphs(i)%vertex_id(j)
        local_node(:,j) = node(:,in)
      enddo

      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname), i - 1)
      call monolis_output_node(foname_full, subgraphs(i)%n_vertex, local_node)

      !> internal n_vertex
      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".n_internal", i - 1)
      call monolis_output_internal_vertex_number(foname_full, subgraphs(i)%n_internal_vertex)

      !> global vertex_id
      if(.not. is_1_origin) subgraphs(i)%vertex_id = subgraphs(i)%vertex_id - 1
      if(.not. is_1_origin) com(i)%send_item = com(i)%send_item - 1
      if(.not. is_1_origin) com(i)%recv_item = com(i)%recv_item - 1

      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".id", i - 1)
      call monolis_output_global_id(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id)

      !> send
      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".send", i - 1)
      call monolis_output_send_com_table(foname_full, com(i))

      !> recv
      foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finname)//".recv", i - 1)
      call monolis_output_recv_com_table(foname_full, com(i))

      call monolis_dealloc_I_1d(id1)
      call monolis_dealloc_R_2d(local_node)
    enddo
  end subroutine node_partition

  subroutine visual_parted_mesh(nnode, node, nelem, nbase, elem, nodeid)
    implicit none
    integer(kint) :: i, j
    integer(kint) :: nnode, nelem, nbase, elem(:,:), nodeid(:), elemid(nelem), domid(nbase)
    real(kdouble) :: node(:,:)
    character :: etype*6

    elemid = 0

    do i = 1, nelem
      do j = 1, nbase
        domid(j) = nodeid(elem(j,i))
      enddo
      if(minval(domid) == maxval(domid))then
        elemid(i) = maxval(domid)
      else
        elemid(i) = -1
      endif
    enddo

    open(20, file = trim(dirname)//"mesh.part.inp", status = "replace")
      write(20,"(5i12)") nnode, nelem, 1, 1, 0
      do i = 1, nnode
        write(20,"(i0,1p3e13.5)") i, node(1,i), node(2,i), node(3,i)
      enddo

      if(nbase == 3) etype = " tri  "
      if(nbase == 4) etype = " tet  "
      if(nbase == 8) etype = " hex  "
      if(nbase ==10) etype = " tet2 "

      do i = 1, nelem
        write(20,"(i0,i4,a,$)") i, 0, etype
        do j = 1, nbase
          write(20,"(i12,$)") elem(j,i)
        enddo
        write(20,*)""
      enddo

      write(20,"(a)")"1 1"
      write(20,"(a)")"node_domid, unknown"
      do i = 1, nnode
        write(20,"(i0,x,i0,x,i0,x,i0)") i, nodeid(i)
      enddo

      write(20,"(a)")"1 1"
      write(20,"(a)")"elem_domid, unknown"
      do i = 1, nelem
        write(20,"(i0,x,i0,x,i0,x,i0)") i, elemid(i)
      enddo
    close(20)
  end subroutine visual_parted_mesh

end program gedatsu_partitioner_simple_mesh
