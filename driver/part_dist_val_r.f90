program gedatsu_nodal_val_r_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph), allocatable :: graph(:)
  integer(kint) :: i, j, in, n_domain, n_node, n_dof, shift
  character(monolis_charlen) :: finame, dirname, label, fidname, foname_full
  logical :: is_get, is_1_origin
  real(kdouble), allocatable :: val(:,:), val_local(:,:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_nodal_val_r_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_nodal_val_r_partitioner {options} -n {number of domains}"
    write(*,"(a)")"- output filename: {input filename}.{domain_id}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input filename}: (default) node_val.dat"
    write(*,"(a)")"-id {input id filename}: (default) graph.dat.id"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)") &
    & "./gedatsu_nodal_val_r_partitioner {options} -n {number of domains}"
    stop monolis_fail
  endif

  finame = "node_val.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  fidname = "graph.dat.id"
  call monolis_get_arg_input_S("-id", fidname, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_d_tag(dirname, is_get)

  call monolis_input_distval_r(finame, label, n_node, n_dof, val)

  allocate(graph(n_domain))

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(fidname), i - 1)
    call monolis_input_global_id(foname_full, graph(i)%n_vertex, graph(i)%vertex_id)
  enddo

  shift = 0
  is_1_origin = .true.
  do i = 1, n_domain
    if(minval(graph(i)%vertex_id) == 0) is_1_origin = .false.
  enddo
  if(.not. is_1_origin) shift = 1

  do i = 1, n_domain
    call monolis_alloc_R_2d(val_local, n_dof, graph(i)%n_vertex)

    do j = 1, graph(i)%n_vertex
      in = graph(i)%vertex_id(j) + shift
      val_local(:,j) = val(:,in)
    enddo

    foname_full = monolis_get_output_file_name_by_domain_id(dirname, trim(finame), i - 1)
    call monolis_output_distval_r(foname_full, label, graph(i)%n_vertex, n_dof, val_local)

    call monolis_dealloc_I_1d(graph(i)%vertex_id)
    call monolis_dealloc_R_2d(val_local)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_nodal_val_r_partitioner