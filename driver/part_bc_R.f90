program gedatsu_bc_partitioner_R
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph), allocatable :: graph(:)
  integer(kint) :: i, j, in, n_domain, n_bc, n_dof, shift, idx
  integer(kint) :: n_bc_local
  character(monolis_charlen) :: finame, dirname, fidname, foname_full
  logical :: is_get, is_1_origin
  integer(kint), allocatable :: perm(:)
  integer(kint), allocatable :: i_bc(:,:), i_bc_local(:,:)
  real(kdouble), allocatable :: r_bc(:), r_bc_local(:)

  call monolis_mpi_initialize()

  call gedatsu_std_log_string("gedatsu_bc_partitioner")

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_bc_partitioner {options} -n {number of domains}"
    write(*,"(a)")"- output filename: {input filename}.{domain_id}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input filename}: (default) bc.dat"
    write(*,"(a)")"-ig {input graph filename}: (default) graph.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_bc_partitioner {options} -n {number of domains}"
    write(*,"(a)")"- output filename: {input filename}.{domain_id}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input filename}: (default) bc.dat"
    write(*,"(a)")"-ig {input graph filename}: (default) graph.dat"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_fail
  endif

  if(n_domain <= 1) stop

  fidname = "graph.dat"
  call monolis_get_arg_input_S("-ig", fidname, is_get)

  finame = "bc.dat"
  call monolis_get_arg_input_i_tag(finame, is_get)

  dirname = "./parted.0"
  call monolis_get_arg_input_S("-d", dirname, is_get)

  call monolis_input_bc_R(finame, n_bc, n_dof, i_bc, r_bc)

  allocate(graph(n_domain))

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(fidname)//".id", i - 1)
    call monolis_input_global_id(foname_full, graph(i)%n_vertex, graph(i)%vertex_id)
  enddo

  shift = 0
  is_1_origin = .true.
  do i = 1, n_domain
    if(minval(graph(i)%vertex_id) == 0) is_1_origin = .false.
  enddo
  if(.not. is_1_origin) shift = 1

  do i = 1, n_domain
    graph(i)%vertex_id = graph(i)%vertex_id + shift

    call monolis_alloc_I_1d(perm, graph(i)%n_vertex)
    call monolis_get_sequence_array_I(perm, graph(i)%n_vertex, 1, 1)
    call monolis_qsort_I_2d(graph(i)%vertex_id, perm, 1, graph(i)%n_vertex)

    n_bc_local = 0
    do j = 1, n_bc
      call monolis_bsearch_I(graph(i)%vertex_id, 1, graph(i)%n_vertex, i_bc(1,j) + shift, idx)
      if(idx > 0) n_bc_local = n_bc_local + 1
    enddo

    call monolis_alloc_I_2d(i_bc_local, n_dof + 1, n_bc_local)
    call monolis_alloc_R_1d(r_bc_local, n_bc_local)

    n_bc_local = 0
    do j = 1, n_bc
      call monolis_bsearch_I(graph(i)%vertex_id, 1, graph(i)%n_vertex, i_bc(1,j) + shift, idx)
      if(idx > 0)then
        n_bc_local = n_bc_local + 1
        in = perm(idx)
        i_bc_local(1,n_bc_local) = in - shift
        i_bc_local(2,n_bc_local) = i_bc(2,j)
        r_bc_local(n_bc_local) = r_bc(j)
      endif
    enddo

    foname_full = monolis_get_output_file_name_by_domain_id(".", dirname, trim(finame), i - 1)
    call monolis_output_bc_R(foname_full, n_bc_local, n_dof, i_bc_local, r_bc_local)

    call monolis_dealloc_I_1d(graph(i)%vertex_id)
    call monolis_dealloc_I_2d(i_bc_local)
    call monolis_dealloc_R_1d(r_bc_local)
    call monolis_dealloc_I_1d(perm)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_bc_partitioner_R
