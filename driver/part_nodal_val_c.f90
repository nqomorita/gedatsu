program gedatsu_nodal_val_c_partitioner
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: i, j, in, n_domain, n_node, n_dof
  character(monolis_charlen) :: finame, dirname, label, fidname, foname_full
  logical :: is_get
  complex(kdouble), allocatable :: val(:,:), val_local(:,:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_nodal_val_c_partitioner {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input filename}: (default) node_val.dat"
    write(*,"(a)")"-o {output filename}: (default) node_val.dat.{domain_id}"
    write(*,"(a)")"-d {output directory name}: (default) ./parted.0"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain, is_get)

  if(.not. is_get)then
    call monolis_std_error_string("input parameter 'n' are not set")
    write(*,"(a)") &
    & "./gedatsu_nodal_val_c_partitioner {options} -n {number of domains}"
    stop monolis_fail
  endif

  fidname = "graph.id"
  call monolis_get_arg_input_S("-id", fidname, is_get)

  finame = "node_val.dat"
  call monolis_input_distval_c(finame, label, n_node, n_dof, val)

  dirname = "./parted.0"
  call monolis_get_arg_input_S("-d", dirname, is_get)

  do i = 1, n_domain
    foname_full = monolis_get_output_file_name(dirname, trim(fidname), i)
    call monolis_input_global_id(foname_full, graph%n_vertex, graph%vertex_id)

    call monolis_alloc_C_2d(val_local, n_dof, graph%n_vertex)

    do j = 1, graph%n_vertex
      in = graph%vertex_id(j)
      val_local(:,j) = val(:,in)
    enddo

    foname_full = monolis_get_output_file_name(dirname, trim(finame), i)
    call monolis_output_distval_c(foname_full, label, graph%n_vertex, n_dof, val_local)

    call monolis_dealloc_I_1d(graph%vertex_id)
    call monolis_dealloc_C_2d(val_local)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_nodal_val_c_partitioner
