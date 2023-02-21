program gedatsu_partitioner_nodal_val_i
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_domain, n_node, n_dof
  integer(kint) :: i
  !character(monolis_charlen) :: finame, dirname, foname, foname_full
  character(monolis_charlen) :: finame, label
  logical :: is_get
  type(gedatsu_graph), allocatable :: subgraphs(:)
  integer(kint), allocatable :: val(:,:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_partitioner_nodal_val_i {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input filename}: (defualt) node_val.dat"
    write(*,"(a)")"-o {output filename}: (defualt) node_val.dat.{domain_id}"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  call monolis_get_arg_input_n_tag(n_domain)

  finame = "node_val.dat"
  call monolis_input_distval_i(finame, label, n_node, n_dof, val)

!  call monolis_input_distval_i(fname, label, n_node, n_dof, val)

!  call monolis_output_distval_i(fname, label, n_node, n_dof, val)

  call monolis_mpi_finalize()
end program gedatsu_partitioner_nodal_val_i
