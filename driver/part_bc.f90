program gedatsu_partitioner_bc
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  !integer(kint) :: n_domain, ncond, ndof
  !type(monolis_mesh), allocatable :: mesh(:)
  !integer(kint), allocatable :: icond(:,:)
  !real(kdouble), allocatable :: cond(:)
  !character :: fname*100

  !call monolis_global_initialize()

  !call monolis_get_part_bc_arg(n_domain, fname)

  !if(n_domain <= 1) stop

  !call monolis_input_condition(fname, ncond, ndof, icond, cond)

  !allocate(mesh(n_domain))

  !call monolis_par_input_node_id(n_domain, mesh)

  !call monolis_par_output_condition(n_domain, mesh, fname, ncond, ndof, icond, cond)

  !call monolis_global_finalize()
end program gedatsu_partitioner_bc
