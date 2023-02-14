program gedatsu_test
  use mod_monolis_utils
  use mod_gedatsu_dlb_test
  implicit none

  call monolis_mpi_initialize()

  call gedatsu_dlb_test()

  call monolis_mpi_finalize()
end program gedatsu_test
