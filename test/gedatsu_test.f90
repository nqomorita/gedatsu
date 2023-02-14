program gedatsu_test
  use mod_monolis_utils
  use mod_gedatsu_def_dlb_test
  use mod_gedatsu_def_graph_test
  implicit none

  call monolis_mpi_initialize()

  call gedatsu_def_dlb_test()
  call gedatsu_def_graph_test()

  call monolis_mpi_finalize()
end program gedatsu_test
