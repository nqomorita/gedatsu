program gedatsu_test
  use mod_monolis_utils
  use mod_gedatsu_def_dlb_test
  use mod_gedatsu_def_graph_test
  use mod_gedatsu_graph_handler_test
  use mod_gedatsu_graph_convert_test
  use mod_gedatsu_graph_part_test
  use mod_gedatsu_graph_repart_test
  implicit none

  call monolis_mpi_initialize()

  call gedatsu_def_dlb_test()
  call gedatsu_def_graph_test()

  call gedatsu_graph_convert_test()
  call gedatsu_graph_handler_test()
  call gedatsu_graph_part_test()

  call gedatsu_graph_repart_test()

  call monolis_mpi_finalize()
end program gedatsu_test