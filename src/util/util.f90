module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none

contains

  subroutine gedatsu_global_initialize()
    implicit none
    !call gedatsu_mpi_initialize()
    !myrank = gedatsu_global_myrank()
    !mycomm = gedatsu_global_comm()
  end subroutine gedatsu_global_initialize

  subroutine gedatsu_global_finalize()
    implicit none
    !call gedatsu_mpi_finalize()
  end subroutine gedatsu_global_finalize

  subroutine gedatsu_initialize(gedatsu)
    implicit none
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_initialize

  subroutine gedatsu_finalize(gedatsu)
    implicit none
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_finalize

  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop
end module mod_gedatsu_util
