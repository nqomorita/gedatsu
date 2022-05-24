module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none
  private

  logical, save :: is_debug

contains

  subroutine gedatsu_global_initialize()
    implicit none
    call gedatsu_mpi_initialize()

    !myrank = gedatsu_global_myrank()
    !mycomm = gedatsu_global_comm()
  end subroutine gedatsu_global_initialize

  subroutine gedatsu_global_finalize()
    implicit none
    call gedatsu_mpi_finalize()
  end subroutine gedatsu_global_finalize

  subroutine gedatsu_initialize(gedatsu)
    implicit none
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_initialize

  subroutine gedatsu_finalize(gedatsu)
    implicit none
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_finalize

  !> set parameter section

  !> debug section
  subroutine gedatsu_set_debug_flag(flag)
    implicit none
    logical :: flag
    is_debug = flag
  end subroutine gedatsu_set_debug_flag

  subroutine gedatsu_debug_header(header)
    implicit none
    character(*) :: header

    if(.not. is_debug) return
    !if(myrank == 0) write(*,"(a)")"** gedatsu debug: "//trim(header)
  end subroutine gedatsu_debug_header

  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop
end module mod_gedatsu_util
