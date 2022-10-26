!> MPI util モジュール
module mod_gedatsu_mpi_util
  use mod_gedatsu_prm
  implicit none
#ifndef WITH_NOMPI
  include 'mpif.h'
#endif

#ifndef WITH_NOMPI
  integer(gint), parameter :: gedatsu_mpi_status_size = MPI_STATUS_SIZE
#else
  integer(gint), parameter :: gedatsu_mpi_status_size = 1
#endif

contains

  !> gedatsu ライブラリで利用する MPI の初期化関数
  subroutine gedatsu_mpi_initialize()
    implicit none
    integer(gint) :: ierr
#ifndef WITH_NOMPI
    call MPI_init(ierr)
#endif
  end subroutine gedatsu_mpi_initialize

  !> gedatsu ライブラリを利用する MPI の修了処理関数
  subroutine gedatsu_mpi_finalize()
    implicit none
    integer(gint) :: ierr
#ifndef WITH_NOMPI
    call MPI_finalize(ierr)
#endif
  end subroutine gedatsu_mpi_finalize

end module mod_gedatsu_mpi_util
