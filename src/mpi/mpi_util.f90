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

  !> MPI のグローバルコミュニケータを取得する関数
  function gedatsu_mpi_global_comm()
    implicit none
    integer(gint) :: gedatsu_mpi_global_comm

#ifdef WITH_MPI
    gedatsu_mpi_global_comm = MPI_COMM_WORLD
#else
    gedatsu_mpi_global_comm = 0
#endif
  end function gedatsu_mpi_global_comm

  !> MPI のグローバルランクサイズを取得する関数
  function gedatsu_mpi_global_comm_size()
    implicit none
    integer(gint) :: gedatsu_mpi_global_comm_size, ierr

#ifndef WITH_NOMPI
    call MPI_comm_size(MPI_COMM_WORLD, gedatsu_mpi_global_comm_size, ierr)
#else
    gedatsu_mpi_global_comm_size = 1
#endif
  end function gedatsu_mpi_global_comm_size

  !> MPI のグローバルランクを取得する関数
  function gedatsu_mpi_global_my_rank()
    implicit none
    integer(gint) :: gedatsu_mpi_global_my_rank, ierr

#ifndef WITH_NOMPI
    call MPI_comm_rank(MPI_COMM_WORLD, gedatsu_mpi_global_my_rank, ierr)
#else
    gedatsu_mpi_global_my_rank = 0
#endif
  end function gedatsu_mpi_global_my_rank

  !> MPI バリア関数
  subroutine gedatsu_mpi_barrier_(comm)
    implicit none
    integer(gint) :: comm
    integer(gint) :: ierr
#ifndef WITH_NOMPI
    call MPI_barrier(comm, ierr)
#endif
  end subroutine gedatsu_mpi_barrier_
end module mod_gedatsu_mpi_util
