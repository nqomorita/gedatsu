module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none

contains

  !> gedatsu ライブラリを利用するためのグローバル初期化関数
  subroutine gedatsu_global_initialize()
    implicit none
    !call gedatsu_mpi_initialize()
    !myrank = gedatsu_global_myrank()
    !mycomm = gedatsu_global_comm()
  end subroutine gedatsu_global_initialize

  !> gedatsu ライブラリを利用するためのグローバル終了関数
  subroutine gedatsu_global_finalize()
    implicit none
    !call gedatsu_mpi_finalize()
  end subroutine gedatsu_global_finalize

  !> gedatsu 構造体の初期化関数
  subroutine gedatsu_initialize(gedatsu)
    implicit none
    !> graph 構造体
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_initialize

  !> gedatsu 構造体の終了関数
  subroutine gedatsu_finalize(gedatsu)
    implicit none
    !> graph 構造体
    type(gedatsu_graph) :: gedatsu

  end subroutine gedatsu_finalize

  !> エラーストップ関数
  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop
end module mod_gedatsu_util
