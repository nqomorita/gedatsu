!> util モジュール
module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none

  !> @defgroup group1 初期化・終了処理
  !> 初期化・終了処理に関連する関数グループ

  !> @defgroup group2 グラフデータ処理
  !> グラフデータ処理に関連する関数グループ

  !> @defgroup group3 動的負荷分散処理
  !> 動的負荷分散処理に関連する関数グループ

contains

  !> @ingroup group1
  !> gedatsu ライブラリを利用するためのグローバル初期化関数
  !> @details 全ての gedatsu ライブラリの関数を呼ぶ前に実行しておく必要がある。
  !> MPI の初期化処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_initialize()
    implicit none
    !call gedatsu_mpi_initialize()
    !myrank = gedatsu_global_myrank()
    !mycomm = gedatsu_global_comm()
  end subroutine gedatsu_global_initialize

  !> @ingroup group1
  !> gedatsu ライブラリを利用するためのグローバル終了関数
  !> @details MPI の終了処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_finalize()
    implicit none
    !call gedatsu_mpi_finalize()
  end subroutine gedatsu_global_finalize

  !> エラーストップ関数
  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop
end module mod_gedatsu_util
