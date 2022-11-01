!> util モジュール
module mod_gedatsu_util
  use mod_gedatsu_prm
  use mod_gedatsu_mpi_util

  implicit none

contains

  !> @ingroup group_init
  !> gedatsu ライブラリを利用するためのグローバル初期化関数
  !> @details 全ての gedatsu ライブラリの関数を呼ぶ前に実行しておく必要がある。
  !> MPI の初期化処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_initialize()
    implicit none

    call gedatsu_mpi_initialize()
  end subroutine gedatsu_global_initialize

  !> @ingroup group_init
  !> gedatsu ライブラリを利用するためのグローバル終了関数
  !> @details MPI の終了処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_finalize()
    implicit none

    call gedatsu_mpi_finalize()
  end subroutine gedatsu_global_finalize

  !> 通常ログ出力関数
  subroutine gedatsu_log_string(fname)
    implicit none
    !> [in] 入力ファイル名
    character(*) :: fname
    write(*,"(a,a)")"** GEDATSU: ", trim(fname)
  end subroutine gedatsu_log_string

  !> エラーストップ関数
  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop

  !> エラー出力関数（ERROR）
  subroutine gedatsu_error_string(fname)
    implicit none
    !> [in] 入力ファイル名
    character(*) :: fname
    write(*,"(a,a)")"** GEDATSU ERROR: ", trim(fname)
  end subroutine gedatsu_error_string

  !> 警告出力関数（WARNING）
  subroutine gedatsu_warning_string(fname)
    implicit none
    !> [in] 入力ファイル名
    character(*) :: fname
    write(*,"(a,a)")"** GEDATSU EARNING: ", trim(fname)
  end subroutine gedatsu_warning_string
end module mod_gedatsu_util
