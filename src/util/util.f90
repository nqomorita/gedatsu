!> util モジュール
module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none

contains

  !> @ingroup group_init
  !> gedatsu ライブラリを利用するためのグローバル初期化関数
  !> @details 全ての gedatsu ライブラリの関数を呼ぶ前に実行しておく必要がある。
  !> MPI の初期化処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_initialize()
    implicit none
  end subroutine gedatsu_global_initialize

  !> @ingroup group_init
  !> gedatsu ライブラリを利用するためのグローバル終了関数
  !> @details MPI の終了処理が実行され、プログラム中で 1 度だけ呼ぶことができる。
  subroutine gedatsu_global_finalize()
    implicit none
  end subroutine gedatsu_global_finalize

  !> エラーストップ関数
  subroutine gedatsu_error_stop()
    implicit none
    error stop gedatsu_fail
  end subroutine gedatsu_error_stop

  !> エラー出力関数
  subroutine gedatsu_error_string(fname)
    implicit none
    !> [in] 入力ファイル名
    character(*) :: fname
    write(*,"(a,a)")"** GEDATSU ERROR: ", trim(fname)
  end subroutine gedatsu_error_string
end module mod_gedatsu_util
