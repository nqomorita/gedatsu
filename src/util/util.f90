!> util モジュール
module mod_gedatsu_util
  use mod_gedatsu_graph
  implicit none

  !> @defgroup group_init 初期化・終了処理
  !> 初期化・終了処理に関連する関数グループ

  !> @defgroup group_io ファイル入出力
  !> ファイル入出力に関連する関数グループ

  !> @defgroup group_graph_1 グラフデータ処理（基本関数）
  !> 基本的なグラフデータ処理に関連する関数グループ

  !> @defgroup group_graph_2 グラフデータ処理（グラフ重み関数）
  !> グラフ重み処理に関連する関数グループ

  !> @defgroup group_graph_3 グラフデータ処理（グラフデータ変換関数）
  !> グラフデータ変換に関連する関数グループ

  !> @defgroup group_graph_4 グラフデータ処理（グラフ分割関数）
  !> グラフ分割に関連する関数グループ

  !> @defgroup group4 動的負荷分散
  !> 動的負荷分散に関連する関数グループ

  !> @defgroup group_dev_alloc 開発者用：メモリ確保
  !> メモリ確保・メモリ開放に関連する関数グループ

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
    write(*,"(a)")"** GEDATSU ERROR: ", trim(fname)
  end subroutine gedatsu_error_string
end module mod_gedatsu_util
