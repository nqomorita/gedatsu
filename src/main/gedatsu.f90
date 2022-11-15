!> gedatsu モジュールファイル
!> @details 全ての gedatsu モジュールをまとめたモジュールファイル
module mod_gedatsu
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_comm
  use mod_gedatsu_util
  use mod_gedatsu_io
  use mod_gedatsu_io_arg
  use mod_gedatsu_io_comm
  use mod_gedatsu_io_file_name
  use mod_gedatsu_alloc
  use mod_gedatsu_mpi
  use mod_gedatsu_mpi_util
  use mod_gedatsu_std
  use mod_gedatsu_wrapper_metis
  use mod_gedatsu_wrapper_parmetis
  use mod_gedatsu_graph_part
  use mod_gedatsu_graph_repart
  use mod_gedatsu_graph_handler
  use mod_gedatsu_graph_convert
  use mod_gedatsu_communicator
  use mod_gedatsu_communicator_serial_util
  use mod_gedatsu_communicator_parallel_util
  use mod_gedatsu_wrapper_metis
  use mod_gedatsu_wrapper_parmetis
  use mod_gedatsu_dlb

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

  !> @defgroup group_dlb 動的負荷分散
  !> 動的負荷分散に関連する関数グループ

  !> @defgroup mpi MPI 関数群
  !> MPI に関連する関数グループ

  !> @defgroup std 標準関数群
  !> 標準的なデータ処理に関連する関数グループ

  !> @defgroup group_dev_alloc 開発者用：メモリ確保
  !> メモリ確保・メモリ開放に関連する関数グループ
end module mod_gedatsu
