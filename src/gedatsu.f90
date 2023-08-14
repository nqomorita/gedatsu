!> gedatsu モジュールファイル
!> @details 全ての gedatsu モジュールをまとめたモジュールファイル
module mod_gedatsu
  use mod_gedatsu_graph
  !> graph section
  use mod_gedatsu_graph_part
  use mod_gedatsu_graph_repart
  use mod_gedatsu_graph_handler
  use mod_gedatsu_graph_convert
  !> wrapper section
  use mod_gedatsu_wrapper_metis
  use mod_gedatsu_wrapper_parmetis
  !> DBL section
  use mod_gedatsu_dlb
  use mod_gedatsu_dlb_handler
  use mod_gedatsu_dlb_comm_nodal
  use mod_gedatsu_dlb_comm_conn

  !> @defgroup graph_init 初期化・終了処理関数群
  !> 初期化・終了処理に関連する関数グループ

  !> @defgroup graph_basic グラフデータ処理関数群（基本関数）
  !> 基本的なグラフデータ処理に関連する関数グループ

  !> @defgroup graph_conv グラフデータ処理関数群（グラフデータ変換関数）
  !> グラフデータ変換に関連する関数グループ

  !> @defgroup graph_part グラフデータ処理関数群（グラフ分割関数）
  !> グラフ分割に関連する関数グループ

  !> @defgroup graph_dlb 動的負荷分散関数群
  !> 動的負荷分散に関連する関数グループ

  !> @defgroup dev_graph_warp 開発者用：グラフライブラリラッパー関数関数群
  !> グラフライブラリのラッパー関数に関連する関数グループ（開発者用）

  !> @defgroup dev_graph_part 開発者用：グラフデータ処理関数群（グラフ分割関数）
  !> グラフ分割に関連する関数グループ（開発者用）

  !> @defgroup dev_graph 開発者用関数
  !> 開発者用関数群
end module mod_gedatsu
