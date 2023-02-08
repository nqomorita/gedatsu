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
  use mod_gedatsu_dlb_comm

  !> @defgroup group_init 初期化・終了処理
  !> 初期化・終了処理に関連する関数グループ

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
end module mod_gedatsu
