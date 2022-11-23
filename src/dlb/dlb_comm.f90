!> 動的負荷分散モジュール
module mod_gedatsu_dlb_comm
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_util
  use mod_gedatsu_graph_repart
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための通信テーブル作成
  subroutine gedatsu_dlb_get_comm_table(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_dlb_get_comm_table
end module mod_gedatsu_dlb_comm
