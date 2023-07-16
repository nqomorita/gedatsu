!> dlb モジュール
module mod_gedatsu_dlb
  use mod_monolis_utils
  implicit none

  !> dlb 構造体
  type gedatsu_dlb
    !> DLB 用通信テーブル構造体
    type(monolis_COM) :: COM_node
    !> DLB 用通信テーブル構造体
    type(monolis_COM) :: COM_edge
    !> 更新後の計算点のグローバル id
    integer(kint), allocatable :: global_id(:)
  end type gedatsu_dlb

  !> dlb データベース構造体
  type gedatsu_update_db
    !> 送信計算点数
    integer(kint) :: n_send_node = 0
    !> 送信エッジ数
    integer(kint) :: n_send_edge = 0
    !> 送信する計算点フラグ
    integer(kint), allocatable :: is_send_node(:)
    !> 送信するエッジフラグ
    integer(kint), allocatable :: is_send_edge(:)
  end type gedatsu_update_db

contains

  !> @ingroup graph_init
  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_initialize(dlb)
    implicit none
    !> [out] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
  end subroutine gedatsu_dlb_initialize

  !> @ingroup graph_init
  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_finalize(dlb)
    implicit none
    !> [out] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
  end subroutine gedatsu_dlb_finalize
end module mod_gedatsu_dlb
