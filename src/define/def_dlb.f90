!> dlb モジュール
module mod_gedatsu_dlb
  use mod_monolis_utils
  implicit none

  !> dlb 構造体
  type gedatsu_dlb
    !> DLB 用通信テーブル構造体
    type(monolis_COM) :: COM
    !> 更新後の計算点のグローバル id
    integer(kint), allocatable :: global_id(:)
    !> 負荷分散の実行判定
    logical :: should_update
  end type gedatsu_dlb

contains

  !> @ingroup graph_init
  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_initialize(dlb)
    implicit none
    !> [out] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb

    dlb%should_update = .false.
  end subroutine gedatsu_dlb_initialize

  !> @ingroup graph_init
  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_finalize(dlb)
    implicit none
    !> [out] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb

    dlb%should_update = .false.
  end subroutine gedatsu_dlb_finalize

  !> @ingroup graph_dlb
  !> dlb 構造体の負荷分散フラグの取得関数
  subroutine gedatsu_dlb_should_update(dlb, should_update)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb), intent(in) :: dlb
    !> [out] 負荷分散の実行判定
    logical, intent(out) :: should_update
    should_update = dlb%should_update
  end subroutine gedatsu_dlb_should_update
end module mod_gedatsu_dlb
