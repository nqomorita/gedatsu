!> dlb モジュール
module mod_gedatsu_dlb
  use mod_gedatsu_prm
  use mod_gedatsu_comm
  use mod_gedatsu_alloc
  implicit none

  !> dlb 構造体
  type gedatsu_dlb
    !> comm 構造体
    type(gedatsu_comm) :: comm
    !> 負荷分散の実行判定
    logical :: should_update
  end type gedatsu_dlb

contains

  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_initialize(dlb)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb

  end subroutine gedatsu_dlb_initialize

  !> dlb 構造体の初期化関数
  subroutine gedatsu_dlb_finalize(dlb)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb

  end subroutine gedatsu_dlb_finalize
end module mod_gedatsu_dlb
