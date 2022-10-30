!> parameter モジュール
module mod_gedatsu_prm
  implicit none

  !> integer 変数の精度
  integer(4), parameter :: gint    = 4
  !> real 変数の精度
  integer(4), parameter :: gdouble = 8

  !> gedatsu ライブラリにおける正常処理フラグ
  integer(gint), parameter :: gedatsu_success = 0
  !> gedatsu ライブラリにおける異常処理フラグ
  integer(gint), parameter :: gedatsu_fail = 1
  !> gedatsu ライブラリにおける文字列長さ
  integer(gint), parameter :: gedatsu_charlen = 1024

  !> parameter 構造体
  type gedatsu_prm
    !> TBA
    integer(gint) :: method = 1
  end type gedatsu_prm

contains

  !> parameter 構造体の初期化関数
  subroutine gedatsu_prm_initialize(param)
    implicit none
    !> [in] parameter 構造体
    type(gedatsu_prm) :: param
    param%method = 1
  end subroutine gedatsu_prm_initialize

  !> parameter 構造体の終了関数
  subroutine gedatsu_prm_finalize(param)
    implicit none
    !> [in] parameter 構造体
    type(gedatsu_prm) :: param
    param%method = 1
  end subroutine gedatsu_prm_finalize

end module mod_gedatsu_prm