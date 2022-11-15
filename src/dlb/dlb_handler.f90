!> 動的負荷分散モジュール
module mod_gedatsu_dlb_handler
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_util
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための事前分析
  subroutine gedatsu_dlb_analysis(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_dlb_analysis

  !> @ingroup group_dlb
  !> 負荷分散：グラフ情報のアップデート
  subroutine gedatsu_dlb_update_graph(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_dlb_update_graph

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_int_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    integer(gint), allocatable :: var(:)

  end subroutine gedatsu_dlb_update_int_1d

  !> @ingroup group_dlb
  !> 負荷分散：2 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_int_2d(dlb, ndof, mdof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度（行列 [i,j] の i 成分）
    integer(gint) :: ndof
    !> [in] 1 節点あたりの自由度（行列 [i,j] の j 成分）
    integer(gint) :: mdof
    !> [in,out] アップデートする配列
    integer(gint), allocatable :: var(:,:)

  end subroutine gedatsu_dlb_update_int_2d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_real_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    real(gdouble), allocatable :: var(:)

  end subroutine gedatsu_dlb_update_real_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_real_2d(dlb, ndof, mdof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度（行列 [i,j] の i 成分）
    integer(gint) :: ndof
    !> [in] 1 節点あたりの自由度（行列 [i,j] の j 成分）
    integer(gint) :: mdof
    !> [in,out] アップデートする配列
    real(gdouble), allocatable :: var(:,:)

  end subroutine gedatsu_dlb_update_real_2d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_bool_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    logical, allocatable :: var(:)

  end subroutine gedatsu_dlb_update_bool_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート
  subroutine gedatsu_dlb_update_bool_2d(dlb, ndof, mdof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度（行列 [i,j] の i 成分）
    integer(gint) :: ndof
    !> [in] 1 節点あたりの自由度（行列 [i,j] の j 成分）
    integer(gint) :: mdof
    !> [in,out] アップデートする配列
    logical, allocatable :: var(:,:)

  end subroutine gedatsu_dlb_update_bool_2d
end module mod_gedatsu_dlb_handler
