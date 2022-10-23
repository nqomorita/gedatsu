!> std モジュール
module mod_gedatsu_std
  use mod_gedatsu_prm
  use mod_gedatsu_util
  implicit none

contains

  !> @ingroup std
  !> クイックソート（整数配列）
  recursive subroutine gedatsu_qsort_int(array, iS, iE)
    implicit none
    !> [inout] 整数配列
    integer(gint), intent(inout) :: array(:)
    !> [in] ソートする開始位置
    integer(gint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(gint), intent(in) :: iE

    integer(gint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array(center)
    left = iS
    right = iE

    do
      do while (array(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array(right))
        right = right - 1
      enddo

      if (left >= right) exit

      tmp = array(left)
      array(left) = array(right)
      array(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call gedatsu_qsort_int(array, iS, left - 1)
    if(right + 1 < iE) call gedatsu_qsort_int(array, right + 1, iE)
  end subroutine gedatsu_qsort_int

  !> @ingroup std
  !> 置換ベクトルを戻すクイックソート（整数配列）
  recursive subroutine gedatsu_qsort_int_with_perm(array, iS, iE, perm)
    implicit none
    !> [inout] 整数配列
    integer(gint), intent(inout) :: array(:)
    !> [inout] 置換ベクトル
    integer(gint), intent(inout) :: perm(:)
    !> [in] ソートする開始位置
    integer(gint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(gint), intent(in) :: iE

    integer(gint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array(center)
    left = iS
    right = iE

    do
      do while (array(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array(right))
        right = right - 1
      enddo

      if (left >= right) exit

      tmp = array(left)
      array(left) = array(right)
      array(right) = tmp

      tmp = perm(left)
      perm(left) = perm(right)
      perm(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call gedatsu_qsort_int_with_perm(array, iS, left - 1, perm)
    if(right + 1 < iE) call gedatsu_qsort_int_with_perm(array, right + 1, iE, perm)
  end subroutine gedatsu_qsort_int_with_perm

  !> @ingroup std
  !> 整数配列の二分探索
  subroutine gedatsu_bsearch_int(array, iS, iE, val, idx)
    implicit none
    !> [in] 整数配列
    integer(gint), intent(in) :: array(:)
    !> [in] ソートする開始位置
    integer(gint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(gint), intent(in) :: iE
    !> [in] 検索する整数値
    integer(gint), intent(in) :: val
    !> [out] 検索した結果の位置（検索結果がない場合 -1 を返す）
    integer(gint), intent(out) :: idx

    integer(gint) :: center, left, right, pivot

    left = iS
    right = iE

    do
      if (left > right) then
        idx = -1
        exit
      endif
      center = (left + right) / 2
      pivot = array(center)
      if (val < pivot) then
        right = center - 1
        cycle
      elseif (pivot < val) then
        left = center + 1
        cycle
      else
        idx = center
        exit
      endif
    enddo
  end subroutine gedatsu_bsearch_int

  !> @ingroup std
  !> 整数配列の二分探索
  subroutine gedatsu_get_sequence_array_int(array, n, origin, difference)
    implicit none
    !> [out] 整数配列
    integer(gint), intent(out) :: array(:)
    !> [in] 整数配列のサイズ
    integer(gint), intent(in) :: n
    !> [in] 初項
    integer(gint), intent(in) :: origin
    !> [in] 交差
    integer(gint), intent(in) :: difference

    integer(gint) :: i

    do i = 1, n
      array(i) = origin + (i-1)*difference
    enddo
  end subroutine gedatsu_get_sequence_array_int

end module mod_gedatsu_std
