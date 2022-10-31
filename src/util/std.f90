!> std モジュール
module mod_gedatsu_std
  use mod_gedatsu_prm
  use mod_gedatsu_util
  implicit none

contains

  !> @ingroup std
  !> クイックソート（1次元整数配列）
  recursive subroutine gedatsu_qsort_int_1d(array, iS, iE)
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

    if(iS < left - 1)  call gedatsu_qsort_int_1d(array, iS, left - 1)
    if(right + 1 < iE) call gedatsu_qsort_int_1d(array, right + 1, iE)
  end subroutine gedatsu_qsort_int_1d

  !> @ingroup std
  !> 置換ベクトルを戻すクイックソート（1次元整数配列）
  recursive subroutine gedatsu_qsort_int_1d_with_perm(array, iS, iE, perm)
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

    if(iS < left - 1)  call gedatsu_qsort_int_1d_with_perm(array, iS, left - 1, perm)
    if(right + 1 < iE) call gedatsu_qsort_int_1d_with_perm(array, right + 1, iE, perm)
  end subroutine gedatsu_qsort_int_1d_with_perm

  !> @ingroup std
  !> クイックソート（2次元整数配列）
  recursive subroutine gedatsu_qsort_int_2d(array1, array2, iS, iE)
    implicit none
    !> [inout] ソートされる整数配列
    integer(gint), intent(inout) :: array1(:)
    !> [inout] ソートに従属する整数配列
    integer(gint), intent(inout) :: array2(:)
    !> [in] ソートする開始位置
    integer(gint), intent(in) :: iS
    !> [in] ソートする終了位置
    integer(gint), intent(in) :: iE

    integer(gint) :: pivot, center, left, right, tmp

    if (iS >= iE) return

    center = (iS + iE) / 2
    pivot = array1(center)
    left = iS
    right = iE

    do
      do while (array1(left) < pivot)
        left = left + 1
      enddo
      do while (pivot < array1(right))
        right = right - 1
      enddo

      if (left >= right) exit

      tmp = array1(left)
      array1(left) = array1(right)
      array1(right) = tmp

      tmp = array2(left)
      array2(left) = array2(right)
      array2(right) = tmp

      left = left + 1
      right = right - 1
    enddo

    if(iS < left - 1)  call gedatsu_qsort_int_2d(array1, array2, iS, left - 1)
    if(right + 1 < iE) call gedatsu_qsort_int_2d(array1, array2, right + 1, iE)
  end subroutine gedatsu_qsort_int_2d

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

  !> @ingroup std
  !> 整数配列の重複要素の削除
  subroutine gedatsu_get_uniq_int(array, len, newlen)
    implicit none
    !> [in,out] 整数配列
    integer(gint), intent(inout) :: array(:)
    !> [in] 整数配列のサイズ
    integer(gint), intent(in) :: len
    !> [out] 重複を省いた要素数
    integer(gint), intent(out) :: newlen
    integer(gint) :: i, ndup

    ndup = 0
    do i = 2, len
      if(array(i) == array(i - 1 - ndup))then
        ndup = ndup + 1
      elseif(ndup > 0)then
        array(i - ndup) = array(i)
      endif
    end do
    newlen = len - ndup
  end subroutine gedatsu_get_uniq_int

end module mod_gedatsu_std
