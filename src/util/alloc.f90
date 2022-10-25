!> メモリ確保モジュール
module mod_gedatsu_alloc
  use mod_gedatsu_prm
  use mod_gedatsu_util
  implicit none

contains

  !> @ingroup group_dev_alloc
  !> 1 次元整数配列のメモリ確保
  !> @details 初期値 0 でメモリ確保がなされる。
  subroutine gedatsu_alloc_int_1d(var, i)
    implicit none
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(gint) :: i
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_int_1d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i), source = 0, stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_int_1d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_int_1d

  !> @ingroup group_dev_alloc
  !> 1 次元整数配列のメモリ開放
  subroutine gedatsu_dealloc_int_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    integer(gint), allocatable :: var(:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_int_1d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_int_1d

  !> @ingroup group_dev_alloc
  !> 1 次元整数配列のメモリ再確保
  !> @details 再確保で増えた配列部分は初期値 0 でメモリ確保がなされる。
  subroutine gedatsu_realloc_int_1d(var, i)
    implicit none
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: var(:)
    !> [in] 再確保後の配列サイズ
    integer(gint) :: i
    integer(gint), allocatable :: temp(:)
    integer(gint) :: j, iold

    if(.not. allocated(var))then
      call gedatsu_alloc_int_1d(var, i)
      return
    endif

    iold = size(var)

    call gedatsu_alloc_int_1d(temp, iold)

    temp(:) = var(:)

    call gedatsu_dealloc_int_1d(var)

    call gedatsu_alloc_int_1d(var, i)

    do j = 1, min(iold, i)
      var(j) = temp(j)
    enddo
  end subroutine gedatsu_realloc_int_1d

  !> @ingroup group_dev_alloc
  !> 2 次元整数配列のメモリ確保
  !> @details 配列サイズは var(i,j) として確保される。
  subroutine gedatsu_alloc_int_2d(var, i, j)
    implicit none
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(gint) :: i
    !> [in] 配列サイズ
    integer(gint) :: j
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_int_2d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i,j), source = 0, stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_int_2d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_int_2d

  !> @ingroup group_dev_alloc
  !> 2 次元整数配列のメモリ開放
  subroutine gedatsu_dealloc_int_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    integer(gint), allocatable :: var(:,:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_int_2d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_int_2d

  !> @ingroup group_dev_alloc
  !> 1 次元浮動小数点配列のメモリ確保
  subroutine gedatsu_alloc_real_1d(var, i)
    implicit none
    !> [in] メモリ確保する配列
    real(gdouble), allocatable :: var(:)
    !> [in] 配列サイズ
    integer(gint) :: i
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_real_1d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i), source = 0.0d0, stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_real_1d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_real_1d

  !> @ingroup group_dev_alloc
  !> 1 次元浮動小数点配列のメモリ開放
  subroutine gedatsu_dealloc_real_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    real(gdouble), allocatable :: var(:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_real_1d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_real_1d

  !> @ingroup group_dev_alloc
  !> 2 次元浮動小数点配列のメモリ確保
  !> @details 配列サイズは var(i,j) として確保される。
  subroutine gedatsu_alloc_real_2d(var, i, j)
    implicit none
    !> [in] メモリ確保する配列
    real(gdouble), allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(gint) :: i
    !> [in] 配列サイズ
    integer(gint) :: j
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_real_2d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i,j), source = 0.0d0, stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_real_2d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_real_2d

  !> 2 次元浮動小数点配列のメモリ開放
  subroutine gedatsu_dealloc_real_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    real(gdouble), allocatable :: var(:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_real_2d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_real_2d

  !> @ingroup group_dev_alloc
  !> 1 次元論理型配列のメモリ確保
  subroutine gedatsu_alloc_bool_1d(var, i)
    implicit none
    !> [in] メモリ確保する配列
    logical, allocatable :: var(:)
    !> [in] 配列サイズ
    integer(gint) :: i
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_bool_1d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i), source = .false., stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_bool_1d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_bool_1d

  !> @ingroup group_dev_alloc
  !> 1 次元論理型配列のメモリ開放
  subroutine gedatsu_dealloc_bool_1d(var)
    implicit none
    !> [in] メモリ開放する配列
    logical, allocatable :: var(:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_bool_1d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_bool_1d

  !> @ingroup group_dev_alloc
  !> 2 次元論理型配列のメモリ確保
  !> @details 配列サイズは var(i,j) として確保される。
  subroutine gedatsu_alloc_bool_2d(var, i, j)
    implicit none
    !> [in] メモリ確保する配列
    logical, allocatable :: var(:,:)
    !> [in] 配列サイズ
    integer(gint) :: i
    !> [in] 配列サイズ
    integer(gint) :: j
    integer(gint) :: ierr

    if(allocated(var))then
      call gedatsu_error_string("gedatsu_alloc_bool_2d")
      call gedatsu_error_string("input arg. is already allocated")
      call gedatsu_error_stop()
    endif

    allocate(var(i,j), source = .false., stat = ierr)

    if(ierr /= 0)then
      call gedatsu_error_string("gedatsu_alloc_bool_2d")
      call gedatsu_error_string("allocation is failed")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_alloc_bool_2d

  !> @ingroup group_dev_alloc
  !> 2 次元論理型配列のメモリ開放
  subroutine gedatsu_dealloc_bool_2d(var)
    implicit none
    !> [in] メモリ開放する配列
    logical, allocatable :: var(:,:)

    if(.not. allocated(var))then
      return
      !call gedatsu_error_string("gedatsu_dealloc_bool_2d")
      !call gedatsu_error_string("input arg. is not allocated")
      !call gedatsu_error_stop()
    endif

    deallocate(var)
  end subroutine gedatsu_dealloc_bool_2d

end module mod_gedatsu_alloc
