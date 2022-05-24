module mod_gedatsu_prm
  implicit none

  integer(4), parameter :: gint    = 4
  integer(4), parameter :: gdouble = 8

  integer(gint), parameter :: gedatsu_success = 0
  integer(gint), parameter :: gedatsu_fail = 1
  integer(gint), parameter :: gedatsu_charlen = 1024

  type gedatsu_prm
    integer(gint) :: method = 1
  end type gedatsu_prm

contains

  subroutine gedatsu_prm_initialize(param)
    implicit none
    type(gedatsu_prm) :: param

  end subroutine gedatsu_prm_initialize

  subroutine gedatsu_prm_finalize(param)
    implicit none
    type(gedatsu_prm) :: param

  end subroutine gedatsu_prm_finalize

end module mod_gedatsu_prm