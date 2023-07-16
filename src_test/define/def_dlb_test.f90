!> dlb テストモジュール
module mod_gedatsu_def_dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_def_dlb_test()
    implicit none
    call gedatsu_dlb_initialize_test()
    call gedatsu_dlb_finalize_test()
  end subroutine gedatsu_def_dlb_test

  subroutine gedatsu_dlb_initialize_test()
    implicit none
    type(gedatsu_dlb) :: dlb

    call monolis_std_log_string("gedatsu_dlb_initialize")

    call gedatsu_dlb_initialize(dlb)
  end subroutine gedatsu_dlb_initialize_test

  subroutine gedatsu_dlb_finalize_test()
    implicit none
    type(gedatsu_dlb) :: dlb

    call monolis_std_log_string("gedatsu_dlb_finalize")

    call gedatsu_dlb_finalize(dlb)
  end subroutine gedatsu_dlb_finalize_test

end module mod_gedatsu_def_dlb_test
