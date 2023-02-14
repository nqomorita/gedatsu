!> dlb テストモジュール
module mod_gedatsu_dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_dlb_test()
    implicit none
    call gedatsu_dlb_initialize_test()
    call gedatsu_dlb_finalize_test()
    call gedatsu_dlb_should_update_test()
  end subroutine gedatsu_dlb_test

  subroutine gedatsu_dlb_initialize_test()
    implicit none
    type(gedatsu_dlb) :: dlb

    call monolis_std_log_string("gedatsu_dlb_initialize_test")

    call gedatsu_dlb_initialize(dlb)

    call monolis_test_check_eq_L1("gedatsu_dlb_initialize_test case 1", dlb%should_update, .false.)
  end subroutine gedatsu_dlb_initialize_test

  subroutine gedatsu_dlb_finalize_test()
    implicit none
    type(gedatsu_dlb) :: dlb

    call monolis_std_log_string("gedatsu_dlb_finalize_test")

    call gedatsu_dlb_finalize(dlb)

    call monolis_test_check_eq_L1("gedatsu_dlb_finalize_test case 1", dlb%should_update, .false.)
  end subroutine gedatsu_dlb_finalize_test

  subroutine gedatsu_dlb_should_update_test()
    implicit none
    type(gedatsu_dlb) :: dlb
    logical :: should_update

    call monolis_std_log_string("gedatsu_dlb_should_update_test")

    call gedatsu_dlb_initialize(dlb)

    call gedatsu_dlb_should_update(dlb, should_update)

    call monolis_test_check_eq_L1("gedatsu_dlb_should_update_test case 1", should_update, .false.)
  end subroutine gedatsu_dlb_should_update_test
end module mod_gedatsu_dlb_test
