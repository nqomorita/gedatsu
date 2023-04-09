!> graph テストモジュール
module mod_gedatsu_def_graph_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_def_graph_test()
    implicit none

    call gedatsu_graph_initialize_test()
    call gedatsu_graph_finalize_test()
  end subroutine gedatsu_def_graph_test

  subroutine gedatsu_graph_initialize_test()
    implicit none
    type(gedatsu_graph) :: graph

    call monolis_std_log_string("gedatsu_graph_initialize")

    call gedatsu_graph_initialize(graph)

    call monolis_test_check_eq_I1("gedatsu_graph_initialize case 1", graph%n_vertex, 0)

    call monolis_test_check_eq_I1("gedatsu_graph_initialize case 1", graph%n_internal_vertex, 0)
  end subroutine gedatsu_graph_initialize_test

  subroutine gedatsu_graph_finalize_test()
    implicit none
    type(gedatsu_graph) :: graph

    call monolis_std_log_string("gedatsu_graph_finalize")

    call gedatsu_graph_finalize(graph)

    call monolis_test_check_eq_I1("gedatsu_graph_finalize case 1", graph%n_vertex, 0)

    call monolis_test_check_eq_I1("gedatsu_graph_finalize case 1", graph%n_internal_vertex, 0)
  end subroutine gedatsu_graph_finalize_test
end module mod_gedatsu_def_graph_test