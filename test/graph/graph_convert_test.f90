!> グラフデータ変換テストモジュール
module mod_gedatsu_graph_convert_test
  use mod_gedatsu
  use mod_monolis_utils

  implicit none

contains

  subroutine gedatsu_graph_convert_test()
    implicit none

    call gedatsu_convert_simple_elem_to_connectivity_graph_test()
    call gedatsu_convert_connectivity_graph_to_nodal_graph_test()
  end subroutine gedatsu_graph_convert_test

  subroutine gedatsu_convert_simple_elem_to_connectivity_graph_test()
    implicit none
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint) :: elem(2,3)
    integer(kint), allocatable :: index(:)
    integer(kint), allocatable :: item(:)

    call monolis_std_log_string("gedatsu_convert_simple_elem_to_connectivity_graph_test")

    n_elem = 3

    n_base = 2

    elem(1,1) = 1; elem(2,1) = 2
    elem(1,2) = 2; elem(2,2) = 3
    elem(1,3) = 3; elem(2,3) = 4

    call gedatsu_convert_simple_elem_to_connectivity_graph(n_elem, n_base, elem, index, item)

    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 1", index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 2", index(2), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 3", index(3), 4)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 4", index(4), 6)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 5", item(1), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 6", item(2), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 7", item(3), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 8", item(4), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 9", item(5), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph_test case 10", item(6), 4)
  end subroutine gedatsu_convert_simple_elem_to_connectivity_graph_test

  subroutine gedatsu_convert_connectivity_graph_to_nodal_graph_test()
    implicit none
    integer(kint) :: n_node
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint) :: elem(2,3)
    integer(kint), allocatable :: conn_index(:)
    integer(kint), allocatable :: conn_item(:)
    integer(kint), allocatable :: nodal_index(:)
    integer(kint), allocatable :: nodal_item(:)

    call monolis_std_log_string("gedatsu_convert_connectivity_graph_to_nodal_graph_test")

    n_node = 4

    n_elem = 3

    n_base = 2

    elem(1,1) = 1; elem(2,1) = 2
    elem(1,2) = 2; elem(2,2) = 3
    elem(1,3) = 3; elem(2,3) = 4

    call gedatsu_convert_simple_elem_to_connectivity_graph(n_elem, n_base, elem, conn_index, conn_item)

    call gedatsu_convert_connectivity_graph_to_nodal_graph &
      & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)

    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 1", nodal_index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 2", nodal_index(2), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 3", nodal_index(3), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 4", nodal_index(4), 5)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 5", nodal_index(5), 6)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 6", nodal_item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 7", nodal_item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 8", nodal_item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 9", nodal_item(4), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 10", nodal_item(5), 4)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test case 11", nodal_item(6), 3)
  end subroutine gedatsu_convert_connectivity_graph_to_nodal_graph_test
end module mod_gedatsu_graph_convert_test
