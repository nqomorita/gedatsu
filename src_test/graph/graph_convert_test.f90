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
    call gedatsu_check_connectivity_graph_test()
  end subroutine gedatsu_graph_convert_test

  subroutine gedatsu_convert_simple_elem_to_connectivity_graph_test()
    implicit none
    integer(kint) :: n_elem
    integer(kint) :: n_base
    integer(kint) :: elem(2,3)
    integer(kint), allocatable :: index(:)
    integer(kint), allocatable :: item(:)

    call monolis_std_log_string("gedatsu_convert_simple_mesh_to_connectivity_graph")

    n_elem = 3

    n_base = 2

    elem(1,1) = 1; elem(2,1) = 2
    elem(1,2) = 2; elem(2,2) = 3
    elem(1,3) = 3; elem(2,3) = 4

    call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, index, item)

    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 1", index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 2", index(2), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 3", index(3), 4)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 4", index(4), 6)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 5", item(1), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 6", item(2), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 7", item(3), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 8", item(4), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 9", item(5), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_simple_elem_to_connectivity_graph 10", item(6), 4)
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

    call monolis_std_log_string("gedatsu_convert_simple_mesh_to_connectivity_graph")
    call monolis_std_log_string("gedatsu_convert_connectivity_graph_to_nodal_graph")

    n_node = 4

    n_elem = 3

    n_base = 2

    elem(1,1) = 1; elem(2,1) = 2
    elem(1,2) = 2; elem(2,2) = 3
    elem(1,3) = 3; elem(2,3) = 4

    call gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, conn_index, conn_item)

    call gedatsu_convert_connectivity_graph_to_nodal_graph &
      & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)

    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 1", nodal_index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 2", nodal_index(2), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 3", nodal_index(3), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 4", nodal_index(4), 5)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 5", nodal_index(5), 6)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 6", nodal_item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 7", nodal_item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 8", nodal_item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 9", nodal_item(4), 2)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 10", nodal_item(5), 4)
    call monolis_test_check_eq_I1("gedatsu_convert_connectivity_graph_to_nodal_graph_test 11", nodal_item(6), 3)
  end subroutine gedatsu_convert_connectivity_graph_to_nodal_graph_test

  subroutine gedatsu_check_connectivity_graph_test()
    implicit none
    !> 節点グラフ
    type(gedatsu_graph) :: node
    !> コネクティビティグラフ
    type(gedatsu_graph) :: conn
    integer(kint) :: n_vertex, n_edge
    integer(kint) :: edge(2,8)
    logical :: is_valid

    call monolis_std_log_string("gedatsu_graph_set_n_vertex")
    call monolis_std_log_string("gedatsu_graph_set_edge")
    call monolis_std_log_string("gedatsu_graph_set_n_vertex")
    call monolis_std_log_string("gedatsu_check_connectivity_graph")

    !> node graph
    n_vertex = 5

    call gedatsu_graph_set_n_vertex(node, n_vertex)

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2;
    edge(1,2) = 2; edge(2,2) = 1;
    edge(1,3) = 2; edge(2,3) = 3;
    edge(1,4) = 3; edge(2,4) = 2;
    edge(1,5) = 3; edge(2,5) = 4;
    edge(1,6) = 4; edge(2,6) = 3;
    edge(1,7) = 4; edge(2,7) = 5;
    edge(1,8) = 5; edge(2,8) = 4;

    call gedatsu_graph_set_edge(node, n_edge, edge)

    !> conn graph case 1
    n_vertex = 4

    call gedatsu_graph_set_n_vertex(conn, n_vertex)

    n_edge = 6

    edge(1,1) = 1; edge(2,1) = 1;
    edge(1,2) = 1; edge(2,2) = 2;
    edge(1,3) = 2; edge(2,3) = 2;
    edge(1,4) = 2; edge(2,4) = 3;
    edge(1,5) = 3; edge(2,5) = 3;
    edge(1,6) = 3; edge(2,6) = 4;

    call gedatsu_graph_set_edge(conn, n_edge, edge)

    call gedatsu_check_connectivity_graph(node, conn, is_valid)

    call monolis_test_check_eq_L1("gedatsu_check_connectivity_graph_test 1", is_valid, .true.)

    !> conn graph case 2
    call gedatsu_graph_finalize(conn)

    n_vertex = 4

    call gedatsu_graph_set_n_vertex(conn, n_vertex)

    n_edge = 6

    edge(1,1) = 1; edge(2,1) = 1;
    edge(1,2) = 1; edge(2,2) = 2;
    edge(1,3) = 2; edge(2,3) = 2;
    edge(1,4) = 2; edge(2,4) = 3;
    edge(1,5) = 3; edge(2,5) = 1;
    edge(1,6) = 3; edge(2,6) = 3;

    call gedatsu_graph_set_edge(conn, n_edge, edge)

    call gedatsu_check_connectivity_graph(node, conn, is_valid)

    call monolis_test_check_eq_L1("gedatsu_check_connectivity_graph_test 2", is_valid, .false.)
  end subroutine gedatsu_check_connectivity_graph_test
end module mod_gedatsu_graph_convert_test
