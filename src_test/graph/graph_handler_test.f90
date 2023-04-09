!> グラフ操作テストモジュール
module mod_gedatsu_graph_handler_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_graph_handler_test()
    implicit none

    call  gedatsu_graph_set_n_vertex_test()
    call  gedatsu_graph_add_n_vertex_test()
    call  gedatsu_graph_add_n_vertex_with_vertex_id_test()
    call  gedatsu_graph_get_n_vertex_test()
    call  gedatsu_graph_get_n_vertex_in_internal_region_test()
    call  gedatsu_graph_get_n_vertex_in_overlap_region_test()
    call  gedatsu_graph_get_vertex_id_in_internal_region_test()
    call  gedatsu_graph_get_vertex_id_in_overlap_region_test()
    call  gedatsu_graph_get_n_edge_test()
    call  gedatsu_graph_get_n_edge_in_internal_region_test()
    call  gedatsu_graph_get_n_edge_in_overlap_region_test()
    call  gedatsu_graph_get_edge_in_internal_region_test()
    call  gedatsu_graph_get_edge_in_overlap_region_test()
    call  gedatsu_graph_set_edge_test()
    call  gedatsu_graph_add_edge_test()

    call monolis_std_log_string("gedatsu_graph_debug_write")
  end subroutine gedatsu_graph_handler_test

  subroutine gedatsu_graph_set_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_set_n_vertex")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex case 4", size(graph%index), 4)
  end subroutine gedatsu_graph_set_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add

    call monolis_std_log_string("gedatsu_graph_add_n_vertex")

    n_vertex_add = 3

    call gedatsu_graph_add_n_vertex(graph, n_vertex_add)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 4", size(graph%index), 4)

    n_vertex_add = 3

    call gedatsu_graph_add_n_vertex(graph, n_vertex_add)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 5", graph%n_vertex, 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 6", size(graph%vertex_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 7", size(graph%vertex_domain_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex case 8", size(graph%index), 7)
  end subroutine gedatsu_graph_add_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add
    integer(kint) :: vertex_id(3), i_ans(6)

    call monolis_std_log_string("gedatsu_graph_add_n_vertex_with_vertex_id")

    n_vertex_add = 3

    vertex_id(1) = 10
    vertex_id(2) = 20
    vertex_id(3) = 30

    call gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 4", size(graph%index), 4)

    n_vertex_add = 3

    vertex_id(1) = 40
    vertex_id(2) = 50
    vertex_id(3) = 60

    call gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 5", graph%n_vertex, 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 6", size(graph%vertex_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 7", size(graph%vertex_domain_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id case 8", size(graph%index), 7)

    i_ans(1) = 10
    i_ans(2) = 20
    i_ans(3) = 30
    i_ans(4) = 40
    i_ans(5) = 50
    i_ans(6) = 60

    call monolis_test_check_eq_I ("gedatsu_graph_add_n_vertex_with_vertex_id case 9", graph%vertex_id, i_ans)
  end subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test

  subroutine gedatsu_graph_get_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_set_n_vertex")
    call monolis_std_log_string("gedatsu_graph_get_n_vertex")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_vertex = 0

    call gedatsu_graph_get_n_vertex(graph, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex case 1", n_vertex, 3)
  end subroutine gedatsu_graph_get_n_vertex_test

  subroutine gedatsu_graph_get_n_vertex_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_internal_region")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1

    domain_id = 0

    n_vertex = 0

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_in_internal_region case 1", n_vertex, 2)
  end subroutine gedatsu_graph_get_n_vertex_in_internal_region_test

  subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8)

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_overlap_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    n_vertex = 0

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_in_overlap_region case 1", n_vertex, 1)
  end subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test

  subroutine gedatsu_graph_get_vertex_id_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8)
    integer(kint) :: ids(2)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_internal_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 10
    graph%vertex_id(2) = 20
    graph%vertex_id(3) = 30
    graph%vertex_id(4) = 40
    graph%vertex_id(5) = 50

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids)

    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region case 1", n_vertex, 2)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region case 2", ids(1), 10)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region case 3", ids(2), 20)
  end subroutine gedatsu_graph_get_vertex_id_in_internal_region_test

  subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8), ids(1)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_overlap_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 10
    graph%vertex_id(2) = 20
    graph%vertex_id(3) = 30
    graph%vertex_id(4) = 40
    graph%vertex_id(5) = 50

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)

    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_overlap_region case 1", n_vertex, 1)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_overlap_region case 2", ids(1), 30)
  end subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test

  subroutine gedatsu_graph_get_n_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,8)

    call monolis_std_log_string("gedatsu_graph_get_n_edge")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    n_edge = 0

    call gedatsu_graph_get_n_edge(graph, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge case 1", n_edge, 8)
  end subroutine gedatsu_graph_get_n_edge_test

  subroutine gedatsu_graph_get_n_edge_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_internal_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    n_edge = 0

    domain_id = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_in_internal_region case 1", n_edge, 2)
  end subroutine gedatsu_graph_get_n_edge_in_internal_region_test

  subroutine gedatsu_graph_get_n_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_overlap_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    n_edge = 0

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_in_overlap_region case 1", n_edge, 2)
  end subroutine gedatsu_graph_get_n_edge_in_overlap_region_test

  subroutine gedatsu_graph_get_edge_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,8), new(2,2)

    call monolis_std_log_string("gedatsu_graph_get_edge_in_internal_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 1
    graph%vertex_id(2) = 2
    graph%vertex_id(3) = 3
    graph%vertex_id(4) = 4
    graph%vertex_id(5) = 5

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call gedatsu_graph_get_edge_in_internal_region(graph, domain_id, new)

    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region case 1", new(1,1), 1)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region case 2", new(2,1), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region case 2", new(1,2), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region case 2", new(2,2), 1)
  end subroutine gedatsu_graph_get_edge_in_internal_region_test

  subroutine gedatsu_graph_get_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, domain_id
    integer(kint) :: edge(2,8), ovl_edge(2,2)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_internal_region")
    call monolis_std_log_string("gedatsu_graph_get_edge_in_overlap_region")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 1
    graph%vertex_id(2) = 2
    graph%vertex_id(3) = 3
    graph%vertex_id(4) = 4
    graph%vertex_id(5) = 5

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    n_edge = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, ovl_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 1", ovl_edge(1,1), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 2", ovl_edge(2,1), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 3", ovl_edge(1,2), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 4", ovl_edge(2,2), 2)
  end subroutine gedatsu_graph_get_edge_in_overlap_region_test

  subroutine gedatsu_graph_set_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge, n_vertex
    integer(kint) :: edge(2,8)

    call monolis_std_log_string("gedatsu_graph_set_edge")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_edge = 8

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 2; edge(2,3) = 3
    edge(1,4) = 3; edge(2,4) = 2
    edge(1,5) = 3; edge(2,5) = 4
    edge(1,6) = 4; edge(2,6) = 3
    edge(1,7) = 4; edge(2,7) = 5
    edge(1,8) = 5; edge(2,8) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 1", size(graph%item), 8)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 2", graph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 3", graph%item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 4", graph%item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 5", graph%item(4), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 6", graph%item(5), 4)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 7", graph%item(6), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 8", graph%item(7), 5)
    call monolis_test_check_eq_I1("gedatsu_graph_set_edge case 9", graph%item(8), 4)
  end subroutine gedatsu_graph_set_edge_test

  subroutine gedatsu_graph_add_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge, n_add_edge, n_vertex
    integer(kint) :: edge(2,6), add(2,2)

    call monolis_std_log_string("gedatsu_graph_add_edge")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_edge = 6

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 1
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 3
    edge(1,5) = 4; edge(2,5) = 5
    edge(1,6) = 5; edge(2,6) = 4

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    n_add_edge = 2

    add(1,1) = 2; add(2,1) = 3
    add(1,2) = 3; add(2,2) = 2

    call gedatsu_graph_add_edge(graph, n_add_edge, add)

    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 1", size(graph%item), 8)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 2", graph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 3", graph%item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 4", graph%item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 5", graph%item(4), 2)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 6", graph%item(5), 4)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 7", graph%item(6), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 8", graph%item(7), 5)
    call monolis_test_check_eq_I1("gedatsu_graph_add_edge case 9", graph%item(8), 4)
  end subroutine gedatsu_graph_add_edge_test
end module mod_gedatsu_graph_handler_test
