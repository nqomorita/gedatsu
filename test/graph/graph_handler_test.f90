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
  end subroutine gedatsu_graph_handler_test

  subroutine gedatsu_graph_set_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_set_n_vertex_test")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex_test case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex_test case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex_test case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_set_n_vertex_test case 4", size(graph%index), 4)
  end subroutine gedatsu_graph_set_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add

    call monolis_std_log_string("gedatsu_graph_add_n_vertex_test")

    n_vertex_add = 3

    call gedatsu_graph_add_n_vertex(graph, n_vertex_add)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 4", size(graph%index), 4)

    n_vertex_add = 3

    call gedatsu_graph_add_n_vertex(graph, n_vertex_add)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 5", graph%n_vertex, 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 6", size(graph%vertex_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 7", size(graph%vertex_domain_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_test case 8", size(graph%index), 7)
  end subroutine gedatsu_graph_add_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add
    integer(kint) :: vertex_id(3), i_ans(6)

    call monolis_std_log_string("gedatsu_graph_add_n_vertex_with_vertex_id_test")

    n_vertex_add = 3

    vertex_id(1) = 10
    vertex_id(2) = 20
    vertex_id(3) = 30

    call gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 1", graph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 2", size(graph%vertex_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 3", size(graph%vertex_domain_id), 3)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 4", size(graph%index), 4)

    n_vertex_add = 3

    vertex_id(1) = 40
    vertex_id(2) = 50
    vertex_id(3) = 60

    call gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)

    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 5", graph%n_vertex, 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 6", size(graph%vertex_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 7", size(graph%vertex_domain_id), 6)
    call monolis_test_check_eq_I1("gedatsu_graph_add_n_vertex_with_vertex_id_test case 8", size(graph%index), 7)

    i_ans(1) = 10
    i_ans(2) = 20
    i_ans(3) = 30
    i_ans(4) = 40
    i_ans(5) = 50
    i_ans(6) = 60

    call monolis_test_check_eq_I ("gedatsu_graph_add_n_vertex_with_vertex_id_test case 9", graph%vertex_id, i_ans)
  end subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test

  subroutine gedatsu_graph_get_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_test")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_vertex = 0

    call gedatsu_graph_get_n_vertex(graph, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_test case 1", n_vertex, 3)
  end subroutine gedatsu_graph_get_n_vertex_test

  subroutine gedatsu_graph_get_n_vertex_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_internal_region_test")

    n_vertex = 3

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1

    domain_id = 0

    n_vertex = 0

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_in_internal_region_test case 1", n_vertex, 2)
  end subroutine gedatsu_graph_get_n_vertex_in_internal_region_test

  subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_overlap_region_test")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    n_vertex = 0

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_vertex_in_overlap_region_test case 1", n_vertex, 1)
  end subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test

  subroutine gedatsu_graph_get_vertex_id_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4)
    integer(kint) :: ids(2)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_internal_region_test")

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

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, ids)

    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region_test case 1", n_vertex, 2)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region_test case 2", ids(1), 10)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_internal_region_test case 3", ids(2), 20)
  end subroutine gedatsu_graph_get_vertex_id_in_internal_region_test

  subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4), ids(1)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_overlap_region_test")

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

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)

    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_overlap_region_test case 1", n_vertex, 1)
    call monolis_test_check_eq_I1("gedatsu_graph_get_vertex_id_in_overlap_region_test case 2", ids(1), 30)
  end subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test

  subroutine gedatsu_graph_get_n_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_test")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    n_edge = 0

    call gedatsu_graph_get_n_edge(graph, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_test case 1", n_edge, 4)
  end subroutine gedatsu_graph_get_n_edge_test

  subroutine gedatsu_graph_get_n_edge_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_internal_region_test")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    n_edge = 0

    domain_id = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_in_internal_region_test case 1", n_edge, 1)
  end subroutine gedatsu_graph_get_n_edge_in_internal_region_test

  subroutine gedatsu_graph_get_n_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_overlap_region_test")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_domain_id(1) = 0
    graph%vertex_domain_id(2) = 0
    graph%vertex_domain_id(3) = 1
    graph%vertex_domain_id(4) = 1
    graph%vertex_domain_id(5) = 1

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    n_edge = 0

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)

    call monolis_test_check_eq_I1("gedatsu_graph_get_n_edge_in_overlap_region_test case 1", n_edge, 2)
  end subroutine gedatsu_graph_get_n_edge_in_overlap_region_test

  subroutine gedatsu_graph_get_edge_in_internal_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex, n_edge, edge(2,4), new(2,1)

    call monolis_std_log_string("gedatsu_graph_get_edge_in_internal_region_test")

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

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call gedatsu_graph_get_edge_in_internal_region(graph, domain_id, new)

    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 1", new(1,1), 1)
    call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 2", new(2,1), 2)
  end subroutine gedatsu_graph_get_edge_in_internal_region_test

  subroutine gedatsu_graph_get_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    type(gedatsu_graph) :: subgraph
    integer(kint) :: n_vertex, n_edge, domain_id
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_edge_in_overlap_region_test")

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

    n_edge = 4

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 2; edge(2,2) = 3
    edge(1,3) = 3; edge(2,3) = 4
    edge(1,4) = 4; edge(2,4) = 5

    call gedatsu_graph_set_edge(graph, n_edge, edge)

    domain_id = 0

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    !call gedatsu_graph_get_edge_in_overlap_region(graph, subgraph, domain_id, edge)

    !call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 1", new(1,1), 1)
    !call monolis_test_check_eq_I1("gedatsu_graph_get_edge_in_internal_region_test case 2", new(2,1), 2)
  end subroutine gedatsu_graph_get_edge_in_overlap_region_test

  subroutine gedatsu_graph_set_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_set_edge")

    !call gedatsu_graph_set_edge(graph, n_edge, edge)
  end subroutine gedatsu_graph_set_edge_test

  subroutine gedatsu_graph_add_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_add_edge_test")

    !call gedatsu_graph_add_edge(graph, n_edge, edge)
  end subroutine gedatsu_graph_add_edge_test
end module mod_gedatsu_graph_handler_test
