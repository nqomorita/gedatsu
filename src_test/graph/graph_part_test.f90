!> グラフ分割テストモジュール
module mod_gedatsu_graph_part_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_graph_part_test()
    implicit none

    call gedatsu_graph_partition_test()
    call gedatsu_graph_partition_with_weight_test()
    call gedatsu_check_vertex_domain_id_test()
    call gedatsu_get_parted_graph_main_test()
    call gedatsu_add_overlapping_nodes_test()

    call monolis_std_log_string("gedatsu_part_graph_metis")
    call monolis_std_log_string("gedatsu_part_graph_metis_with_weight")

    call monolis_std_log_string("gedatsu_get_parted_connectivity_main")
    call monolis_std_log_string("gedatsu_get_parted_graph")
  end subroutine gedatsu_graph_part_test

  subroutine gedatsu_graph_partition_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,8)
    integer(kint) :: n_domain
    type(gedatsu_graph) :: subgraphs(2)

    call monolis_std_log_string("gedatsu_graph_partition")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 1
    graph%vertex_id(2) = 2
    graph%vertex_id(3) = 3
    graph%vertex_id(4) = 4
    graph%vertex_id(5) = 5

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

    n_domain = 2

    call gedatsu_graph_partition(graph, n_domain, subgraphs)

    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 1", subgraphs(1)%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 1", subgraphs(2)%n_vertex, 4)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 2", subgraph%n_internal_vertex, 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 3", subgraph%vertex_id(1), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 4", subgraph%vertex_id(2), 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 5", subgraph%vertex_id(3), 3)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 6", subgraph%vertex_domain_id(1), 0)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 7", subgraph%vertex_domain_id(2), 0)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 8", subgraph%vertex_domain_id(3), 0)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 9", subgraph%index(1), 0)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 10", subgraph%index(2), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 11", subgraph%index(3), 3)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 12", subgraph%index(4), 4)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 13", subgraph%item(1), 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 14", subgraph%item(2), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 15", subgraph%item(3), 3)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 16", subgraph%item(4), 2)
  end subroutine gedatsu_graph_partition_test

  subroutine gedatsu_graph_partition_with_weight_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,8)
    integer(kint) :: n_domain
    integer(kint), allocatable :: node_wgt(:,:)
    integer(kint), allocatable :: edge_wgt(:,:)
    type(gedatsu_graph) :: subgraphs(2)

    call monolis_std_log_string("gedatsu_graph_partition_with_weight")

    n_vertex = 5

    call gedatsu_graph_set_n_vertex(graph, n_vertex)

    graph%vertex_id(1) = 1
    graph%vertex_id(2) = 2
    graph%vertex_id(3) = 3
    graph%vertex_id(4) = 4
    graph%vertex_id(5) = 5

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

    n_domain = 2

    call monolis_alloc_I_2d(node_wgt, 1, 5)

    node_wgt(1,1) = 1
    node_wgt(1,2) = 1
    node_wgt(1,3) = 1
    node_wgt(1,4) = 1
    node_wgt(1,5) = 2

    call gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)

!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 1", subgraphs(1)%n_vertex, 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 1", subgraphs(2)%n_vertex, 5)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 2", subgraph%n_internal_vertex, 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 3", subgraph%vertex_id(1), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 4", subgraph%vertex_id(2), 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 5", subgraph%vertex_id(3), 3)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 6", subgraph%vertex_domain_id(1), 0)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 7", subgraph%vertex_domain_id(2), 0)
!    !call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 8", subgraph%vertex_domain_id(3), 0)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 9", subgraph%index(1), 0)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 10", subgraph%index(2), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 11", subgraph%index(3), 3)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 12", subgraph%index(4), 4)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 13", subgraph%item(1), 2)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 14", subgraph%item(2), 1)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 15", subgraph%item(3), 3)
!    call monolis_test_check_eq_I1("gedatsu_graph_partition_test case a 16", subgraph%item(4), 2)
  end subroutine gedatsu_graph_partition_with_weight_test

  subroutine gedatsu_check_vertex_domain_id_test()
    implicit none
    integer(kint) :: n_vertex
    integer(kint) :: n_domain
    integer(kint) :: vertex_domain_id(3)

    call monolis_std_log_string("gedatsu_check_vertex_domain_id")

    n_vertex = 3

    n_domain = 3

    vertex_domain_id(1) = 1
    vertex_domain_id(2) = 2
    vertex_domain_id(3) = 3

    call gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)

    call monolis_test_assert_pass("gedatsu_check_vertex_domain_id case 1")
  end subroutine gedatsu_check_vertex_domain_id_test

  subroutine gedatsu_get_parted_graph_main_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,8)
    integer(kint) :: domain_id
    type(gedatsu_graph) :: subgraph

    call monolis_std_log_string("gedatsu_get_parted_graph_main")

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

    call gedatsu_get_parted_graph_main(graph, domain_id, subgraph)

    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 1", subgraph%n_vertex, 2)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 2", subgraph%n_internal_vertex, 2)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 3", subgraph%vertex_id(1), 1)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 4", subgraph%vertex_id(2), 2)
    !call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 5", subgraph%vertex_domain_id(1), 0)
    !call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 6", subgraph%vertex_domain_id(2), 0)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 7", subgraph%index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 8", subgraph%index(2), 1)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 9", subgraph%index(3), 2)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 10", subgraph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case a 11", subgraph%item(2), 1)

    domain_id = 1

    call gedatsu_graph_finalize(subgraph)

    call gedatsu_get_parted_graph_main(graph, domain_id, subgraph)

    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 1", subgraph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 2", subgraph%n_internal_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 3", subgraph%vertex_id(1), 3)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 4", subgraph%vertex_id(2), 4)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 5", subgraph%vertex_id(3), 5)
    !call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 6", subgraph%vertex_domain_id(1), 1)
    !call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 7", subgraph%vertex_domain_id(2), 1)
    !call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 8", subgraph%vertex_domain_id(3), 1)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 9", subgraph%index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 10", subgraph%index(2), 1)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 11", subgraph%index(3), 3)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 12", subgraph%index(4), 4)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 13", subgraph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 14", subgraph%item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 15", subgraph%item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_get_parted_graph_main case b 16", subgraph%item(4), 2)
  end subroutine gedatsu_get_parted_graph_main_test

  subroutine gedatsu_add_overlapping_nodes_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex, n_edge, edge(2,8)
    integer(kint) :: domain_id
    type(gedatsu_graph) :: subgraph

    call monolis_std_log_string("gedatsu_add_overlapping_nodes")

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

    call gedatsu_get_parted_graph_main(graph, domain_id, subgraph)

    call gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)

    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 1", subgraph%n_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 2", subgraph%n_internal_vertex, 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 3", subgraph%vertex_id(1), 1)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 4", subgraph%vertex_id(2), 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 5", subgraph%vertex_id(3), 3)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 6", subgraph%vertex_domain_id(1), 0)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 7", subgraph%vertex_domain_id(2), 0)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 8", subgraph%vertex_domain_id(3), 0)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 9", subgraph%index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 10", subgraph%index(2), 1)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 11", subgraph%index(3), 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 12", subgraph%index(4), 4)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 13", subgraph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 14", subgraph%item(2), 1)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 15", subgraph%item(3), 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case a 16", subgraph%item(4), 2)

    call gedatsu_graph_finalize(subgraph)

    domain_id = 1

    call gedatsu_get_parted_graph_main(graph, domain_id, subgraph)

    call gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)

    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 1", subgraph%n_vertex, 4)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 2", subgraph%n_internal_vertex, 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 3", subgraph%vertex_id(1), 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 4", subgraph%vertex_id(2), 4)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 5", subgraph%vertex_id(3), 5)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 6", subgraph%vertex_id(4), 2)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 7", subgraph%vertex_domain_id(1), 0)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 8", subgraph%vertex_domain_id(2), 0)
    !call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 9", subgraph%vertex_domain_id(3), 0)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 9", subgraph%index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 10", subgraph%index(2), 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 11", subgraph%index(3), 4)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 12", subgraph%index(4), 5)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 13", subgraph%index(5), 6)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 14", subgraph%item(1), 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 15", subgraph%item(2), 4)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 16", subgraph%item(3), 1)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 17", subgraph%item(4), 3)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 18", subgraph%item(5), 2)
    call monolis_test_check_eq_I1("gedatsu_add_overlapping_nodes case b 19", subgraph%item(6), 1)
  end subroutine gedatsu_add_overlapping_nodes_test
end module mod_gedatsu_graph_part_test
