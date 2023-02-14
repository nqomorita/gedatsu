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
    call gedatsu_get_parted_graph_test()
    call gedatsu_get_parted_graph_main_test()
    call gedatsu_add_overlapping_nodes_test()
  end subroutine gedatsu_graph_part_test

  subroutine gedatsu_graph_partition_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_domain
    type(gedatsu_graph), allocatable :: subgraphs(:)

    call monolis_std_log_string("gedatsu_graph_partition_test")

    call gedatsu_graph_partition(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_test

  subroutine gedatsu_graph_partition_with_weight_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_domain
    integer(kint), allocatable :: node_wgt(:,:)
    integer(kint), allocatable :: edge_wgt(:,:)
    type(gedatsu_graph), allocatable :: subgraphs(:)

    call monolis_std_log_string("gedatsu_graph_partition_with_weight_test")

    call gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
  end subroutine gedatsu_graph_partition_with_weight_test

  subroutine gedatsu_check_vertex_domain_id_test()
    implicit none
    integer(kint) :: n_vertex
    integer(kint) :: n_domain
    integer(kint) :: vertex_domain_id(3)

    call monolis_std_log_string("gedatsu_check_vertex_domain_id_test")

    call gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)
  end subroutine gedatsu_check_vertex_domain_id_test

  subroutine gedatsu_get_parted_graph_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_domain
    type(gedatsu_graph), allocatable :: subgraphs(:)

    call monolis_std_log_string("gedatsu_get_parted_graph_test")

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_get_parted_graph_test

  subroutine gedatsu_get_parted_graph_main_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    type(gedatsu_graph) :: subgraph

    call monolis_std_log_string("gedatsu_get_parted_graph_main_test")

    call gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
  end subroutine gedatsu_get_parted_graph_main_test

  subroutine gedatsu_add_overlapping_nodes_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    type(gedatsu_graph) :: subgraph

    call monolis_std_log_string("gedatsu_add_overlapping_nodes_test")

    call gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
  end subroutine gedatsu_add_overlapping_nodes_test
end module mod_gedatsu_graph_part_test
