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
    call  gedatsu_graph_get_n_vertex_in_subdomain_test()
    call  gedatsu_graph_get_n_vertex_in_overlap_region_test()
    call  gedatsu_graph_get_vertex_id_in_subdomain_test()
    call  gedatsu_graph_get_vertex_id_in_overlap_region_test()
    call  gedatsu_graph_get_n_edge_test()
    call  gedatsu_graph_get_n_edge_in_subdomain_test()
    call  gedatsu_graph_get_n_edge_in_overlap_region_test()
    call  gedatsu_graph_get_edge_in_subdomain_test()
    call  gedatsu_graph_get_edge_in_overlap_region_test()
    call  gedatsu_graph_set_edge_test()
    call  gedatsu_graph_add_edge_test()
  end subroutine gedatsu_graph_handler_test

  subroutine gedatsu_graph_set_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_set_n_vertex_test")

    call gedatsu_graph_set_n_vertex(graph, n_vertex)
  end subroutine gedatsu_graph_set_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add

    call monolis_std_log_string("gedatsu_graph_add_n_vertex_test")

    call gedatsu_graph_add_n_vertex(graph, n_vertex_add)
  end subroutine gedatsu_graph_add_n_vertex_test

  subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex_add
    integer(kint) :: vertex_id(3)

    call monolis_std_log_string("gedatsu_graph_add_n_vertex_with_vertex_id_test")

    call gedatsu_graph_add_n_vertex_with_vertex_id(graph, n_vertex_add, vertex_id)
  end subroutine gedatsu_graph_add_n_vertex_with_vertex_id_test

  subroutine gedatsu_graph_get_n_vertex_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_test")

    call gedatsu_graph_get_n_vertex(graph, n_vertex)
  end subroutine gedatsu_graph_get_n_vertex_test

  subroutine gedatsu_graph_get_n_vertex_in_subdomain_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_subdomain_test")

    call gedatsu_graph_get_n_vertex_in_subdomain(graph, domain_id, n_vertex)
  end subroutine gedatsu_graph_get_n_vertex_in_subdomain_test

  subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_vertex

    call monolis_std_log_string("gedatsu_graph_get_n_vertex_in_overlap_region_test")

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)
  end subroutine gedatsu_graph_get_n_vertex_in_overlap_region_test

  subroutine gedatsu_graph_get_vertex_id_in_subdomain_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: ids(3)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_subdomain_test")

    call gedatsu_graph_get_vertex_id_in_subdomain(graph, domain_id, ids)
  end subroutine gedatsu_graph_get_vertex_id_in_subdomain_test

  subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: ids(2)

    call monolis_std_log_string("gedatsu_graph_get_vertex_id_in_overlap_region_test")

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, ids)
  end subroutine gedatsu_graph_get_vertex_id_in_overlap_region_test

  subroutine gedatsu_graph_get_n_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge

    call monolis_std_log_string("gedatsu_graph_get_n_edge_test")

    call gedatsu_graph_get_n_edge(graph, n_edge)
  end subroutine gedatsu_graph_get_n_edge_test

  subroutine gedatsu_graph_get_n_edge_in_subdomain_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_edge

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_subdomain_test")

    call gedatsu_graph_get_n_edge_in_subdomain(graph, domain_id, n_edge)
  end subroutine gedatsu_graph_get_n_edge_in_subdomain_test

  subroutine gedatsu_graph_get_n_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: n_edge

    call monolis_std_log_string("gedatsu_graph_get_n_edge_in_overlap_region_test")

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)
  end subroutine gedatsu_graph_get_n_edge_in_overlap_region_test

  subroutine gedatsu_graph_get_edge_in_subdomain_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: domain_id
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_edge_in_subdomain_test")

    call gedatsu_graph_get_edge_in_subdomain(graph, domain_id, edge)
  end subroutine gedatsu_graph_get_edge_in_subdomain_test

  subroutine gedatsu_graph_get_edge_in_overlap_region_test()
    implicit none
    type(gedatsu_graph) :: graph
    type(gedatsu_graph) :: subgraph
    integer(kint) :: domain_id
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_get_edge_in_overlap_region_test")

    call gedatsu_graph_get_edge_in_overlap_region(graph, subgraph, domain_id, edge)
  end subroutine gedatsu_graph_get_edge_in_overlap_region_test

  subroutine gedatsu_graph_set_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_set_edge")

    call gedatsu_graph_set_edge(graph, n_edge, edge)
  end subroutine gedatsu_graph_set_edge_test

  subroutine gedatsu_graph_add_edge_test()
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint) :: n_edge
    integer(kint) :: edge(2,4)

    call monolis_std_log_string("gedatsu_graph_add_edge_test")

    call gedatsu_graph_add_edge(graph, n_edge, edge)
  end subroutine gedatsu_graph_add_edge_test
end module mod_gedatsu_graph_handler_test
