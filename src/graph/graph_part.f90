module mod_gedatsu_graph_part
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

  implicit none

contains

  !> wrapper section

  subroutine gedatsu_part_nodal_graph(graph, n_domain)
    use iso_c_binding
    implicit none
    type(gedatsu_graph) :: graph
    integer(gint), pointer :: part_id(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(gint) :: i, n_domain

    call gedatsu_debug_header("gedatsu_part_nodal_graph")

    allocate(graph%vertex_domain_id(graph%n_vertex), source = 1)
    allocate(part_id(graph%n_vertex), source = 0)

    if(n_domain == 1) return

    call gedatsu_get_partitioned_graph(graph%n_vertex, graph%index, graph%item, n_domain, part_id)

    do i = 1, graph%n_vertex
      graph%vertex_domain_id(i) = part_id(i) + 1
    enddo

    deallocate(part_id)
  end subroutine gedatsu_part_nodal_graph

  !> main routines

  subroutine gedatsu_get_partitioned_graph(nnode, index, item, n_domain, part_id)
    use iso_c_binding
    implicit none
    integer(gint), pointer :: part_id(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(c_int), pointer :: node_wgt(:) => null()
    integer(c_int), pointer :: edge_wgt(:) => null()
    integer(gint) :: n_domain, nnode

    call gedatsu_debug_header("gedatsu_get_partitioned_graph")
    call gedatsu_part_graph_metis_kway(nnode, index, item, n_domain, node_wgt, edge_wgt, part_id)
  end subroutine gedatsu_get_partitioned_graph

  subroutine gedatsu_get_partitioned_graph_with_weight(nnode, index, item, n_domain, node_wgt_in, edge_wgt_in, part_id)
    use iso_c_binding
    implicit none
    integer(gint), pointer :: part_id(:), node_wgt_in(:), edge_wgt_in(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(c_int), pointer :: node_wgt(:)
    integer(c_int), pointer :: edge_wgt(:)
    integer(gint) :: n_domain, nnode

    call gedatsu_debug_header("gedatsu_get_partitioned_graph_with_weight")

    allocate(node_wgt(nnode), source = 0)
    allocate(edge_wgt(nnode), source = 0)
    node_wgt = node_wgt_in
    edge_wgt = edge_wgt_in

    call gedatsu_part_graph_metis_kway(nnode, index, item, n_domain, node_wgt, edge_wgt, part_id)

    deallocate(node_wgt)
    deallocate(edge_wgt)
  end subroutine gedatsu_get_partitioned_graph_with_weight

end module mod_gedatsu_graph_part
