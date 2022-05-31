module mod_gedatsu_graph_part
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  subroutine gedatsu_part_graph(mesh, graph, n_domain)
    use iso_c_binding
    implicit none
    type(gedatsu_mesh) :: mesh
    type(gedatsu_graph) :: graph
    integer(kint), pointer :: part_id(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(kint) :: i, n_domain

    call gedatsu_debug_header("gedatsu_part_graph")

    allocate(graph%node_domid_raw(mesh%nnode), source = 1)
    allocate(part_id(mesh%nnode), source = 0)

    if(n_domain == 1) return

    call gedatsu_convert_mesh_to_connectivity &
     & (mesh%nelem, mesh%nbase_func, mesh%elem, graph%ebase_func, graph%connectivity)

    call gedatsu_convert_connectivity_to_nodal_graph &
     & (mesh%nnode, mesh%nelem, graph%ebase_func, graph%connectivity, index, item)

    call gedatsu_get_partitioned_graph(mesh%nnode, index, item, n_domain, part_id)

    graph%nnode = mesh%nnode
    graph%nelem = mesh%nelem
    do i = 1, mesh%nnode
      graph%node_domid_raw(i) = part_id(i) + 1
    enddo

    deallocate(part_id)
  end subroutine gedatsu_part_graph

  subroutine gedatsu_part_nodal_graph(graph, n_domain)
    use iso_c_binding
    implicit none
    type(gedatsu_graph) :: graph
    integer(kint), pointer :: part_id(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(kint) :: i, n_domain

    call gedatsu_debug_header("gedatsu_part_nodal_graph")

    allocate(graph%node_domid_raw(graph%N), source = 1)
    allocate(part_id(graph%N), source = 0)

    if(n_domain == 1) return

    call gedatsu_get_partitioned_graph(graph%N, graph%index, graph%item, n_domain, part_id)

    do i = 1, graph%N
      graph%node_domid_raw(i) = part_id(i) + 1
    enddo

    deallocate(part_id)
  end subroutine gedatsu_part_nodal_graph

  subroutine gedatsu_get_partitioned_graph(nnode, index, item, n_domain, part_id)
    use iso_c_binding
    implicit none
    integer(kint), pointer :: part_id(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(c_int), pointer :: node_wgt(:) => null()
    integer(c_int), pointer :: edge_wgt(:) => null()
    integer(kint) :: n_domain, nnode

    call gedatsu_debug_header("gedatsu_get_partitioned_graph")
    call gedatsu_get_mesh_part_kway(nnode, index, item, n_domain, node_wgt, edge_wgt, part_id)
  end subroutine gedatsu_get_partitioned_graph

  subroutine gedatsu_get_partitioned_graph_with_node_weight(nnode, index, item, n_domain, wgt, part_id)
    use iso_c_binding
    implicit none
    integer(kint), pointer :: part_id(:), wgt(:)
    integer(c_int), pointer :: index(:), item(:)
    integer(c_int), pointer :: node_wgt(:)
    integer(c_int), pointer :: edge_wgt(:) => null()
    integer(kint) :: n_domain, nnode

    call gedatsu_debug_header("gedatsu_get_partitioned_graph_with_node_weight")
    allocate(node_wgt(nnode), source = 0)
    node_wgt = wgt

    call gedatsu_get_mesh_part_kway(nnode, index, item, n_domain, node_wgt, edge_wgt, part_id)

    deallocate(node_wgt)
  end subroutine gedatsu_get_partitioned_graph_with_node_weight

end module mod_gedatsu_graph_part
