module mod_gedatsu_wrapper_metis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

!  subroutine gedatsu_convert_mesh_to_nodal_graph &
!    & (n_node, n_elem, mesh_index, mesh_item, graph_index, graph_item)
!    use iso_c_binding
!    implicit none
!    integer(gint) :: i, j, n_node, numflag
!    integer(gint) :: n_elem
!    integer(gint), pointer :: mesh_index(:), mesh_item(:)
!    integer(c_int), pointer :: graph_index(:), graph_item(:)
!    type(c_ptr) :: xadj, adjncy
!#if WITH_METIS64
!    integer(c_int64_t) :: n_elem8, n_node8, numflag8
!    integer(c_int64_t), pointer :: mesh_index8(:), mesh_item8(:)
!    integer(c_int64_t), pointer :: index8(:), item8(:)
!#endif
!
!    call gedatsu_debug_header("gedatsu_convert_mesh_to_nodal_graph")
!
!    numflag = 0
!
!    !> convert to 0 origin
!    mesh_item = mesh_item - 1
!
!#ifdef WITH_METIS
!    call METIS_MESHTONODAL(n_elem, n_node, mesh_index, mesh_item, numflag, xadj, adjncy)
!    call c_f_pointer(xadj, graph_index, shape=[n_node+1])
!    call c_f_pointer(adjncy, graph_item, shape=[graph_index(n_node+1)])
!#elif WITH_METIS64
!    n_node8 = n_node
!    n_elem8 = n_elem
!    numflag8 = numflag
!    allocate(mesh_index8(n_elem+1))
!    allocate(mesh_item8(mesh_index(n_elem+1)))
!    mesh_index8 = mesh_index
!    mesh_item8 = mesh_item
!    call METIS_MESHTONODAL(n_elem8, n_node8, mesh_index8, mesh_item8, numflag8, xadj, adjncy)
!    call c_f_pointer(xadj, index8, shape=[n_node+1])
!    call c_f_pointer(adjncy, item8, shape=[index8(n_node+1)])
!    allocate(graph_index(n_node+1))
!    allocate(graph_item(index8(n_node+1)))
!    graph_index = index8
!    graph_item = item8
!#else
!    call gedatsu_warning_header("gedatsu_convert_mesh_to_nodal_graph: METIS is NOT enabled")
!    stop
!#endif
!
!    !> convert to 1 origin
!    mesh_item = mesh_item + 1
!
!    !> convert to 1 origin
!    graph_item = graph_item + 1
!  end subroutine gedatsu_convert_mesh_to_nodal_graph
!
!  subroutine gedatsu_part_graph_metis_kway(n_vertex, graph_index, graph_item, n_part, node_wgt, edge_wgt, part_id)
!    use iso_c_binding
!    implicit none
!    integer(gint) :: n_vertex, ncon, n_part, objval
!    integer(gint), pointer :: part_id(:)
!    integer(c_int), pointer :: node_wgt(:)
!    integer(c_int), pointer :: edge_wgt(:)
!    integer(gint), pointer :: vsize(:) => null()
!    integer(gint), pointer :: ubvec(:) => null()
!    real(gdouble), pointer :: options(:) => null()
!    real(gdouble), pointer :: tpwgts(:) => null()
!    integer(c_int), pointer :: graph_index(:), graph_item(:)
!#if WITH_METIS64
!    integer(c_int64_t) :: n_node8, ncon8, n_part8, objval8
!    integer(c_int64_t), pointer :: part_id8(:)
!    integer(c_int64_t), pointer :: node_wgt8(:)
!    integer(c_int64_t), pointer :: edge_wgt8(:) => null()
!    integer(c_int64_t), pointer :: vsize8(:) => null()
!    integer(c_int64_t), pointer :: ubvec8(:)  => null()
!    integer(c_int64_t), pointer :: index8(:), item8(:)
!#endif
!
!    call gedatsu_debug_header("gedatsu_part_graph_metis_kway")
!
!    if(n_part /= 1)then
!      ncon = 1
!      !> convert to 0 origin
!      graph_item = graph_item - 1
!
!#ifdef WITH_METIS
!      call METIS_PARTGRAPHRECURSIVE(n_vertex, ncon, graph_index, graph_item, &
!        & node_wgt, vsize, edge_wgt, n_part, tpwgts, ubvec, options, objval, part_id)
!#elif WITH_METIS64
!      ncon8 = 1
!      n_part8 = n_part
!      n_node8 = n_vertex
!      allocate(node_wgt8(n_vertex))
!      node_wgt8 = node_wgt
!      allocate(index8(n_vertex+1))
!      index8 = graph_index
!      allocate(item8(graph_index(n_vertex+1)))
!      item8 = item
!      allocate(part_id8(n_vertex))
!      call METIS_PARTGRAPHRECURSIVE(n_node8, ncon8, index8, item8, &
!        & node_wgt8, vsize8, edge_wgt8, n_part8, tpwgts, ubvec8, options, objval8, part_id8)
!      part_id = part_id8
!#else
!    call gedatsu_warning_header("gedatsu_part_graph_metis_kway: METIS is NOT enabled")
!    stop
!#endif
!
!      !> convert to 1 origin
!      graph_item = graph_item + 1
!    endif
!  end subroutine gedatsu_part_graph_metis_kway

end module mod_gedatsu_wrapper_metis
