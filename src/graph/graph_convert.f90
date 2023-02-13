!> グラフデータ変換モジュール
module mod_gedatsu_graph_convert
  use mod_monolis_utils
  use mod_gedatsu_graph

  implicit none

contains

!  subroutine gedatsu_convert_single_elem_to_mesh(n_elem, nb, elem, mesh_index, mesh_item)
!    use iso_c_binding
!    implicit none
!    integer(gint) :: i, j, nb
!    integer(gint) :: n_elem, elem(:,:)
!    integer(gint), pointer :: mesh_index(:), mesh_item(:)
!
!    allocate(mesh_index(n_elem+1), source = 0)
!    allocate(mesh_item(n_elem*nb), source = 0)
!
!    do i = 1, n_elem
!      mesh_index(i+1) = i*nb
!    enddo
!
!    do i = 1, n_elem
!      do j = 1, nb
!        mesh_item(nb*(i-1) + j) = elem(j,i)
!      enddo
!    enddo
!  end subroutine gedatsu_convert_single_elem_to_mesh

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

end module mod_gedatsu_graph_convert
