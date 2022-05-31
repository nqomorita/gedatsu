module mod_gedatsu_graph_convert
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

  implicit none

contains

  subroutine gedatsu_convert_mesh_to_connectivity(nelem, nb, elem, ebase_func, connectivity)
    use iso_c_binding
    implicit none
    integer(gint) :: i, j, nb
    integer(gint) :: nelem, elem(:,:)
    integer(gint), pointer :: ebase_func(:), connectivity(:)

    allocate(ebase_func(nelem+1), source = 0)
    allocate(connectivity(nelem*nb), source = 0)

    do i = 1, nelem
      ebase_func(i+1) = i*nb
    enddo

    do i = 1, nelem
      do j = 1, nb
        connectivity(nb*(i-1) + j) = elem(j,i)
      enddo
    enddo
  end subroutine gedatsu_convert_mesh_to_connectivity

  subroutine gedatsu_convert_connectivity_to_nodal_graph &
    & (nnode, nelem, ebase_func, connectivity, index, item)
    use iso_c_binding
    implicit none
    integer(gint) :: nnode, nelem
    integer(gint), pointer :: ebase_func(:), connectivity(:)
    integer(c_int), pointer :: index(:), item(:)

    call gedatsu_convert_connectivity_to_nodal &
     & (nnode, nelem, ebase_func, connectivity, index, item)
  end subroutine gedatsu_convert_connectivity_to_nodal_graph

  subroutine gedatsu_convert_connectivity_to_nodal &
    & (nnode, nelem, ebase_func, connectivity, index, item)
    use iso_c_binding
    implicit none
    integer(gint) :: i, j, nnode, numflag
    integer(gint) :: nelem
    integer(gint), pointer :: ebase_func(:), connectivity(:)
    integer(c_int), pointer :: index(:), item(:)
    type(c_ptr) :: xadj, adjncy
#if WITH_METIS64
    integer(c_int64_t) :: nelem8, nnode8, numflag8
    integer(c_int64_t), pointer :: ebase_func8(:), connectivity8(:)
    integer(c_int64_t), pointer :: index8(:), item8(:)
#endif

    call gedatsu_debug_header("gedatsu_convert_connectivity_to_nodal")

    numflag = 0

    !> convert to 0 origin
    connectivity = connectivity - 1

#ifdef WITH_METIS
    call METIS_MESHTONODAL(nelem, nnode, ebase_func, connectivity, numflag, xadj, adjncy)
    call c_f_pointer(xadj, index, shape=[nnode+1])
    call c_f_pointer(adjncy, item, shape=[index(nnode+1)])
#elif WITH_METIS64
    nnode8 = nnode
    nelem8 = nelem
    numflag8 = numflag
    allocate(ebase_func8(nelem+1))
    allocate(connectivity8(ebase_func(nelem+1)))
    ebase_func8 = ebase_func
    connectivity8 = connectivity
    call METIS_MESHTONODAL(nelem8, nnode8, ebase_func8, connectivity8, numflag8, xadj, adjncy)
    call c_f_pointer(xadj, index8, shape=[nnode+1])
    call c_f_pointer(adjncy, item8, shape=[index8(nnode+1)])
    allocate(index(nnode+1))
    allocate(item(index8(nnode+1)))
    index = index8
    item = item8
#else
    call gedatsu_warning_header("gedatsu_convert_connectivity_to_nodal: METIS is NOT enabled")
    stop
#endif

    !> convert to 1 origin
    connectivity = connectivity + 1

    !> convert to 1 origin
    item = item + 1
  end subroutine gedatsu_convert_connectivity_to_nodal

end module mod_gedatsu_graph_convert
