module mod_gedatsu_wrapper_metis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  subroutine gedatsu_get_mesh_part_kway(nnode, index, item, npart, node_wgt, edge_wgt, part_id)
    use iso_c_binding
    implicit none
    integer(gint) :: nnode, ncon, npart, objval
    integer(gint), pointer :: part_id(:)
    integer(c_int), pointer :: node_wgt(:)
    integer(c_int), pointer :: edge_wgt(:)
    integer(gint), pointer :: vsize(:) => null()
    integer(gint), pointer :: ubvec(:) => null()
    real(gdouble), pointer :: options(:) => null()
    real(gdouble), pointer :: tpwgts(:) => null()
    integer(c_int), pointer :: index(:), item(:)
#if WITH_METIS64
    integer(c_int64_t) :: nnode8, ncon8, npart8, objval8
    integer(c_int64_t), pointer :: part_id8(:)
    integer(c_int64_t), pointer :: node_wgt8(:)
    integer(c_int64_t), pointer :: edge_wgt8(:) => null()
    integer(c_int64_t), pointer :: vsize8(:) => null()
    integer(c_int64_t), pointer :: ubvec8(:)  => null()
    integer(c_int64_t), pointer :: index8(:), item8(:)
#endif

    call gedatsu_debug_header("gedatsu_get_mesh_part_kway")

    if(npart /= 1)then
      ncon = 1
      !> convert to 0 origin
      item = item - 1

#ifdef WITH_METIS
      call METIS_PARTGRAPHRECURSIVE(nnode, ncon, index, item, node_wgt, vsize, edge_wgt, npart, tpwgts, ubvec, &
        & options, objval, part_id)
#elif WITH_METIS64
      ncon8 = 1
      npart8 = npart
      nnode8 = nnode
      allocate(node_wgt8(nnode))
      node_wgt8 = node_wgt
      allocate(index8(nnode+1))
      index8 = index
      allocate(item8(index(nnode+1)))
      item8 = item
      allocate(part_id8(nnode))
      call METIS_PARTGRAPHRECURSIVE(nnode8, ncon8, index8, item8, node_wgt8, vsize8, edge_wgt8, npart8, tpwgts, ubvec8, &
        & options, objval8, part_id8)
      part_id = part_id8
#else
    call gedatsu_warning_header("gedatsu_get_mesh_part_kway: METIS is NOT enabled")
    stop
#endif

      !> convert to 1 origin
      item = item + 1
    endif
  end subroutine gedatsu_get_mesh_part_kway

end module mod_gedatsu_wrapper_metis
