program gedatsu_convertor_simple_mesh2graph
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  !type(monolis_graph) :: graph
  !character :: fname*100

  !call monolis_global_initialize()

  !call monolis_convert_mesh_to_connectivity &
  !  & (mesh%nelem, mesh%nbase_func, mesh%elem, graph%ebase_func, graph%connectivity)

  !call monolis_convert_connectivity_to_nodal_graph &
  !  & (mesh%nnode, mesh%nelem, graph%ebase_func, graph%connectivity, index, item)

  !shift = 0
  !if(minval(mesh%nid) == 0) shift = -1 !> for C binding

  !fname = "graph.dat"
  !call output_graph_format(fname, mesh%nnode, mesh%nid, index, item, shift)

  !call monolis_global_finalize()
end program gedatsu_convertor_simple_mesh2graph
