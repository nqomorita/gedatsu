program gedatsu_graph_partitioner
  use mod_gedatsu
  implicit none
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> 分割数
  integer(gint) :: n_domain

  call gedatsu_global_initialize()

!  call monolis_get_nodal_graph_part_arg(fname, n_domain)

!  if(n_domain <= 1) return

!  call monolis_input_graph(fname, graph_format)

!  call monolis_get_graph_from_graph_format(graph_format, graph)

!  call monolis_part_nodal_graph(graph, n_domain)

!  call monolis_get_overlap_commtable_graph(graph, comm, node_list, n_domain)

!  call monolis_output_parted_nodal_graph(fname, graph, graph_format, comm, node_list, n_domain)

!  call monolis_part_finalize()

  call gedatsu_global_finalize()
end program gedatsu_graph_partitioner
