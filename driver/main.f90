program gedatsu_graph_partitioner
  use mod_gedatsu
  implicit none
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> 分割数
  integer(gint) :: n_domain
  !> 入力ファイル名
  character(gedatsu_charlen) :: fname

  call gedatsu_global_initialize()

  call gedatsu_get_arg_graph_partitioner(fname, n_domain)

  if(n_domain <= 1) stop

  call gedatsu_input_graph(fname, graph)

!  call monolis_part_nodal_graph(graph, n_domain)

!  call monolis_get_overlap_commtable_graph(graph, comm, node_list, n_domain)

!  call monolis_output_parted_nodal_graph(fname, graph, graph_format, comm, node_list, n_domain)

  call gedatsu_global_finalize()
end program gedatsu_graph_partitioner
