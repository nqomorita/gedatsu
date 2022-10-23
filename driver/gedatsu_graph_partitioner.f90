program gedatsu_graph_partitioner
  use mod_gedatsu
  implicit none
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> 分割後の graph 構造体
  type(gedatsu_graph), allocatable :: subgraphs(:)
  !> 分割数
  integer(gint) :: n_domain
  !> 入力ファイル名
  character(gedatsu_charlen) :: fname

  call gedatsu_global_initialize()

  call gedatsu_get_arg_graph_partitioner(fname, n_domain)

  if(n_domain <= 1) stop

  call gedatsu_input_graph(fname, graph)

  call gedatsu_graph_partition(graph, n_domain, subgraphs)

!  call monolis_get_commnication_table(graph, comm, node_list, n_domain)

!  call monolis_output_parted_nodal_graph(fname, graph, graph_format, comm, node_list, n_domain)

  call gedatsu_global_finalize()
end program gedatsu_graph_partitioner