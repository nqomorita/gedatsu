program parmetis_test
  use mod_gedatsu
  implicit none
  !> 分割後の graph 構造体
  type(gedatsu_graph) :: subgraph
  !> 分割数
  integer(gint) :: n_domain
  !> 入力ファイル名
  character(gedatsu_charlen) :: finame
  !> 出力ファイル名
  character(gedatsu_charlen) :: foname
  !> 出力ディレクトリ名
  character(gedatsu_charlen) :: fdname

  fdname = "parted.0"

  call gedatsu_global_initialize()

  call gedatsu_get_arg_graph_partitioner(finame, n_domain)

  foname = gedatsu_get_input_file_name(fdname, finame, gedatsu_mpi_global_my_rank())

  call gedatsu_input_graph(foname, subgraph)

  foname = gedatsu_get_input_file_name(fdname, "node.id", gedatsu_mpi_global_my_rank())

  call gedatsu_input_node_id(foname, subgraph)

  foname = gedatsu_get_input_file_name(fdname, "internal_node", gedatsu_mpi_global_my_rank())

  call gedatsu_input_internal_node_number(foname, subgraph)

  call gedatsu_graph_repartition(subgraph)

  call gedatsu_global_finalize()
end program parmetis_test
