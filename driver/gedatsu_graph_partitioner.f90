program gedatsu_graph_partitioner
  use mod_gedatsu
  implicit none
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> 分割後の graph 構造体
  type(gedatsu_graph), allocatable :: subgraphs(:)
  !> 分割領域に対応する comm 構造体
  type(gedatsu_comm), allocatable :: comms(:)
  !> 分割数
  integer(gint) :: n_domain
  !> 入力ファイル名
  character(gedatsu_charlen) :: finame
  !> 出力ファイル名
  character(gedatsu_charlen) :: foname
  !> 出力ディレクトリ名
  character(gedatsu_charlen) :: fdname
  integer(gint) :: i

  fdname = "parted.0"

  call system('if [ ! -d parted.0 ]; then (echo "** create parted.0"; mkdir -p parted.0); fi')

  call gedatsu_log_string("start partitioning")
  call gedatsu_global_initialize()
  call gedatsu_get_arg_graph_partitioner(finame, n_domain)

  if(n_domain <= 1) stop

  call gedatsu_log_string("input graph")
  call gedatsu_input_graph(finame, graph)

  call gedatsu_log_string("graph partition")
  allocate(subgraphs(n_domain))
  call gedatsu_graph_partition(graph, n_domain, subgraphs)

  call gedatsu_log_string("generating communication table")
  allocate(comms(n_domain))
  call gedatsu_comm_get_comm_table_serial(graph, subgraphs, n_domain, comms)

  call gedatsu_log_string("output graph")
  do i = 1, n_domain
    foname = gedatsu_get_output_file_name(fdname, finame, i - 1)
    call gedatsu_output_graph(foname, subgraphs(i))

    foname = gedatsu_get_output_file_name(fdname, "node.id", i - 1)
    call gedatsu_output_node_id(foname, subgraphs(i))

    foname = gedatsu_get_output_file_name(fdname, "internal_node", i - 1)
    call gedatsu_output_internal_node_number(foname, subgraphs(i))

    foname = gedatsu_get_output_file_name(fdname, "gedatsu.send", i - 1)
    call gedatsu_output_send_comm_table(foname, comms(i))

    foname = gedatsu_get_output_file_name(fdname, "gedatsu.recv", i - 1)
    call gedatsu_output_recv_comm_table(foname, comms(i))
  enddo

  call gedatsu_log_string("end partitioning")
  call gedatsu_global_finalize()
end program gedatsu_graph_partitioner
