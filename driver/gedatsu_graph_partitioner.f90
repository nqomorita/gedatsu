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
  !> 入力ファイル名（グラフファイル）
  character(gedatsu_charlen) :: fname_input_graph
  !> 入力ファイル名（nodal weight ファイル）
  character(gedatsu_charlen) :: fname_input_nodal_weight
  !> 入力ファイル名（edge weight ファイル）
  character(gedatsu_charlen) :: fname_input_edge_weight
  !> 出力ファイル名
  character(gedatsu_charlen) :: fname_output
  !> 出力ディレクトリ名
  character(gedatsu_charlen) :: fname_output_dirctory
  !> ノード重み
  integer(gint), allocatable :: node_wgt(:,:)
  !> エッジ重み
  integer(gint), allocatable :: edge_wgt(:,:)
  integer(gint) :: i, n_dof
  character(gedatsu_charlen) :: label
  logical :: is_nodal_weight
  logical :: is_edge_weight

  fname_output_dirctory = "parted.0"

  call system('if [ ! -d parted.0 ]; then (echo "** create parted.0"; mkdir -p parted.0); fi')

  call gedatsu_log_string("start partitioning")
  call gedatsu_global_initialize()
  call gedatsu_get_arg_graph_partitioner(fname_input_graph, n_domain)
  call gedatsu_get_arg_graph_partitioner_weight(fname_input_nodal_weight, is_nodal_weight, &
    & fname_input_edge_weight, is_edge_weight)

  if(n_domain <= 1) stop

  call gedatsu_log_string("input graph")
  call gedatsu_input_graph(fname_input_graph, graph)

  call gedatsu_log_string("graph partition")
  allocate(subgraphs(n_domain))

  if(is_nodal_weight .or. is_edge_weight)then
    if(is_nodal_weight) call gedatsu_input_distval_i(fname_input_nodal_weight, label, n_dof, node_wgt)
    if(is_edge_weight ) call gedatsu_input_distval_i(fname_input_edge_weight , label, n_dof, edge_wgt)
    call gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
  else
    call gedatsu_graph_partition(graph, n_domain, subgraphs)
  endif

  call gedatsu_log_string("generating communication table")
  allocate(comms(n_domain))
  call gedatsu_comm_get_comm_table_serial(graph, subgraphs, n_domain, comms)

  call gedatsu_log_string("output graph")
  do i = 1, n_domain
    fname_output = gedatsu_get_output_file_name(fname_output_dirctory, fname_input_graph, i - 1)
    call gedatsu_output_graph(fname_output, subgraphs(i))

    fname_output = gedatsu_get_output_file_name(fname_output_dirctory, "node.id", i - 1)
    call gedatsu_output_node_id(fname_output, subgraphs(i))

    fname_output = gedatsu_get_output_file_name(fname_output_dirctory, "internal_node", i - 1)
    call gedatsu_output_internal_node_number(fname_output, subgraphs(i))

    fname_output = gedatsu_get_output_file_name(fname_output_dirctory, "gedatsu.send", i - 1)
    call gedatsu_output_send_comm_table(fname_output, comms(i))

    fname_output = gedatsu_get_output_file_name(fname_output_dirctory, "gedatsu.recv", i - 1)
    call gedatsu_output_recv_comm_table(fname_output, comms(i))
  enddo

  call gedatsu_log_string("end partitioning")
  call gedatsu_global_finalize()
end program gedatsu_graph_partitioner
