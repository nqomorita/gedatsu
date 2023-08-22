program dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none
  !> dlb 構造体
  type(gedatsu_dlb) :: dlb_node
  type(gedatsu_dlb) :: dlb_conn
  !> graph 構造体
  type(gedatsu_graph) :: node_graph_org
  type(gedatsu_graph) :: node_graph_new
  type(gedatsu_graph) :: conn_graph_org
  type(gedatsu_graph) :: conn_graph_new
  !> 通信構造体
  type(monolis_COM) :: COM
  !> 領域数
  integer(kint) :: n_domain
  !> ファイル名
  character(monolis_charlen) :: finame
  integer(kint), allocatable :: node_wgt(:,:), edge_wgt(:,:)
  integer(kint), allocatable :: var_org(:), var_new(:)

  call monolis_mpi_initialize()
  call gedatsu_dlb_initialize(dlb_node)
  call gedatsu_dlb_initialize(dlb_conn)

  n_domain = monolis_mpi_get_global_comm_size()

  !> IO section
  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")
  call monolis_input_graph(finame, node_graph_org%n_vertex, node_graph_org%vertex_id, &
    & node_graph_org%index, node_graph_org%item)

  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat.id")
  call monolis_input_global_id(finame, node_graph_org%n_vertex, node_graph_org%vertex_id)

  call monolis_com_initialize_by_parted_files(COM, monolis_mpi_get_global_comm(), &
    & MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")

  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "conn.dat")
  call monolis_input_graph(finame, conn_graph_org%n_vertex, conn_graph_org%vertex_id, &
    & conn_graph_org%index, conn_graph_org%item)

  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "conn.dat.id")
  call monolis_input_global_id(finame, conn_graph_org%n_vertex, conn_graph_org%vertex_id)

  !> repart section
  call gedatsu_dlb_analysis_with_weight(dlb_node, node_graph_org, COM, node_wgt, edge_wgt)

  !> nodal graph
  call gedatsu_dlb_update_nodal_graph(dlb_node, node_graph_org, COM, node_graph_new)

  call monolis_alloc_I_1d(var_org, node_graph_org%n_vertex)
  call monolis_alloc_I_1d(var_new, node_graph_new%n_vertex)

  var_org = monolis_mpi_get_global_my_rank()
  var_new = monolis_mpi_get_global_my_rank()

  call gedatsu_dlb_update_I_1d(dlb_node, 1, var_org, var_new)

  !> connectivity graph
  call gedatsu_dlb_update_connectivity_graph(dlb_conn, node_graph_org, node_graph_new, &
    & conn_graph_org, COM, conn_graph_new)

!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%n_vertex", conn_graph_org%n_vertex
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%vertex_id", conn_graph_org%vertex_id
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%index", conn_graph_org%index
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_org%item", conn_graph_org%item

!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%n_vertex", conn_graph_new%n_vertex
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%vertex_id", conn_graph_new%vertex_id
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%index", conn_graph_new%index
!write(100+monolis_mpi_get_global_my_rank(),*)"conn_graph_new%item", conn_graph_new%item

  call monolis_dealloc_I_1d(var_org)
  call monolis_dealloc_I_1d(var_new)

  call monolis_alloc_I_1d(var_org, conn_graph_org%n_vertex)
  call monolis_alloc_I_1d(var_new, conn_graph_new%n_vertex)

  var_org = monolis_mpi_get_global_my_rank()
  var_new = monolis_mpi_get_global_my_rank()

!write(100+monolis_mpi_get_global_my_rank(),*)"var_org", var_org
!write(100+monolis_mpi_get_global_my_rank(),*)"var_new", var_new

  call gedatsu_dlb_update_I_1d(dlb_conn, 1, var_org, var_new)

!write(100+monolis_mpi_get_global_my_rank(),*)"var_org", var_org
!write(100+monolis_mpi_get_global_my_rank(),*)"var_new", var_new

  call monolis_mpi_finalize()
end program dlb_test
