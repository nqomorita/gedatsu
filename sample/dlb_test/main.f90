program dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none
  !> dlb 構造体
  type(gedatsu_dlb) :: dlb
  !> graph 構造体
  type(gedatsu_graph) :: node_graph_org
  !> graph 構造体
  type(gedatsu_graph) :: node_graph_new
  type(gedatsu_graph) :: conn_graph_org
  type(gedatsu_graph) :: conn_graph_new
  !> コミュニケータ 構造体
  type(monolis_COM) :: COM
  !> 分割数
  integer(kint) :: n_domain
  !> 入力ファイル名
  character(monolis_charlen) :: finame
  integer(kint), allocatable :: node_wgt(:,:), edge_wgt(:,:)
  integer(kint), allocatable :: var_org(:), var_new(:)

  call monolis_mpi_initialize()
  call gedatsu_dlb_initialize(dlb)

  n_domain = monolis_mpi_get_global_comm_size()

  !> IO section
  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")
  call monolis_input_graph(finame, node_graph_org%n_vertex, node_graph_org%vertex_id, &
    & node_graph_org%index, node_graph_org%item)

  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat.id")
  call monolis_input_global_id(finame, node_graph_org%n_vertex, node_graph_org%vertex_id)

  call monolis_com_initialize_by_parted_files(COM, monolis_mpi_get_global_comm(), &
    & MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")

  !> repart section
  write(*,*)"gedatsu_dlb_analysis_with_weight"
  call gedatsu_dlb_analysis_with_weight(dlb, node_graph_org, COM, node_wgt, edge_wgt)

  !> nodal graph
  write(*,*)"gedatsu_dlb_update_nodal_graph"
  call gedatsu_dlb_update_nodal_graph(dlb, node_graph_org, COM, node_graph_new)

  call monolis_alloc_I_1d(var_org, node_graph_org%n_vertex)
  call monolis_alloc_I_1d(var_new, node_graph_new%n_vertex)

  call gedatsu_dlb_update_I_1d(dlb, 1, var_org, var_new)

  !> connectivity graph
  write(*,*)"gedatsu_dlb_update_connectivity_graph"
  call gedatsu_dlb_update_connectivity_graph(dlb, node_graph_org, conn_graph_org, COM, conn_graph_new)

  call monolis_mpi_finalize()
end program dlb_test
