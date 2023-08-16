program dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none
  !> dlb 構造体
  type(gedatsu_dlb) :: dlb
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> graph 構造体
  type(gedatsu_graph) :: graph_new
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
  call monolis_input_graph(finame, graph%n_vertex, graph%vertex_id, graph%index, graph%item)

  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat.id")
  call monolis_input_global_id(finame, graph%n_vertex, graph%vertex_id)

  call monolis_com_initialize_by_parted_files(COM, monolis_mpi_get_global_comm(), &
    & MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")

  !> repart section
  call gedatsu_dlb_analysis_with_weight(dlb, graph, COM, node_wgt, edge_wgt)

  call gedatsu_dlb_update_nodal_graph(dlb, graph, COM, graph_new)

  call monolis_alloc_I_1d(var_org, graph%n_vertex)
  call monolis_alloc_I_1d(var_new, graph_new%n_vertex)

  var_org = monolis_mpi_get_global_my_rank()
  var_new = monolis_mpi_get_global_my_rank()

  call gedatsu_dlb_update_I_1d(dlb, 1, var_org, var_new)

  call monolis_mpi_finalize()
end program dlb_test
