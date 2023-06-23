program dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none
  !> dlb 構造体
  type(gedatsu_dlb) :: dlb
  !> graph 構造体
  type(gedatsu_graph) :: graph
  !> コミュニケータ 構造体
  type(monolis_COM) :: COM
  !> 分割数
  integer(kint) :: n_domain
  !> 入力ファイル名
  character(monolis_charlen) :: finame
  integer(kint), allocatable :: node_wgt(:,:), edge_wgt(:,:)

  call monolis_mpi_initialize()
  call gedatsu_dlb_initialize(dlb)

  n_domain = monolis_mpi_get_global_comm_size()

  !> IO section
  finame = monolis_get_global_input_file_name(MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")
  call monolis_input_graph(finame, graph%n_vertex, graph%vertex_id, graph%index, graph%item)

  call monolis_com_initialize_by_parted_files(COM, monolis_mpi_get_global_comm(), &
    & MONOLIS_DEFAULT_TOP_DIR, MONOLIS_DEFAULT_PART_DIR, "graph.dat")

  !> repart section
  call gedatsu_dlb_analysis_with_weight(dlb, graph, COM, node_wgt, edge_wgt)

  call monolis_mpi_finalize()
end program dlb_test
