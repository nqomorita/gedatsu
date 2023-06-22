program dlb_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none
  !> dlb 構造体
  type(gedatsu_dlb) :: dlb
  !> 分割後の graph 構造体
  type(gedatsu_graph) :: subgraph
  !> 分割数
  integer(kint) :: n_domain
  !> 入力ファイル名
  character(monolis_charlen) :: finame
  !> 出力ファイル名
  character(monolis_charlen) :: foname
  !> 出力ディレクトリ名
  character(monolis_charlen) :: fdname

!  fdname = "parted.0"
!
!  call gedatsu_global_initialize()
!
!  call gedatsu_get_arg_graph_partitioner(finame, n_domain)
!
!  !> IO section
!  foname = gedatsu_get_input_file_name(fdname, finame, gedatsu_mpi_global_my_rank())
!  call gedatsu_input_graph(foname, subgraph)
!
!  foname = gedatsu_get_input_file_name(fdname, "node.id", gedatsu_mpi_global_my_rank())
!  call gedatsu_input_node_id(foname, subgraph)
!
!  foname = gedatsu_get_input_file_name(fdname, "internal_node", gedatsu_mpi_global_my_rank())
!  call gedatsu_input_internal_node_number(foname, subgraph)
!
!  dlb%comm%comm = gedatsu_mpi_global_comm()
!
!  call gedatsu_dlb_analysis(dlb, subgraph)
!
!  call gedatsu_global_finalize()
end program dlb_test
