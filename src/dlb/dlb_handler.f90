!> 動的負荷分散モジュール
module mod_gedatsu_dlb_handler
  use mod_gedatsu_graph
  use mod_gedatsu_graph_repart
  use mod_gedatsu_dlb
  use mod_gedatsu_dlb_comm
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための事前分析
  subroutine gedatsu_dlb_analysis_with_weight(dlb, graph, COM, node_wgt, edge_wgt)
    implicit none
    !> [out] dlb 構造体
    type(gedatsu_dlb), intent(out) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [in] ノード重み
    integer(kint), allocatable, intent(in) :: node_wgt(:,:)
    !> [in] エッジ重み
    integer(kint), allocatable, intent(in) :: edge_wgt(:,:)

    call monolis_com_get_n_internal_vertex(COM, graph%n_internal_vertex)

    call gedatsu_graph_repartition_with_weight(graph, COM, node_wgt, edge_wgt)
  end subroutine gedatsu_dlb_analysis_with_weight

  !> @ingroup group_dlb
  !> 負荷分散：負荷分散の実行チェック
  subroutine gedatsu_dlb_update_check(dlb, graph, should_update)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] graph 構造体
    logical :: should_update
    should_update = .true.
  end subroutine gedatsu_dlb_update_check

  !> @ingroup group_dlb
  !> 負荷分散：ノードグラフ情報のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_nodal_graph(dlb, graph_org, graph_new, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_org
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_new
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    type(gedatsu_update_db), allocatable :: update_db(:)
    integer(kint) :: comm_size

    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    allocate(update_db(comm_size))

    call gedatsu_dlb_get_comm_table_main(dlb, graph_org, update_db, COM)

    call gedatsu_dlb_update_nodal_graph_main(dlb, graph_org, graph_new, update_db, COM)
  end subroutine gedatsu_dlb_update_nodal_graph

  !> @ingroup group_dlb
  !> 負荷分散：付随グラフ情報のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_connectivity_graph(dlb, graph_org, graph_new, COM)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_org
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_new
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM

    !call gedatsu_dlb_update_graph_main(dlb, graph_org, graph_new, COM)
  end subroutine gedatsu_dlb_update_connectivity_graph

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_I_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデートする配列
    integer(kint) :: var_org(:)
    integer(kint) :: var_new(:)

  end subroutine gedatsu_dlb_update_I_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元実数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_R_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデートする配列
    real(kdouble) :: var_org(:)
    real(kdouble) :: var_new(:)

  end subroutine gedatsu_dlb_update_R_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元複素数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_C_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデートする配列
    complex(kdouble) :: var_org(:)
    complex(kdouble) :: var_new(:)

  end subroutine gedatsu_dlb_update_C_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元論理型配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_L_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデートする配列
    logical :: var_org(:)
    logical :: var_new(:)

  end subroutine gedatsu_dlb_update_L_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_I_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in] 元の配列
    integer(kint) :: var_send(:)
    !> [out] 負荷分散後の配列
    integer(kint) :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_I_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元実数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_R_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in] 元の配列
    real(kdouble) :: var_send(:)
    !> [out] 負荷分散後の配列
    real(kdouble) :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_R_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元複素数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_C_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in] 元の配列
    real(kdouble) :: var_send(:)
    !> [out] 負荷分散後の配列
    complex(kdouble) :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_C_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元論理型配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_L_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in] 元の配列
    logical :: var_send(:)
    !> [out] 負荷分散後の配列
    logical :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_L_1d
end module mod_gedatsu_dlb_handler
