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
  subroutine gedatsu_dlb_update_check(dlb, graph, COM, should_update)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    !> [in] graph 構造体
    logical :: should_update
    should_update = .true.
  end subroutine gedatsu_dlb_update_check

  !> @ingroup group_dlb
  !> 負荷分散：ノードグラフ情報のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_nodal_graph(dlb, graph_org, COM, graph_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_org
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_new
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph_tmp
    !> [in] COM 構造体
    type(monolis_COM), intent(in) :: COM
    integer(kint), allocatable :: recv_global_id(:)
    integer(kint), allocatable :: recv_domain_org(:)
    integer(kint) :: comm_size

    comm_size = monolis_mpi_get_local_comm_size(COM%comm)

    call gedatsu_dlb_get_comm_table_main(dlb, graph_org, COM)

    call gedatsu_dlb_get_temporary_nodal_graph(dlb, graph_org, graph_tmp, &
      & recv_global_id, recv_domain_org, COM)

    call gedatsu_dlb_get_new_nodal_graph(graph_tmp, graph_new, COM)

    call gedatsu_dlb_get_comm_table_modify(dlb, graph_tmp, graph_new, &
      & recv_global_id, recv_domain_org, COM)
  end subroutine gedatsu_dlb_update_nodal_graph

  !> @ingroup group_dlb
  !> 負荷分散：付随グラフ情報のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_connectivity_graph(dlb, graph_org, COM, graph_new)
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
    !> [in,out] アップデート前の配列
    integer(kint) :: var_org(:)
    !> [in,out] アップデート後の配列
    integer(kint) :: var_new(:)

    call monolis_SendRecv_I(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & var_org, var_new, ndof, dlb%COM_node%comm)
  end subroutine gedatsu_dlb_update_I_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元実数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_R_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデート前の配列
    real(kdouble) :: var_org(:)
    !> [in,out] アップデート後の配列
    real(kdouble) :: var_new(:)

    call monolis_SendRecv_R(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & var_org, var_new, ndof, dlb%COM_node%comm)
  end subroutine gedatsu_dlb_update_R_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元複素数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_C_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデート前の配列
    complex(kdouble) :: var_org(:)
    !> [in,out] アップデート後の配列
    complex(kdouble) :: var_new(:)

    call monolis_SendRecv_C(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & var_org, var_new, ndof, dlb%COM_node%comm)
  end subroutine gedatsu_dlb_update_C_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元論理型配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_L_1d(dlb, ndof, var_org, var_new)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(kint) :: ndof
    !> [in,out] アップデート前の配列
    logical :: var_org(:)
    !> [in,out] アップデート後の配列
    logical :: var_new(:)

    call monolis_SendRecv_L(dlb%COM_node%send_n_neib, dlb%COM_node%send_neib_pe, &
       & dlb%COM_node%recv_n_neib, dlb%COM_node%recv_neib_pe, &
       & dlb%COM_node%send_index, dlb%COM_node%send_item, &
       & dlb%COM_node%recv_index, dlb%COM_node%recv_item, &
       & var_org, var_new, ndof, dlb%COM_node%comm)
  end subroutine gedatsu_dlb_update_L_1d
end module mod_gedatsu_dlb_handler
