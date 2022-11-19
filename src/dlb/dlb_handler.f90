!> 動的負荷分散モジュール
module mod_gedatsu_dlb_handler
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_util
  use mod_gedatsu_graph_repart
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための事前分析
  subroutine gedatsu_dlb_analysis(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    type(gedatsu_comm) :: comm

    call gedatsu_comm_get_comm_table_parallel(graph, comm)

    call gedatsu_graph_repartition(graph, comm)

    !call gedatsu_dlb_update_check(dlb, graph)

    !call gedatsu_dlb_get_comm_table(dlb, graph)
  end subroutine gedatsu_dlb_analysis

  !> @ingroup group_dlb
  !> 負荷分散：グラフ情報のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_graph(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in,out] graph 構造体
    type(gedatsu_graph) :: graph

  end subroutine gedatsu_dlb_update_graph

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_int_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    integer(gint), allocatable :: var(:)

  end subroutine gedatsu_dlb_update_int_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_real_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    real(gdouble), allocatable :: var(:)

  end subroutine gedatsu_dlb_update_real_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列のアップデート（配列のメモリ再確保）
  subroutine gedatsu_dlb_update_bool_1d(dlb, ndof, var)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in,out] アップデートする配列
    logical, allocatable :: var(:)

  end subroutine gedatsu_dlb_update_bool_1d

  !> @ingroup group_dlb
  !> 負荷分散：グラフ情報の情報送受信
  subroutine gedatsu_dlb_SendRecv_graph(dlb, graph_send, graph_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 元の graph 構造体
    type(gedatsu_graph) :: graph_send
    !> [out] 負荷分散後の graph 構造体
    type(gedatsu_graph) :: graph_recv

  end subroutine gedatsu_dlb_SendRecv_graph

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_int_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in] 元の配列
    integer(gint) :: var_send(:)
    !> [out] 負荷分散後の配列
    integer(gint) :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_int_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_real_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in] 元の配列
    real(gdouble) :: var_send(:)
    !> [out] 負荷分散後の配列
    real(gdouble) :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_real_1d

  !> @ingroup group_dlb
  !> 負荷分散：1 次元整数配列の情報送受信
  subroutine gedatsu_dlb_SendRecv_bool_1d(dlb, ndof, var_send, var_recv)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] 1 節点あたりの自由度
    integer(gint) :: ndof
    !> [in] 元の配列
    logical :: var_send(:)
    !> [out] 負荷分散後の配列
    logical :: var_recv(:)

  end subroutine gedatsu_dlb_SendRecv_bool_1d
end module mod_gedatsu_dlb_handler
