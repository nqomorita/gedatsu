!> metis ラッパーモジュール
module mod_gedatsu_wrapper_parmetis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  !> metis ラッパー関数（グラフ重みなし）
  subroutine gedatsu_part_graph_parmetis(n_vertex, index, item, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint) :: n_vertex
    !> [in] メモリ確保する配列
    integer(gint), pointer :: index(:)
    !> [in] メモリ確保する配列
    integer(gint), pointer :: item(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), pointer :: part_id(:)

  end subroutine gedatsu_part_graph_parmetis

  !> metis ラッパー関数（グラフ重みあり）
  subroutine gedatsu_part_graph_parmetis_with_weight(n_vertex, index, item, node_wgt, edge_wgt, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint) :: n_vertex
    !> [in] メモリ確保する配列
    integer(gint), pointer :: index(:)
    !> [in] メモリ確保する配列
    integer(gint), pointer :: item(:)
    !> [in] メモリ確保する配列
    integer(gint), pointer :: node_wgt(:)
    !> [in] メモリ確保する配列
    integer(gint), pointer :: edge_wgt(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), pointer :: part_id(:)

  end subroutine gedatsu_part_graph_parmetis_with_weight

  !> metis ラッパーのメイン関数
  subroutine gedatsu_part_graph_parmetis_main(vtxdist, index, item, node_wgt, edge_wgt, comm, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(c_int) :: vtxdist(:)
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: index(:)
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: item(:)
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: node_wgt(:)
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: edge_wgt(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), pointer :: part_id(:)
    !> [in] メモリ確保する配列
    integer(gint) :: comm
    integer(gint) :: ncon
    integer(gint), pointer :: wflag(:) => null()
    integer(gint), pointer :: nflag(:) => null()
    integer(gint), pointer :: edgecut(:) => null()
    integer(gint), pointer :: vsize(:) => null()
    integer(gint), pointer :: ubvec(:) => null()
    real(gdouble), pointer :: options(:) => null()
    real(gdouble), pointer :: tpwgts(:) => null()

    if(n_part /= 1)then
      ncon = 1
      !> convert to 0 origin
      item = item - 1

#ifdef NO_PARMETIS
    call gedatsu_warning_header("gedatsu_part_graph_parmetis_main: PARMETIS is NOT enabled")
    stop
#else
      call ParMETIS_V3_PartKway(vtxdist, index, item, &
        & node_wgt, edge_wgt, wflag, nflag, ncon, n_part, tpwgts, ubvec, options, edgecut, part_id, comm)
#endif

      !> convert to 1 origin
      item = item + 1
    endif
  end subroutine gedatsu_part_graph_parmetis_main
end module mod_gedatsu_wrapper_parmetis
