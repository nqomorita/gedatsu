!> metis ラッパーモジュール
module mod_gedatsu_wrapper_parmetis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  !> parmetis ラッパー関数（グラフ重みなし）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_part_graph_parmetis(vtxdist, index, item, n_part, part_id)
    implicit none
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: vtxdist(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: index(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: item(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: part_id(:)

    integer(gint), allocatable :: node_wgt(:)
    integer(gint), allocatable :: edge_wgt(:)

    call gedatsu_part_graph_parmetis_with_weight(vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id)
  end subroutine gedatsu_part_graph_parmetis

  !> metis ラッパー関数（グラフ重みあり）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_part_graph_parmetis_with_weight(vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: vtxdist(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: index(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: item(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: node_wgt(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: edge_wgt(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: part_id(:)
    !> [in] メモリ確保する配列
    integer(gint), allocatable :: edgecut(:)
    !> [in] メモリ確保する配列
    !> [in] メモリ確保する配列
    integer(gint) :: comm

    integer(gint) :: ncon, objval, nz, nflag, wflag, n_vertex
    integer(c_int), pointer :: index_c(:) => null()
    integer(c_int), pointer :: item_c(:) => null()
    integer(c_int), pointer :: node_wgt_c(:) => null()
    integer(c_int), pointer :: edge_wgt_c(:) => null()
    integer(c_int), pointer :: part_id_c(:) => null()
    integer(c_int), pointer :: vsize(:) => null()
    integer(c_int), pointer :: ubvec(:) => null()
    real(gdouble), pointer :: options(:) => null()
    real(gdouble), pointer :: tpwgts(:) => null()

    if(n_part /= 1)then
#ifdef NO_PARMETIS
      call gedatsu_error_string("gedatsu_part_graph_parmetis_with_weight: ParMETIS is NOT enabled")
      stop
#else
      n_vertex = 0

      !> convert to 0 origin
      item = item - 1

      !> allocate section
      allocate(index_c(n_vertex+1), source = 0)
      index_c = index

      nz = index(n_vertex+1)
      allocate(item_c(nz), source = 0)
      item_c = item

      allocate(part_id_c(n_vertex), source = 0)

      if(allocated(node_wgt))then
        allocate(node_wgt_c(n_vertex), source = 0)
        node_wgt_c = node_wgt
      else
        node_wgt_c => null()
      endif

      if(allocated(edge_wgt))then
        allocate(edge_wgt_c(nz), source = 0)
        edge_wgt_c = edge_wgt
      else
        edge_wgt_c => null()
      endif

      ncon = 1

      !> parmetis call
      call ParMETIS_V3_PartKway(vtxdist, index, item, &
        & node_wgt, edge_wgt, wflag, nflag, ncon, n_part, tpwgts, ubvec, options, edgecut, part_id, comm)

      part_id = part_id_c

      !> deallocate section
      deallocate(index_c)
      deallocate(item_c)
      if(associated(node_wgt_c)) deallocate(node_wgt_c)
      if(associated(edge_wgt_c)) deallocate(edge_wgt_c)
      deallocate(part_id_c)

      !> convert to 1 origin
      item = item + 1
#endif
    endif
  end subroutine gedatsu_part_graph_parmetis_with_weight
end module mod_gedatsu_wrapper_parmetis
