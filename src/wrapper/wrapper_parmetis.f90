!> metis ラッパーモジュール
module mod_gedatsu_wrapper_parmetis
  use mod_monolis_utils

  implicit none

contains

  !> @ingroup dev_graph_warp
  !> parmetis ラッパー関数（グラフ重みなし）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_repart_graph_parmetis(n_vertex, vertex_id, &
    &  vtxdist, index, item, n_part, part_id, comm)
    implicit none
    !> [in] グラフのノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] グラフのノード id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] 領域ごとのノード数を示す配列
    integer(kint), intent(in) :: vtxdist(:)
    !> [in] graph の CSR 圧縮形式の index 配列
    integer(kint), intent(in) :: index(:)
    !> [in,out] graph の CSR 圧縮形式の item 配列
    integer(kint), intent(inout) :: item(:)
    !> [in] 分割数
    integer(kint), intent(in) :: n_part
    !> [out] 領域番号
    integer(kint), intent(out) :: part_id(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(kint), allocatable :: node_wgt(:,:)
    integer(kint), allocatable :: edge_wgt(:,:)

    call gedatsu_repart_graph_parmetis_with_weight(n_vertex, vertex_id, &
      & vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id, comm)
  end subroutine gedatsu_repart_graph_parmetis

  !> @ingroup dev_graph_warp
  !> parmetis ラッパー関数（グラフ重みあり）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_repart_graph_parmetis_with_weight(n_vertex, vertex_id, &
    & vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id, comm)
    use iso_c_binding
    implicit none
    !> [in] グラフのノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] グラフのノード id
    integer(kint), intent(in) :: vertex_id(:)
    !> [in] 領域ごとのノード数を示す配列
    integer(kint), intent(in) :: vtxdist(:)
    !> [in] graph の CSR 圧縮形式の index 配列
    integer(kint), intent(in) :: index(:)
    !> [in,out] graph の CSR 圧縮形式の item 配列
    integer(kint), intent(inout) :: item(:)
    !> [in] ノード重み
    integer(kint), allocatable, intent(in) :: node_wgt(:,:)
    !> [in] エッジ重み
    integer(kint), allocatable, intent(in) :: edge_wgt(:,:)
    !> [in] 分割数
    integer(kint), intent(in) :: n_part
    !> [out] 領域番号
    integer(kint), intent(out) :: part_id(:)
    !> [in] MPI コミュニケータ
    integer(kint), intent(in) :: comm
    integer(c_int) :: ncon, nz, nflag, wflag, i
    integer(c_int), pointer :: vtxdist_c(:) => null()
    integer(c_int), pointer :: index_c(:) => null()
    integer(c_int), pointer :: item_c(:) => null()
    integer(c_int), pointer :: node_wgt_c(:) => null()
    integer(c_int), pointer :: edge_wgt_c(:) => null()
    integer(c_int), pointer :: part_id_c(:) => null()
    integer(c_int), pointer :: vsize(:) => null()
    integer(c_int), pointer :: edgecut(:) => null()
    integer(c_int), pointer :: options(:) => null()
    real(c_float), pointer :: tpwgts(:) => null()
    real(c_float), pointer :: ubvec(:) => null()
    real(c_float), pointer :: itr(:) => null()

    if(n_part > 1)then
#ifdef NO_PARMETIS
      call gedatsu_error_string("gedatsu_repart_graph_parmetis_with_weight: ParMETIS is NOT enabled")
      stop
#else
      !# convert to 0 origin
      item = item - 1

      ncon = 1

      nflag = 0

      !# allocate section
      allocate(vtxdist_c(n_part+1), source = 0)
      vtxdist_c = vtxdist

      allocate(index_c(n_vertex+1), source = 0)
      index_c = index

      nz = index(n_vertex+1)
      allocate(item_c(nz), source = 0)
      do i = 1, nz
        item_c(i) = vertex_id(item(i) + 1) - 1
      enddo

      allocate(part_id_c(n_vertex), source = 0)

      !# No weight
      wflag = 0

      if(allocated(node_wgt))then
        !# Only node weight
        wflag = 2
        allocate(node_wgt_c(n_vertex), source = 0)
        node_wgt_c = node_wgt(1,:)
      else
        node_wgt_c => null()
      endif

      if(allocated(edge_wgt))then
        if(wflag == 2)then
          !# Both weight
          wflag = 3
        else
          !# Only edge weight
          wflag = 1
        endif
        allocate(edge_wgt_c(nz), source = 0)
        edge_wgt_c = edge_wgt(1,:)
      else
        edge_wgt_c => null()
      endif

      allocate(vsize(n_vertex), source = 0)

      allocate(tpwgts(ncon*n_part), source = 0.0)

      allocate(ubvec(ncon), source = 1.05)

      allocate(options(3), source = 0)

      allocate(edgecut(1), source = 0)

      allocate(itr(1), source = 0.1)

      do i = 1, n_part
        tpwgts(i) = 1.0/n_part
      enddo

      !# parmetis call
      call ParMETIS_V3_AdaptiveRepart(vtxdist_c, index_c, item_c, &
        & node_wgt_c, vsize, edge_wgt_c, wflag, nflag, ncon, &
        & n_part, tpwgts, ubvec, itr, options, edgecut, part_id_c, comm)

      part_id = part_id_c

      !# deallocate section
      deallocate(vtxdist_c)
      deallocate(index_c)
      deallocate(item_c)
      deallocate(part_id_c)
      if(associated(node_wgt_c)) deallocate(node_wgt_c)
      if(associated(edge_wgt_c)) deallocate(edge_wgt_c)
      deallocate(vsize)
      deallocate(tpwgts)
      deallocate(ubvec)
      deallocate(options)
      deallocate(edgecut)
      deallocate(itr)

      !# convert to 1 origin
      item = item + 1
#endif
    endif
  end subroutine gedatsu_repart_graph_parmetis_with_weight
end module mod_gedatsu_wrapper_parmetis
