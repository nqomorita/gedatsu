!> metis ラッパーモジュール
module mod_gedatsu_wrapper_parmetis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  !> parmetis ラッパー関数（グラフ重みなし）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_repart_graph_parmetis(n_vertex, vertex_id, &
    &  vtxdist, index, item, n_part, part_id, comm)
    implicit none
    !> [in] グラフのノード数
    integer(gint) :: n_vertex
    !> [in] グラフのノード id
    integer(gint) :: vertex_id(:)
    !> [in] 領域ごとのノード数を示す配列
    integer(gint), allocatable :: vtxdist(:)
    !> [in] graph の CSR 圧縮形式の index 配列
    integer(gint), allocatable :: index(:)
    !> [in] graph の CSR 圧縮形式の item 配列
    integer(gint), allocatable :: item(:)
    !> [in] 分割数
    integer(gint) :: n_part
    !> [in] 領域番号
    integer(gint), allocatable :: part_id(:)
    !> [in] MPI コミュニケータ
    integer(gint) :: comm

    integer(gint), allocatable :: node_wgt(:)
    integer(gint), allocatable :: edge_wgt(:)

vtxdist(1) = 0
vtxdist(2) = 3
vtxdist(3) = 6

    call gedatsu_repart_graph_parmetis_with_weight(n_vertex, vertex_id, &
      & vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id, comm)
  end subroutine gedatsu_repart_graph_parmetis

  !> parmetis ラッパー関数（グラフ重みあり）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_repart_graph_parmetis_with_weight(n_vertex, vertex_id, &
    & vtxdist, index, item, node_wgt, edge_wgt, n_part, part_id, comm)
    use iso_c_binding
    implicit none
    !> [in] グラフのノード数
    integer(gint) :: n_vertex
    !> [in] グラフのノード id
    integer(gint) :: vertex_id(:)
    !> [in] 領域ごとのノード数を示す配列
    integer(gint), allocatable :: vtxdist(:)
    !> [in] graph の CSR 圧縮形式の index 配列
    integer(gint), allocatable :: index(:)
    !> [in] graph の CSR 圧縮形式の item 配列
    integer(gint), allocatable :: item(:)
    !> [in] ノード重み
    integer(gint), allocatable :: node_wgt(:)
    !> [in] エッジ重み
    integer(gint), allocatable :: edge_wgt(:)
    !> [in] 分割数
    integer(gint) :: n_part
    !> [in] 領域番号
    integer(gint), allocatable :: part_id(:)
    !> [in] MPI コミュニケータ
    integer(gint) :: comm

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
      !> convert to 0 origin
      item = item - 1

      ncon = 1

      nflag = 0

      wflag = 0 !> No weight
      !wflag = 1 !> Only edge weight
      !wflag = 2 !> Only node weight
      !wflag = 3 !> Both weight

      !> allocate section
      allocate(vtxdist_c(n_part+1), source = 0)
      vtxdist_c = vtxdist

      allocate(index_c(n_vertex+1), source = 0)
      index_c = index

      nz = index(n_vertex+1)
      allocate(item_c(nz), source = 0)
      do i = 1, n_vertex
        item_c(i) = vertex_id(item(i) + 1)
      enddo

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

      allocate(vsize(n_vertex), source = 0)

      allocate(tpwgts(ncon*n_part), source = 0.0)

      allocate(ubvec(ncon), source = 1.05)

      allocate(options(3), source = 0)

      allocate(edgecut(1), source = 0)

      allocate(itr(1), source = 0.1)

      tpwgts(1) = 1.0/n_part
      tpwgts(2) = 1.0/n_part

write(*,*)"vtxdist_c", vtxdist_c
write(*,*)"index_c", index_c
write(*,*)"item_c", item_c

      !> parmetis call
      call ParMETIS_V3_AdaptiveRepart(vtxdist_c, index_c, item_c, &
        & node_wgt_c, vsize, edge_wgt_c, wflag, nflag, ncon, &
        & n_part, tpwgts, ubvec, itr, options, edgecut, part_id_c, comm)

write(*,*)"edgecut", edgecut
write(*,*)"part_id", part_id_c

      part_id = part_id_c

      !> deallocate section
      deallocate(vtxdist_c)
      deallocate(index_c)
      deallocate(item_c)
      if(associated(node_wgt_c)) deallocate(node_wgt_c)
      if(associated(edge_wgt_c)) deallocate(edge_wgt_c)
      deallocate(part_id_c)

      !> convert to 1 origin
      item = item + 1
#endif
    endif
  end subroutine gedatsu_repart_graph_parmetis_with_weight
end module mod_gedatsu_wrapper_parmetis
