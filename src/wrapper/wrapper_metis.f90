!> metis ラッパーモジュール
module mod_gedatsu_wrapper_metis
  use mod_monolis_utils

  implicit none

contains

  !> @ingroup dev_graph_warp
  !> metis ラッパー関数（グラフ重みなし）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_part_graph_metis(n_vertex, index, item, n_part, part_id)
    implicit none
    !> [in] グラフのノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] graph の CSR 圧縮形式の index 配列
    integer(kint), intent(in) :: index(:)
    !> [in,out] graph の CSR 圧縮形式の item 配列
    integer(kint), intent(inout) :: item(:)
    !> [in] 分割数
    integer(kint), intent(in) :: n_part
    !> [out] 領域番号
    integer(kint), intent(out) :: part_id(:)
    integer(kint), allocatable :: node_wgt(:,:)
    integer(kint), allocatable :: edge_wgt(:,:)

    call gedatsu_part_graph_metis_with_weight(n_vertex, index, item, node_wgt, edge_wgt, n_part, part_id)
  end subroutine gedatsu_part_graph_metis

  !> @ingroup dev_graph_warp
  !> metis ラッパー関数（グラフ重みあり）
  !> @details 戻り値の領域番号は 0 オリジン
  subroutine gedatsu_part_graph_metis_with_weight(n_vertex, index, item, node_wgt, edge_wgt, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] グラフのノード数
    integer(kint), intent(in) :: n_vertex
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
    integer(kint) :: ncon, objval, nz
    integer(c_int), pointer :: index_c(:) => null()
    integer(c_int), pointer :: item_c(:) => null()
    integer(c_int), pointer :: node_wgt_c(:) => null()
    integer(c_int), pointer :: edge_wgt_c(:) => null()
    integer(c_int), pointer :: part_id_c(:) => null()
    integer(c_int), pointer :: vsize(:) => null()
    integer(c_int), pointer :: ubvec(:) => null()
    real(c_float), pointer :: options(:) => null()
    real(c_float), pointer :: tpwgts(:) => null()

    if(n_part /= 1)then
#ifdef NO_METIS
      call monolis_std_error_string("gedatsu_part_graph_metis_with_weight")
      call monolis_std_error_string("METIS is NOT enabled")
      call monolis_std_error_stop()
#else
      !# convert to 0 origin
      item = item - 1

      !# allocate section
      allocate(index_c(n_vertex+1), source = 0)
      index_c = index

      nz = index(n_vertex+1)
      allocate(item_c(nz), source = 0)
      item_c = item

      allocate(part_id_c(n_vertex), source = 0)

      if(allocated(node_wgt))then
        allocate(node_wgt_c(n_vertex), source = 0)
        node_wgt_c = node_wgt(1,:)
      else
        node_wgt_c => null()
      endif

      if(allocated(edge_wgt))then
        allocate(edge_wgt_c(nz), source = 0)
        edge_wgt_c = edge_wgt(1,:)
      else
        edge_wgt_c => null()
      endif

      ncon = 1

      !# metis call
      call METIS_PARTGRAPHRECURSIVE(n_vertex, ncon, index_c, item_c, &
        & node_wgt_c, vsize, edge_wgt_c, n_part, tpwgts, ubvec, options, objval, part_id_c)

      part_id = part_id_c

      !# deallocate section
      deallocate(index_c)
      deallocate(item_c)
      deallocate(part_id_c)
      if(associated(node_wgt_c)) deallocate(node_wgt_c)
      if(associated(edge_wgt_c)) deallocate(edge_wgt_c)

      !# convert to 1 origin
      item = item + 1
#endif
    endif
  end subroutine gedatsu_part_graph_metis_with_weight
end module mod_gedatsu_wrapper_metis
