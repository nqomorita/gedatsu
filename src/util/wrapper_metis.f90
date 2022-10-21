!> metis ラッパーモジュール
module mod_gedatsu_wrapper_metis
  use mod_gedatsu_prm
  use mod_gedatsu_util

  implicit none

contains

  !> metis ラッパー関数（グラフ重みなし）
  subroutine gedatsu_part_graph_metis(n_vertex, index, item, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint) :: n_vertex
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: index(:)
    !> [in] メモリ確保する配列
    integer(c_int), pointer :: item(:)
    !> [in] メモリ確保する配列
    integer(gint) :: n_part
    !> [in] メモリ確保する配列
    integer(gint), pointer :: part_id(:)

  end subroutine gedatsu_part_graph_metis

  !> metis ラッパー関数（グラフ重みあり）
  subroutine gedatsu_part_graph_metis_with_weight(n_vertex, index, item, node_wgt, edge_wgt, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint) :: n_vertex
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

  end subroutine gedatsu_part_graph_metis_with_weight

  !> metis ラッパーのメイン関数
  subroutine gedatsu_part_graph_metis_main(n_vertex, index, item, node_wgt, edge_wgt, n_part, part_id)
    use iso_c_binding
    implicit none
    !> [in] メモリ確保する配列
    integer(gint) :: n_vertex
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
    integer(gint) :: ncon, objval
    integer(gint), pointer :: vsize(:) => null()
    integer(gint), pointer :: ubvec(:) => null()
    real(gdouble), pointer :: options(:) => null()
    real(gdouble), pointer :: tpwgts(:) => null()

!#if WITH_METIS64
!    integer(c_int64_t) :: n_node8, ncon8, n_part8, objval8
!    integer(c_int64_t), pointer :: part_id8(:)
!    integer(c_int64_t), pointer :: node_wgt8(:)
!    integer(c_int64_t), pointer :: edge_wgt8(:) => null()
!    integer(c_int64_t), pointer :: vsize8(:) => null()
!    integer(c_int64_t), pointer :: ubvec8(:)  => null()
!    integer(c_int64_t), pointer :: index8(:), item8(:)
!#endif

    if(n_part /= 1)then
      ncon = 1
      !> convert to 0 origin
      item = item - 1

#ifdef NO_METIS
    call gedatsu_warning_header("gedatsu_part_graph_metis_main: METIS is NOT enabled")
    stop
#else
      call METIS_PARTGRAPHRECURSIVE(n_vertex, ncon, index, item, &
        & node_wgt, vsize, edge_wgt, n_part, tpwgts, ubvec, options, objval, part_id)
!#elif WITH_METIS64
!      ncon8 = 1
!      n_part8 = n_part
!      n_node8 = n_vertex
!      allocate(node_wgt8(n_vertex))
!      node_wgt8 = node_wgt
!      allocate(index8(n_vertex+1))
!      index8 = graph_index
!      allocate(item8(graph_index(n_vertex+1)))
!      item8 = item
!      allocate(part_id8(n_vertex))
!      call METIS_PARTGRAPHRECURSIVE(n_node8, ncon8, index8, item8, &
!        & node_wgt8, vsize8, edge_wgt8, n_part8, tpwgts, ubvec8, options, objval8, part_id8)
!      part_id = part_id8
#endif

      !> convert to 1 origin
      item = item + 1
    endif
  end subroutine gedatsu_part_graph_metis_main

end module mod_gedatsu_wrapper_metis
