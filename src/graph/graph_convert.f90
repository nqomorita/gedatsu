!> グラフデータ変換モジュール
module mod_gedatsu_graph_convert
  use mod_monolis_utils
  use mod_gedatsu_graph

  implicit none

contains

  !> @ingroup graph_conv
  !> 単一メッシュ形式からコネクティビティグラフ形式に変換
  subroutine gedatsu_convert_simple_mesh_to_connectivity_graph(n_elem, n_base, elem, index, item)
    implicit none
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] 要素を構成する形状関数の数
    integer(kint), intent(in) :: n_base
    !> [in] 要素コネクティビティ
    integer(kint), intent(in) :: elem(:,:)
    !> [out] コネクティビティグラフの index 配列
    integer(kint), allocatable, intent(out) :: index(:)
    !> [out] コネクティビティグラフの item 配列
    integer(kint), allocatable, intent(out) :: item(:)
    integer(kint) :: i, j

    call monolis_alloc_I_1d(index, n_elem + 1)
    call monolis_alloc_I_1d(item, n_elem*n_base)

    do i = 1, n_elem
      index(i + 1) = i*n_base
    enddo

    do i = 1, n_elem
      do j = 1, n_base
        item(n_base*(i-1) + j) = elem(j,i)
      enddo
    enddo
  end subroutine gedatsu_convert_simple_mesh_to_connectivity_graph

  !> @ingroup graph_conv
  !> 単一メッシュ形式から節点グラフ形式に変換
  subroutine gedatsu_convert_connectivity_graph_to_nodal_graph( &
    & n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)
    use iso_c_binding
    implicit none
    !> [in] 節点数
    integer(kint), intent(in) :: n_node
    !> [in] 要素数
    integer(kint), intent(in) :: n_elem
    !> [in] コネクティビティグラフの index 配列
    integer(kint), intent(in) :: conn_index(:)
    !> [in,out] コネクティビティグラフの item 配列
    integer(kint), intent(inout) :: conn_item(:)
    !> [out] 節点グラフの index 配列
    integer(kint), allocatable, intent(out) :: nodal_index(:)
    !> [out] 節点グラフの item 配列
    integer(kint), allocatable :: nodal_item(:)
    integer(kint) :: i, jS, jE, numflag
    integer(c_int), pointer :: index_c(:) => null()
    integer(c_int), pointer :: item_c(:) => null()
    type(c_ptr) :: xadj, adjncy
!#if WITH_METIS64
!    integer(c_int64_t) :: n_elem8, n_node8, numflag8
!    integer(c_int64_t), pointer :: mesh_index8(:), mesh_item8(:)
!    integer(c_int64_t), pointer :: index8(:), item8(:)
!#endif

#ifdef NO_METIS
      call monolis_std_error_string("gedatsu_convert_connectivity_graph_to_nodal_graph")
      call monolis_std_error_string("METIS is NOT enabled")
      call monolis_std_error_stop()
#else
    numflag = 0

    !> convert to 0 origin
    conn_item = conn_item - 1

!#ifdef WITH_METIS
    call METIS_MESHTONODAL(n_elem, n_node, conn_index, conn_item, numflag, xadj, adjncy)

    call c_f_pointer(xadj, index_c, shape = [n_node + 1])

    call c_f_pointer(adjncy, item_c, shape = [index_c(n_node + 1)])

    call monolis_alloc_I_1d(nodal_index, n_node + 1)

    call monolis_alloc_I_1d(nodal_item, index_c(n_node + 1))

    nodal_index = index_c

    nodal_item = item_c

    deallocate(index_c)

    deallocate(item_c)
!#elif WITH_METIS64
!    n_node8 = n_node
!    n_elem8 = n_elem
!    numflag8 = numflag
!    allocate(mesh_index8(n_elem+1))
!    allocate(mesh_item8(mesh_index(n_elem+1)))
!    mesh_index8 = mesh_index
!    mesh_item8 = mesh_item
!    call METIS_MESHTONODAL(n_elem8, n_node8, mesh_index8, mesh_item8, numflag8, xadj, adjncy)
!    call c_f_pointer(xadj, index8, shape=[n_node+1])
!    call c_f_pointer(adjncy, item8, shape=[index8(n_node+1)])
!    allocate(graph_index(n_node+1))
!    allocate(graph_item(index8(n_node+1)))
!    graph_index = index8
!    graph_item = item8
!#endif

    !> convert to 1 origin
    conn_item = conn_item + 1

    !> convert to 1 origin
    nodal_item = nodal_item + 1

    do i = 1, n_node
      jS = nodal_index(i) + 1
      jE = nodal_index(i + 1)
      call monolis_qsort_I_1d(nodal_item, jS, jE)
    enddo
#endif
  end subroutine gedatsu_convert_connectivity_graph_to_nodal_graph

  !> @ingroup dev_graph_part
  !> 節点グラフとコネクティビティグラフが有効か比較
  !> 無向グラフの入力が前提のアルゴリズム
  subroutine gedatsu_check_connectivity_graph(node, conn, is_valid)
    implicit none
    !> [in] 節点グラフ
    type(gedatsu_graph), intent(in) :: node
    !> [in] コネクティビティグラフ
    type(gedatsu_graph), intent(in) :: conn
    !> [out] コネクティビティグラフの有効フラグ
    logical, intent(out) :: is_valid
    integer(kint) :: i, j, jS, jE, k, kS, kE, kn
    integer(kint) :: i1, i2

    is_valid = .false.

    do i = 1, conn%n_vertex
      jS = conn%index(i) + 1
      jE = conn%index(i + 1)
      aa:do j = jS + 1, jE
        i1 = conn%item(jS)
        i2 = conn%item(j)
        if(i1 == i2) cycle aa
        kS = node%index(i1) + 1
        kE = node%index(i1 + 1)
        do k = kS, kE
          kn = node%item(k)
          if(i2 == kn)then
            cycle aa
          endif
        enddo
        return
      enddo aa
    enddo

    is_valid = .true.
  end subroutine gedatsu_check_connectivity_graph
end module mod_gedatsu_graph_convert
