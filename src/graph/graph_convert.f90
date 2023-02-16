!> グラフデータ変換モジュール
module mod_gedatsu_graph_convert
  use mod_monolis_utils
  use mod_gedatsu_graph

  implicit none

contains

  !> @ingroup graph_conv
  !> 単一メッシュ形式からコネクティビティグラフ形式に変換
  subroutine gedatsu_convert_simple_elem_to_connectivity_graph(n_elem, n_base, elem, index, item)
    implicit none
    !> 要素数
    integer(kint) :: n_elem
    !> 要素を構成する節点数
    integer(kint) :: n_base
    !> 要素
    integer(kint) :: elem(:,:)
    !> コネクティビティグラフの index 配列
    integer(kint), allocatable :: index(:)
    !> コネクティビティグラフの item 配列
    integer(kint), allocatable :: item(:)
    integer(kint) :: i, j, nb

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
  end subroutine gedatsu_convert_simple_elem_to_connectivity_graph

  !> @ingroup graph_conv
  !> 単一メッシュ形式から節点グラフ形式に変換
  subroutine gedatsu_convert_connectivity_graph_to_nodal_graph &
    & (n_node, n_elem, conn_index, conn_item, nodal_index, nodal_item)
    use iso_c_binding
    implicit none
    !> 節点数
    integer(kint) :: n_node
    !> 要素数
    integer(kint) :: n_elem
    !> コネクティビティグラフの index 配列
    integer(kint), allocatable :: conn_index(:)
    !> コネクティビティグラフの item 配列
    integer(kint), allocatable :: conn_item(:)
    !> 節点グラフの index 配列
    integer(kint), allocatable :: nodal_index(:)
    !> 節点グラフの item 配列
    integer(kint), allocatable :: nodal_item(:)
    integer(kint) :: numflag
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
#endif
  end subroutine gedatsu_convert_connectivity_graph_to_nodal_graph

end module mod_gedatsu_graph_convert
