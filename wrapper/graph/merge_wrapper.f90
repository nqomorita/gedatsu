module mod_gedatsu_graph_merge_wrapper
  ! use mod_monolis_utils
  use mod_gedatsu_graph_merge
  use iso_c_binding
  implicit none

contains

  !> vertex_idなどの一次元配列をFortran側のgraphs構造体に再構成するための中間層関数
  !> 引数は全てallocateされている前提
  subroutine gedatsu_merge_distval_R_c( &
  & sum_n_vertex, sum_index, sum_item, sum_list_struct_R_n, sum_merged_n_dof_list, &
  & n_graphs, &
  & n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, &
  & merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, merged_index, merged_item, &
  & n_dof_list_n, n_dof_list_array, list_struct_R_n, list_struct_R_array, &
  & merged_n_dof_list, merged_array_R) &
  & bind(c, name = "gedatsu_merge_distval_R_c")
    implicit none
    integer(c_int), intent(in), value :: sum_n_vertex, sum_index, sum_item, sum_list_struct_R_n, sum_merged_n_dof_list
    integer(c_int), intent(in), value :: n_graphs
    integer(c_int), intent(in), target :: n_vertex(n_graphs), n_internal_vertex(n_graphs)
    integer(c_int), intent(in), target :: vertex_id(sum_n_vertex), vertex_domain_id(sum_n_vertex), index(sum_index), item(sum_item)
    integer(c_int), intent(in), value :: merged_n_vertex, merged_n_internal_vertex
    integer(c_int), intent(in), target :: merged_vertex_id(merged_n_vertex), merged_vertex_domain_id(merged_n_vertex)
    integer(c_int), intent(in), target :: merged_index(merged_n_vertex+1), merged_item(merged_index(merged_n_vertex+1))
    integer(c_int), intent(in), target :: n_dof_list_n(n_graphs), list_struct_R_n(n_graphs)
    integer(c_int), intent(in), target :: n_dof_list_array(sum_n_vertex)
    real(c_double), intent(in), target :: list_struct_R_array(sum_list_struct_R_n)
    integer(c_int), intent(inout), target :: merged_n_dof_list(merged_n_vertex)
    real(c_double), intent(inout), target :: merged_array_R(sum_merged_n_dof_list)

    type(gedatsu_graph) :: graphs(n_graphs), merged_graph
    type(monolis_list_I) :: n_dof_list(n_graphs)
    type(monolis_list_R) :: list_struct_R(n_graphs)
    integer(kint) :: i, iS, iE
    integer(kint), allocatable :: Iarray(:)
    real(kdouble), allocatable :: Darray(:)

    !> graphs
    iS = 1
    do i = 1, n_graphs
      call gedatsu_graph_initialize(graphs(i))
      call gedatsu_graph_set_n_vertex(graphs(i), n_vertex(i))
      graphs(i)%n_internal_vertex = n_internal_vertex(i)
      iE = iS + n_vertex(i) - 1
      graphs(i)%vertex_id(1:n_vertex(i)) = vertex_id(iS:iE)
      graphs(i)%vertex_domain_id(1:n_vertex(i)) = vertex_domain_id(iS:iE)
      iS = iS + n_vertex(i)
    enddo
    iS = 1
    do i = 1, n_graphs
      iE = iS + n_vertex(i)
      graphs(i)%index(1:n_vertex(i)+1) = index(iS:iE)
      iS = iS + n_vertex(i) + 1
    enddo
    iS = 1
    do i = 1, n_graphs
      call monolis_alloc_I_1d(graphs(i)%item, graphs(i)%index(n_vertex(i)+1))
      iE = iS + graphs(i)%index(n_vertex(i)+1) - 1
      graphs(i)%item(1:graphs(i)%index(n_vertex(i)+1)) = item(iS:iE)
      iS = iS + graphs(i)%index(n_vertex(i)+1)
    enddo

    !> merged_graph
    call gedatsu_graph_initialize(merged_graph)
    call gedatsu_graph_set_n_vertex(merged_graph, merged_n_vertex)
    merged_graph%n_internal_vertex = merged_n_internal_vertex
    merged_graph%vertex_id(:) = merged_vertex_id(:)
    merged_graph%vertex_domain_id(:) = merged_vertex_domain_id(:)
    merged_graph%index(:) = merged_index(:)
    call monolis_alloc_I_1d(merged_graph%item, merged_index(merged_n_vertex+1))
    merged_graph%item(:) = merged_item(:)

    !> n_dof_list
    call monolis_list_initialize_I(n_dof_list, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_I_1d(Iarray)
      call monolis_alloc_I_1d(Iarray, n_dof_list_n(i))
      iE = iS + n_dof_list_n(i) - 1
      Iarray(1:n_dof_list_n(i)) = n_dof_list_array(iS:iE)
      call monolis_list_set_I(n_dof_list, i, n_dof_list_n(i), Iarray)
      iS = iS + n_dof_list_n(i)
    enddo

    !> list_struct_R
    call monolis_list_initialize_R(list_struct_R, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_R_1d(Darray)
      call monolis_alloc_R_1d(Darray, list_struct_R_n(i))
      iE = iS + list_struct_R_n(i) - 1
      Darray(1:list_struct_R_n(i)) = list_struct_R_array(iS:iE)
      call monolis_list_set_R(list_struct_R, i, list_struct_R_n(i), Darray)
      iS = iS + list_struct_R_n(i)
    enddo

    !> Fortranの結合関数
    call gedatsu_merge_distval_R_core(n_graphs, graphs, merged_graph, n_dof_list, list_struct_R, &
    & merged_n_dof_list, merged_array_R)
  end subroutine gedatsu_merge_distval_R_c

subroutine gedatsu_merge_distval_I_c( &
  & sum_n_vertex, sum_index, sum_item, sum_list_struct_C_n, sum_merged_n_dof_list, &
  & n_graphs, &
  & n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, &
  & merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, merged_index, merged_item, &
  & n_dof_list_n, n_dof_list_array, list_struct_I_n, list_struct_I_array, &
  & merged_n_dof_list, merged_array_I) &
  & bind(c, name = "gedatsu_merge_distval_I_c")
    implicit none
    integer(c_int), intent(in), value :: sum_n_vertex, sum_index, sum_item, sum_list_struct_C_n, sum_merged_n_dof_list
    integer(c_int), intent(in), value :: n_graphs
    integer(c_int), intent(in), target :: n_vertex(n_graphs), n_internal_vertex(n_graphs)
    integer(c_int), intent(in), target :: vertex_id(sum_n_vertex), vertex_domain_id(sum_n_vertex), index(sum_index), item(sum_item)
    integer(c_int), intent(in), value :: merged_n_vertex, merged_n_internal_vertex
    integer(c_int), intent(in), target :: merged_vertex_id(merged_n_vertex), merged_vertex_domain_id(merged_n_vertex)
    integer(c_int), intent(in), target :: merged_index(merged_n_vertex+1), merged_item(merged_index(merged_n_vertex+1))
    integer(c_int), intent(in), target :: n_dof_list_n(n_graphs), list_struct_I_n(n_graphs)
    integer(c_int), intent(in), target :: n_dof_list_array(sum_n_vertex)
    integer(c_int), intent(in), target :: list_struct_I_array(sum_list_struct_C_n)
    integer(c_int), intent(inout), target :: merged_n_dof_list(merged_n_vertex)
    integer(c_int), intent(inout), target :: merged_array_I(sum_merged_n_dof_list)

    type(gedatsu_graph) :: graphs(n_graphs), merged_graph
    type(monolis_list_I) :: n_dof_list(n_graphs)
    type(monolis_list_I) :: list_struct_I(n_graphs)
    integer(kint) :: i, iS, iE
    integer(kint), allocatable :: Iarray1(:), Iarray2(:)

    !> graphs
    iS = 1
    do i = 1, n_graphs
      call gedatsu_graph_initialize(graphs(i))
      call gedatsu_graph_set_n_vertex(graphs(i), n_vertex(i))
      graphs(i)%n_internal_vertex = n_internal_vertex(i)
      iE = iS + n_vertex(i) - 1
      graphs(i)%vertex_id(1:n_vertex(i)) = vertex_id(iS:iE)
      graphs(i)%vertex_domain_id(1:n_vertex(i)) = vertex_domain_id(iS:iE)
      iS = iS + n_vertex(i)
    enddo
    iS = 1
    do i = 1, n_graphs
      iE = iS + n_vertex(i)
      graphs(i)%index(1:n_vertex(i)+1) = index(iS:iE)
      iS = iS + n_vertex(i) + 1
    enddo
    iS = 1
    do i = 1, n_graphs
      call monolis_alloc_I_1d(graphs(i)%item, graphs(i)%index(n_vertex(i)+1))
      iE = iS + graphs(i)%index(n_vertex(i)+1) - 1
      graphs(i)%item(1:graphs(i)%index(n_vertex(i)+1)) = item(iS:iE)
      iS = iS + graphs(i)%index(n_vertex(i)+1)
    enddo

    !> merged_graph
    call gedatsu_graph_initialize(merged_graph)
    call gedatsu_graph_set_n_vertex(merged_graph, merged_n_vertex)
    merged_graph%n_internal_vertex = merged_n_internal_vertex
    merged_graph%vertex_id(:) = merged_vertex_id(:)
    merged_graph%vertex_domain_id(:) = merged_vertex_domain_id(:)
    merged_graph%index(:) = merged_index(:)
    call monolis_alloc_I_1d(merged_graph%item, merged_index(merged_n_vertex+1))
    merged_graph%item(:) = merged_item(:)

    !> n_dof_list
    call monolis_list_initialize_I(n_dof_list, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_I_1d(Iarray1)
      call monolis_alloc_I_1d(Iarray1, n_dof_list_n(i))
      iE = iS + n_dof_list_n(i) - 1
      Iarray1(1:n_dof_list_n(i)) = n_dof_list_array(iS:iE)
      call monolis_list_set_I(n_dof_list, i, n_dof_list_n(i), Iarray1)
      iS = iS + n_dof_list_n(i)
    enddo

    !> list_struct_I
    call monolis_list_initialize_I(list_struct_I, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_I_1d(Iarray2)
      call monolis_alloc_I_1d(Iarray2, list_struct_I_n(i))
      iE = iS + list_struct_I_n(i) - 1
      Iarray2(1:list_struct_I_n(i)) = list_struct_I_array(iS:iE)
      call monolis_list_set_I(list_struct_I, i, list_struct_I_n(i), Iarray2)
      iS = iS + list_struct_I_n(i)
    enddo

    !> Fortranの結合関数
    call gedatsu_merge_distval_I_core(n_graphs, graphs, merged_graph, n_dof_list, list_struct_I, &
    & merged_n_dof_list, merged_array_I)
  end subroutine gedatsu_merge_distval_I_c

  subroutine gedatsu_merge_distval_C_c( &
  & sum_n_vertex, sum_index, sum_item, sum_list_struct_C_n, sum_merged_n_dof_list, &
  & n_graphs, &
  & n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, &
  & merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, merged_index, merged_item, &
  & n_dof_list_n, n_dof_list_array, list_struct_C_n, list_struct_C_array, &
  & merged_n_dof_list, merged_array_C) &
  & bind(c, name = "gedatsu_merge_distval_C_c")
    implicit none
    integer(c_int), intent(in), value :: sum_n_vertex, sum_index, sum_item, sum_list_struct_C_n, sum_merged_n_dof_list
    integer(c_int), intent(in), value :: n_graphs
    integer(c_int), intent(in), target :: n_vertex(n_graphs), n_internal_vertex(n_graphs)
    integer(c_int), intent(in), target :: vertex_id(sum_n_vertex), vertex_domain_id(sum_n_vertex), index(sum_index), item(sum_item)
    integer(c_int), intent(in), value :: merged_n_vertex, merged_n_internal_vertex
    integer(c_int), intent(in), target :: merged_vertex_id(merged_n_vertex), merged_vertex_domain_id(merged_n_vertex)
    integer(c_int), intent(in), target :: merged_index(merged_n_vertex+1), merged_item(merged_index(merged_n_vertex+1))
    integer(c_int), intent(in), target :: n_dof_list_n(n_graphs), list_struct_C_n(n_graphs)
    integer(c_int), intent(in), target :: n_dof_list_array(sum_n_vertex)
    complex(c_double), intent(in), target :: list_struct_C_array(sum_list_struct_C_n)
    integer(c_int), intent(inout), target :: merged_n_dof_list(merged_n_vertex)
    complex(c_double), intent(inout), target :: merged_array_C(sum_merged_n_dof_list)

    type(gedatsu_graph) :: graphs(n_graphs), merged_graph
    type(monolis_list_I) :: n_dof_list(n_graphs)
    type(monolis_list_C) :: list_struct_C(n_graphs)
    integer(kint) :: i, iS, iE
    integer(kint), allocatable :: Iarray(:)
    complex(kdouble), allocatable :: Carray(:)

    !> graphs
    iS = 1
    do i = 1, n_graphs
      call gedatsu_graph_initialize(graphs(i))
      call gedatsu_graph_set_n_vertex(graphs(i), n_vertex(i))
      graphs(i)%n_internal_vertex = n_internal_vertex(i)
      iE = iS + n_vertex(i) - 1
      graphs(i)%vertex_id(1:n_vertex(i)) = vertex_id(iS:iE)
      graphs(i)%vertex_domain_id(1:n_vertex(i)) = vertex_domain_id(iS:iE)
      iS = iS + n_vertex(i)
    enddo
    iS = 1
    do i = 1, n_graphs
      iE = iS + n_vertex(i)
      graphs(i)%index(1:n_vertex(i)+1) = index(iS:iE)
      iS = iS + n_vertex(i) + 1
    enddo
    iS = 1
    do i = 1, n_graphs
      call monolis_alloc_I_1d(graphs(i)%item, graphs(i)%index(n_vertex(i)+1))
      iE = iS + graphs(i)%index(n_vertex(i)+1) - 1
      graphs(i)%item(1:graphs(i)%index(n_vertex(i)+1)) = item(iS:iE)
      iS = iS + graphs(i)%index(n_vertex(i)+1)
    enddo

    !> merged_graph
    call gedatsu_graph_initialize(merged_graph)
    call gedatsu_graph_set_n_vertex(merged_graph, merged_n_vertex)
    merged_graph%n_internal_vertex = merged_n_internal_vertex
    merged_graph%vertex_id(:) = merged_vertex_id(:)
    merged_graph%vertex_domain_id(:) = merged_vertex_domain_id(:)
    merged_graph%index(:) = merged_index(:)
    call monolis_alloc_I_1d(merged_graph%item, merged_index(merged_n_vertex+1))
    merged_graph%item(:) = merged_item(:)

    !> n_dof_list
    call monolis_list_initialize_I(n_dof_list, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_I_1d(Iarray)
      call monolis_alloc_I_1d(Iarray, n_dof_list_n(i))
      iE = iS + n_dof_list_n(i) - 1
      Iarray(1:n_dof_list_n(i)) = n_dof_list_array(iS:iE)
      call monolis_list_set_I(n_dof_list, i, n_dof_list_n(i), Iarray)
      iS = iS + n_dof_list_n(i)
    enddo

    !> list_struct_C
    call monolis_list_initialize_C(list_struct_C, n_graphs)
    iS = 1
    do i = 1, n_graphs
      call monolis_dealloc_C_1d(Carray)
      call monolis_alloc_C_1d(Carray, list_struct_C_n(i))
      iE = iS + list_struct_C_n(i) - 1
      Carray(1:list_struct_C_n(i)) = list_struct_C_array(iS:iE)
      call monolis_list_set_C(list_struct_C, i, list_struct_C_n(i), Carray)
      iS = iS + list_struct_C_n(i)
    enddo

    !> Fortranの結合関数
    call gedatsu_merge_distval_C_core(n_graphs, graphs, merged_graph, n_dof_list, list_struct_C, &
    & merged_n_dof_list, merged_array_C)
  end subroutine gedatsu_merge_distval_C_c


end module mod_gedatsu_graph_merge_wrapper
