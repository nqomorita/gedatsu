module mod_gedatsu_graph_merge_wrapper
  use mod_monolis_utils
  use mod_gedatsu_graph_merge
  use iso_c_binding
  implicit none

contains

  subroutine gedatsu_merge_nodal_subgraphs_c( &
    & n_graphs, &
    & n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, &
    & my_rank, comm, comm_size, &
    & recv_n_neib, recv_neib_pe, recv_index, recv_item, &
    & send_n_neib, send_neib_pe, send_index, send_item, &
    & merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, &
    & merged_index, merged_item, merged_my_rank, merged_comm, merged_comm_size, &
    & merged_recv_n_neib, merged_recv_neib_pe, merged_recv_index, merged_recv_item, &
    & merged_send_n_neib, merged_send_neib_pe, merged_send_index, merged_send_item, &
    order_type) &
    & bind(c, name = "gedatsu_merge_nodal_subgraphs_c_main")
    implicit none
    integer(c_int), intent(in), value :: n_graphs
    integer(c_int), intent(in), target :: n_vertex(n_graphs), n_internal_vertex(n_graphs)
    integer(c_int), intent(in), target :: my_rank(n_graphs), comm(n_graphs), comm_size(n_graphs)
    integer(c_int), intent(in), target :: recv_n_neib(n_graphs), send_n_neib(n_graphs)

    !> graphsのvertex_idとかをまとめたCポインタ(int**)は、c_ptrでうけとる。（じゃないと、ポインタを要素にもつ配列って扱えなくない？）。

    type(c_ptr), intent(in), target :: vertex_id(:), vertex_domain_id(:)
    type(c_ptr), intent(in), target :: index(:), item(:)
    type(c_ptr), intent(in), target :: recv_neib_pe(:), recv_index(:), recv_item(:)
    type(c_ptr), intent(in), target :: send_neib_pe(:), send_index(:), send_item(:)

    integer(c_int), intent(inout), target :: merged_n_vertex, merged_n_internal_vertex
    integer(c_int), intent(inout), target :: merged_my_rank, merged_comm, merged_comm_size, merged_recv_n_neib, merged_send_n_neib
    integer(c_int), intent(inout), target :: merged_vertex_id(:), merged_vertex_domain_id(:), merged_index(:), merged_item(:)
    integer(c_int), intent(inout), target :: merged_recv_neib_pe(:), merged_recv_index(:), merged_recv_item(:)
    integer(c_int), intent(inout), target :: merged_send_neib_pe(:), merged_send_index(:), merged_send_item(:)

    integer(c_int), intent(in), value :: order_type

    type(gedatsu_graph), allocatable :: graphs(:)
    type(monolis_COM), allocatable :: monoCOMs(:)
    type(gedatsu_graph) :: merged_graph
    type(monolis_COM) :: merged_monoCOM
    integer(kint) :: i, nindex, nitem
    integer(kint), pointer :: ptr(:)

    allocate(graphs(n_graphs))
    allocate(monoCOMs(n_graphs))

    !> for graphs
    do i = 1, n_graphs
      call gedatsu_graph_initialize(graphs(i))
      call gedatsu_graph_set_n_vertex(graphs(i), n_vertex(i))
      graphs(i)%n_internal_vertex = n_internal_vertex(i)

      call c_f_pointer(vertex_id(i), ptr, [n_vertex(i)])
      graphs(i)%vertex_id = ptr

      call c_f_pointer(vertex_domain_id(i), ptr, [n_vertex(i)])
      graphs(i)%vertex_domain_id = ptr

      nindex = n_vertex(i) + 1
      call c_f_pointer(index(i), ptr, [nindex])
      nitem = ptr(nindex)
      call monolis_alloc_I_1d(graphs(i)%item, nitem)
      call c_f_pointer(item(i), ptr, [nitem])
      graphs(i)%item = ptr

    enddo

    !> for monoCOMs
    do i = 1, n_graphs
      call monolis_com_initialize_by_self(monoCOMs(i))
      monoCOMs(i)%my_rank = my_rank(i)
      monoCOMs(i)%comm = comm(i)
      monoCOMs(i)%comm_size = comm_size(i)

      monoCOMs(i)%recv_n_neib = recv_n_neib(i)
      call c_f_pointer(recv_neib_pe(i), ptr, [recv_n_neib(i)])
      monoCOMs(i)%recv_neib_pe = ptr
      nindex = recv_n_neib(i) + 1
      call c_f_pointer(recv_index(i), ptr, [nindex])
      monoCOMs(i)%recv_index = ptr
      nitem = ptr(nindex)
      call c_f_pointer(recv_item(i), ptr, [nitem])
      monoCOMs(i)%recv_item = ptr

      monoCOMs(i)%send_n_neib = send_n_neib(i)
      call c_f_pointer(send_neib_pe(i), ptr, [send_n_neib(i)])
      monoCOMs(i)%send_neib_pe = ptr
      nindex = send_n_neib(i) + 1
      call c_f_pointer(send_index(i), ptr, [nindex])
      monoCOMs(i)%send_index = ptr
      nitem = ptr(nindex)
      call c_f_pointer(send_item(i), ptr, [nitem])
      monoCOMs(i)%send_item = ptr
    enddo

    !> インデックスを1スタートに修正
    graphs(i)%vertex_id = graphs(i)%vertex_id + 1
    graphs(i)%item = graphs(i)%item + 1
    monoCOMs(i)%recv_item = monoCOMs(i)%recv_item + 1
    monoCOMs(i)%send_item = monoCOMs(i)%send_item + 1

    call gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)

    !> インデックスを０スタートに修正
    merged_graph%vertex_id = merged_graph%vertex_id - 1
    merged_graph%item = merged_graph%item - 1
    do i = 1, n_graphs
      monoCOMs(i)%recv_item = monoCOMs(i)%recv_item - 1
      monoCOMs(i)%send_item = monoCOMs(i)%send_item - 1
    enddo
    merged_monoCOM%recv_item = merged_monoCOM%recv_item - 1
    merged_monoCOM%send_item = merged_monoCOM%send_item - 1

    !> for merged_graph
    merged_n_vertex = merged_graph%n_vertex
    merged_n_internal_vertex = merged_graph%n_internal_vertex
    merged_vertex_id = merged_graph%vertex_id
    merged_vertex_domain_id = merged_graph%vertex_domain_id
    merged_index = merged_graph%index
    merged_item = merged_graph%item

    !> for merged_monoCOM
    merged_my_rank = merged_monoCOM%my_rank
    merged_comm = merged_monoCOM%comm
    merged_comm_size = merged_monoCOM%comm_size
    merged_recv_n_neib = merged_monoCOM%recv_n_neib
    merged_recv_neib_pe = merged_monoCOM%recv_neib_pe
    merged_recv_index = merged_monoCOM%recv_index
    merged_recv_item = merged_monoCOM%recv_item
    merged_send_n_neib = merged_monoCOM%send_n_neib
    merged_send_neib_pe = merged_monoCOM%send_neib_pe
    merged_send_index = merged_monoCOM%send_index
    merged_send_item = merged_monoCOM%send_item

  end subroutine gedatsu_merge_nodal_subgraphs_c

  subroutine gedatsu_merged_connectivity_subgraphs_c( &
    & n_nodal_graphs, &
    & nodal_n_vertex, nodal_n_internal_vertex, nodal_vertex_id, nodal_vertex_domain_id, nodal_index, nodal_item, &
    & merged_nodal_n_vertex, merged_nodal_n_internal_vertex, merged_nodal_vertex_id, merged_nodal_vertex_domain_id, &
    & merged_nodal_index, merged_nodal_item, merged_nodal_my_rank, merged_nodal_comm, merged_nodal_comm_size, &
    & merged_nodal_recv_n_neib, merged_nodal_recv_neib_pe, merged_nodal_recv_index, merged_nodal_recv_item, &
    & merged_nodal_send_n_neib, merged_nodal_send_neib_pe, merged_nodal_send_index, merged_nodal_send_item, &
    & n_conn_graphs, &
    & conn_n_vertex, conn_n_internal_vertex, conn_vertex_id, conn_vertex_domain_id, conn_index, conn_item, &
    & merged_conn_n_vertex, merged_conn_n_internal_vertex, merged_conn_vertex_id, merged_conn_vertex_domain_id, &
    & merged_conn_index, merged_conn_item ) &
    & bind(c, name = "gedatsu_merge_connectivity_subgraphs_c_main")
    implicit none
    integer(c_int), intent(in), value :: n_nodal_graphs
    integer(c_int), intent(in), target :: nodal_n_vertex(n_nodal_graphs), nodal_n_internal_vertex(n_nodal_graphs)
    type(c_ptr), intent(in), target :: nodal_vertex_id(:), nodal_vertex_domain_id(:)
    type(c_ptr), intent(in), target :: nodal_index(:), nodal_item(:)
    integer(c_int), intent(in), target :: merged_nodal_n_vertex, merged_nodal_n_internal_vertex
    integer(c_int), intent(in), target :: merged_nodal_my_rank, merged_nodal_comm, merged_nodal_comm_size
    integer(c_int), intent(in), target :: merged_nodal_recv_n_neib, merged_nodal_send_n_neib
    integer(c_int), intent(in), target :: merged_nodal_vertex_id(:), merged_nodal_vertex_domain_id(:)
    integer(c_int), intent(in), target :: merged_nodal_index(:), merged_nodal_item(:)
    integer(c_int), intent(in), target :: merged_nodal_recv_neib_pe(:), merged_nodal_recv_index(:), merged_nodal_recv_item(:)
    integer(c_int), intent(in), target :: merged_nodal_send_neib_pe(:), merged_nodal_send_index(:), merged_nodal_send_item(:)

    integer(c_int), intent(in), value :: n_conn_graphs
    integer(c_int), intent(in), target :: conn_n_vertex(n_conn_graphs), conn_n_internal_vertex(n_conn_graphs)
    type(c_ptr), intent(in), target :: conn_vertex_id(:), conn_vertex_domain_id(:)
    type(c_ptr), intent(in), target :: conn_index(:), conn_item(:)
    integer(c_int), intent(inout), target :: merged_conn_n_vertex, merged_conn_n_internal_vertex
    integer(c_int), intent(inout), target :: merged_conn_vertex_id(:), merged_conn_vertex_domain_id(:)
    integer(c_int), intent(inout), target :: merged_conn_index(:), merged_conn_item(:)

    type(gedatsu_graph), allocatable :: nodal_graphs(:), conn_graphs(:)
    type(gedatsu_graph) :: merged_nodal_graph, merged_conn_graph
    type(monolis_COM) :: merged_nodal_monoCOM
    integer(kint) :: i, nindex, nitem
    integer(kint), pointer :: ptr(:)

    allocate(nodal_graphs(n_nodal_graphs))
    allocate(conn_graphs(n_conn_graphs))

    !> nodal_graphs
    do i = 1, n_nodal_graphs
      call gedatsu_graph_initialize(nodal_graphs(i))
      call gedatsu_graph_set_n_vertex(nodal_graphs(i), nodal_n_vertex(i))
      nodal_graphs(i)%n_internal_vertex = nodal_n_internal_vertex(i)

      call c_f_pointer(nodal_vertex_id(i), ptr, [nodal_n_vertex(i)])
      nodal_graphs(i)%vertex_id = ptr

      call c_f_pointer(nodal_vertex_domain_id(i), ptr, [nodal_n_vertex(i)])
      nodal_graphs(i)%vertex_domain_id = ptr

      nindex = nodal_n_vertex(i) + 1
      call c_f_pointer(nodal_index(i), ptr, [nindex])
      nitem = ptr(nindex)
      call monolis_alloc_I_1d(nodal_graphs(i)%item, nitem)
      call c_f_pointer(nodal_item(i), ptr, [nitem])
      nodal_graphs(i)%item = ptr
    enddo

    !> conn_graphs
    do i = 1, n_conn_graphs
      call gedatsu_graph_initialize(conn_graphs(i))
      call gedatsu_graph_set_n_vertex(conn_graphs(i), conn_n_vertex(i))
      conn_graphs(i)%n_internal_vertex = conn_n_internal_vertex(i)

      call c_f_pointer(conn_vertex_id(i), ptr, [conn_n_vertex(i)])
      conn_graphs(i)%vertex_id = ptr

      call c_f_pointer(conn_vertex_domain_id(i), ptr, [conn_n_vertex(i)])
      conn_graphs(i)%vertex_domain_id = ptr

      nindex = conn_n_vertex(i) + 1
      call c_f_pointer(conn_index(i), ptr, [nindex])
      nitem = ptr(nindex)
      call monolis_alloc_I_1d(conn_graphs(i)%item, nitem)
      call c_f_pointer(conn_item(i), ptr, [nitem])
      conn_graphs(i)%item = ptr
    enddo

    !> merged_nodal_graph
    merged_nodal_graph%n_vertex = merged_nodal_n_vertex
    merged_nodal_graph%n_internal_vertex = merged_nodal_n_internal_vertex
    merged_nodal_graph%vertex_id = merged_nodal_vertex_id
    merged_nodal_graph%vertex_domain_id = merged_nodal_vertex_domain_id
    merged_nodal_graph%index = merged_nodal_index
    merged_nodal_graph%item = merged_nodal_item

    !> merged_nodal_monoCOM
    merged_nodal_monoCOM%my_rank = merged_nodal_my_rank
    merged_nodal_monoCOM%comm = merged_nodal_comm
    merged_nodal_monoCOM%comm_size = merged_nodal_comm_size
    merged_nodal_monoCOM%recv_n_neib = merged_nodal_recv_n_neib
    merged_nodal_monoCOM%recv_neib_pe = merged_nodal_recv_neib_pe
    merged_nodal_monoCOM%recv_index = merged_nodal_recv_index
    merged_nodal_monoCOM%recv_item = merged_nodal_recv_item
    merged_nodal_monoCOM%send_n_neib = merged_nodal_send_n_neib
    merged_nodal_monoCOM%send_neib_pe = merged_nodal_send_neib_pe
    merged_nodal_monoCOM%send_index = merged_nodal_send_index
    merged_nodal_monoCOM%send_item = merged_nodal_send_item

    !> インデックスを1スタートに修正
    do i = 1, n_nodal_graphs
      nodal_graphs(i)%vertex_id = nodal_graphs(i)%vertex_id + 1
      nodal_graphs(i)%item = nodal_graphs(i)%item + 1
    enddo
    do i = 1, n_conn_graphs
      conn_graphs(i)%vertex_id = conn_graphs(i)%vertex_id + 1
      conn_graphs(i)%item = conn_graphs(i)%item + 1
    enddo
    merged_nodal_monoCOM%recv_item = merged_nodal_monoCOM%recv_item - 1
    merged_nodal_monoCOM%send_item = merged_nodal_monoCOM%send_item - 1

    call gedatsu_merge_connectivity_subgraphs( &
    & n_nodal_graphs, nodal_graphs, merged_nodal_graph, merged_nodal_monoCOM, n_conn_graphs, conn_graphs, merged_conn_graph)

    !> インデックスを0スタートに修正
    merged_conn_graph%vertex_id = merged_conn_graph%vertex_id - 1
    merged_conn_graph%item = merged_conn_graph%item - 1
    merged_nodal_monoCOM%recv_item = merged_nodal_monoCOM%recv_item - 1
    merged_nodal_monoCOM%send_item = merged_nodal_monoCOM%send_item - 1

    !> merged_conn_graph
    merged_conn_n_vertex = merged_conn_graph%n_vertex
    merged_conn_n_internal_vertex = merged_conn_graph%n_internal_vertex
    merged_conn_vertex_id = merged_conn_graph%vertex_id
    merged_conn_vertex_domain_id = merged_conn_graph%vertex_domain_id
    merged_conn_index = merged_conn_graph%index
    merged_conn_item = merged_conn_graph%item

  end subroutine gedatsu_merged_connectivity_subgraphs_c

  subroutine gedatsu_merge_distval_R_c( &
    & n_graphs, &
    & n_vertex, n_internal_vertex, vertex_id, vertex_domain_id, index, item, &
    & merged_n_vertex, merged_n_internal_vertex, merged_vertex_id, merged_vertex_domain_id, &
    & merged_index, merged_item, &
    & &
    & mreged_n_dof_list, merged_array_R) &
    & bind(c, name = "gedatsu_merge_distval_R_c_main")
    implicit none
    integer(c_int), intent(in), value :: n_graphs



    type(gedatsu_graph) :: graphs(n_graphs), merged_graph
    type(monolis_list_I) :: n_dof_list
    type(monolis_list_R) :: list_struct_R
    integer(kint),allocatable :: merged_n_dof_list
    real(kdouble),allocatable :: merged_array_R


    call gedatsu_merge_distval_R(n_graphs, graphs, merged_graph, n_dof_list, list_struct_R, merged_n_dof_list, merged_array_R)

  end subroutine gedatsu_merge_distval_R_c



  ! subroutine gedatsu_list_initialize_R_c() &
  !   & bind(c, name = "gedatsu_list_initialize_R_c_main")
  !   implicit none
  ! end subroutine gedatsu_list_initialize_R_c

end module mod_gedatsu_graph_merge_wrapper
