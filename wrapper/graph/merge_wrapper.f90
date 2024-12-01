module mod_gedatsu_graph_merge_wrapper
  use mod_monolis_utils
  use mod_gedatsu_graph_merge
  use iso_c_binding
  implicit none

contains

  !> vertex_id等は、n_graphs個のグラフそれぞれがもつ配列
  !> → 各グラフのvertex_idの先頭ポインタを並べた配列 (サイズ n_graphs) を引数にとる
  subroutine gedatsu_merge_nodal_subgraphs_c(n_graphs, n_vertex, n_internal_vertex, vertex_id_ptr, vertex_domain_id_ptr, &
    & index_ptr, item_ptr, index_size, item_size, &
    & my_rank, comm, comm_size, &
    & recv_n_neib, recv_neib_pe_ptr, recv_index_ptr, recv_item_ptr, recv_neib_pe_size, recv_index_size, recv_item_size, &
    & send_n_neib, send_neib_pe_ptr, send_index_ptr, send_item_ptr, send_neib_pe_size, send_index_size, send_item_size, &
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

    type(c_ptr), intent(in), target :: vertex_id_ptr(:,:), vertex_domain_id_ptr(:,:)
    type(c_ptr), intent(in), target :: index_ptr(:,:), item_ptr(:,:)
    type(c_ptr), intent(in), target :: recv_neib_pe_ptr(:,:), recv_index_ptr(:,:), recv_item_ptr(:,:)
    type(c_ptr), intent(in), target :: send_neib_pe_ptr(:,:), send_index_ptr(:,:), send_item_ptr(:,:)
    integer(c_int), intent(in), target :: index_size(n_graphs), item_size(n_graphs)
    integer(c_int), intent(in), target :: recv_neib_pe_size(n_graphs), recv_index_size(n_graphs), recv_item_size(n_graphs)
    integer(c_int), intent(in), target :: send_neib_pe_size(n_graphs), send_index_size(n_graphs), send_item_size(n_graphs)

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
    integer(c_int) :: i
    integer(c_int), pointer :: ptr(:) !> vertex_idやitemなどの、各要素を参照するポインタ

    allocate(graphs(n_graphs))
    allocate(monoCOMs(n_graphs))

    !> for graphs
    do i = 1, n_graphs
      call gedatsu_graph_initialize(graphs(i))
      call gedatsu_graph_set_n_vertex(graphs(i), n_vertex(i))
      graphs(i)%n_internal_vertex = n_internal_vertex(i)

      call c_f_pointer(vertex_id_ptr(i,1), ptr, [n_vertex(i)])
      graphs(i)%vertex_id = ptr

      call c_f_pointer(vertex_domain_id_ptr(i,1), ptr, [n_vertex(i)])
      graphs(i)%vertex_domain_id = ptr

      call c_f_pointer(index_ptr(i,1), ptr, [index_size(i)])
      graphs(i)%index = ptr

      call monolis_alloc_I_1d(graphs(i)%item, item_size(i))
      call c_f_pointer(item_ptr(i,1), ptr, [item_size(i)])
      graphs(i)%item = ptr

      !> インデックスを１スタートに修正
      graphs(i)%vertex_id = graphs(i)%vertex_id + 1
      graphs(i)%item = graphs(i)%item + 1

    enddo

    !> for monoCOMs
    do i = 1, n_graphs
      call monolis_com_initialize_by_self(monoCOMs(i))
      monoCOMs(i)%my_rank = my_rank(i)
      monoCOMs(i)%comm = comm(i)
      monoCOMs(i)%comm_size = comm_size(i)

      monoCOMs(i)%recv_n_neib = recv_n_neib(i)
      call c_f_pointer(recv_neib_pe_ptr(i,1), ptr, [recv_neib_pe_size(i)])
      monoCOMs(i)%recv_neib_pe = ptr
      call c_f_pointer(recv_index_ptr(i,1), ptr, [recv_index_size(i)])
      monoCOMs(i)%recv_index = ptr
      call c_f_pointer(recv_item_ptr(i,1), ptr, [recv_item_size(i)])
      monoCOMs(i)%recv_item = ptr

      monoCOMs(i)%send_n_neib = send_n_neib(i)
      call c_f_pointer(send_neib_pe_ptr(i,1), ptr, [send_neib_pe_size(i)])
      monoCOMs(i)%send_neib_pe = ptr
      call c_f_pointer(send_index_ptr(i,1), ptr, [send_index_size(i)])
      monoCOMs(i)%send_index = ptr
      call c_f_pointer(send_item_ptr(i,1), ptr, [send_item_size(i)])
      monoCOMs(i)%send_item = ptr

      monoCOMs(i)%recv_item = monoCOMs(i)%recv_item + 1
      monoCOMs(i)%send_item = monoCOMs(i)%send_item + 1
    enddo

    call gedatsu_merge_nodal_subgraphs(n_graphs, graphs, monoCOMs, merged_graph, merged_monoCOM, order_type)

    !> // TODO インデックスを０スタートに修正
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

  subroutine gedatsu_merged_connectivity_subgraphs_c
    ! & bind(c, name = "gedatsu_merge_connectivity_subgraphs_c_main")
    implicit none
    
  end subroutine gedatsu_merged_connectivity_subgraphs_c

  subroutine gedatsu_merge_distval_R_c
    ! & bind(c, name = "gedatsu_merge_distval_R_c_main")
    implicit none
    
  end subroutine gedatsu_merge_distval_R_c

end module mod_gedatsu_graph_merge_wrapper
