!> グラフ分割モジュール
!# gedatsu_graph_partition(graph, n_domain, subgraphs)
!# gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
!# gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)
!# gedatsu_get_parted_graph(graph, n_domain, subgraphs)
!# gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
!# gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
!# gedatsu_com_get_comm_table_serial(graph, n_domain, subgraphs, com)
!# gedatsu_comm_get_all_external_node_serial(subgraphs, n_domain, outer_node_id_all_global, displs)
!# gedatsu_get_metagraph(com, n_domain, metagraph)
module mod_gedatsu_graph_part
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_graph_handler
  use mod_gedatsu_wrapper_metis
  implicit none

contains

  !> @ingroup graph_part
  !> グラフを分割する（節点重みなし）
  subroutine gedatsu_graph_partition_METIS(graph, n_domain, subgraphs)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] 分割数
    integer(kint), intent(in) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), intent(out) :: subgraphs(:)

    if(.not. allocated(graph%vertex_domain_id))then
      call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)
    endif

    call gedatsu_part_graph_metis(graph%n_vertex, graph%index, graph%item, n_domain, graph%vertex_domain_id)

    call gedatsu_check_vertex_domain_id(graph%n_vertex, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_METIS

  !> @ingroup graph_part
  !> グラフを分割する（節点重みあり）
  subroutine gedatsu_graph_partition_METIS_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] 分割数
    integer(kint), intent(in) :: n_domain
    !> [in] ノード重み
    integer(kint), allocatable, intent(in) :: node_wgt(:,:)
    !> [in] エッジ重み
    integer(kint), allocatable, intent(in) :: edge_wgt(:,:)
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), intent(out) :: subgraphs(:)

    if(.not. allocated(graph%vertex_domain_id))then
      call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)
    endif

    call gedatsu_part_graph_metis_with_weight(graph%n_vertex, graph%index, graph%item, &
      & node_wgt, edge_wgt, n_domain, graph%vertex_domain_id)

    call gedatsu_check_vertex_domain_id(graph%n_vertex, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_METIS_with_weight

  !> @ingroup graph_part
  !> グラフを分割する（分割結果の外部ファイル入力）
  subroutine gedatsu_graph_partition_given_subdomain(graph, n_domain, given_domain_id, subgraphs)
    implicit none
    !> [in,out] graph 構造体
    type(gedatsu_graph), intent(inout) :: graph
    !> [in] 分割数
    integer(kint), intent(in) :: n_domain
    !> [in] 外部入力された既知の領域分割結果
    integer(kint), intent(in) :: given_domain_id(:,:)
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), intent(out) :: subgraphs(:)

    if(.not. allocated(graph%vertex_domain_id))then
      call monolis_alloc_I_1d(graph%vertex_domain_id, graph%n_vertex)
    endif

    graph%vertex_domain_id = given_domain_id(1,:)

    call gedatsu_check_vertex_domain_id(graph%n_vertex, n_domain, graph%vertex_domain_id)

    call gedatsu_get_parted_graph(graph, n_domain, subgraphs)
  end subroutine gedatsu_graph_partition_given_subdomain

  !> @ingroup dev_graph_part
  !> 領域番号に従ってオーバーラップ領域を含めた分割グラフを取得
  subroutine gedatsu_check_vertex_domain_id(n_vertex, n_domain, vertex_domain_id)
    implicit none
    !> [in] ノード数
    integer(kint), intent(in) :: n_vertex
    !> [in] 分割数
    integer(kint), intent(in) :: n_domain
    !> [in,out] 領域分割番号
    integer(kint), intent(in) :: vertex_domain_id(:)
    integer(kint) :: newlen
    integer(kint), allocatable :: temp(:)

    call monolis_alloc_I_1d(temp, n_vertex)

    temp = vertex_domain_id

    call monolis_qsort_I_1d(temp, 1, n_vertex)

    call monolis_get_uniq_array_I(temp, n_vertex, newlen)

    if(newlen /= n_domain)then
      call monolis_std_error_string("gedatsu_check_vertex_domain_id")
      call monolis_std_error_string("domain which not has the vertex is found")
    endif
  end subroutine gedatsu_check_vertex_domain_id

  !> @ingroup dev_graph_part
  !> 領域番号に従ってオーバーラップ領域を含めた分割グラフを取得
  subroutine gedatsu_get_parted_graph(graph, n_domain, subgraphs)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 分割数
    integer(kint), intent(in) :: n_domain
    !> [out] 分割後の graph 構造体
    type(gedatsu_graph), intent(out) :: subgraphs(:)
    integer(kint) :: i

    do i = 1, n_domain
      call gedatsu_get_parted_graph_main(graph, i - 1, subgraphs(i))

      call gedatsu_add_overlapping_nodes(graph, i - 1, subgraphs(i))
    enddo
  end subroutine gedatsu_get_parted_graph

  !> @ingroup dev_graph_part
  !> 領域番号 domain_id に属するオーバーラップ領域を含めない分割グラフを取得
  subroutine gedatsu_get_parted_graph_main(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [in,out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph), intent(inout) :: subgraph
    integer(kint) :: n_vertex, n_edge
    integer(kint), allocatable :: edge(:,:)

    call gedatsu_graph_get_n_vertex_in_internal_region(graph, domain_id, n_vertex)

    if(n_vertex == 0) call monolis_std_warning_string("gedatsu_get_parted_graph_main")
    if(n_vertex == 0) call monolis_std_warning_string("n_vertex equals zero")

    subgraph%n_internal_vertex = n_vertex

    call gedatsu_graph_set_n_vertex(subgraph, n_vertex)

    call gedatsu_graph_get_n_edge_in_internal_region(graph, domain_id, n_edge)

    call gedatsu_graph_get_vertex_id_in_internal_region(graph, domain_id, subgraph%vertex_id)

    if(n_edge == 0) return

    call monolis_alloc_I_1d(subgraph%item, n_edge)

    call monolis_alloc_I_2d(edge, 2, n_edge)

    call gedatsu_graph_get_edge_in_internal_region(graph, domain_id, edge)

    call gedatsu_graph_set_edge(subgraph, n_edge, edge, .true.)
  end subroutine gedatsu_get_parted_graph_main

  !> @ingroup dev_graph_part
  !> 領域番号 domain_id に属するオーバーラップ領域をグラフ構造体に追加
  subroutine gedatsu_add_overlapping_nodes(graph, domain_id, subgraph)
    implicit none
    !> [in] graph 構造体
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域番号
    integer(kint), intent(in) :: domain_id
    !> [in,out] 領域番号 domain_id に属する分割 graph 構造体
    type(gedatsu_graph), intent(inout) :: subgraph
    integer(kint) :: n_vertex, n_edge
    integer(kint), allocatable :: OVL_vertex_id(:), edge(:,:)

    call gedatsu_graph_get_n_vertex_in_overlap_region(graph, domain_id, n_vertex)

    if(n_vertex == 0) return

    call monolis_alloc_I_1d(OVL_vertex_id, n_vertex)

    call gedatsu_graph_get_vertex_id_in_overlap_region(graph, domain_id, OVL_vertex_id)

    call gedatsu_graph_add_n_vertex_with_vertex_id(subgraph, n_vertex, OVL_vertex_id)

    call gedatsu_graph_get_n_edge_in_overlap_region(graph, domain_id, n_edge)

    if(n_edge == 0) return

    call monolis_alloc_I_2d(edge, 2, n_edge)

    call gedatsu_graph_get_edge_in_overlap_region(graph, domain_id, edge)

    call gedatsu_graph_add_edge(subgraph, n_edge, edge, .true.)
  end subroutine gedatsu_add_overlapping_nodes

  !> @ingroup dev_graph_part
  !> 通信テーブルを作成（逐次実行版）
  subroutine gedatsu_com_get_comm_table_serial(graph, n_domain, subgraphs, com)
    implicit none
    !> [in] 全体グラフ
    type(gedatsu_graph), intent(in) :: graph
    !> [in] 領域分割数
    integer(kint), intent(in) :: n_domain
    !> [in] 分割グラフ
    type(gedatsu_graph), intent(in) :: subgraphs(:)
    !> [out] 分割領域に対応する com 構造体
    type(monolis_COM), intent(out) :: com(:)
    !> 全ての外部節点番号（グローバル番号）
    integer(kint), allocatable :: outer_node_id_all_global(:)
    !> 全ての外部節点が属する領域番号
    integer(kint), allocatable :: outer_domain_id_all(:)
    !> 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable :: displs(:)
    integer(kint) :: i, n_domain_i
    type(monolis_comm_node_list), allocatable :: recv_list(:)

    call gedatsu_comm_get_all_external_node_serial(subgraphs, n_domain, outer_node_id_all_global, displs)

    call monolis_comm_get_all_external_node_domain_id_serial(graph%vertex_domain_id, n_domain, &
      & outer_node_id_all_global, outer_domain_id_all, displs)

    allocate(recv_list(n_domain))

    do i = 1, n_domain
      n_domain_i = i - 1
      call monolis_comm_get_recv_serial(n_domain, n_domain_i, subgraphs(i)%n_internal_vertex, &
        & outer_node_id_all_global, outer_domain_id_all, displs, com(i), recv_list)
    enddo

    do i = 1, n_domain
      call monolis_comm_get_send_serial(n_domain, &
        & subgraphs(i)%n_vertex, subgraphs(i)%vertex_id, com(i), recv_list(i))
    enddo
  end subroutine gedatsu_com_get_comm_table_serial

  !> @ingroup dev_graph_part
  !> 全ての外部節点を取得（逐次実行版）
  subroutine gedatsu_comm_get_all_external_node_serial(subgraphs, n_domain, outer_node_id_all_global, displs)
    implicit none
    !> [in] 分割グラフ
    type(gedatsu_graph), intent(in) :: subgraphs(:)
    !> [in] 領域分割数
    integer(kint), intent(in) :: n_domain
    !> [out] 全ての外部節点番号
    integer(kint), allocatable, intent(out) :: outer_node_id_all_global(:)
    !> [out] 全ての外部節点配列の各領域に属する節点数
    integer(kint), allocatable, intent(out) :: displs(:)
    integer(kint) :: i, j, in

    call monolis_alloc_I_1d(displs, n_domain + 1)

    do i = 1, n_domain
      displs(i + 1) = displs(i) + subgraphs(i)%n_vertex - subgraphs(i)%n_internal_vertex
    enddo

    call monolis_alloc_I_1d(outer_node_id_all_global, displs(n_domain + 1))

    in = 0
    do i = 1, n_domain
      do j = subgraphs(i)%n_internal_vertex + 1, subgraphs(i)%n_vertex
        in = in + 1
        outer_node_id_all_global(in) = subgraphs(i)%vertex_id(j)
      enddo
    enddo
  end subroutine gedatsu_comm_get_all_external_node_serial

  !> @ingroup dev_graph_part
  !> グローバルコネクティビティからローカルコネクティビティを取得
  subroutine gedatsu_get_parted_connectivity_main(id, domain_id, &
    & global_node_graph, global_conn_graph, local_conn_graph)
    implicit none
    !> [in] 取得する分割番号
    integer(kint), intent(in) :: id
    !> [in] 節点が所属する領域番号配列
    integer(kint), intent(in) :: domain_id(:)
    !>
    type(gedatsu_graph) :: global_node_graph
    !>
    type(gedatsu_graph) :: global_conn_graph
    !>
    type(gedatsu_graph) :: local_conn_graph
    integer(kint) :: i, j, jS, jE, minid, in, n_conn, l_n_vertex, l_n_internal_vertex
    integer(kint), allocatable :: list(:)
    integer(kint), allocatable :: is_conn_flag(:)
    logical, allocatable :: is_used_node(:)

    call monolis_alloc_L_1d(is_used_node, global_node_graph%n_vertex)

    a1:do i = 1, global_node_graph%n_vertex
      jS = global_node_graph%index(i) + 1
      jE = global_node_graph%index(i + 1)
      if(domain_id(i) /= id) cycle
      is_used_node(i) = .true.
      do j = jS, jE
        in = global_node_graph%item(j)
        is_used_node(in) = .true.
      enddo
    enddo a1

    l_n_vertex = 0
    n_conn = 0

    !# 要素数と節点数を数える
    !# is_conn_flag = 0：領域外
    !# is_conn_flag = 1：領域内・内部
    !# is_conn_flag = 2：領域内・外部
    call monolis_alloc_I_1d(is_conn_flag, global_conn_graph%n_vertex)

    aa:do i = 1, global_conn_graph%n_vertex
      jS = global_conn_graph%index(i) + 1
      jE = global_conn_graph%index(i + 1)

      !# 領域外判定
      do j = jS, jE
        in = global_conn_graph%item(j)
        if(.not. is_used_node(in)) cycle aa
      enddo

      call monolis_alloc_I_1d(list, jE - jS + 1)
      do j = jS, jE
        in = global_conn_graph%item(j)
        list(j - jS + 1) = domain_id(in)
      enddo
      minid = minval(list)
      call monolis_dealloc_I_1d(list)

      !# 内部・外部判定
      if(minid == id)then
        is_conn_flag(i) = 1
      else
        is_conn_flag(i) = 2
      endif

      n_conn = n_conn + jE - jS + 1
      l_n_vertex = l_n_vertex + 1
    enddo aa

    local_conn_graph%n_vertex = l_n_vertex
    call monolis_alloc_I_1d(local_conn_graph%vertex_id, l_n_vertex)
    call monolis_alloc_I_1d(local_conn_graph%index, l_n_vertex + 1)
    call monolis_alloc_I_1d(local_conn_graph%item, n_conn)

    l_n_vertex = 0
    l_n_internal_vertex = 0
    n_conn = 0

    !! 内部要素の取得
    do i = 1, global_conn_graph%n_vertex
      if(is_conn_flag(i) == 1)then
        jS = global_conn_graph%index(i) + 1
        jE = global_conn_graph%index(i + 1)

        do j = jS, jE
          n_conn = n_conn + 1
          local_conn_graph%item(n_conn) = global_conn_graph%item(j)
        enddo

        l_n_vertex = l_n_vertex + 1
        l_n_internal_vertex = l_n_internal_vertex + 1
        local_conn_graph%index(l_n_vertex + 1) = n_conn
        local_conn_graph%vertex_id(l_n_vertex) = global_conn_graph%vertex_id(i)
      endif
    enddo

    local_conn_graph%n_internal_vertex = l_n_internal_vertex

    !! 外部要素の取得
    do i = 1, global_conn_graph%n_vertex
      if(is_conn_flag(i) == 2)then
        jS = global_conn_graph%index(i) + 1
        jE = global_conn_graph%index(i + 1)

        do j = jS, jE
          n_conn = n_conn + 1
          local_conn_graph%item(n_conn) = global_conn_graph%item(j)
        enddo

        l_n_vertex = l_n_vertex + 1
        local_conn_graph%index(l_n_vertex + 1) = n_conn
        local_conn_graph%vertex_id(l_n_vertex) = global_conn_graph%vertex_id(i)
      endif
    enddo
  end subroutine gedatsu_get_parted_connectivity_main

  !> @ingroup graph_part
  !> グラフ分割結果のメタグラフを出力
  subroutine gedatsu_get_metagraph(com, n_domain, metagraph)
    implicit none
    !> [in] com 構造体
    type(monolis_COM) :: com(:)
    !> [in] 分割数
    integer(kint) :: n_domain
    !> [out] graph 構造体
    type(gedatsu_graph) :: metagraph
    integer(kint) :: n_neib, i, j, in

    call gedatsu_graph_finalize(metagraph)

    metagraph%n_vertex = n_domain
    call monolis_alloc_I_1d(metagraph%vertex_id, n_domain)
    call monolis_alloc_I_1d(metagraph%index, n_domain + 1)

    do i = 1, n_domain
      metagraph%vertex_id(i) = i
    enddo

    n_neib = 0
    do i = 1, n_domain
      n_neib = n_neib + com(i)%recv_n_neib
      metagraph%index(i + 1) = n_neib
    enddo

    call monolis_alloc_I_1d(metagraph%item, n_neib)

    in = 0
    do i = 1, n_domain
      do j = 1, com(i)%recv_n_neib
        in = in + 1
        metagraph%item(in) = com(i)%recv_neib_pe(j)
      enddo
    enddo
  end subroutine gedatsu_get_metagraph
end module mod_gedatsu_graph_part
