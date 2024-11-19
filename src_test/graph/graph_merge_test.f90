!> グラフ結合テストモジュール
module mod_gedatsu_graph_merge_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

  contains
  subroutine gedatsu_graph_merge_test()
    implicit none
    call gedatsu_list_initialize_R_test()
    call gedatsu_list_initialize_I_test()
    call gedatsu_list_initialize_C_test()
    call gedatsu_list_finalize_R_test()
    call gedatsu_list_finalize_I_test()
    call gedatsu_list_finalize_C_test()
    call gedatsu_list_set_R_test()
    call gedatsu_list_set_I_test()
    call gedatsu_list_set_C_test()
    call gedatsu_list_get_R_test()
    call gedatsu_list_get_I_test()
    call gedatsu_list_get_C_test()

    call gedatsu_merge_nodal_subgraphs_test()
    call gedatsu_merge_connectivity_subgraphs_test()
    call gedatsu_merge_distval_R_test()
    call gedatsu_merge_distval_I_test()
    call gedatsu_merge_distval_C_test()
  end subroutine gedatsu_graph_merge_test

  subroutine gedatsu_list_initialize_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)

    call monolis_std_log_string("gedatsu_list_initialize_R")
    call gedatsu_list_initialize_R(list_struct_R, 1)
    call monolis_test_check_eq_I1("gedatsu_list_initialize_R", list_struct_R(1)%n, 0)
  end subroutine gedatsu_list_initialize_R_test

  subroutine gedatsu_list_initialize_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)

    call monolis_std_log_string("gedatsu_list_initialize_I")
    call gedatsu_list_initialize_I(list_struct_I, 1)
    call monolis_test_check_eq_I1("gedatsu_list_initialize_I", list_struct_I(1)%n, 0)
  end subroutine gedatsu_list_initialize_I_test

  subroutine gedatsu_list_initialize_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)

    call monolis_std_log_string("gedatsu_list_initialize_C")
    call gedatsu_list_initialize_C(list_struct_C, 1)
    call monolis_test_check_eq_I1("gedatsu_list_initialize_C", list_struct_C(1)%n, 0)
  end subroutine gedatsu_list_initialize_C_test

  subroutine gedatsu_list_finalize_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)

    call monolis_std_log_string("gedatsu_list_finalize_R")
    list_struct_R(1)%n = 1
    call gedatsu_list_finalize_R(list_struct_R)
    call monolis_test_check_eq_I1("gedatsu_list_finalize_R", list_struct_R(1)%n, 0)
  end subroutine gedatsu_list_finalize_R_test

  subroutine gedatsu_list_finalize_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)

    call monolis_std_log_string("gedatsu_list_finalize_I")
    list_struct_I(1)%n = 1
    call gedatsu_list_finalize_I(list_struct_I)
    call monolis_test_check_eq_I1("gedatsu_list_finalize_I", list_struct_I(1)%n, 0)
  end subroutine gedatsu_list_finalize_I_test

  subroutine gedatsu_list_finalize_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)

    call monolis_std_log_string("gedatsu_list_finalize_C")
    list_struct_C(1)%n = 1
    call gedatsu_list_finalize_C(list_struct_C)
    call monolis_test_check_eq_I1("gedatsu_list_finalize_C", list_struct_C(1)%n, 0)
  end subroutine gedatsu_list_finalize_C_test

  subroutine gedatsu_list_set_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)
    real(kdouble) :: array(1)

    call monolis_std_log_string("gedatsu_list_set_R")
    call gedatsu_list_initialize_R(list_struct_R, 1)
    array(1) = 1.0d0
    call gedatsu_list_set_R(list_struct_R, 1, 1, array)
    call monolis_test_check_eq_I1("gedatsu_list_set_R n", list_struct_R(1)%n, 1)
    call monolis_test_check_eq_R("gedatsu_list_set_R array", list_struct_R(1)%array, array)
  end subroutine gedatsu_list_set_R_test

  subroutine gedatsu_list_set_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)
    integer(kint) :: array(1)

    call monolis_std_log_string("gedatsu_list_set_I")
    call gedatsu_list_initialize_I(list_struct_I, 1)
    array(1) = 1
    call gedatsu_list_set_I(list_struct_I, 1, 1, array)
    call monolis_test_check_eq_I1("gedatsu_list_set_I n", list_struct_I(1)%n, 1)
    call monolis_test_check_eq_I("gedatsu_list_set_I array", list_struct_I(1)%array, array)
  end subroutine gedatsu_list_set_I_test

  subroutine gedatsu_list_set_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)
    complex(kdouble) :: array(1)

    call monolis_std_log_string("gedatsu_list_set_C")
    call gedatsu_list_initialize_C(list_struct_C, 1)
    array(1) = (1.0d0, 1.0d0)
    call gedatsu_list_set_C(list_struct_C, 1, 1, array)
    call monolis_test_check_eq_I1("gedatsu_list_set_C n", list_struct_C(1)%n, 1)
    call monolis_test_check_eq_C("gedatsu_list_set_C array", list_struct_C(1)%array, array)
  end subroutine gedatsu_list_set_C_test

  subroutine gedatsu_list_get_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)
    real(kdouble), allocatable :: array(:)

    call monolis_std_log_string("gedatsu_list_get_R")

    list_struct_R(1)%n = 1
    call monolis_alloc_R_1d(list_struct_R(1)%array, 1)
    list_struct_R(1)%array = 1.0d0

    call gedatsu_list_get_R(list_struct_R, 1, array)
    call monolis_test_check_eq_R("gedatsu_list_get_R", list_struct_R(1)%array, array)
  end subroutine gedatsu_list_get_R_test

  subroutine gedatsu_list_get_I_test()
    implicit none
    type(monolis_list_I) :: list_struct_I(1)
    integer(kint), allocatable :: array(:)

    call monolis_std_log_string("gedatsu_list_get_I")

    list_struct_I(1)%n = 1
    call monolis_alloc_I_1d(list_struct_I(1)%array, 1)
    list_struct_I(1)%array = 1

    call gedatsu_list_get_I(list_struct_I, 1, array)
    call monolis_test_check_eq_I("gedatsu_list_get_I", list_struct_I(1)%array, array)
  end subroutine gedatsu_list_get_I_test

  subroutine gedatsu_list_get_C_test()
    implicit none
    type(monolis_list_C) :: list_struct_C(1)
    complex(kdouble), allocatable :: array(:)

    call monolis_std_log_string("gedatsu_list_get_C")

    list_struct_C(1)%n = 1
    call monolis_alloc_C_1d(list_struct_C(1)%array, 1)
    list_struct_C(1)%array = (1.0d0, 1.0d0)

    call gedatsu_list_get_C(list_struct_C, 1, array)
    call monolis_test_check_eq_C("gedatsu_list_get_C", list_struct_C(1)%array, array)
  end subroutine gedatsu_list_get_C_test

  subroutine gedatsu_merge_nodal_subgraphs_test()
    implicit none
    type(gedatsu_graph) :: graphs(3), merged_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    integer(kint), allocatable :: edge(:,:), check_vertex_id(:), check_vertex_domain_id(:), check_index(:), check_item(:)

    call monolis_std_log_string("gedatsu_merge_nodal_subgraphs")

    call gedatsu_graph_initialize(graphs(1))
    call gedatsu_graph_initialize(graphs(2))
    call gedatsu_graph_initialize(graphs(3))
    call gedatsu_graph_set_n_vertex(graphs(1), 5)
    call gedatsu_graph_set_n_vertex(graphs(2), 5)
    call gedatsu_graph_set_n_vertex(graphs(3), 6)
    graphs(1)%n_internal_vertex = 2
    graphs(2)%n_internal_vertex = 2
    graphs(3)%n_internal_vertex = 1

    graphs(1)%vertex_id(1) = 1
    graphs(1)%vertex_id(2) = 4
    graphs(1)%vertex_id(3) = 2
    graphs(1)%vertex_id(4) = 5
    graphs(1)%vertex_id(5) = 6

    graphs(2)%vertex_id(1) = 2
    graphs(2)%vertex_id(2) = 3
    graphs(2)%vertex_id(3) = 1
    graphs(2)%vertex_id(4) = 4
    graphs(2)%vertex_id(5) = 5

    graphs(3)%vertex_id(1) = 5
    graphs(3)%vertex_id(2) = 2
    graphs(3)%vertex_id(3) = 3
    graphs(3)%vertex_id(4) = 4
    graphs(3)%vertex_id(5) = 6
    graphs(3)%vertex_id(6) = 7

    !> edgeの作成・グラフへの追加（計算点番号は、結合前グラフにおけるローカル番号）
    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,9) = 4
    edge(1,10) = 4; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 3
    edge(1,12) = 4; edge(2,12) = 5
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(graphs(1), 14, edge)

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,9) = 1
    edge(1,10) = 4; edge(2,10) = 3
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 5; edge(2,12) = 1
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(graphs(2), 14, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 1
    edge(1,10) = 3; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 1
    edge(1,12) = 4; edge(2,12) = 2
    edge(1,13) = 4; edge(2,13) = 5
    edge(1,14) = 5; edge(2,14) = 1
    edge(1,15) = 5; edge(2,15) = 4
    edge(1,16) = 5; edge(2,16) = 6
    edge(1,17) = 6; edge(2,17) = 1
    edge(1,18) = 6; edge(2,18) = 5
    call gedatsu_graph_set_edge(graphs(3), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    !> 結合
    call gedatsu_merge_nodal_subgraphs(3, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> 結合結果の確認
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_vertex", &
    & merged_graph%n_vertex, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID n_internal_vertex", &
    & merged_graph%n_internal_vertex, 5)
    call monolis_alloc_I_1d(check_vertex_domain_id, 7)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_domain_id", &
    & merged_graph%vertex_domain_id, check_vertex_domain_id)
    call monolis_alloc_I_1d(check_vertex_id, 7)
    check_vertex_id(1) = 1
    check_vertex_id(2) = 4
    check_vertex_id(3) = 2
    check_vertex_id(4) = 3
    check_vertex_id(5) = 5
    check_vertex_id(6) = 6
    check_vertex_id(7) = 7
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID vertex_id", &
    & merged_graph%vertex_id, check_vertex_id)
    call monolis_alloc_I_1d(check_index, 8)
    check_index(1) = 0
    check_index(2) = 2
    check_index(3) = 6
    check_index(4) = 10
    check_index(5) = 12
    check_index(6) = 17
    check_index(7) = 20
    check_index(8) = 22
    call monolis_alloc_I_1d(check_item, 22)
    check_item(1) = 2
    check_item(2) = 3
    check_item(3) = 1
    check_item(4) = 3
    check_item(5) = 5
    check_item(6) = 6
    check_item(7) = 1
    check_item(8) = 2
    check_item(9) = 4
    check_item(10) = 5
    check_item(11) = 3
    check_item(12) = 5
    check_item(13) = 2
    check_item(14) = 3
    check_item(15) = 4
    check_item(16) = 6
    check_item(17) = 7
    check_item(18) = 2
    check_item(19) = 5
    check_item(20) = 7
    check_item(21) = 5
    check_item(22) = 6
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_DOMAIN_ID item", &
    & merged_graph%item, check_item)

    !> 結合
    call gedatsu_merge_nodal_subgraphs(3, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_NODAL_ID)

    !> 結合結果の確認
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID n_vertex", &
    & merged_graph%n_vertex, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID n_internal_vertex", &
    & merged_graph%n_internal_vertex, 5)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID vertex_domain_id", &
    & merged_graph%vertex_domain_id, check_vertex_domain_id)
    check_vertex_id(1) = 1
    check_vertex_id(2) = 2
    check_vertex_id(3) = 3
    check_vertex_id(4) = 4
    check_vertex_id(5) = 5
    check_vertex_id(6) = 6
    check_vertex_id(7) = 7
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID vertex_id", &
    & merged_graph%vertex_id, check_vertex_id)
    check_index(1) = 0
    check_index(2) = 2
    check_index(3) = 6
    check_index(4) = 8
    check_index(5) = 12
    check_index(6) = 17
    check_index(7) = 20
    check_index(8) = 22
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID index", &
    & merged_graph%index, check_index)
    check_item(1) = 2
    check_item(2) = 4
    check_item(3) = 1
    check_item(4) = 3
    check_item(5) = 4
    check_item(6) = 5
    check_item(7) = 2
    check_item(8) = 5
    check_item(9) = 1
    check_item(10) = 2
    check_item(11) = 5
    check_item(12) = 6
    check_item(13) = 2
    check_item(14) = 3
    check_item(15) = 4
    check_item(16) = 6
    check_item(17) = 7
    check_item(18) = 4
    check_item(19) = 5
    check_item(20) = 7
    check_item(21) = 5
    check_item(22) = 6
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph ORDER_NODAL_ID item", &
    & merged_graph%item, check_item)
  end subroutine gedatsu_merge_nodal_subgraphs_test

  subroutine gedatsu_merge_connectivity_subgraphs_test()
    implicit none
    type(gedatsu_graph) :: nodal_graphs(3), merged_nodal_graph, conn_graphs(3), merged_conn_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    integer(kint), allocatable :: edge(:,:), check_vertex_domain_id(:), check_vertex_id(:), check_index(:), check_item(:)

    call monolis_std_log_string("gedatsu_merge_connectivity_subgraphs")

    call gedatsu_graph_initialize(nodal_graphs(1))
    call gedatsu_graph_initialize(nodal_graphs(2))
    call gedatsu_graph_initialize(nodal_graphs(3))
    call gedatsu_graph_set_n_vertex(nodal_graphs(1), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(2), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(3), 6)
    nodal_graphs(1)%n_internal_vertex = 2
    nodal_graphs(2)%n_internal_vertex = 2
    nodal_graphs(3)%n_internal_vertex = 1

    nodal_graphs(1)%vertex_id(1) = 1
    nodal_graphs(1)%vertex_id(2) = 4
    nodal_graphs(1)%vertex_id(3) = 2
    nodal_graphs(1)%vertex_id(4) = 5
    nodal_graphs(1)%vertex_id(5) = 6

    nodal_graphs(2)%vertex_id(1) = 2
    nodal_graphs(2)%vertex_id(2) = 3
    nodal_graphs(2)%vertex_id(3) = 1
    nodal_graphs(2)%vertex_id(4) = 4
    nodal_graphs(2)%vertex_id(5) = 5

    nodal_graphs(3)%vertex_id(1) = 5
    nodal_graphs(3)%vertex_id(2) = 2
    nodal_graphs(3)%vertex_id(3) = 3
    nodal_graphs(3)%vertex_id(4) = 4
    nodal_graphs(3)%vertex_id(5) = 6
    nodal_graphs(3)%vertex_id(6) = 7

    !> edgeの作成・グラフへの追加（計算点番号は、結合前グラフにおけるローカル番号）
    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,9) = 4
    edge(1,10) = 4; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 3
    edge(1,12) = 4; edge(2,12) = 5
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(1), 14, edge)

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,9) = 1
    edge(1,10) = 4; edge(2,10) = 3
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 5; edge(2,12) = 1
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(2), 14, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 1
    edge(1,10) = 3; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 1
    edge(1,12) = 4; edge(2,12) = 2
    edge(1,13) = 4; edge(2,13) = 5
    edge(1,14) = 5; edge(2,14) = 1
    edge(1,15) = 5; edge(2,15) = 4
    edge(1,16) = 5; edge(2,16) = 6
    edge(1,17) = 6; edge(2,17) = 1
    edge(1,18) = 6; edge(2,18) = 5
    call gedatsu_graph_set_edge(nodal_graphs(3), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    call gedatsu_merge_nodal_subgraphs(3, nodal_graphs, monoCOMs, merged_nodal_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> ここからコネクティビティグラフの結合
    call gedatsu_graph_initialize(conn_graphs(1))
    call gedatsu_graph_initialize(conn_graphs(2))
    call gedatsu_graph_initialize(conn_graphs(3))
    call gedatsu_graph_set_n_vertex(conn_graphs(1), 3)
    call gedatsu_graph_set_n_vertex(conn_graphs(2), 3)
    call gedatsu_graph_set_n_vertex(conn_graphs(3), 4)
    conn_graphs(1)%n_internal_vertex = 3
    conn_graphs(2)%n_internal_vertex = 1
    conn_graphs(3)%n_internal_vertex = 1

    conn_graphs(1)%vertex_id(1) = 1
    conn_graphs(1)%vertex_id(2) = 2
    conn_graphs(1)%vertex_id(3) = 4

    conn_graphs(2)%vertex_id(1) = 3
    conn_graphs(2)%vertex_id(2) = 1
    conn_graphs(2)%vertex_id(3) = 2

    conn_graphs(3)%vertex_id(1) = 5
    conn_graphs(3)%vertex_id(2) = 3
    conn_graphs(3)%vertex_id(3) = 2
    conn_graphs(3)%vertex_id(4) = 4

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 9)
    edge(1,1) = 1; edge(2,1) = 1
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 2
    edge(1,4) = 2; edge(2,4) = 2
    edge(1,5) = 2; edge(2,5) = 3
    edge(1,6) = 2; edge(2,6) = 4
    edge(1,7) = 3; edge(2,7) = 2
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 5
    call gedatsu_graph_set_edge_conn(conn_graphs(1), 9, edge)

    edge(1,1) = 1; edge(2,1) = 1
    edge(1,2) = 1; edge(2,2) = 2
    edge(1,3) = 1; edge(2,3) = 5
    edge(1,4) = 2; edge(2,4) = 1
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 3
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 5
    edge(1,9) = 3; edge(2,9) = 4
    call gedatsu_graph_set_edge_conn(conn_graphs(2), 9, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 12)
    edge(1,1) = 1; edge(2,1) = 1
    edge(1,2) = 1; edge(2,2) = 6
    edge(1,3) = 1; edge(2,3) = 5
    edge(1,4) = 2; edge(2,4) = 1
    edge(1,5) = 2; edge(2,5) = 2
    edge(1,6) = 2; edge(2,6) = 3
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 2
    edge(1,10) = 4; edge(2,10) = 1
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 4; edge(2,12) = 4
    call gedatsu_graph_set_edge_conn(conn_graphs(3), 12, edge)

    call gedatsu_merge_connectivity_subgraphs(3, nodal_graphs, merged_nodal_graph, merged_monoCOM, &
    & 3, conn_graphs, merged_conn_graph)

    !> 結合結果の確認
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test conn_graph n_vertex", merged_conn_graph%n_vertex, 5)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test conn_graph n_internal_vertex", &
    & merged_conn_graph%n_internal_vertex, 5)
    call monolis_dealloc_I_1d(check_vertex_domain_id)
    call monolis_alloc_I_1d(check_vertex_domain_id, 5)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test conn_graph vertex_domain_id", &
    & merged_conn_graph%vertex_domain_id, check_vertex_domain_id)
    call monolis_alloc_I_1d(check_vertex_id, 5)
    check_vertex_id(1) = 1
    check_vertex_id(2) = 2
    check_vertex_id(3) = 3
    check_vertex_id(4) = 4
    check_vertex_id(5) = 5
    call monolis_test_check_eq_I("gedatsu_graph_merge_test conn_graph vertex_id", merged_conn_graph%vertex_id, check_vertex_id)
    call monolis_dealloc_I_1d(check_index)
    call monolis_alloc_I_1d(check_index, 6)
    check_index(1) = 0
    check_index(2) = 3
    check_index(3) = 6
    check_index(4) = 9
    check_index(5) = 12
    check_index(6) = 15
    call monolis_test_check_eq_I("gedatsu_graph_merge_test conn_graph index", merged_conn_graph%index, check_index)
    call monolis_dealloc_I_1d(check_item)
    call monolis_alloc_I_1d(check_item, 15)
    check_item(1) = 1
    check_item(2) = 3
    check_item(3) = 2
    check_item(4) = 2
    check_item(5) = 3
    check_item(6) = 5
    check_item(7) = 3
    check_item(8) = 4
    check_item(9) = 5
    check_item(10) = 2
    check_item(11) = 5
    check_item(12) = 6
    check_item(13) = 5
    check_item(14) = 7
    check_item(15) = 6
    call monolis_test_check_eq_I("gedatsu_graph_merge_test conn_graph item", merged_conn_graph%item, check_item)
  end subroutine gedatsu_merge_connectivity_subgraphs_test

  subroutine gedatsu_merge_distval_R_test()
    implicit none
    type(gedatsu_graph) :: nodal_graphs(3), merged_nodal_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    type(monolis_list_R) :: list_struct_R(3)
    type(monolis_list_I) :: n_dof_list(3)
    integer(kint), allocatable :: edge(:,:), merged_n_dof_list(:), check_n_dof_list(:)
    real(kdouble), allocatable ::  merged_array_R(:), check_array(:)

    call monolis_std_log_string("gedatsu_merge_distval_R")

    call gedatsu_graph_initialize(nodal_graphs(1))
    call gedatsu_graph_initialize(nodal_graphs(2))
    call gedatsu_graph_initialize(nodal_graphs(3))
    call gedatsu_graph_set_n_vertex(nodal_graphs(1), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(2), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(3), 6)
    nodal_graphs(1)%n_internal_vertex = 2
    nodal_graphs(2)%n_internal_vertex = 2
    nodal_graphs(3)%n_internal_vertex = 1

    nodal_graphs(1)%vertex_id(1) = 1
    nodal_graphs(1)%vertex_id(2) = 4
    nodal_graphs(1)%vertex_id(3) = 2
    nodal_graphs(1)%vertex_id(4) = 5
    nodal_graphs(1)%vertex_id(5) = 6

    nodal_graphs(2)%vertex_id(1) = 2
    nodal_graphs(2)%vertex_id(2) = 3
    nodal_graphs(2)%vertex_id(3) = 1
    nodal_graphs(2)%vertex_id(4) = 4
    nodal_graphs(2)%vertex_id(5) = 5

    nodal_graphs(3)%vertex_id(1) = 5
    nodal_graphs(3)%vertex_id(2) = 2
    nodal_graphs(3)%vertex_id(3) = 3
    nodal_graphs(3)%vertex_id(4) = 4
    nodal_graphs(3)%vertex_id(5) = 6
    nodal_graphs(3)%vertex_id(6) = 7

    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,9) = 4
    edge(1,10) = 4; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 3
    edge(1,12) = 4; edge(2,12) = 5
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(1), 14, edge)

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,9) = 1
    edge(1,10) = 4; edge(2,10) = 3
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 5; edge(2,12) = 1
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(2), 14, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 1
    edge(1,10) = 3; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 1
    edge(1,12) = 4; edge(2,12) = 2
    edge(1,13) = 4; edge(2,13) = 5
    edge(1,14) = 5; edge(2,14) = 1
    edge(1,15) = 5; edge(2,15) = 4
    edge(1,16) = 5; edge(2,16) = 6
    edge(1,17) = 6; edge(2,17) = 1
    edge(1,18) = 6; edge(2,18) = 5
    call gedatsu_graph_set_edge(nodal_graphs(3), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    call gedatsu_merge_nodal_subgraphs(3, nodal_graphs, monoCOMs, merged_nodal_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> 物理量配列の結合
    n_dof_list(1)%n = 5
    n_dof_list(2)%n = 5
    n_dof_list(3)%n = 6
    call monolis_alloc_I_1d(n_dof_list(1)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(2)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(3)%array, 6)
    n_dof_list(1)%array(:) = 1
    n_dof_list(2)%array(:) = 1
    n_dof_list(3)%array(:) = 1

    list_struct_R(1)%n = 5
    list_struct_R(2)%n = 5
    list_struct_R(3)%n = 6
    call monolis_alloc_R_1d(list_struct_R(1)%array, 5)
    call monolis_alloc_R_1d(list_struct_R(2)%array, 5)
    call monolis_alloc_R_1d(list_struct_R(3)%array, 6)
    list_struct_R(1)%array(1) = 1.0d0
    list_struct_R(1)%array(2) = 4.0d0
    list_struct_R(1)%array(3) = 2.0d0
    list_struct_R(1)%array(4) = 5.0d0
    list_struct_R(1)%array(5) = 6.0d0
    list_struct_R(2)%array(1) = 2.0d0
    list_struct_R(2)%array(2) = 3.0d0
    list_struct_R(2)%array(3) = 1.0d0
    list_struct_R(2)%array(4) = 4.0d0
    list_struct_R(2)%array(5) = 5.0d0
    list_struct_R(3)%array(1) = 5.0d0
    list_struct_R(3)%array(2) = 2.0d0
    list_struct_R(3)%array(3) = 3.0d0
    list_struct_R(3)%array(4) = 4.0d0
    list_struct_R(3)%array(5) = 6.0d0
    list_struct_R(3)%array(6) = 7.0d0

    call gedatsu_merge_distval_R(3, nodal_graphs, merged_nodal_graph, n_dof_list, list_struct_R, merged_n_dof_list, merged_array_R)

    !> 確認
    call monolis_alloc_I_1d(check_n_dof_list, 7)
    call monolis_alloc_R_1d(check_array, 7)
    check_n_dof_list(1) = 1
    check_n_dof_list(2) = 1
    check_n_dof_list(3) = 1
    check_n_dof_list(4) = 1
    check_n_dof_list(5) = 1
    check_n_dof_list(6) = 1
    check_n_dof_list(7) = 1
    check_array(1) = 1.0d0      !> nodal_graphsをORDER_DOMAIN_IDで結合したのでこうなる
    check_array(2) = 4.0d0
    check_array(3) = 2.0d0
    check_array(4) = 3.0d0
    check_array(5) = 5.0d0
    check_array(6) = 6.0d0
    check_array(7) = 7.0d0

    call monolis_test_check_eq_I("gedatsu_graph_merge_test dist_val_R n_dof_list", merged_n_dof_list, check_n_dof_list)
    call monolis_test_check_eq_R("gedatsu_graph_merge_test dist_val_R array", merged_array_R, check_array)
  end subroutine gedatsu_merge_distval_R_test

  subroutine gedatsu_merge_distval_I_test()
    implicit none
    type(gedatsu_graph) :: nodal_graphs(3), merged_nodal_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    type(monolis_list_I) :: list_struct_I(3)
    type(monolis_list_I) :: n_dof_list(3)
    integer(kint), allocatable :: edge(:,:), merged_n_dof_list(:), check_n_dof_list(:), merged_array_I(:), check_array(:)

    call monolis_std_log_string("gedatsu_merge_distval_I")

    call gedatsu_graph_initialize(nodal_graphs(1))
    call gedatsu_graph_initialize(nodal_graphs(2))
    call gedatsu_graph_initialize(nodal_graphs(3))
    call gedatsu_graph_set_n_vertex(nodal_graphs(1), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(2), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(3), 6)
    nodal_graphs(1)%n_internal_vertex = 2
    nodal_graphs(2)%n_internal_vertex = 2
    nodal_graphs(3)%n_internal_vertex = 1

    nodal_graphs(1)%vertex_id(1) = 1
    nodal_graphs(1)%vertex_id(2) = 4
    nodal_graphs(1)%vertex_id(3) = 2
    nodal_graphs(1)%vertex_id(4) = 5
    nodal_graphs(1)%vertex_id(5) = 6

    nodal_graphs(2)%vertex_id(1) = 2
    nodal_graphs(2)%vertex_id(2) = 3
    nodal_graphs(2)%vertex_id(3) = 1
    nodal_graphs(2)%vertex_id(4) = 4
    nodal_graphs(2)%vertex_id(5) = 5

    nodal_graphs(3)%vertex_id(1) = 5
    nodal_graphs(3)%vertex_id(2) = 2
    nodal_graphs(3)%vertex_id(3) = 3
    nodal_graphs(3)%vertex_id(4) = 4
    nodal_graphs(3)%vertex_id(5) = 6
    nodal_graphs(3)%vertex_id(6) = 7

    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,9) = 4
    edge(1,10) = 4; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 3
    edge(1,12) = 4; edge(2,12) = 5
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(1), 14, edge)

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,9) = 1
    edge(1,10) = 4; edge(2,10) = 3
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 5; edge(2,12) = 1
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(2), 14, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 1
    edge(1,10) = 3; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 1
    edge(1,12) = 4; edge(2,12) = 2
    edge(1,13) = 4; edge(2,13) = 5
    edge(1,14) = 5; edge(2,14) = 1
    edge(1,15) = 5; edge(2,15) = 4
    edge(1,16) = 5; edge(2,16) = 6
    edge(1,17) = 6; edge(2,17) = 1
    edge(1,18) = 6; edge(2,18) = 5
    call gedatsu_graph_set_edge(nodal_graphs(3), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    call gedatsu_merge_nodal_subgraphs(3, nodal_graphs, monoCOMs, merged_nodal_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> 物理量配列の結合
    n_dof_list(1)%n = 5
    n_dof_list(2)%n = 5
    n_dof_list(3)%n = 6
    call monolis_alloc_I_1d(n_dof_list(1)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(2)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(3)%array, 6)
    n_dof_list(1)%array(:) = 1
    n_dof_list(2)%array(:) = 1
    n_dof_list(3)%array(:) = 1

    list_struct_I(1)%n = 5
    list_struct_I(2)%n = 5
    list_struct_I(3)%n = 6
    call monolis_alloc_I_1d(list_struct_I(1)%array, 5)
    call monolis_alloc_I_1d(list_struct_I(2)%array, 5)
    call monolis_alloc_I_1d(list_struct_I(3)%array, 6)
    list_struct_I(1)%array(1) = 1
    list_struct_I(1)%array(2) = 4
    list_struct_I(1)%array(3) = 2
    list_struct_I(1)%array(4) = 5
    list_struct_I(1)%array(5) = 6
    list_struct_I(2)%array(1) = 2
    list_struct_I(2)%array(2) = 3
    list_struct_I(2)%array(3) = 1
    list_struct_I(2)%array(4) = 4
    list_struct_I(2)%array(5) = 5
    list_struct_I(3)%array(1) = 5
    list_struct_I(3)%array(2) = 2
    list_struct_I(3)%array(3) = 3
    list_struct_I(3)%array(4) = 4
    list_struct_I(3)%array(5) = 6
    list_struct_I(3)%array(6) = 7

    call gedatsu_merge_distval_I(3, nodal_graphs, merged_nodal_graph, n_dof_list, list_struct_I, merged_n_dof_list, merged_array_I)

    !> 確認
    call monolis_alloc_I_1d(check_n_dof_list, 7)
    call monolis_alloc_I_1d(check_array, 7)
    check_n_dof_list(1) = 1
    check_n_dof_list(2) = 1
    check_n_dof_list(3) = 1
    check_n_dof_list(4) = 1
    check_n_dof_list(5) = 1
    check_n_dof_list(6) = 1
    check_n_dof_list(7) = 1
    check_array(1) = 1
    check_array(2) = 4
    check_array(3) = 2
    check_array(4) = 3
    check_array(5) = 5
    check_array(6) = 6
    check_array(7) = 7

    call monolis_test_check_eq_I("gedatsu_graph_merge_test dist_val_I n_dof_list", merged_n_dof_list, check_n_dof_list)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test dist_val_I array", merged_array_I, check_array)
  end subroutine gedatsu_merge_distval_I_test

  subroutine gedatsu_merge_distval_C_test()
    implicit none
    type(gedatsu_graph) :: nodal_graphs(3), merged_nodal_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    type(monolis_list_C) :: list_struct_C(3)
    type(monolis_list_I) :: n_dof_list(3)
    integer(kint), allocatable :: edge(:,:), merged_n_dof_list(:), check_n_dof_list(:)
    complex(kdouble), allocatable ::  merged_array_C(:), check_array(:)

    call monolis_std_log_string("gedatsu_merge_distval_C")

    call gedatsu_graph_initialize(nodal_graphs(1))
    call gedatsu_graph_initialize(nodal_graphs(2))
    call gedatsu_graph_initialize(nodal_graphs(3))
    call gedatsu_graph_set_n_vertex(nodal_graphs(1), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(2), 5)
    call gedatsu_graph_set_n_vertex(nodal_graphs(3), 6)
    nodal_graphs(1)%n_internal_vertex = 2
    nodal_graphs(2)%n_internal_vertex = 2
    nodal_graphs(3)%n_internal_vertex = 1

    nodal_graphs(1)%vertex_id(1) = 1
    nodal_graphs(1)%vertex_id(2) = 4
    nodal_graphs(1)%vertex_id(3) = 2
    nodal_graphs(1)%vertex_id(4) = 5
    nodal_graphs(1)%vertex_id(5) = 6

    nodal_graphs(2)%vertex_id(1) = 2
    nodal_graphs(2)%vertex_id(2) = 3
    nodal_graphs(2)%vertex_id(3) = 1
    nodal_graphs(2)%vertex_id(4) = 4
    nodal_graphs(2)%vertex_id(5) = 5

    nodal_graphs(3)%vertex_id(1) = 5
    nodal_graphs(3)%vertex_id(2) = 2
    nodal_graphs(3)%vertex_id(3) = 3
    nodal_graphs(3)%vertex_id(4) = 4
    nodal_graphs(3)%vertex_id(5) = 6
    nodal_graphs(3)%vertex_id(6) = 7

    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,9) = 4
    edge(1,10) = 4; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 3
    edge(1,12) = 4; edge(2,12) = 5
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(1), 14, edge)

    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,9) = 1
    edge(1,10) = 4; edge(2,10) = 3
    edge(1,11) = 4; edge(2,11) = 5
    edge(1,12) = 5; edge(2,12) = 1
    edge(1,13) = 5; edge(2,13) = 2
    edge(1,14) = 5; edge(2,14) = 4
    call gedatsu_graph_set_edge(nodal_graphs(2), 14, edge)

    call monolis_dealloc_I_2d(edge)
    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,9) = 1
    edge(1,10) = 3; edge(2,10) = 2
    edge(1,11) = 4; edge(2,11) = 1
    edge(1,12) = 4; edge(2,12) = 2
    edge(1,13) = 4; edge(2,13) = 5
    edge(1,14) = 5; edge(2,14) = 1
    edge(1,15) = 5; edge(2,15) = 4
    edge(1,16) = 5; edge(2,16) = 6
    edge(1,17) = 6; edge(2,17) = 1
    edge(1,18) = 6; edge(2,18) = 5
    call gedatsu_graph_set_edge(nodal_graphs(3), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    call gedatsu_merge_nodal_subgraphs(3, nodal_graphs, monoCOMs, merged_nodal_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> 物理量配列の結合
    n_dof_list(1)%n = 5
    n_dof_list(2)%n = 5
    n_dof_list(3)%n = 6
    call monolis_alloc_I_1d(n_dof_list(1)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(2)%array, 5)
    call monolis_alloc_I_1d(n_dof_list(3)%array, 6)
    n_dof_list(1)%array(:) = 1
    n_dof_list(2)%array(:) = 1
    n_dof_list(3)%array(:) = 1

    list_struct_C(1)%n = 5
    list_struct_C(2)%n = 5
    list_struct_C(3)%n = 6
    call monolis_alloc_C_1d(list_struct_C(1)%array, 5)
    call monolis_alloc_C_1d(list_struct_C(2)%array, 5)
    call monolis_alloc_C_1d(list_struct_C(3)%array, 6)
    list_struct_C(1)%array(1) = (1.0d0, 1.0d0)
    list_struct_C(1)%array(2) = (4.0d0, 1.0d0)
    list_struct_C(1)%array(3) = (2.0d0, 1.0d0)
    list_struct_C(1)%array(4) = (5.0d0, 1.0d0)
    list_struct_C(1)%array(5) = (6.0d0, 1.0d0)
    list_struct_C(2)%array(1) = (2.0d0, 1.0d0)
    list_struct_C(2)%array(2) = (3.0d0, 1.0d0)
    list_struct_C(2)%array(3) = (1.0d0, 1.0d0)
    list_struct_C(2)%array(4) = (4.0d0, 1.0d0)
    list_struct_C(2)%array(5) = (5.0d0, 1.0d0)
    list_struct_C(3)%array(1) = (5.0d0, 1.0d0)
    list_struct_C(3)%array(2) = (2.0d0, 1.0d0)
    list_struct_C(3)%array(3) = (3.0d0, 1.0d0)
    list_struct_C(3)%array(4) = (4.0d0, 1.0d0)
    list_struct_C(3)%array(5) = (6.0d0, 1.0d0)
    list_struct_C(3)%array(6) = (7.0d0, 1.0d0)

    call gedatsu_merge_distval_C(3, nodal_graphs, merged_nodal_graph, n_dof_list, list_struct_C, merged_n_dof_list, merged_array_C)

    !> 確認
    call monolis_alloc_I_1d(check_n_dof_list, 7)
    call monolis_alloc_C_1d(check_array, 7)
    check_n_dof_list(1) = 1
    check_n_dof_list(2) = 1
    check_n_dof_list(3) = 1
    check_n_dof_list(4) = 1
    check_n_dof_list(5) = 1
    check_n_dof_list(6) = 1
    check_n_dof_list(7) = 1
    check_array(1) = (1.0d0, 1.0d0)
    check_array(2) = (4.0d0, 1.0d0)
    check_array(3) = (2.0d0, 1.0d0)
    check_array(4) = (3.0d0, 1.0d0)
    check_array(5) = (5.0d0, 1.0d0)
    check_array(6) = (6.0d0, 1.0d0)
    check_array(7) = (7.0d0, 1.0d0)

    call monolis_test_check_eq_I("gedatsu_graph_merge_test dist_val_C n_dof_list", merged_n_dof_list, check_n_dof_list)
    call monolis_test_check_eq_C("gedatsu_graph_merge_test dist_val_C array", merged_array_C, check_array)
  end subroutine gedatsu_merge_distval_C_test
end module mod_gedatsu_graph_merge_test
