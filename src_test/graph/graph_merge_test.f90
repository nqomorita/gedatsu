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
  end subroutine gedatsu_list_initialize_I_test

  subroutine gedatsu_list_initialize_C_test()
    implicit none
  end subroutine gedatsu_list_initialize_C_test

  subroutine gedatsu_list_finalize_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R(1)

    call monolis_std_log_string("gedatsu_list_finalize_R")

    call gedatsu_list_finalize_R(list_struct_R)
  end subroutine gedatsu_list_finalize_R_test

  subroutine gedatsu_list_finalize_I_test()
    implicit none
  end subroutine gedatsu_list_finalize_I_test

  subroutine gedatsu_list_finalize_C_test()
    implicit none
  end subroutine gedatsu_list_finalize_C_test

  subroutine gedatsu_list_set_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R

    call monolis_std_log_string("gedatsu_list_set_R")

    ! call gedatsu_list_set_R(list_struct_R, )
  end subroutine gedatsu_list_set_R_test

  subroutine gedatsu_list_set_I_test()
    implicit none
  end subroutine gedatsu_list_set_I_test

  subroutine gedatsu_list_set_C_test()
    implicit none
  end subroutine gedatsu_list_set_C_test

  subroutine gedatsu_list_get_R_test()
    implicit none
    type(monolis_list_R) :: list_struct_R

    call monolis_std_log_string("gedatsu_list_get_R")

    ! call gedatsu_list_get_R(list_struct_R,)
  end subroutine gedatsu_list_get_R_test

  subroutine gedatsu_list_get_I_test()
    implicit none
  end subroutine gedatsu_list_get_I_test

  subroutine gedatsu_list_get_C_test()
    implicit none
  end subroutine gedatsu_list_get_C_test

  subroutine gedatsu_merge_nodal_subgraphs_test()
    implicit none
    type(gedatsu_graph) :: graphs(3), merged_graph
    type(monolis_COM) :: monoCOMs(3), merged_monoCOM
    integer(kint), allocatable :: edge(:,:), check_id(:), check_index(:), check_item(:)

    call monolis_std_log_string("gedatsu_graph_partition")

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
    call monolis_alloc_I_2d(edge, 2, 13)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 2; edge(2,3) = 1
    edge(1,4) = 2; edge(2,4) = 3
    edge(1,5) = 2; edge(2,5) = 4
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 2
    edge(1,9) = 3; edge(2,8) = 4
    edge(1,10) = 4; edge(2,8) = 3
    edge(1,11) = 4; edge(2,8) = 5
    edge(1,12) = 5; edge(2,8) = 2
    edge(1,13) = 6; edge(2,8) = 4
    call gedatsu_graph_set_edge(graphs(1), 13, edge)

    call monolis_alloc_I_2d(edge, 2, 14)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 2; edge(2,5) = 1
    edge(1,6) = 2; edge(2,6) = 5
    edge(1,7) = 3; edge(2,7) = 1
    edge(1,8) = 3; edge(2,8) = 4
    edge(1,9) = 4; edge(2,8) = 1
    edge(1,10) = 4; edge(2,8) =3
    edge(1,11) = 4; edge(2,8) =5
    edge(1,12) = 5; edge(2,8) =1
    edge(1,13) = 6; edge(2,8) =2
    edge(1,14) = 6; edge(2,8) =4
    call gedatsu_graph_set_edge(graphs(1), 14, edge)

    call monolis_alloc_I_2d(edge, 2, 18)
    edge(1,1) = 1; edge(2,1) = 2
    edge(1,2) = 1; edge(2,2) = 3
    edge(1,3) = 1; edge(2,3) = 4
    edge(1,4) = 1; edge(2,4) = 5
    edge(1,5) = 1; edge(2,5) = 6
    edge(1,6) = 2; edge(2,6) = 1
    edge(1,7) = 2; edge(2,7) = 3
    edge(1,8) = 2; edge(2,8) = 4
    edge(1,9) = 3; edge(2,8) = 1
    edge(1,10) = 3; edge(2,8) =2
    edge(1,11) = 4; edge(2,8) =1
    edge(1,12) = 4; edge(2,8) =2
    edge(1,13) = 4; edge(2,8) =5
    edge(1,13) = 5; edge(2,8) =1
    edge(1,13) = 5; edge(2,8) =4
    edge(1,13) = 5; edge(2,8) =6
    edge(1,13) = 6; edge(2,8) =1
    edge(1,13) = 6; edge(2,8) =5
    call gedatsu_graph_set_edge(graphs(1), 18, edge)

    call monolis_com_initialize_by_self(monoCOMs(1))
    call monolis_com_initialize_by_self(monoCOMs(2))
    call monolis_com_initialize_by_self(monoCOMs(3))

    !> 結合
    call gedatsu_merge_nodal_subgraphs(3, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_DOMAIN_ID)

    !> 結合結果の確認
    call monolis_alloc_I_1d(check_id, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph n_vertex", merged_graph%n_vertex, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph n_internal_vertex", merged_graph%n_internal_vertex, 5)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph vertex_domain_id", merged_graph%vertex_domain_id, check_id)
    check_id(1) = 1
    check_id(2) = 4
    check_id(3) = 2
    check_id(4) = 3
    check_id(5) = 5
    check_id(6) = 6
    check_id(7) = 7
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph vertex_id", merged_graph%vertex_id, check_id)
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

    !> 結合
    call gedatsu_merge_nodal_subgraphs(3, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_NODAL_ID)

    !> 結合結果の確認
    call monolis_alloc_I_1d(check_id, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph n_vertex", merged_graph%n_vertex, 7)
    call monolis_test_check_eq_I1("gedatsu_graph_merge_test nodal_graph n_internal_vertex", merged_graph%n_internal_vertex, 5)
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph vertex_domain_id", merged_graph%vertex_domain_id, check_id)
    check_id(1) = 1
    check_id(2) = 2
    check_id(3) = 3
    check_id(4) = 4
    check_id(5) = 5
    check_id(6) = 6
    check_id(7) = 7
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph vertex_id", merged_graph%vertex_id, check_id)
    call monolis_alloc_I_1d(check_index, 8)
    check_index(1) = 0
    check_index(2) = 2
    check_index(3) = 6
    check_index(4) = 8
    check_index(5) = 12
    check_index(6) = 17
    check_index(7) = 20
    check_index(8) = 22
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph index", merged_graph%index, check_index)
    call monolis_alloc_I_1d(check_item, 21)
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
    call monolis_test_check_eq_I("gedatsu_graph_merge_test nodal_graph item", merged_graph%item, check_item)
  end subroutine gedatsu_merge_nodal_subgraphs_test

  subroutine gedatsu_merge_connectivity_subgraphs_test()
    implicit none
  end subroutine gedatsu_merge_connectivity_subgraphs_test

  subroutine gedatsu_merge_distval_R_test()
    implicit none
  end subroutine gedatsu_merge_distval_R_test

  subroutine gedatsu_merge_distval_I_test()
    implicit none
  end subroutine gedatsu_merge_distval_I_test

  subroutine gedatsu_merge_distval_C_test()
    implicit none
  end subroutine gedatsu_merge_distval_C_test
end module mod_gedatsu_graph_merge_test
