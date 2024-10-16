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
    type(gedatsu_graph) :: graphs(2), merged_graph
    type(monolis_COM) :: monoCOMs(2), merged_monoCOM

    !> グラフ作成
    ! call gedatsu_graph_initialize(graphs(1))
    ! call gedatsu_graph_initialize(graphs(2))
    ! call gedatsu_graph_set_n_vertex(graphs(1), 6)
    ! call gedatsu_graph_set_n_vertex(graphs(2), 6)

    ! !> 結合結果の確認
    ! call gedatsu_merge_nodal_subgraphs(2, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_DOMAIN_ID)
    !> merged_graph
    !> merged_monoCOM

    call gedatsu_merge_nodal_subgraphs(2, graphs, monoCOMs, merged_graph, merged_monoCOM, ORDER_NODAL_ID)

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
