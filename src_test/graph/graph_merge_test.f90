!> グラフ結合テストモジュール
module mod_gedatsu_graph_merge_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains
  subroutine gedatsu_graph_merge_test()
    implicit none
    call gedatsu_merge_nodal_subgraphs_test()
    call gedatsu_merge_connectivity_subgraphs_test()
    call gedatsu_merge_distval_R_test()
    call gedatsu_merge_distval_I_test()
    call gedatsu_merge_distval_C_test()
  end subroutine gedatsu_graph_merge_test

  subroutine gedatsu_merge_nodal_subgraphs_test()
    implicit none
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
