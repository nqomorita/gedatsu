!> グラフ分割テストモジュール
module mod_gedatsu_graph_repart_test
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_graph_repart_test()
    implicit none

    call gedatsu_graph_repartition_test()
    call gedatsu_graph_repartition_with_weight_test()
  end subroutine gedatsu_graph_repart_test

  subroutine gedatsu_graph_repartition_test()
    implicit none
    type(gedatsu_graph) :: graph
    type(monolis_COM) :: COM

    !call gedatsu_graph_repartition(graph, COM)
  end subroutine gedatsu_graph_repartition_test

  subroutine gedatsu_graph_repartition_with_weight_test()
    implicit none
    type(gedatsu_graph) :: graph
    type(monolis_COM) :: COM

    !call gedatsu_graph_repartition_with_weight(graph, COM)
  end subroutine gedatsu_graph_repartition_with_weight_test
end module mod_gedatsu_graph_repart_test
