!> 動的負荷分散モジュール
module mod_gedatsu_dlb_comm
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_util
  use mod_gedatsu_graph_repart
  !use mod_gedatsu_comm
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための通信テーブル作成
  subroutine gedatsu_dlb_get_comm_table(dlb, graph)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i, n_move_vertex, my_rank

    my_rank = gedatsu_mpi_global_my_rank()

    !> 内点が自分の領域番号でない場合、その節点数を取得
    n_move_vertex = 0
    do i = 1, graph%n_internal_vertex
      if(graph%vertex_domain_id(i) /= my_rank) n_move_vertex = n_move_vertex + 1
    enddo

    write(*,*)"n_move_vertex: ", n_move_vertex

    !call gedatsu_dlb_get_new_n_vertex()

    !call gedatsu_dlb_get_new_n_neib_domain()

    !call gedatsu_dlb_get_new_neib_domain_id()
  end subroutine gedatsu_dlb_get_comm_table
end module mod_gedatsu_dlb_comm
