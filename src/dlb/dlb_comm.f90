!> 動的負荷分散モジュール
module mod_gedatsu_dlb_comm
  use mod_monolis_utils
  use mod_gedatsu_graph
  use mod_gedatsu_dlb
  use mod_gedatsu_graph_repart
  !use mod_gedatsu_comm
  implicit none

contains

  !> @ingroup group_dlb
  !> 動的負荷分散のための通信テーブル作成
  subroutine gedatsu_dlb_get_comm_table(dlb, graph, comm)
    implicit none
    !> [in] dlb 構造体
    type(gedatsu_dlb) :: dlb
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    !> [in] MPI コミュニケータ
    integer(kint) :: comm
    integer(kint) :: i, n_internal_vertex, n_move_vertex, my_rank
    integer(kint) :: n_neib_domain
    integer(kint), allocatable :: move_vertex_domain_id(:)
    integer(kint), allocatable :: neib_domain_id(:)

    my_rank = monolis_mpi_local_my_rank(comm)

    !> 内点が自分の領域番号でない場合、その節点数を取得
    n_internal_vertex = 0
    n_move_vertex = 0
    do i = 1, graph%n_internal_vertex
      if(graph%vertex_domain_id(i) == my_rank + 1)then
        n_internal_vertex = n_internal_vertex + 1
      else
        n_move_vertex = n_move_vertex + 1
      endif
    enddo

    write(*,*)"n_internal_vertex: ", n_internal_vertex
    write(*,*)"n_move_vertex: ", n_move_vertex

    !> 自領域でない領域番号を取得
    call gedatsu_alloc_int_1d(move_vertex_domain_id, n_move_vertex)

    n_move_vertex = 0
    do i = 1, graph%n_internal_vertex
      if(graph%vertex_domain_id(i) /= my_rank + 1)then
        n_move_vertex = n_move_vertex + 1
        move_vertex_domain_id(n_move_vertex) = graph%vertex_domain_id(i)
      endif
    enddo

    call gedatsu_qsort_int_1d(move_vertex_domain_id, 1, n_move_vertex)

    n_neib_domain = 0

    call gedatsu_get_uniq_int(move_vertex_domain_id, n_move_vertex, n_neib_domain)

    call gedatsu_alloc_int_1d(neib_domain_id, n_neib_domain)

    neib_domain_id = move_vertex_domain_id(1:n_neib_domain)

    write(*,*)"n_neib_domain: ", n_neib_domain
    write(*,*)"neib_domain_id: ", neib_domain_id

    !> 個数共有
    !call gedatsu_allgather_I1(sval, rbuf, comm)

    !call gedatsu_dlb_get_new_n_vertex()

    !call gedatsu_dlb_get_new_n_neib_domain()

    !call gedatsu_dlb_get_new_neib_domain_id()
  end subroutine gedatsu_dlb_get_comm_table
end module mod_gedatsu_dlb_comm
