!> IO 引数モジュール
module mod_gedatsu_io_comm
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_comm
  implicit none

contains

  !> gedatsu 通信テーブルの出力
  subroutine gedatsu_output_send_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_output_comm_table_main(fname, &
      & comm%send_n_neib, comm%send_neib_pe, comm%send_index, comm%send_item)
  end subroutine gedatsu_output_send_comm_table

  !> gedatsu 通信テーブルの出力
  subroutine gedatsu_output_recv_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_output_comm_table_main(fname, &
      & comm%recv_n_neib, comm%recv_neib_pe, comm%recv_index, comm%recv_item)
  end subroutine gedatsu_output_recv_comm_table

  !> gedatsu 通信テーブルの出力
  subroutine gedatsu_output_comm_table_main(fname, n_neib, neib_pe, index, item)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] 出力ファイル名
    integer(gint) :: n_neib
    !> [in] 出力ファイル名
    integer(gint) :: neib_pe(:)
    !> [in] 出力ファイル名
    integer(gint) :: index(:)
    !> [in] 出力ファイル名
    integer(gint) :: item(:)
    integer(gint) :: i

    open(20, file = fname, status = "replace")
      write(20,"(i0,x,i0)")n_neib, index(n_neib + 1)

      do i = 1, n_neib
        write(20,"(i0)")neib_pe(i)
      enddo

      do i = 1, n_neib + 1
        write(20,"(i0)")index(i)
      enddo

      do i = 1, index(n_neib)
        write(20,"(i0)")item(i)
      enddo
    close(20)
  end subroutine gedatsu_output_comm_table_main
end module mod_gedatsu_io_comm
