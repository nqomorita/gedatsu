!> IO 引数モジュール
module mod_gedatsu_io_comm
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  use mod_gedatsu_comm
  use mod_gedatsu_alloc
  implicit none

contains

  !> gedatsu 通信テーブル send の出力
  subroutine gedatsu_output_send_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_output_comm_table_main(fname, &
      & comm%send_n_neib, comm%send_neib_pe, comm%send_index, comm%send_item)
  end subroutine gedatsu_output_send_comm_table

  !> gedatsu 通信テーブル recv の出力
  subroutine gedatsu_output_recv_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_output_comm_table_main(fname, &
      & comm%recv_n_neib, comm%recv_neib_pe, comm%recv_index, comm%recv_item)
  end subroutine gedatsu_output_recv_comm_table

  !> gedatsu 通信テーブルの出力（メイン関数）
  subroutine gedatsu_output_comm_table_main(fname, n_neib, neib_pe, index, item)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] 隣接領域数
    integer(gint) :: n_neib
    !> [in] 隣接領域 id
    integer(gint) :: neib_pe(:)
    !> [in] 通信テーブルの index 配列
    integer(gint) :: index(:)
    !> [in] 通信テーブルの item 配列
    integer(gint) :: item(:)
    integer(gint) :: i

    open(20, file = fname, status = "replace")
      write(20,"(i0,x,i0)")n_neib, index(n_neib + 1)

      do i = 1, n_neib
        write(20,"(i0)")neib_pe(i) - 1
      enddo

      do i = 1, n_neib + 1
        write(20,"(i0)")index(i)
      enddo

      do i = 1, index(n_neib + 1)
        write(20,"(i0)")item(i)
      enddo
    close(20)
  end subroutine gedatsu_output_comm_table_main

  !> gedatsu 通信テーブル send の入力
  subroutine gedatsu_input_send_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_input_comm_table_main(fname, &
      & comm%send_n_neib, comm%send_neib_pe, comm%send_index, comm%send_item)
  end subroutine gedatsu_input_send_comm_table

  !> gedatsu 通信テーブル recv の入力
  subroutine gedatsu_input_recv_comm_table(fname, comm)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> 分割領域に対応する comm 構造体
    type(gedatsu_comm) :: comm

    call gedatsu_input_comm_table_main(fname, &
      & comm%recv_n_neib, comm%recv_neib_pe, comm%recv_index, comm%recv_item)
  end subroutine gedatsu_input_recv_comm_table

  !> gedatsu 通信テーブルの入力（汎用関数）
  subroutine gedatsu_input_comm_table_main(fname, n_neib, neib_pe, index, item)
    implicit none
    !> [in] 入力ファイル名
    character(gedatsu_charlen) :: fname
    !> [out] 隣接領域数
    integer(gint) :: n_neib
    !> [out] 隣接領域 id
    integer(gint), allocatable :: neib_pe(:)
    !> [out] 通信テーブルの index 配列
    integer(gint), allocatable :: index(:)
    !> [out] 通信テーブルの item 配列
    integer(gint), allocatable :: item(:)
    integer(gint) :: i, nz

    open(20, file = fname, status = "old")
      read(20,*)n_neib, nz

      call gedatsu_alloc_int_1d(neib_pe, n_neib)
      call gedatsu_alloc_int_1d(index, n_neib + 1)
      call gedatsu_alloc_int_1d(item, nz)

      do i = 1, n_neib
        read(20,*)neib_pe(i)
      enddo

      do i = 1, n_neib + 1
        read(20,*)index(i)
      enddo

      do i = 1, nz
        read(20,*)item(i)
      enddo
    close(20)
  end subroutine gedatsu_input_comm_table_main
end module mod_gedatsu_io_comm
