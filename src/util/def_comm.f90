module mod_gedatsu_comm
  use mod_gedatsu_prm
  use mod_gedatsu_alloc
  implicit none

  !> comm 構造体
  type gedatsu_comm
    !> MPI コミュニケータ
    integer(gint) :: comm
    !> 受信する領域数
    integer(gint) :: recv_n_neib
    !> 受信する領域番号リスト
    integer(gint), allocatable :: recv_neib_pe(:)
    !> 受信するノード番号の index 配列
    integer(gint), allocatable :: recv_index(:)
    !> 受信するノード番号の item 配列
    integer(gint), allocatable :: recv_item(:)
    !> 送信する領域数
    integer(gint) :: send_n_neib
    !> 送信する領域番号リスト
    integer(gint), allocatable :: send_neib_pe(:)
    !> 送信するノード番号の index 配列
    integer(gint), allocatable :: send_index(:)
    !> 送信するノード番号の item 配列
    integer(gint), allocatable :: send_item(:)
  end type gedatsu_comm

contains

  !> comm 構造体の初期化関数
  subroutine gedatsu_comm_initialize(comm)
    implicit none
    !> [in] comm 構造体
    type(gedatsu_comm) :: comm

    comm%comm = 0

    comm%recv_n_neib = 0
    call gedatsu_dealloc_int_1d(comm%recv_neib_pe)
    call gedatsu_dealloc_int_1d(comm%recv_index)
    call gedatsu_dealloc_int_1d(comm%recv_item)

    comm%send_n_neib = 0
    call gedatsu_dealloc_int_1d(comm%send_neib_pe)
    call gedatsu_dealloc_int_1d(comm%send_index)
    call gedatsu_dealloc_int_1d(comm%send_item)
  end subroutine gedatsu_comm_initialize

  !> comm 構造体の初期化関数
  subroutine gedatsu_comm_finalize(comm)
    implicit none
    !> [in] comm 構造体
    type(gedatsu_comm) :: comm

    comm%comm = 0

    comm%recv_n_neib = 0
    call gedatsu_dealloc_int_1d(comm%recv_neib_pe)
    call gedatsu_dealloc_int_1d(comm%recv_index)
    call gedatsu_dealloc_int_1d(comm%recv_item)

    comm%send_n_neib = 0
    call gedatsu_dealloc_int_1d(comm%send_neib_pe)
    call gedatsu_dealloc_int_1d(comm%send_index)
    call gedatsu_dealloc_int_1d(comm%send_item)
  end subroutine gedatsu_comm_finalize
end module mod_gedatsu_comm
