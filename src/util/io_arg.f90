!> IO 引数モジュール
module mod_gedatsu_io_arg
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  implicit none

contains

  !> gedatsu_graph_partitioner の引数入力
  subroutine gedatsu_get_arg_graph_partitioner(fname, n_domain)
    implicit none
    !> [out] 入力ファイル名
    character(gedatsu_charlen) :: fname
    !> [out] 分割数
    integer(gint) :: n_domain

    call gedatsu_output_arg_help()

    n_domain = 1
    call gedatsu_get_arg_n_domain(n_domain)

    fname = "graph.dat"
    call gedatsu_get_arg_input_fname(fname)

  end subroutine gedatsu_get_arg_graph_partitioner

  !> gedatsu 実行ファイル引数の分割数の取得
  subroutine gedatsu_get_arg_n_domain(n_domain)
    implicit none
    !> [out] 分割数
    integer(gint) :: n_domain
    integer(gint) :: i, count
    character(gedatsu_charlen) :: argc1
    character(gedatsu_charlen) :: argc2

    if(mod(count,2) /= 0)then
      call gedatsu_output_arg_help()
      write(*,"(a)") "* gedatsu input arg error"
      call gedatsu_error_stop()
    endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == "-n")then
        read(argc2,*) n_domain
      endif
    enddo
  end subroutine gedatsu_get_arg_n_domain

  !> gedatsu 実行ファイル引数のファイル名の取得
  subroutine gedatsu_get_arg_input_fname(fname)
    implicit none
    !> [out] ファイル名
    character(gedatsu_charlen) :: fname
    integer(gint) :: i, count
    character(gedatsu_charlen) :: argc1
    character(gedatsu_charlen) :: argc2

    if(mod(count,2) /= 0)then
      call gedatsu_output_arg_help()
      write(*,"(a)") "* gedatsu input arg error"
      call gedatsu_error_stop()
    endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == "-i")then
        read(argc2,*) fname
      endif
    enddo
  end subroutine gedatsu_get_arg_input_fname

  !> gedatsu 実行ファイルのヘルプ出力
  subroutine gedatsu_output_arg_help()
    implicit none
    integer(gint) :: count
    character(gedatsu_charlen) :: argc1

    count = iargc()
    if(count == 1)then
      call getarg(1, argc1)
      if(trim(argc1) == "-h")then
        write(*,"(a)")"-n {num of subdomain}"
        write(*,"(a)")"-i {input file name}"
        write(*,"(a)")"-h: help"
        stop
      endif
    endif
  end subroutine gedatsu_output_arg_help

end module mod_gedatsu_io_arg
