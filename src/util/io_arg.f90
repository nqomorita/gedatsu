!> IO 引数モジュール
module mod_gedatsu_io_arg
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util
  implicit none

contains

  !> gedatsu_graph_partitioner の引数入力
  subroutine gedatsu_get_arg_graph_partitioner(fname, n_domain)
    implicit none
    !> [out] 入力ファイル名
    character(gedatsu_charlen) :: fname
    !> [out] 分割数
    integer(gint) :: n_domain
    character(gedatsu_charlen) :: tag
    logical :: is_find

    call gedatsu_output_arg_help()

    tag = "-n"
    n_domain = 1
    call gedatsu_get_arg_input_int(tag, n_domain, is_find)

    tag = "-i"
    fname = "graph.dat"
    call gedatsu_get_arg_input_char(tag, fname, is_find)
  end subroutine gedatsu_get_arg_graph_partitioner

  !> gedatsu_graph_partitioner の重みの引数入力
  subroutine gedatsu_get_arg_graph_partitioner_weight(fname_input_nodal_weight, is_nodal_weight, &
    & fname_input_edge_weight, is_edge_weight)
    implicit none
    !> [out] 入力ファイル名
    character(gedatsu_charlen) :: fname_input_nodal_weight
    !> [out] 入力ファイル名
    character(gedatsu_charlen) :: fname_input_edge_weight
    !> [out] 引数の取得判定
    logical :: is_nodal_weight
    !> [out] 引数の取得判定
    logical :: is_edge_weight
    character(gedatsu_charlen) :: tag

    call gedatsu_output_arg_help()

    tag = "-inw"
    fname_input_nodal_weight = "nodal_weight.dat"
    call gedatsu_get_arg_input_char(tag, fname_input_nodal_weight, is_nodal_weight)

    tag = "-iew"
    fname_input_edge_weight = "edge_weight.dat"
    call gedatsu_get_arg_input_char(tag, fname_input_edge_weight, is_edge_weight)
  end subroutine gedatsu_get_arg_graph_partitioner_weight

  !> gedatsu 実行ファイルの整数型引数の取得
  subroutine gedatsu_get_arg_input_int(tag, var, is_get)
    implicit none
    !> [in] 引数のタグ
    character(gedatsu_charlen) :: tag
    !> [out] 分割数
    integer(gint) :: var
    !> [out] 引数の取得判定
    logical :: is_get
    integer(gint) :: i, count
    character(gedatsu_charlen) :: argc1
    character(gedatsu_charlen) :: argc2

    is_get = .false.

    count = iargc()
    if(mod(count,2) /= 0)then
      call gedatsu_output_arg_help()
      write(*,"(a)") "* gedatsu input arg error"
      call gedatsu_error_stop()
    endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == trim(tag))then
        read(argc2,*) var
        is_get = .true.
      endif
    enddo
  end subroutine gedatsu_get_arg_input_int

  !> gedatsu 実行ファイルの文字列型引数の取得
  subroutine gedatsu_get_arg_input_char(tag, var, is_get)
    implicit none
    !> [in] 引数のタグ
    character(gedatsu_charlen) :: tag
    !> [out] 文字列型
    character(gedatsu_charlen) :: var
    !> [out] 引数の取得判定
    logical :: is_get
    integer(gint) :: i, count
    character(gedatsu_charlen) :: argc1
    character(gedatsu_charlen) :: argc2

    is_get = .false.

    count = iargc()
    if(mod(count,2) /= 0)then
      call gedatsu_output_arg_help()
      write(*,"(a)") "* gedatsu input arg error"
      call gedatsu_error_stop()
    endif

    do i = 1, count/2
      call getarg(2*i-1, argc1)
      call getarg(2*i  , argc2)
      if(trim(argc1) == trim(tag))then
        read(argc2,*) var
        is_get = .true.
      endif
    enddo
  end subroutine gedatsu_get_arg_input_char

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
