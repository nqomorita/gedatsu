!> IO モジュール
module mod_gedatsu_io
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  implicit none

contains

  !> @ingroup group_io
  !> gedatsu graph フォーマットの入力
  !> @note gedatsu IO では gedatsu グラフ形式フォーマットを入出力するが、
  !> プログラム内部では CSR 圧縮形式によってグラフを保持する。
  subroutine gedatsu_input_graph(fname, graph)
    implicit none
    !> [in] 入力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i, in, j, tmp, nz

    nz = 0
    open(20, file = fname, status = "old")
      read(20,*) graph%n_vertex
      do i = 1, graph%n_vertex
        read(20,*) tmp, in
        nz = nz + in
      enddo
    close(20)

    call gedatsu_alloc_int_1d(graph%vertex_id, graph%n_vertex)
    call gedatsu_alloc_int_1d(graph%index, graph%n_vertex+1)
    call gedatsu_alloc_int_1d(graph%item, nz)

    nz = 0
    open(20, file = fname, status = "old")
      read(20,*) graph%n_vertex
      do i = 1, graph%n_vertex
        read(20,*) graph%vertex_id(i), in, (graph%item(nz+j), j = 1, in)
        graph%index(i+1) = graph%index(i) + in
        nz = nz + in
      enddo
    close(20)
  end subroutine gedatsu_input_graph

  !> @ingroup group_io
  !> gedatsu graph フォーマットの出力
  subroutine gedatsu_output_graph(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i, in, j, jS, jE

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_vertex
      do i = 1, graph%n_vertex
        jS = graph%index(i) + 1
        jE = graph%index(i+1)
        in = jE - jS + 1
        write(20,"(i0,x,i0,x,$)") graph%vertex_id(i), in
        do j = jS, jE
          write(20,"(i0,x,$)") graph%item(j)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine gedatsu_output_graph

  !> @ingroup group_io
  !> gedatsu node フォーマットの入力
  subroutine gedatsu_input_node(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_input_node

  !> @ingroup group_io
  !> gedatsu node フォーマットの出力
  subroutine gedatsu_output_node(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_output_node

  !> @ingroup group_io
  !> gedatsu elem フォーマットの入力
  !> @note gedatsu IO では elem 形式フォーマットを入出力するが、
  !> プログラム内部では dedatsu graph の CSR 圧縮形式によってグラフを保持する。
  subroutine gedatsu_input_elem(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_input_elem

  !> @ingroup group_io
  !> gedatsu elem フォーマットの出力
  subroutine gedatsu_output_elem(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_output_elem

  !> @ingroup group_io
  !> gedatsu bc フォーマットの入力
  subroutine gedatsu_input_bc(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_input_bc

  !> @ingroup group_io
  !> gedatsu bc フォーマットの出力
  subroutine gedatsu_output_bc(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_output_bc

  !> @ingroup group_io
  !> gedatsu distval フォーマットの入力
  subroutine gedatsu_input_distval(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_input_distval

  !> @ingroup group_io
  !> gedatsu distval フォーマットの出力
  subroutine gedatsu_output_distval(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
  end subroutine gedatsu_output_distval

  !> エラー出力関数
  subroutine gedatsu_error_string(fname)
    implicit none
    !> [in] 入力ファイル名
    character(*) :: fname
    write(*,"(a)")"** GEDATSU ERROR: ", trim(fname)
  end subroutine gedatsu_error_string

end module mod_gedatsu_io
