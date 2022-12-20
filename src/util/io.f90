!> IO モジュール
module mod_gedatsu_io
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_alloc
  use mod_gedatsu_util
  use mod_gedatsu_io_file_name
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
    integer(gint) :: i, in, j, tmp, nz, ierr

    nz = 0
    open(20, file = fname, status = "old", iostat = ierr)
      call gedatsu_input_file_error_check(ierr)
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
    open(20, file = fname, status = "old", iostat = ierr)
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
        write(20,"(i0,x,i0,$)") i, in
        do j = jS, jE
          write(20,"(x,i0,$)") graph%item(j)
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

    open(20, file = trim(fname), status = "old")
      read(20,*) graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_input_node

  !> @ingroup group_io
  !> gedatsu node フォーマットの出力
  subroutine gedatsu_output_node(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_node

  !> @ingroup group_io
  !> gedatsu node id フォーマットの入力
  subroutine gedatsu_input_node_id(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i, n_node

    open(20, file = trim(fname), status = "old")
      read(20,*) n_node
      do i = 1, graph%n_vertex
        read(20,*) graph%vertex_id(i)
      enddo
    close(20)
  end subroutine gedatsu_input_node_id

  !> @ingroup group_io
  !> gedatsu node id フォーマットの出力
  subroutine gedatsu_output_node_id(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_vertex
      do i = 1, graph%n_vertex
        write(20,"(i0)") graph%vertex_id(i)
      enddo
    close(20)
  end subroutine gedatsu_output_node_id

  !> @ingroup group_io
  !> gedatsu 内部節点自由度数の入力
  subroutine gedatsu_input_internal_node_number(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "old")
      read(20,*) graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_input_internal_node_number

  !> @ingroup group_io
  !> gedatsu 内部節点自由度数の出力
  subroutine gedatsu_output_internal_node_number(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_internal_node_number

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

    open(20, file = trim(fname), status = "old")
      read(20,*) graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_input_elem

  !> @ingroup group_io
  !> gedatsu elem フォーマットの出力
  subroutine gedatsu_output_elem(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_elem

  !> @ingroup group_io
  !> gedatsu bc フォーマットの入力
  subroutine gedatsu_input_bc(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "old")
      read(20,*) graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_input_bc

  !> @ingroup group_io
  !> gedatsu bc フォーマットの出力
  subroutine gedatsu_output_bc(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_bc

  !> @ingroup group_io
  !> gedatsu distval フォーマットの入力（整数型）
  subroutine gedatsu_input_distval_i(fname, label, n_dof, val)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [out] ラベル名
    character(gedatsu_charlen) :: label
    !> [out] 節点あたりのデータ数
    integer(gint) :: n_dof
    !> [out] データ
    integer(gint), allocatable :: val(:,:)
    integer(gint) :: i, j, n_node

    open(20, file = trim(fname), status = "old")
      read(20,*) label
      read(20,*) n_node, n_dof

      call gedatsu_alloc_int_2d(val, n_dof, n_node)

      do i = 1, n_node
        read(20,*) (val(j,i), j = 1, n_dof)
      enddo
    close(20)
  end subroutine gedatsu_input_distval_i

  !> @ingroup group_io
  !> gedatsu distval フォーマットの出力（整数型）
  subroutine gedatsu_output_distval_i(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_distval_i

  !> @ingroup group_io
  !> gedatsu distval フォーマットの入力（浮動小数点数型）
  subroutine gedatsu_input_distval_r(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "old")
      read(20,*) graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_input_distval_r

  !> @ingroup group_io
  !> gedatsu distval フォーマットの出力（浮動小数点数型）
  subroutine gedatsu_output_distval_r(fname, graph)
    implicit none
    !> [in] 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> [in] graph 構造体
    type(gedatsu_graph) :: graph

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_internal_vertex
    close(20)
  end subroutine gedatsu_output_distval_r

  !> Fortran open 文のエラー処理
  subroutine gedatsu_input_file_error_check(ierr)
    implicit none
    integer(gint) :: ierr
    if(ierr /= 0)then
      call gedatsu_error_string("file open")
      call gedatsu_error_stop()
    endif
  end subroutine gedatsu_input_file_error_check
end module mod_gedatsu_io
