!> IO モジュール
module mod_gedatsu_io
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  implicit none

  !> @note gedatsu IO では gedatsu グラフ形式フォーマットを入出力するが、
  !> プログラム内部では CSR 圧縮形式によってグラフを保持する。

contains

  !> @ingroup group_io
  !> gedatsu graph フォーマットの入力
  subroutine gedatsu_graph_input(fname, graph)
    implicit none
    !> 入力ファイル名
    character(gedatsu_charlen) :: fname
    !> graph 構造体
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

    allocate(graph%vertex_id(graph%n_vertex), source = 0)
    allocate(graph%index(0:graph%n_vertex), source = 0)
    allocate(graph%item(nz), source = 0)

    nz = 0
    open(20, file = fname, status = "old")
      read(20,*) graph%n_vertex
      do i = 1, graph%n_vertex
        read(20,*) graph%vertex_id(i), in, (graph%item(nz+j), j = 1, in)
        graph%index(i) = graph%index(i-1) + in
        nz = nz + in
      enddo
    close(20)
  end subroutine gedatsu_graph_input

  !> @ingroup group_io
  !> gedatsu graph フォーマットの出力
  subroutine gedatsu_graph_output(fname, graph)
    implicit none
    !> 出力ファイル名
    character(gedatsu_charlen) :: fname
    !> graph 構造体
    type(gedatsu_graph) :: graph
    integer(gint) :: i, in, j, jS, jE

    open(20, file = trim(fname), status = "replace")
      write(20,"(i0)") graph%n_vertex
      do i = 1, graph%n_vertex
        jS = graph%index(i-1) + 1
        jE = graph%index(i)
        in = jE - jS + 1
        write(20,"(i0,x,i0,x,$)") graph%vertex_id(i), in
        do j = jS, jE
          write(20,"(i0,x,$)") graph%item(j)
        enddo
        write(20,*)""
      enddo
    close(20)
  end subroutine gedatsu_graph_output

end module mod_gedatsu_io
