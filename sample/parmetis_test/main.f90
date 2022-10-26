program parmetis_test
  use mod_gedatsu
  implicit none
  !> 分割後の graph 構造体
  type(gedatsu_graph), allocatable :: subgraphs(:)
  !> 分割数
  integer(gint) :: n_domain
  !> 入力ファイル名
  character(gedatsu_charlen) :: finame
  !> 出力ファイル名
  character(gedatsu_charlen) :: foname
  !> 出力ディレクトリ名
  character(gedatsu_charlen) :: fdname
  integer(gint) :: i

  fdname = "parted.0"

  call system('if [ ! -d parted.0 ]; then (echo "** create parted.0"; mkdir -p parted.0); fi')

  call gedatsu_global_initialize()

  call gedatsu_get_arg_graph_partitioner(finame, n_domain)

  allocate(subgraphs(n_domain))

  do i = 1, n_domain
    foname = gedatsu_get_output_file_name(fdname, finame, i - 1)

    call gedatsu_input_graph(finame, subgraphs(i))
  enddo

  call gedatsu_global_finalize()
end program parmetis_test
