program parmetis_test
  use mod_gedatsu
  implicit none
  !> �ָ���� graph ������
  type(gedatsu_graph) :: subgraph
  !> �ָ���
  integer(gint) :: n_domain
  !> �����ե�������
  character(gedatsu_charlen) :: finame
  !> �����ե�������
  character(gedatsu_charlen) :: foname
  !> �����ǥ��쥯�ȥ���
  character(gedatsu_charlen) :: fdname
  integer(gint) :: i

  fdname = "parted.0"

  call gedatsu_global_initialize()

  call gedatsu_get_arg_graph_partitioner(finame, n_domain)

  foname = gedatsu_get_input_file_name(fdname, finame, i - 1)

write(*,*)"foname: ", trim(foname)

  call gedatsu_input_graph(finame, subgraph)

  call gedatsu_graph_repartition(subgraph)

  call gedatsu_global_finalize()
end program parmetis_test
