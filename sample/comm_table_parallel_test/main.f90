program main
  use mod_gedatsu
  implicit none
  !> [in] 分割領域に対応する graph 構造体
  type(gedatsu_graph) :: graph
  !> [out] 分割領域に対応する comm 構造体
  type(gedatsu_comm) :: comm
  integer(gint), allocatable :: nid(:)

  call gedatsu_global_initialize()

  if(gedatsu_mpi_global_comm_size() /= 2)then
    write(*,*) "error: mpirun -np 2"
    stop
  endif

  graph%n_vertex = 3
  graph%n_internal_vertex = 2

  comm%comm = gedatsu_mpi_global_comm()

  allocate(graph%vertex_id(3), source = 0)
  if(gedatsu_mpi_global_my_rank() == 0)then
    graph%vertex_id(1) = 1
    graph%vertex_id(2) = 2
    graph%vertex_id(3) = 3
  elseif(gedatsu_mpi_global_my_rank() == 1)then
    graph%vertex_id(1) = 3
    graph%vertex_id(2) = 4
    graph%vertex_id(3) = 2
  endif

  call gedatsu_comm_get_comm_table_parallel(graph, comm)

  call gedatsu_comm_debug_write(comm)

  call gedatsu_global_finalize()
end program main
