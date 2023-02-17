program gedatsu_partitioner_nodal_graph
  use mod_monolis_utils
  use mod_gedatsu
  implicit none
  type(gedatsu_graph) :: graph
  integer(kint) :: n_domain, i
  character(monolis_charlen) :: finame, dirname, foname, foname_full
  logical :: is_get
  type(gedatsu_graph), allocatable :: subgraphs(:)
  integer(kint), allocatable :: node_wgt(:,:)
  integer(kint), allocatable :: edge_wgt(:,:)

  call monolis_mpi_initialize()

  call monolis_check_arg_input("-h", is_get)

  if(is_get)then
    write(*,"(a)")"usage:"
    write(*,"(a)") &
    & "./gedatsu_partitioner_nodal_graph {options} -n {number of domains}"
    write(*,"(a)")""
    write(*,"(a)")"-n {number of domains}: (default) 1"
    write(*,"(a)")"-i {input node filename}: (defualt) graph.dat"
    write(*,"(a)")"-inw {input node weight filename}: (defualt) node_weight.dat"
    write(*,"(a)")"-iew {input edge weight filename}: (defualt) edge_weight.dat"
    write(*,"(a)")"-o {output graph filename}: (defualt) graph.dat.{domain_id}"
    write(*,"(a)")"-h : help"
    stop monolis_success
  endif

  finame = "graph.dat"
  call monolis_get_arg_input_i_tag(finame)

  foname = "graph.dat"
  call monolis_get_arg_input_o_tag(foname)

  n_domain = 1
  call monolis_get_arg_input_I("-n", n_domain, is_get)

  call monolis_std_log_I1("[number of domains]", n_domain)

  if(.not. is_get)then
    call monolis_std_error_string("input parameters are not set")
    call monolis_std_error_string("./gedatsu_partitioner_nodal_graph {options} -n {number of domains}")
    stop monolis_fail
  endif

  call monolis_input_graph(finame, graph%n_vertex, graph%vertex_id, graph%index, graph%item)

  !> add edge weight section

  allocate(subgraphs(n_domain))

  call gedatsu_graph_partition_with_weight(graph, n_domain, node_wgt, edge_wgt, subgraphs)

  dirname = "./"
  do i = 1, n_domain
    foname_full = monolis_get_output_file_name(dirname, foname, i)
    call monolis_output_graph(foname_full, subgraphs(i)%n_vertex, subgraphs(i)%vertex_id, subgraphs(i)%index, subgraphs(i)%item)
  enddo

  call monolis_mpi_finalize()
end program gedatsu_partitioner_nodal_graph
