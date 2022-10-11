!> グラフ分割モジュール
module mod_gedatsu_graph_part
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

  implicit none

contains

!  !> wrapper section
!  subroutine gedatsu_graph_partition(graph, n_domain)
!    use iso_c_binding
!    implicit none
!    type(gedatsu_graph) :: graph
!    integer(gint), pointer :: part_id(:)
!    integer(gint) :: i, n_domain
!
!    call gedatsu_debug_header("gedatsu_graph_partition")
!
!    allocate(graph%vertex_domain_id(graph%n_vertex), source = 1)
!    allocate(part_id(graph%n_vertex), source = 0)
!
!    if(n_domain == 1) return
!
!    call gedatsu_get_partitioned_graph(graph%n_vertex, graph%index, graph%item, n_domain, part_id)
!
!    do i = 1, graph%n_vertex
!      graph%vertex_domain_id(i) = part_id(i) + 1
!    enddo
!
!    deallocate(part_id)
!  end subroutine gedatsu_graph_partition
!
!  !> main routines
!  subroutine gedatsu_get_partitioned_graph(n_vertex, graph_index, graph_item, n_domain, part_id)
!    use iso_c_binding
!    implicit none
!    integer(gint), pointer :: part_id(:)
!    integer(c_int), pointer :: graph_index(:), graph_item(:)
!    integer(c_int), pointer :: node_wgt(:) => null()
!    integer(c_int), pointer :: edge_wgt(:) => null()
!    integer(gint) :: n_domain, n_vertex
!
!    call gedatsu_debug_header("gedatsu_get_partitioned_graph")
!    call gedatsu_part_graph_metis_kway(n_vertex, graph_index, graph_item, n_domain, node_wgt, edge_wgt, part_id)
!  end subroutine gedatsu_get_partitioned_graph
!
!  subroutine gedatsu_get_partitioned_graph_with_weight(n_vertex, graph_index, graph_item, n_domain, &
!    & node_wgt, edge_wgt, part_id)
!    use iso_c_binding
!    implicit none
!    integer(gint), pointer :: part_id(:), node_wgt(:), edge_wgt(:)
!    integer(c_int), pointer :: graph_index(:), graph_item(:)
!    integer(c_int), pointer :: node_wgt_(:)
!    integer(c_int), pointer :: edge_wgt_(:)
!    integer(gint) :: n_domain, n_vertex
!
!    call gedatsu_debug_header("gedatsu_get_partitioned_graph_with_weight")
!
!    allocate(node_wgt_(n_vertex), source = 0)
!    allocate(edge_wgt_(n_vertex), source = 0)
!    node_wgt_ = node_wgt
!    edge_wgt_ = edge_wgt
!
!    call gedatsu_part_graph_metis_kway(n_vertex, graph_index, graph_item, n_domain, node_wgt_, edge_wgt_, part_id)
!
!    deallocate(node_wgt_)
!    deallocate(edge_wgt_)
!  end subroutine gedatsu_get_partitioned_graph_with_weight

end module mod_gedatsu_graph_part
