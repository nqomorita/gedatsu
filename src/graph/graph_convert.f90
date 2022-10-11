module mod_gedatsu_graph_convert
  use mod_gedatsu_prm
  use mod_gedatsu_graph
  use mod_gedatsu_util

  implicit none

contains

!  subroutine gedatsu_convert_single_elem_to_mesh(n_elem, nb, elem, mesh_index, mesh_item)
!    use iso_c_binding
!    implicit none
!    integer(gint) :: i, j, nb
!    integer(gint) :: n_elem, elem(:,:)
!    integer(gint), pointer :: mesh_index(:), mesh_item(:)
!
!    allocate(mesh_index(n_elem+1), source = 0)
!    allocate(mesh_item(n_elem*nb), source = 0)
!
!    do i = 1, n_elem
!      mesh_index(i+1) = i*nb
!    enddo
!
!    do i = 1, n_elem
!      do j = 1, nb
!        mesh_item(nb*(i-1) + j) = elem(j,i)
!      enddo
!    enddo
!  end subroutine gedatsu_convert_single_elem_to_mesh

end module mod_gedatsu_graph_convert
