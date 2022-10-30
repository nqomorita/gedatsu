!> MPI util モジュール
module mod_gedatsu_mpi
  use mod_gedatsu_prm
  use mod_gedatsu_mpi_util
  implicit none
  private

  public :: gedatsu_allreduce_I1
  public :: gedatsu_allreduce_I
  public :: gedatsu_allreduce_R1
  public :: gedatsu_allreduce_R
  public :: gedatsu_Isend_I
  public :: gedatsu_Irecv_I
  public :: gedatsu_Isend_R
  public :: gedatsu_Irecv_R
  public :: gedatsu_gatherv_I
  public :: gedatsu_gatherv_R
  public :: gedatsu_scatterv_I
  public :: gedatsu_scatterv_R
  public :: gedatsu_allgather_I1

  integer(gint), parameter :: tagSum = 1
  integer(gint), parameter :: tagMax = 2
  integer(gint), parameter :: tagMin = 3

contains

  subroutine gedatsu_allreduce_I1(val, tag, comm)
    implicit none
    integer(gint), intent(in) :: tag, comm
    integer(gint)  :: n, ierr, val, in(1), out(1)

#ifndef WITH_NOMPI
    in = val
    out = 0
    n = 1
    if(tag == tagSum)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == tagMax)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == tagMin)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MIn, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine gedatsu_allreduce_I1

  subroutine gedatsu_allreduce_I(n, val, tag, comm)
    implicit none
    integer(gint), intent(in) :: tag, comm
    integer(gint)  :: n, ierr, val(n), temp(n)

#ifndef WITH_NOMPI
    temp = 0
    if(tag == tagSum)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == tagMax)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == tagMin)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MIn, comm, ierr)
    endif
    val = temp
#endif
  end subroutine gedatsu_allreduce_I

  subroutine gedatsu_allreduce_R1(val, tag, comm)
    implicit none
    integer(gint), intent(in) :: tag, comm
    integer(gint) :: n, ierr
    real(gdouble) :: val, in(1), out(1)

#ifndef WITH_NOMPI
    in = val
    out = 0.0d0
    n = 1
    if(tag == tagSum)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == tagMax)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == tagMin)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MIn, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine gedatsu_allreduce_R1

  subroutine gedatsu_allreduce_R(n, val, tag, comm)
    implicit none
    integer(gint), intent(in) :: tag, comm
    integer(gint) :: n, ierr
    real(gdouble) :: val(n), temp(n)

#ifndef WITH_NOMPI
    temp = 0.0d0
    if(tag == tagSum)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == tagMax)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == tagMin)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MIn, comm, ierr)
    endif
    val = temp
#endif
  end subroutine gedatsu_allreduce_R

  subroutine gedatsu_Isend_I(n, ws, pe_id, comm, req)
    implicit none
    integer(gint) :: ws(:), n, pe_id, comm, req, ierr

#ifndef WITH_NOMPI
    call MPI_Isend(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Isend_I

  subroutine gedatsu_Irecv_I(n, ws, pe_id, comm, req)
    implicit none
    integer(gint) :: ws(:), n, pe_id, comm, req, ierr

#ifndef WITH_NOMPI
    call MPI_Irecv(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Irecv_I

  subroutine gedatsu_Isend_R(n, ws, pe_id, comm, req)
    implicit none
    integer(gint) :: n, pe_id, comm, req, ierr
    real(gdouble) :: ws(:)

#ifndef WITH_NOMPI
    call MPI_Isend(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Isend_R

  subroutine gedatsu_Irecv_R(n, ws, pe_id, comm, req)
    implicit none
    integer(gint) :: n, pe_id, comm, req, ierr
    real(gdouble) :: ws(:)

#ifndef WITH_NOMPI
    call MPI_Irecv(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Irecv_R

  subroutine gedatsu_gatherv_I(sbuf, sc, rbuf, rcs, disp, root, comm)
    implicit none
    integer(gint) :: sc
    integer(gint) :: rcs(:)
    integer(gint) :: disp(:)
    integer(gint) :: root
    integer(gint) :: comm
    integer(gint) :: sbuf(:)
    integer(gint) :: rbuf(:)

#ifndef WITH_NOMPI
    integer(gint) :: ierr
    call MPI_gatherv( sbuf, sc, MPI_REAL8, rbuf, rcs, disp, MPI_REAL8, root, comm, ierr )
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine gedatsu_gatherv_I

  subroutine gedatsu_scatterv_I(sbuf, scs, disp, rbuf, rc, root, comm)
    implicit none
    integer(gint) :: scs(:)
    integer(gint) :: disp(:)
    integer(gint) :: rc
    integer(gint) :: root
    integer(gint) :: comm
    integer(gint) :: sbuf(:)
    integer(gint) :: rbuf(:)

#ifndef WITH_NOMPI
    integer(gint) :: ierr
    call MPI_scatterv( sbuf, scs, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr )
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine gedatsu_scatterv_I

  subroutine gedatsu_gatherv_R(sbuf, sc, rbuf, rcs, disp, root, comm)
    implicit none
    integer(gint) :: sc
    integer(gint) :: rcs(:)
    integer(gint) :: disp(:)
    integer(gint) :: root
    integer(gint) :: comm
    real(gdouble) :: sbuf(:)
    real(gdouble) :: rbuf(:)

#ifndef WITH_NOMPI
    integer(gint) :: ierr
    call MPI_gatherv( sbuf, sc, MPI_REAL8, rbuf, rcs, disp, MPI_REAL8, root, comm, ierr )
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine gedatsu_gatherv_R

  subroutine gedatsu_scatterv_R(sbuf, scs, disp, rbuf, rc, root, comm)
    implicit none
    integer(gint) :: scs(:)
    integer(gint) :: disp(:)
    integer(gint) :: rc
    integer(gint) :: root
    integer(gint) :: comm
    real(gdouble) :: rbuf(:)
    real(gdouble) :: sbuf(:)

#ifndef WITH_NOMPI
    integer(gint) :: ierr
    call MPI_scatterv( sbuf, scs, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr )
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine gedatsu_scatterv_R

  subroutine gedatsu_allgather_I1(sval, rbuf, comm)
    implicit none
    integer(gint) :: sval
    integer(gint) :: rbuf, out(1)
    integer(gint) :: comm

#ifndef WITH_NOMPI
    integer(gint) :: ierr
    call MPI_allgather(sval, 1, MPI_INTEGER, out, 1, MPI_INTEGER, comm, ierr )
    rbuf = out(1)
#else
    rbuf(1) = sval
#endif
  end subroutine gedatsu_allgather_I1
end module mod_gedatsu_mpi
