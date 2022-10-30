!> MPI util モジュール
module mod_gedatsu_mpi
  use mod_gedatsu_prm
  use mod_gedatsu_mpi_util
  implicit none
  private

  integer(gint), parameter :: gedatsu_mpi_sum = 1
  integer(gint), parameter :: gedatsu_mpi_max = 2
  integer(gint), parameter :: gedatsu_mpi_min = 3

contains

  subroutine gedatsu_allreduce_I1(val, tag, comm)
    implicit none
    integer(gint), intent(in) :: tag, comm
    integer(gint)  :: n, ierr, val, in(1), out(1)

#ifndef WITH_NOMPI
    in = val
    out = 0
    n = 1
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
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
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
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
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
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
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
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
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_gatherv(sbuf, sc, MPI_REAL8, rbuf, rcs, disp, MPI_REAL8, root, comm, ierr)
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
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_scatterv(sbuf, scs, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr)
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
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_gatherv(sbuf, sc, MPI_REAL8, rbuf, rcs, disp, MPI_REAL8, root, comm, ierr)
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
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_scatterv(sbuf, scs, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine gedatsu_scatterv_R

  subroutine gedatsu_allgather_I1(sval, rbuf, comm)
    implicit none
    integer(gint) :: sval
    integer(gint) :: rbuf, out(1)
    integer(gint) :: comm
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_allgather(sval, 1, MPI_INTEGER, out, 1, MPI_INTEGER, comm, ierr)
    rbuf = out(1)
#else
    rbuf(1) = sval
#endif
  end subroutine gedatsu_allgather_I1

  subroutine gedatsu_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & ws, wr, val, ndof, comm)
    implicit none
    integer(gint) :: send_n_neib, recv_n_neib
    integer(gint) :: iS, in, j, k, ierr
    integer(gint) :: i, ndof, comm
    integer(gint), pointer :: send_neib_pe(:)
    integer(gint), pointer :: send_index(:)
    integer(gint), pointer :: send_item (:)
    integer(gint), pointer :: recv_neib_pe(:)
    integer(gint), pointer :: recv_index(:)
    integer(gint), pointer :: recv_item (:)
    integer(gint) :: sta1(gedatsu_mpi_status_size, send_n_neib)
    integer(gint) :: sta2(gedatsu_mpi_status_size, recv_n_neib)
    integer(gint) :: req1(send_n_neib)
    integer(gint) :: req2(recv_n_neib)
    real(gdouble) :: val(:), ws(:), wr(:)

#ifndef WITH_NOMPI
    do i = 1, send_n_neib
      iS = send_index(i - 1)
      in = send_index(i  ) - iS
      if(in == 0) cycle
      do j = iS + 1, iS + in
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val(ndof*(send_item(j) - 1) + k)
        enddo
      enddo
      call gedatsu_Isend_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i - 1)
      in = recv_index(i  ) - iS
      if(in == 0) cycle
      call gedatsu_Irecv_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i - 1)
      in = recv_index(i  ) - iS
      do j = iS + 1, iS + in
        do k = 1, ndof
          val(ndof*(recv_item(j) - 1) + k) = wr(ndof*(j - 1) + k)
        enddo
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine gedatsu_SendRecv_R

  subroutine gedatsu_SendRecv_I(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & ws, wr, val, ndof, comm)
    implicit none
    integer(gint) :: send_n_neib, recv_n_neib
    integer(gint) :: iS, in, j, k, ierr
    integer(gint) :: i, ndof, comm
    integer(gint), pointer :: send_neib_pe(:)
    integer(gint), pointer :: send_index(:)
    integer(gint), pointer :: send_item (:)
    integer(gint), pointer :: recv_neib_pe(:)
    integer(gint), pointer :: recv_index(:)
    integer(gint), pointer :: recv_item (:)
    integer(gint) :: sta1(gedatsu_mpi_status_size, send_n_neib)
    integer(gint) :: sta2(gedatsu_mpi_status_size, recv_n_neib)
    integer(gint) :: req1(send_n_neib)
    integer(gint) :: req2(recv_n_neib)
    integer(gint) :: val(:), ws(:), wr(:)

#ifndef WITH_NOMPI
    do i = 1, send_n_neib
      iS = send_index(i-1)
      in = send_index(i  ) - iS
      if(in == 0) cycle
      do j = iS + 1, iS + in
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val(ndof*(send_item(j) - 1) + k)
        enddo
      enddo
      call gedatsu_Isend_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i-1)
      in = recv_index(i  ) - iS
      if(in == 0) cycle
      call gedatsu_Irecv_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i - 1)
      in = recv_index(i  ) - iS
      do j = iS + 1, iS + in
        do k = 1, ndof
          val(ndof*(recv_item(j) - 1) + k) = wr(ndof*(j - 1) + k)
        enddo
      enddo
    enddo

    call MPI_waitall(send_n_neib, req1, sta1, ierr)
#endif
  end subroutine gedatsu_SendRecv_I
end module mod_gedatsu_mpi
