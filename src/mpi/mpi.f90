!> MPI util モジュール
module mod_gedatsu_mpi
  use mod_gedatsu_prm
  use mod_gedatsu_mpi_util
  implicit none
  private

  !> MPI 演算タグ（和）
  integer(gint), parameter :: gedatsu_mpi_sum = 1
  !> MPI 演算タグ（最大値）
  integer(gint), parameter :: gedatsu_mpi_max = 2
  !> MPI 演算タグ（最小値）
  integer(gint), parameter :: gedatsu_mpi_min = 3

contains

  !> @ingroup mpi
  !> allreduce 関数（整数型）
  subroutine gedatsu_allreduce_I1(val, tag, comm)
    implicit none
    !> [inout] 入出力値（整数型）
    integer(gint) :: val
    !> [in] MPI 演算タグ（gedatsu_mpi_sum, gedatsu_mpi_max, gedatsu_mpi_min）
    integer(gint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(gint), intent(in) :: comm
    integer(gint)  :: n, ierr, in(1), out(1)

#ifndef WITH_NOMPI
    in = val
    out = 0
    n = 1
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
      call MPI_allreduce(in, out, n, MPI_INTEGER, MPI_MIN, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine gedatsu_allreduce_I1

  !> @ingroup mpi
  !> allreduce 関数（整数配列型）
  subroutine gedatsu_allreduce_I(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [inout] 入出力値（整数型）
    integer(gint) :: val(n)
    !> [in] MPI 演算タグ（gedatsu_mpi_sum, gedatsu_mpi_max, gedatsu_mpi_min）
    integer(gint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(gint), intent(in) :: comm
    integer(gint)  :: ierr, temp(n)

#ifndef WITH_NOMPI
    temp = 0
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
      call MPI_allreduce(val, temp, n, MPI_INTEGER, MPI_MIN, comm, ierr)
    endif
    val = temp
#endif
  end subroutine gedatsu_allreduce_I

  !> @ingroup mpi
  !> allreduce 関数（浮動小数点型）
  subroutine gedatsu_allreduce_R1(val, tag, comm)
    implicit none
    !> [inout] 入出力値（浮動小数点型）
    real(gdouble) :: val
    !> [in] MPI 演算タグ（gedatsu_mpi_sum, gedatsu_mpi_max, gedatsu_mpi_min）
    integer(gint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(gint), intent(in) :: comm
    integer(gint) :: n, ierr
    real(gdouble) :: in(1), out(1)

#ifndef WITH_NOMPI
    in = val
    out = 0.0d0
    n = 1
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
      call MPI_allreduce(in, out, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    val = out(1)
#endif
  end subroutine gedatsu_allreduce_R1

  !> @ingroup mpi
  !> allreduce 関数（浮動小数点配列型）
  subroutine gedatsu_allreduce_R(n, val, tag, comm)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [inout] 入出力値（浮動小数点配列型）
    real(gdouble) :: val(n)
    !> [in] MPI 演算タグ（gedatsu_mpi_sum, gedatsu_mpi_max, gedatsu_mpi_min）
    integer(gint), intent(in) :: tag
    !> [in] MPI コミュニケータ
    integer(gint), intent(in) :: comm
    integer(gint) :: ierr
    real(gdouble) :: temp(n)

#ifndef WITH_NOMPI
    temp = 0.0d0
    if(tag == gedatsu_mpi_sum)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_SUM, comm, ierr)
    elseif(tag == gedatsu_mpi_max)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MAX, comm, ierr)
    elseif(tag == gedatsu_mpi_min)then
      call MPI_allreduce(val, temp, n, MPI_REAL8, MPI_MIN, comm, ierr)
    endif
    val = temp
#endif
  end subroutine gedatsu_allreduce_R

  !> @ingroup mpi
  !> Isend 関数（整数配列型）
  subroutine gedatsu_Isend_I(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [in] 送信値
    integer(gint) :: ws(:)
    !> [in] 送信先 MPI ランク
    integer(gint) :: pe_id
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    !> [in] MPI リクエスト
    integer(gint) :: req
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_Isend(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Isend_I

  !> @ingroup mpi
  !> Irecv 関数（整数配列型）
  subroutine gedatsu_Irecv_I(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [in] 受信値
    integer(gint) :: ws(:)
    !> [in] 送信元 MPI ランク
    integer(gint) :: pe_id
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    !> [in] MPI リクエスト
    integer(gint) :: req
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_Irecv(ws, n, MPI_INTEGER, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Irecv_I

  !> @ingroup mpi
  !> Isend 関数（浮動小数点配列型）
  subroutine gedatsu_Isend_R(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [in] 送信値
    real(gdouble) :: ws(:)
    !> [in] 送信先 MPI ランク
    integer(gint) :: pe_id
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    !> [in] MPI リクエスト
    integer(gint) :: req
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_Isend(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Isend_R

  !> @ingroup mpi
  !> Irecv 関数（浮動小数点配列型）
  subroutine gedatsu_Irecv_R(n, ws, pe_id, comm, req)
    implicit none
    !> [in] 配列サイズ
    integer(gint) :: n
    !> [in] 受信値
    real(gdouble) :: ws(:)
    !> [in] 送信元 MPI ランク
    integer(gint) :: pe_id
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    !> [in] MPI リクエスト
    integer(gint) :: req
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_Irecv(ws, n, MPI_REAL8, pe_id, 0, comm, req, ierr)
#endif
  end subroutine gedatsu_Irecv_R

  !> @ingroup mpi
  !> gatherv 関数（浮動小数点配列型）
  subroutine gedatsu_gatherv_I(sbuf, sc, rbuf, rc, disp, root, comm)
    implicit none
    !> [in] 送信データ配列
    integer(gint) :: sbuf(:)
    !> [in] 送信データ個数
    integer(gint) :: sc
    !> [in] 受信データ配列
    integer(gint) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(gint) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(gint) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(gint) :: root
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_gatherv(sbuf, sc, MPI_INTEGER, rbuf, rc, disp, MPI_INTEGER, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine gedatsu_gatherv_I

  !> @ingroup mpi
  !> scatterv 関数（整数配列型）
  subroutine gedatsu_scatterv_I(sbuf, sc, disp, rbuf, rc, root, comm)
    implicit none
    !> [in] 送信データ配列
    integer(gint) :: sbuf(:)
    !> [in] 送信データ個数
    integer(gint) :: sc
    !> [in] 受信データ配列
    integer(gint) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(gint) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(gint) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(gint) :: root
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_scatterv(sbuf, sc, disp, MPI_INTEGER, rbuf, rc, MPI_INTEGER, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine gedatsu_scatterv_I

  subroutine gedatsu_gatherv_R(sbuf, sc, rbuf, rc, disp, root, comm)
    implicit none
    !> [in] 送信データ配列
    real(gdouble) :: sbuf(:)
    !> [in] 送信データ個数
    integer(gint) :: sc
    !> [in] 受信データ配列
    real(gdouble) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(gint) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(gint) :: disp(:)
    !> [in] データを格納する MPI ランク
    integer(gint) :: root
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_gatherv(sbuf, sc, MPI_REAL8, rbuf, rc, disp, MPI_REAL8, root, comm, ierr)
#else
    rbuf(1:sc) = sbuf(1:sc)
#endif
  end subroutine gedatsu_gatherv_R

  subroutine gedatsu_scatterv_R(sbuf, sc, disp, rbuf, rc, root, comm)
    implicit none
    !> [in] 送信データ配列
    real(gdouble) :: sbuf(:)
    !> [in] 送信データ個数
    integer(gint) :: sc
    !> [in] 受信データ配列
    integer(gint) :: disp(:)
    !> [in] データを格納する MPI ランク
    real(gdouble) :: rbuf(:)
    !> [in] 各ランクのデータ個数リスト
    integer(gint) :: rc(:)
    !> [in] 各ランクのデータ格納位置リスト
    integer(gint) :: root
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    integer(gint) :: ierr

#ifndef WITH_NOMPI
    call MPI_scatterv(sbuf, sc, disp, MPI_REAL8, rbuf, rc, MPI_REAL8, root, comm, ierr)
#else
    rbuf(1:rc) = sbuf(1:rc)
#endif
  end subroutine gedatsu_scatterv_R

  !> @ingroup mpi
  !> allgather 関数（整数型）
  subroutine gedatsu_allgather_I1(sval, rbuf, comm)
    implicit none
    !> [in] 送信データ
    integer(gint) :: sval
    !> [in] 受信データ
    integer(gint) :: rbuf
    !> [in] MPI コミュニケータ
    integer(gint) :: comm
    integer(gint) :: ierr, out(1)

#ifndef WITH_NOMPI
    call MPI_allgather(sval, 1, MPI_INTEGER, out, 1, MPI_INTEGER, comm, ierr)
    rbuf = out(1)
#else
    rbuf(1) = sval
#endif
  end subroutine gedatsu_allgather_I1

  subroutine gedatsu_SendRecv_R(send_n_neib, send_neib_pe, recv_n_neib, recv_neib_pe, &
    & send_index, send_item, recv_index, recv_item, &
    & val, ndof, comm)
    implicit none
    integer(gint) :: send_n_neib, recv_n_neib
    integer(gint) :: iS, in, j, k, ierr, ns, nr
    integer(gint) :: i, ndof, comm
    integer(gint) :: send_neib_pe(:)
    integer(gint) :: send_index(:)
    integer(gint) :: send_item (:)
    integer(gint) :: recv_neib_pe(:)
    integer(gint) :: recv_index(:)
    integer(gint) :: recv_item (:)
    integer(gint) :: sta1(gedatsu_mpi_status_size, send_n_neib)
    integer(gint) :: sta2(gedatsu_mpi_status_size, recv_n_neib)
    integer(gint) :: req1(send_n_neib)
    integer(gint) :: req2(recv_n_neib)
    real(gdouble) :: val(:)
    real(gdouble), allocatable :: ws(:)
    real(gdouble), allocatable :: wr(:)

#ifndef WITH_NOMPI

    ns = send_index(send_n_neib)
    nr = recv_index(recv_n_neib)

    allocate(ws(ndof*ns))
    allocate(wr(ndof*nr))

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      do j = iS + 1, iS + in
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val(ndof*(send_item(j) - 1) + k)
        enddo
      enddo
      call gedatsu_Isend_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call gedatsu_Irecv_R(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
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
    & val, ndof, comm)
    implicit none
    integer(gint) :: send_n_neib, recv_n_neib
    integer(gint) :: iS, in, j, k, ierr, ns, nr
    integer(gint) :: i, ndof, comm
    integer(gint) :: send_neib_pe(:)
    integer(gint) :: send_index(:)
    integer(gint) :: send_item (:)
    integer(gint) :: recv_neib_pe(:)
    integer(gint) :: recv_index(:)
    integer(gint) :: recv_item (:)
    integer(gint) :: sta1(gedatsu_mpi_status_size, send_n_neib)
    integer(gint) :: sta2(gedatsu_mpi_status_size, recv_n_neib)
    integer(gint) :: req1(send_n_neib)
    integer(gint) :: req2(recv_n_neib)
    integer(gint) :: val(:)
    integer(gint), allocatable :: ws(:)
    integer(gint), allocatable :: wr(:)

#ifndef WITH_NOMPI
    ns = send_index(send_n_neib)
    nr = recv_index(recv_n_neib)

    allocate(ws(ndof*ns))
    allocate(wr(ndof*nr))

    do i = 1, send_n_neib
      iS = send_index(i)
      in = send_index(i + 1) - iS
      if(in == 0) cycle
      do j = iS + 1, iS + in
        do k = 1, ndof
          ws(ndof*(j - 1) + k) = val(ndof*(send_item(j) - 1) + k)
        enddo
      enddo
      call gedatsu_Isend_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), send_neib_pe(i), comm, req1(i))
    enddo

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
      if(in == 0) cycle
      call gedatsu_Irecv_I(ndof*in, ws(ndof*iS + 1:ndof*iS + ndof*in), recv_neib_pe(i), comm, req2(i))
    enddo

    call MPI_waitall(recv_n_neib, req2, sta2, ierr)

    do i = 1, recv_n_neib
      iS = recv_index(i)
      in = recv_index(i + 1) - iS
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
