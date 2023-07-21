!> グラフ分割テストモジュール
module mod_gedatsu_driver_test_c
  use mod_gedatsu
  use mod_monolis_utils
  implicit none

contains

  subroutine gedatsu_driver_c_test()
    call monolis_std_global_log_string("C Language section: gedatsu_driver_c_test")
    call gedatsu_simple_mesh2graph_convertor_test()
    call gedatsu_nodal_graph_partitioner_test()
    call gedatsu_conn_graph_partitioner_test()
    call gedatsu_nodal_val_partitioner_test()
    call gedatsu_conn_val_partitioner_test()
    call gedatsu_bc_partitioner_test()
    call gedatsu_simple_mesh_partitioner_test()
  end subroutine gedatsu_driver_c_test

  subroutine gedatsu_simple_mesh_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: i, j
    integer(kint) :: n_node, n_node_ans
    integer(kint) :: n_elem, n_elem_ans
    integer(kint) :: n_base, n_base_ans
    integer(kint) :: n_internal, n_internal_ans
    integer(kint) :: n_neib, n_neib_ans
    integer(kint), allocatable :: vertex_id(:), vertex_id_ans(:)
    integer(kint), allocatable :: elem(:,:), elem_ans(:,:)
    integer(kint), pointer :: neib_pe(:) => null()
    integer(kint), pointer :: neib_pe_ans(:) => null()
    integer(kint), pointer :: indexp(:) => null()
    integer(kint), pointer :: indexp_ans(:) => null()
    integer(kint), pointer :: itemp(:) => null()
    integer(kint), pointer :: itemp_ans(:) => null()
    real(kdouble), allocatable :: node(:,:)
    real(kdouble), allocatable :: node_ans(:,:)

    call monolis_std_global_log_string("gedatsu_simple_mesh_partitioner")

    fname = "parted.0.c/node.beam.dat.0"
    call monolis_input_node(fname, n_node, node)

    fname = "parted.0.ans/node.beam.dat.0"
    call monolis_input_node(fname, n_node_ans, node_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org a-0 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-0 2", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-0 2", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-0 2", node_ans(3,i), node(3,i))
    enddo



    call monolis_dealloc_R_2d(node)
    call monolis_dealloc_R_2d(node_ans)

    fname = "parted.0.c/node.beam.dat.1"
    call monolis_input_node(fname, n_node, node)

    fname = "parted.0.ans/node.beam.dat.1"
    call monolis_input_node(fname, n_node_ans, node_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org a-1 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-1 2", node_ans(1,i), node(1,i))
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-1 2", node_ans(2,i), node(2,i))
      call monolis_test_check_eq_R1("gedatsu_simple_mesh_partitioner_test 0org a-1 2", node_ans(3,i), node(3,i))
    enddo



    call monolis_dealloc_R_2d(node)
    call monolis_dealloc_R_2d(node_ans)

    fname = "parted.0.c/node.beam.dat.id.0"
    call monolis_input_global_id(fname, n_node, vertex_id)

    fname = "parted.0.ans/node.beam.dat.id.0"
    call monolis_input_global_id(fname, n_node_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org b-0 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org b-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/node.beam.dat.id.1"
    call monolis_input_global_id(fname, n_node, vertex_id)

    fname = "parted.0.ans/node.beam.dat.id.1"
    call monolis_input_global_id(fname, n_node_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org b-1 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org b-1 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    fname = "parted.0.c/node.beam.dat.n_internal.0"
    call monolis_input_internal_vertex_number(fname, n_internal)

    fname = "parted.0.ans/node.beam.dat.n_internal.0"
    call monolis_input_internal_vertex_number(fname, n_internal_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org c-0 1", n_internal, n_internal_ans)



    fname = "parted.0.c/node.beam.dat.n_internal.1"
    call monolis_input_internal_vertex_number(fname, n_internal)

    fname = "parted.0.ans/node.beam.dat.n_internal.1"
    call monolis_input_internal_vertex_number(fname, n_internal_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org c-1 1", n_internal, n_internal_ans)



    fname = "parted.0.c/node.beam.dat.recv.0"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/node.beam.dat.recv.0"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org d-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org d-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org d-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org d-0 4", itemp(i), itemp_ans(i) - 1)
    enddo



    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/node.beam.dat.send.0"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/node.beam.dat.send.0"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org e-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org e-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org e-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org e-0 4", itemp(i), itemp_ans(i) - 1)
    enddo



    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/node.beam.dat.recv.1"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/node.beam.dat.recv.1"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org f-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org f-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org f-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org f-0 4", itemp(i), itemp_ans(i) - 1)
    enddo


    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/node.beam.dat.send.1"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/node.beam.dat.send.1"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org g-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org g-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org g-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org g-0 4", itemp(i), itemp_ans(i) - 1)
    enddo



    fname = "parted.0.c/elem.beam.dat.0"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    fname = "parted.0.ans/elem.beam.dat.0"
    call monolis_input_elem(fname, n_elem_ans, n_base_ans, elem_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-0 1", n_elem, n_elem_ans)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-0 1", n_base, n_base_ans)

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-0 2", elem_ans(j,i) - 1, elem(j,i))
    enddo
    enddo



    call monolis_dealloc_I_2d(elem)

    fname = "parted.0.c/elem.beam.dat.1"
    call monolis_input_elem(fname, n_elem, n_base, elem)

    fname = "parted.0.ans/elem.beam.dat.1"
    call monolis_input_elem(fname, n_elem_ans, n_base_ans, elem_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-1 1", n_elem, n_elem_ans)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-1 1", n_base, n_base_ans)

    do i = 1, n_elem
    do j = 1, n_base
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org h-1 2", elem_ans(j,i) - 1, elem(j,i))
    enddo
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/elem.beam.dat.id.0"
    call monolis_input_global_id(fname, n_node, vertex_id)

    fname = "parted.0.ans/elem.beam.dat.id.0"
    call monolis_input_global_id(fname, n_node_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org i-0 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org i-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/elem.beam.dat.id.1"
    call monolis_input_global_id(fname, n_node, vertex_id)

    fname = "parted.0.ans/elem.beam.dat.id.1"
    call monolis_input_global_id(fname, n_node_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org i-1 1", n_node, n_node_ans)

    do i = 1, n_node
      call monolis_test_check_eq_I1("gedatsu_simple_mesh_partitioner_test 0org i-1 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo
  end subroutine gedatsu_simple_mesh_partitioner_test

  subroutine gedatsu_bc_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_bc, n_bc_ans
    integer(kint) :: n_dof, n_dof_ans
    integer(kint) :: i
    integer(kint), allocatable :: i_bc(:,:), i_bc_ans(:,:)
    real(kdouble), allocatable :: r_bc(:), r_bc_ans(:)
    complex(kdouble), allocatable :: c_bc(:), c_bc_ans(:)

    call monolis_std_global_log_string("gedatsu_bc_partitioner_C")
    call monolis_std_global_log_string("gedatsu_bc_partitioner_R")

    fname = "parted.0.c/bc.r.dat.0"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    fname = "parted.0.ans/bc.r.dat.0"
    call monolis_input_bc_R(fname, n_bc_ans, n_dof_ans, i_bc_ans, r_bc_ans)

    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 1", n_bc, n_bc_ans)
    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 2", n_dof, n_dof_ans)

    do i = 1, n_bc
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 3", i_bc(1,i), i_bc_ans(1,i) - 1)
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 3", i_bc(2,i), i_bc_ans(2,i))
      call monolis_test_check_eq_R1("gedatsu_bc_partitioner_test 0org R a-0 4", r_bc(i), r_bc_ans(i))
    enddo



    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_I_2d(i_bc_ans)
    call monolis_dealloc_R_1d(r_bc)
    call monolis_dealloc_R_1d(r_bc_ans)

    fname = "parted.0.c/bc.r.dat.1"
    call monolis_input_bc_R(fname, n_bc, n_dof, i_bc, r_bc)

    fname = "parted.0.ans/bc.r.dat.1"
    call monolis_input_bc_R(fname, n_bc_ans, n_dof_ans, i_bc_ans, r_bc_ans)

    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 1", n_bc, n_bc_ans)
    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 2", n_dof, n_dof_ans)

    do i = 1, n_bc
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 3", i_bc(1,i), i_bc_ans(1,i) - 1)
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org R a-0 3", i_bc(2,i), i_bc_ans(2,i))
      call monolis_test_check_eq_R1("gedatsu_bc_partitioner_test 0org R a-0 4", r_bc(i), r_bc_ans(i))
    enddo



    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_I_2d(i_bc_ans)

    fname = "parted.0.c/bc.c.dat.0"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    fname = "parted.0.ans/bc.c.dat.0"
    call monolis_input_bc_C(fname, n_bc_ans, n_dof_ans, i_bc_ans, c_bc_ans)

    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 1", n_bc, n_bc_ans)
    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 2", n_dof, n_dof_ans)

    do i = 1, n_bc
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 3", i_bc(1,i), i_bc_ans(1,i) - 1)
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 3", i_bc(2,i), i_bc_ans(2,i))
      call monolis_test_check_eq_C1("gedatsu_bc_partitioner_test 0org C a-0 4", c_bc(i), c_bc_ans(i))
    enddo



    call monolis_dealloc_I_2d(i_bc)
    call monolis_dealloc_I_2d(i_bc_ans)
    call monolis_dealloc_C_1d(c_bc)
    call monolis_dealloc_C_1d(c_bc_ans)

    fname = "parted.0.c/bc.c.dat.1"
    call monolis_input_bc_C(fname, n_bc, n_dof, i_bc, c_bc)

    fname = "parted.0.ans/bc.c.dat.1"
    call monolis_input_bc_C(fname, n_bc_ans, n_dof_ans, i_bc_ans, c_bc_ans)

    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 1", n_bc, n_bc_ans)
    call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 2", n_dof, n_dof_ans)

    do i = 1, n_bc
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 3", i_bc(1,i), i_bc_ans(1,i) - 1)
      call monolis_test_check_eq_I1("gedatsu_bc_partitioner_test 0org C a-0 3", i_bc(2,i), i_bc_ans(2,i))
      call monolis_test_check_eq_C1("gedatsu_bc_partitioner_test 0org C a-0 4", c_bc(i), c_bc_ans(i))
    enddo

  end subroutine gedatsu_bc_partitioner_test

  subroutine gedatsu_conn_val_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    character(monolis_charlen) :: label
    integer(kint) :: n_node, n_node_ans
    integer(kint) :: n_dof, n_dof_ans
    integer(kint) :: i, j
    integer(kint), allocatable :: ival(:,:), ival_ans(:,:)
    real(kdouble), allocatable :: rval(:,:), rval_ans(:,:)
    complex(kdouble), allocatable :: cval(:,:), cval_ans(:,:)

    call monolis_std_global_log_string("gedatsu_dist_val_partitioner_C")
    call monolis_std_global_log_string("gedatsu_dist_val_partitioner_I")
    call monolis_std_global_log_string("gedatsu_dist_val_partitioner_R")

    fname = "parted.0.c/val.conn.i.dat.0"
    call monolis_input_distval_i(fname, label, n_node, n_dof, ival)

    fname = "parted.0.ans/val.conn.i.dat.0"
    call monolis_input_distval_i(fname, label, n_node_ans, n_dof_ans, ival_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-0 3", ival(j,i), ival_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_I_2d(ival)
    call monolis_dealloc_I_2d(ival_ans)

    fname = "parted.0.c/val.conn.i.dat.1"
    call monolis_input_distval_i(fname, label, n_node, n_dof, ival)

    fname = "parted.0.ans/val.conn.i.dat.1"
    call monolis_input_distval_i(fname, label, n_node_ans, n_dof_ans, ival_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org a-1 3", ival(j,i), ival_ans(j,i))
    enddo
    enddo



    fname = "parted.0.c/val.conn.r.dat.0"
    call monolis_input_distval_r(fname, label, n_node, n_dof, rval)

    fname = "parted.0.ans/val.conn.r.dat.0"
    call monolis_input_distval_r(fname, label, n_node_ans, n_dof_ans, rval_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org b-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org b-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_R1("gedatsu_conn_val_partitioner_test 0org b-0 3", rval(j,i), rval_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_R_2d(rval)
    call monolis_dealloc_R_2d(rval_ans)

    fname = "parted.0.c/val.conn.r.dat.1"
    call monolis_input_distval_r(fname, label, n_node, n_dof, rval)

    fname = "parted.0.ans/val.conn.r.dat.1"
    call monolis_input_distval_r(fname, label, n_node_ans, n_dof_ans, rval_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org b-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org b-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_R1("gedatsu_conn_val_partitioner_test 0org b-1 3", rval(j,i), rval_ans(j,i))
    enddo
    enddo



    fname = "parted.0.c/val.conn.c.dat.0"
    call monolis_input_distval_c(fname, label, n_node, n_dof, cval)

    fname = "parted.0.ans/val.conn.c.dat.0"
    call monolis_input_distval_c(fname, label, n_node_ans, n_dof_ans, cval_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org c-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org c-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_C1("gedatsu_conn_val_partitioner_test 0org b-0 3", cval(j,i), cval_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_C_2d(cval)
    call monolis_dealloc_C_2d(cval_ans)

    fname = "parted.0.c/val.conn.c.dat.1"
    call monolis_input_distval_c(fname, label, n_node, n_dof, cval)

    fname = "parted.0.ans/val.conn.c.dat.1"
    call monolis_input_distval_c(fname, label, n_node_ans, n_dof_ans, cval_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org c-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_conn_val_partitioner_test 0org c-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_C1("gedatsu_conn_val_partitioner_test 0org c-1 3", cval(j,i), cval_ans(j,i))
    enddo
    enddo
  end subroutine gedatsu_conn_val_partitioner_test

  subroutine gedatsu_nodal_val_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    character(monolis_charlen) :: label
    integer(kint) :: n_node, n_node_ans
    integer(kint) :: n_dof, n_dof_ans
    integer(kint) :: i, j
    integer(kint), allocatable :: ival(:,:), ival_ans(:,:)
    real(kdouble), allocatable :: rval(:,:), rval_ans(:,:)
    complex(kdouble), allocatable :: cval(:,:), cval_ans(:,:)

    fname = "parted.0.c/val.node.i.dat.0"
    call monolis_input_distval_i(fname, label, n_node, n_dof, ival)

    fname = "parted.0.ans/val.node.i.dat.0"
    call monolis_input_distval_i(fname, label, n_node_ans, n_dof_ans, ival_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-0 3", ival(j,i), ival_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_I_2d(ival)
    call monolis_dealloc_I_2d(ival_ans)

    fname = "parted.0.c/val.node.i.dat.1"
    call monolis_input_distval_i(fname, label, n_node, n_dof, ival)

    fname = "parted.0.ans/val.node.i.dat.1"
    call monolis_input_distval_i(fname, label, n_node_ans, n_dof_ans, ival_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org a-1 3", ival(j,i), ival_ans(j,i))
    enddo
    enddo



    fname = "parted.0.c/val.node.r.dat.0"
    call monolis_input_distval_r(fname, label, n_node, n_dof, rval)

    fname = "parted.0.ans/val.node.r.dat.0"
    call monolis_input_distval_r(fname, label, n_node_ans, n_dof_ans, rval_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org b-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org b-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_R1("gedatsu_nodal_val_partitioner_test 0org b-0 3", rval(j,i), rval_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_R_2d(rval)
    call monolis_dealloc_R_2d(rval_ans)

    fname = "parted.0.c/val.node.r.dat.1"
    call monolis_input_distval_r(fname, label, n_node, n_dof, rval)

    fname = "parted.0.ans/val.node.r.dat.1"
    call monolis_input_distval_r(fname, label, n_node_ans, n_dof_ans, rval_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org b-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org b-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_R1("gedatsu_nodal_val_partitioner_test 0org b-1 3", rval(j,i), rval_ans(j,i))
    enddo
    enddo



    fname = "parted.0.c/val.node.c.dat.0"
    call monolis_input_distval_c(fname, label, n_node, n_dof, cval)

    fname = "parted.0.ans/val.node.c.dat.0"
    call monolis_input_distval_c(fname, label, n_node_ans, n_dof_ans, cval_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org c-0 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org c-0 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_C1("gedatsu_nodal_val_partitioner_test 0org b-0 3", cval(j,i), cval_ans(j,i))
    enddo
    enddo



    call monolis_dealloc_C_2d(cval)
    call monolis_dealloc_C_2d(cval_ans)

    fname = "parted.0.c/val.node.c.dat.1"
    call monolis_input_distval_c(fname, label, n_node, n_dof, cval)

    fname = "parted.0.ans/val.node.c.dat.1"
    call monolis_input_distval_c(fname, label, n_node_ans, n_dof_ans, cval_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org c-1 1", n_node, n_node_ans)
    call monolis_test_check_eq_I1("gedatsu_nodal_val_partitioner_test 0org c-1 2", n_dof, n_dof_ans)

    do i = 1, n_node
    do j = 1, n_dof
      call monolis_test_check_eq_C1("gedatsu_nodal_val_partitioner_test 0org c-1 3", cval(j,i), cval_ans(j,i))
    enddo
    enddo
  end subroutine gedatsu_nodal_val_partitioner_test

  subroutine gedatsu_conn_graph_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: i
    integer(kint) :: n_vertex, n_vertex_ans
    integer(kint), allocatable :: vertex_id(:), vertex_id_ans(:)
    integer(kint), allocatable :: index(:), index_ans(:)
    integer(kint), allocatable :: item(:), item_ans(:)

    call monolis_std_global_log_string("gedatsu_connectivity_graph_partitioner")

    fname = "parted.0.c/connectivity.dat.0"
    call monolis_input_graph(fname, n_vertex, vertex_id, index, item)

    fname = "parted.0.ans/connectivity.dat.0"
    call monolis_input_graph(fname, n_vertex_ans, vertex_id_ans, index_ans, item_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo

    do i = 1, n_vertex + 1
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 3", index(i), index_ans(i))
    enddo

    do i = 1, index(n_vertex + 1)
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 4", item(i), item_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)
    call monolis_dealloc_I_1d(index)
    call monolis_dealloc_I_1d(index_ans)
    call monolis_dealloc_I_1d(item)
    call monolis_dealloc_I_1d(item_ans)

    fname = "parted.0.c/connectivity.dat.1"
    call monolis_input_graph(fname, n_vertex, vertex_id, index, item)

    fname = "parted.0.ans/connectivity.dat.1"
    call monolis_input_graph(fname, n_vertex_ans, vertex_id_ans, index_ans, item_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo

    do i = 1, n_vertex + 1
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 3", index(i), index_ans(i))
    enddo

    do i = 1, index(n_vertex + 1)
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org a-0 4", item(i), item_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/connectivity.dat.id.0"
    call monolis_input_global_id(fname, n_vertex, vertex_id)

    fname = "parted.0.ans/connectivity.dat.id.0"
    call monolis_input_global_id(fname, n_vertex_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org b-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org b-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/connectivity.dat.id.0"
    call monolis_input_global_id(fname, n_vertex, vertex_id)

    fname = "parted.0.ans/connectivity.dat.id.0"
    call monolis_input_global_id(fname, n_vertex_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org b-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_conn_graph_partitioner_test 0org b-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo
  end subroutine gedatsu_conn_graph_partitioner_test

  subroutine gedatsu_nodal_graph_partitioner_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: i
    integer(kint) :: n_vertex, n_vertex_ans
    integer(kint) :: n_internal, n_internal_ans
    integer(kint) :: n_neib, n_neib_ans
    integer(kint), allocatable :: vertex_id(:), vertex_id_ans(:)
    integer(kint), allocatable :: index(:), index_ans(:)
    integer(kint), allocatable :: item(:), item_ans(:)
    integer(kint), pointer :: neib_pe(:) => null()
    integer(kint), pointer :: neib_pe_ans(:) => null()
    integer(kint), pointer :: indexp(:) => null()
    integer(kint), pointer :: indexp_ans(:) => null()
    integer(kint), pointer :: itemp(:) => null()
    integer(kint), pointer :: itemp_ans(:) => null()

    call monolis_std_global_log_string("gedatsu_nodal_graph_partitioner")

    fname = "parted.0.c/graph.dat.0"
    call monolis_input_graph(fname, n_vertex, vertex_id, index, item)

    fname = "parted.0.ans/graph.dat.0"
    call monolis_input_graph(fname, n_vertex_ans, vertex_id_ans, index_ans, item_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo

    do i = 1, n_vertex + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-0 3", index(i), index_ans(i))
    enddo

    do i = 1, index(n_vertex + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-0 4", item(i), item_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)
    call monolis_dealloc_I_1d(index)
    call monolis_dealloc_I_1d(index_ans)
    call monolis_dealloc_I_1d(item)
    call monolis_dealloc_I_1d(item_ans)

    fname = "parted.0.c/graph.dat.1"
    call monolis_input_graph(fname, n_vertex, vertex_id, index, item)

    fname = "parted.0.ans/graph.dat.1"
    call monolis_input_graph(fname, n_vertex_ans, vertex_id_ans, index_ans, item_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-1 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-1 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo

    do i = 1, n_vertex + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-1 3", index(i), index_ans(i))
    enddo

    do i = 1, index(n_vertex + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org a-1 4", item(i), item_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/graph.dat.id.0"
    call monolis_input_global_id(fname, n_vertex, vertex_id)

    fname = "parted.0.ans/graph.dat.id.0"
    call monolis_input_global_id(fname, n_vertex_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org b-0 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org b-0 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    call monolis_dealloc_I_1d(vertex_id)
    call monolis_dealloc_I_1d(vertex_id_ans)

    fname = "parted.0.c/graph.dat.id.1"
    call monolis_input_global_id(fname, n_vertex, vertex_id)

    fname = "parted.0.ans/graph.dat.id.1"
    call monolis_input_global_id(fname, n_vertex_ans, vertex_id_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org b-1 1", n_vertex, n_vertex_ans)

    do i = 1, n_vertex
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org b-1 2", vertex_id(i), vertex_id_ans(i) - 1)
    enddo



    fname = "parted.0.c/graph.dat.n_internal.0"
    call monolis_input_internal_vertex_number(fname, n_internal)

    fname = "parted.0.ans/graph.dat.n_internal.0"
    call monolis_input_internal_vertex_number(fname, n_internal_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org c-0 1", n_internal, n_internal_ans)



    fname = "parted.0.c/graph.dat.n_internal.1"
    call monolis_input_internal_vertex_number(fname, n_internal)

    fname = "parted.0.ans/graph.dat.n_internal.1"
    call monolis_input_internal_vertex_number(fname, n_internal_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org c-1 1", n_internal, n_internal_ans)



    fname = "parted.0.c/graph.dat.recv.0"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/graph.dat.recv.0"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org d-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org d-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org d-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org d-0 4", itemp(i), itemp_ans(i) - 1)
    enddo



    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/graph.dat.send.0"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/graph.dat.send.0"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org e-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org e-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org e-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org e-0 4", itemp(i), itemp_ans(i) - 1)
    enddo



    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/graph.dat.recv.1"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/graph.dat.recv.1"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org f-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org f-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org f-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org f-0 4", itemp(i), itemp_ans(i) - 1)
    enddo


    call monolis_pdealloc_I_1d(neib_pe)
    call monolis_pdealloc_I_1d(neib_pe_ans)
    call monolis_pdealloc_I_1d(indexp)
    call monolis_pdealloc_I_1d(indexp_ans)
    call monolis_pdealloc_I_1d(itemp)
    call monolis_pdealloc_I_1d(itemp_ans)

    fname = "parted.0.c/graph.dat.send.1"
    call monolis_input_com_table_main(fname, n_neib, neib_pe, indexp, itemp)

    fname = "parted.0.ans/graph.dat.send.1"
    call monolis_input_com_table_main(fname, n_neib_ans, neib_pe_ans, indexp_ans, itemp_ans)

    call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org g-0 1", n_neib, n_neib_ans)

    do i = 1, n_neib
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org g-0 2", neib_pe(i), neib_pe_ans(i))
    enddo

    do i = 1, n_neib + 1
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org g-0 3", indexp(i), indexp_ans(i))
    enddo

    do i = 1, indexp(n_neib + 1)
      call monolis_test_check_eq_I1("gedatsu_nodal_graph_partitioner_test 0org g-0 4", itemp(i), itemp_ans(i) - 1)
    enddo
  end subroutine gedatsu_nodal_graph_partitioner_test

  subroutine gedatsu_simple_mesh2graph_convertor_test()
    implicit none
    character(monolis_charlen) :: fname
    integer(kint) :: n_vertex
    integer(kint), allocatable :: vertex_id(:)
    integer(kint), allocatable :: index(:)
    integer(kint), allocatable :: item(:)

    call monolis_std_global_log_string("gedatsu_simple_mesh2graph_convertor")

    fname = "driver/output.c/graph.conv.dat"
    call monolis_input_graph(fname, n_vertex, vertex_id, index, item)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 1", n_vertex, 6)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 2", index(1), 0)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 3", index(2), 3)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 4", index(3), 8)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 5", index(4), 11)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 6", index(5), 14)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 7", index(6), 19)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org 8", index(7), 22)

    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 1", item(1), 1)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 2", item(2), 3)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 3", item(3), 4)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 4", item(4), 0)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 5", item(5), 2)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 6", item(6), 3)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 7", item(7), 4)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 8", item(8), 5)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 9", item(9), 1)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 10", item(10), 4)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 11", item(11), 5)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 12", item(12), 0)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 13", item(13), 1)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 14", item(14), 4)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 15", item(15), 0)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 16", item(16), 1)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 17", item(17), 2)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 18", item(18), 3)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 19", item(19), 5)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 20", item(20), 1)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 21", item(21), 2)
    call monolis_test_check_eq_I1("gedatsu_simple_mesh2graph_convertor 0org a 22", item(22), 4)
  end subroutine gedatsu_simple_mesh2graph_convertor_test

end module mod_gedatsu_driver_test_c
