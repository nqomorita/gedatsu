/* monolis_merge_c_test.h */
#include<complex.h>
#ifndef GEDATSU_GRAPH_MERGE_C_TEST_H
#define GEDATSU_GRAPH_MERGE_C_TEST_H

#ifdef __cplusplus
extern "C" {
#endif

void gedatsu_graph_merge_c_test();

void gedatsu_list_initialize_R_c_test();
void gedatsu_list_initialize_I_c_test();
void gedatsu_list_initialize_C_c_test();
void gedatsu_list_finalize_R_c_test();
void gedatsu_list_finalize_I_c_test();
void gedatsu_list_finalize_C_c_test();
void gedatsu_list_set_R_c_test();
void gedatsu_list_set_I_c_test();
void gedatsu_list_set_C_c_test();
void gedatsu_list_get_R_c_test();
void gedatsu_list_get_I_c_test();
void gedatsu_list_get_C_c_test();

void gedatsu_merge_nodal_subgraphs_c_test();
void gedatsu_merge_connectivity_subgraphs_c_test();
void gedatsu_merge_distval_R_c_test();
void gedatsu_merge_distval_I_c_test();
void gedatsu_merge_distval_C_c_test();

void monolis_test_check_eq_R(
  const char* header,
  const int size_a,
  const double* a,
  const int size_b,
  const double* b);

void monolis_test_check_eq_I(
  const char* header,
  const int size_a,
  const int* a,
  const int size_b,
  const int* b);

void monolis_test_check_eq_C(
  const char* header,
  const int size_a,
  const double complex* a,
  const int size_b,
  const double complex* b);

#ifdef __cplusplus
}
#endif

#endif
