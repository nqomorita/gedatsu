# function list

## util function

- gedatsu_graph_initialize(graph)
- gedatsu_graph_finalize(graph)

- gedatsu_dlb_initialize(dlb_manager)
- gedatsu_dlb_finalize(dlb_manager)

## wrapper function

- gedatsu_graph_input(fname, graph)
- gedatsu_graph_output(fname, graph)

- gedatsu_graph_get_n_vertex(graph, n_vertex)
- gedatsu_graph_get_ith_vertex_id(graph, i, vertex_id)

- gedatsu_graph_set_n_vertex(graph, n_vertex)
- gedatsu_graph_add_edge(graph, n_edge, array)
- gedatsu_graph_set_vertex_weight_value(graph, i, val)
- gedatsu_graph_set_vertex_weight_array(graph, array)
- gedatsu_graph_set_edge_weight_value(graph, i, val)
- gedatsu_graph_set_edge_weight_array(graph, array)

- gedatsu_graph_add_vertex(graph, n_vertex)
- gedatsu_graph_add_edge(graph, array)
- gedatsu_graph_add_vertex_weight_value(graph, i, val)
- gedatsu_graph_add_vertex_weight_array(graph, array)
- gedatsu_graph_add_edge_weight_value(graph, i, val)
- gedatsu_graph_add_edge_weight_array(graph, array)

- gedatsu_graph_partition(graph, n_domain)
- gedatsu_graph_partition_with_weight(graph, n_domain, vertex_wgt, edge_wgt)

## developer-side function

- gedatsu_convert_single_elem_to_mesh(n_elem, n_base_func, elem, mesh_index, mesh_item)
- gedatsu_convert_mesh_to_nodal_graph(n_node, n_elem, mesh_index, mesh_item, graph_index, graph_item)

- gedatsu_get_partitioned_graph(n_vertex, graph_index, graph_item, n_domain, part_id)
- gedatsu_get_partitioned_graph_with_weight(n_vertex, graph_index, graph_item, n_domain, vertex_wgt, edge_wgt, part_id)

- gedatsu_part_graph_metis_kway(n_vertex, graph_index, graph_item, npart, vertex_wgt, edge_wgt, part_id)
