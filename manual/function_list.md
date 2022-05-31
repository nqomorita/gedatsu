# function list

## util function

- gedatsu_graph_initialize(graph)
- gedatsu_graph_finalize(graph)

## wrapper function

- gedatsu_graph_input(fname, graph)
- gedatsu_graph_output(fname, graph)

- gedatsu_graph_get_n_vertex(graph, n_vertex)
- gedatsu_graph_get_ith_vertex_id(graph, i, vertex_id)

- gedatsu_graph_partition(graph, n_domain)

## developer-side function

- gedatsu_convert_single_elem_to_mesh(n_elem, n_base_func, elem, mesh_index, mesh_item)
- gedatsu_convert_mesh_to_nodal_graph(n_node, n_elem, mesh_index, mesh_item, graph_index, graph_item)

- gedatsu_get_partitioned_graph(nnode, graph_index, graph_item, n_domain, part_id)
- gedatsu_get_partitioned_graph_with_weight(n_node, graph_index, graph_item, n_domain, node_wgt, edge_wgt, part_id)

- gedatsu_part_graph_metis_kway(n_node, graph_index, graph_item, npart, node_wgt, edge_wgt, part_id)

