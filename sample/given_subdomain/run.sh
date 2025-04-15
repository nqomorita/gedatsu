#!/bin/bash

make clean

make 

../../bin/gedatsu_nodal_graph_partitioner -n 2

mv parted.0 parted.org.g.0

../../bin/gedatsu_nodal_graph_partitioner -n 2 --given_subdomain subdomain_id.dat

mv parted.0 parted.given.g.0

diff parted.given.g.0 parted.org.g.0 

../../bin/gedatsu_simple_mesh_partitioner -n 2

mv parted.0 parted.org.s.0

../../bin/gedatsu_simple_mesh_partitioner -n 2 --given_subdomain subdomain_id.dat

mv parted.0 parted.given.s.0

diff parted.given.s.0 parted.org.s.0 

