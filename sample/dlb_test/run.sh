#!/bin/bash

make clean

make 

#../../bin/gedatsu_nodal_graph_partitioner -n 2
../../bin/gedatsu_nodal_graph_partitioner -n 2 -inw weight.dat

../../bin/gedatsu_connectivity_graph_partitioner -n 2 -i conn.dat

mpirun -np 2 ./a.out
