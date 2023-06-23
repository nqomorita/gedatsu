#!/bin/bash

make clean

make 

#../../bin/gedatsu_nodal_graph_partitioner -n 2
../../bin/gedatsu_nodal_graph_partitioner -n 2 -inw weight.dat

mpirun -np 2 ./a.out

