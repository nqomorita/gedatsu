#!/bin/bash

make clean

make 

../../bin/gedatsu_graph_partitioner -n 2

mpirun -np 2 ./a.out

