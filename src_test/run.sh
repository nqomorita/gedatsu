#!/bin/bash

INP=driver/input
OUT=driver/output

../bin/gedatsu_simple_mesh2graph_convertor -i ${INP}/elem.quad.dat -o ${OUT}/graph.conv.dat

../bin/gedatsu_nodal_graph_partitioner -i ${INP}/graph.dat -o graph.dat -n 2

cp ${INP}/graph.dat ./
../bin/gedatsu_connectivity_graph_partitioner -ig ./graph.dat -i ${INP}/connectivity.dat -o connectivity.dat -n 2

#./gedatsu_test
