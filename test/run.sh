#!/bin/bash

INP=driver/input
OUT=driver/output

#../bin/gedatsu_convertor_simple_mesh2graph -i ${INP}/elem.quad.dat -o ${OUT}/graph.conv.dat

../bin/gedatsu_partitioner_nodal_graph -i ${INP}/graph.dat -o graph.dat -n 2

#./gedatsu_test
