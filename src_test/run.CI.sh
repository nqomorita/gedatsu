#!/bin/bash

INPF=driver/input.f
OUTF=driver/output.f
INPC=driver/input.c
OUTC=driver/output.c

../bin/gedatsu_simple_mesh2graph_convertor -i ${INPF}/elem.quad.dat -o ${OUTF}/graph.conv.dat

../bin/gedatsu_nodal_graph_partitioner -i ${INPF}/graph.dat -o graph.dat -n 2

cp ${INPF}/graph.dat ./
../bin/gedatsu_connectivity_graph_partitioner -ig ./graph.dat -i ${INPF}/connectivity.dat -o connectivity.dat -n 2

cp ${INPF}/val.node.i.dat ./
../bin/gedatsu_dist_val_i_partitioner -i ./val.node.i.dat -id graph.dat.id -n 2

cp ${INPF}/val.node.r.dat ./
../bin/gedatsu_dist_val_r_partitioner -i ./val.node.r.dat -id graph.dat.id -n 2

cp ${INPF}/val.node.c.dat ./
../bin/gedatsu_dist_val_c_partitioner -i ./val.node.c.dat -id graph.dat.id -n 2

cp ${INPF}/val.conn.i.dat ./
../bin/gedatsu_dist_val_i_partitioner -i ./val.conn.i.dat -id connectivity.dat.id -n 2

cp ${INPF}/val.conn.r.dat ./
../bin/gedatsu_dist_val_r_partitioner -i ./val.conn.r.dat -id connectivity.dat.id -n 2

cp ${INPF}/val.conn.c.dat ./
../bin/gedatsu_dist_val_c_partitioner -i ./val.conn.c.dat -id connectivity.dat.id -n 2

mv parted.0 parted.0.f

../bin/gedatsu_simple_mesh2graph_convertor -i ${INPC}/elem.quad.dat -o ${OUTC}/graph.conv.dat

../bin/gedatsu_nodal_graph_partitioner -i ${INPC}/graph.dat -o graph.dat -n 2

cp ${INPC}/graph.dat ./
../bin/gedatsu_connectivity_graph_partitioner -ig ./graph.dat -i ${INPC}/connectivity.dat -o connectivity.dat -n 2

cp ${INPC}/val.node.i.dat ./
../bin/gedatsu_dist_val_i_partitioner -i ./val.node.i.dat -id graph.dat.id -n 2

cp ${INPC}/val.node.r.dat ./
../bin/gedatsu_dist_val_r_partitioner -i ./val.node.r.dat -id graph.dat.id -n 2

cp ${INPC}/val.node.c.dat ./
../bin/gedatsu_dist_val_c_partitioner -i ./val.node.c.dat -id graph.dat.id -n 2

cp ${INPF}/val.conn.i.dat ./
../bin/gedatsu_dist_val_i_partitioner -i ./val.conn.i.dat -id connectivity.dat.id -n 2

cp ${INPF}/val.conn.r.dat ./
../bin/gedatsu_dist_val_r_partitioner -i ./val.conn.r.dat -id connectivity.dat.id -n 2

cp ${INPF}/val.conn.c.dat ./
../bin/gedatsu_dist_val_c_partitioner -i ./val.conn.c.dat -id connectivity.dat.id -n 2

mv parted.0 parted.0.c

./gedatsu_test

rm -r parted.0.f

rm -r parted.0.c

rm graph.dat

rm val.*
