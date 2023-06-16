#!/bin/bash

INPF=driver/input.f
OUTF=driver/output.f
INPC=driver/input.c
OUTC=driver/output.c

mkdir ${OUTF}
mkdir ${OUTC}

../bin/gedatsu_simple_mesh2graph_convertor -i ${INPF}/elem.quad.dat -o ${OUTF}/graph.conv.dat

cp ${INPF}/graph.dat ./
../bin/gedatsu_nodal_graph_partitioner -i ./graph.dat -o graph.dat -n 2

cp ${INPF}/graph.dat ./
cp ${INPF}/connectivity.dat ./
../bin/gedatsu_connectivity_graph_partitioner -ig ./graph.dat -i ./connectivity.dat -o connectivity.dat -n 2

cp ${INPF}/val.node.i.dat ./
../bin/gedatsu_dist_val_partitioner_I -i ./val.node.i.dat -ig graph.dat -n 2

cp ${INPF}/val.node.r.dat ./
../bin/gedatsu_dist_val_partitioner_R -i ./val.node.r.dat -ig graph.dat -n 2

cp ${INPF}/val.node.c.dat ./
../bin/gedatsu_dist_val_partitioner_C -i ./val.node.c.dat -ig graph.dat -n 2

cp ${INPF}/val.conn.i.dat ./
../bin/gedatsu_dist_val_partitioner_I -i ./val.conn.i.dat -ig connectivity.dat -n 2

cp ${INPF}/val.conn.r.dat ./
../bin/gedatsu_dist_val_partitioner_R -i ./val.conn.r.dat -ig connectivity.dat -n 2

cp ${INPF}/val.conn.c.dat ./
../bin/gedatsu_dist_val_partitioner_C -i ./val.conn.c.dat -ig connectivity.dat -n 2

cp ${INPF}/bc.r.dat ./
../bin/gedatsu_bc_partitioner_R -i ./bc.r.dat -ig graph.dat -n 2

cp ${INPF}/bc.c.dat ./
../bin/gedatsu_bc_partitioner_C -i ./bc.c.dat -ig graph.dat -n 2

cp ${INPF}/node.beam.dat ./
cp ${INPF}/elem.beam.dat ./
../bin/gedatsu_simple_mesh_partitioner -in ./node.beam.dat -ie ./elem.beam.dat -n 2

mv parted.0 parted.0.f

../bin/gedatsu_simple_mesh2graph_convertor -i ${INPC}/elem.quad.dat -o ${OUTC}/graph.conv.dat

cp ${INPC}/graph.dat ./
../bin/gedatsu_nodal_graph_partitioner -i ./graph.dat -o graph.dat -n 2

cp ${INPC}/graph.dat ./
cp ${INPC}/connectivity.dat ./
../bin/gedatsu_connectivity_graph_partitioner -ig ./graph.dat -i ./connectivity.dat -o connectivity.dat -n 2

cp ${INPC}/val.node.i.dat ./
../bin/gedatsu_dist_val_partitioner_I -i ./val.node.i.dat -ig graph.dat -n 2

cp ${INPC}/val.node.r.dat ./
../bin/gedatsu_dist_val_partitioner_R -i ./val.node.r.dat -ig graph.dat -n 2

cp ${INPC}/val.node.c.dat ./
../bin/gedatsu_dist_val_partitioner_C -i ./val.node.c.dat -ig graph.dat -n 2

cp ${INPC}/val.conn.i.dat ./
../bin/gedatsu_dist_val_partitioner_I -i ./val.conn.i.dat -ig connectivity.dat -n 2

cp ${INPC}/val.conn.r.dat ./
../bin/gedatsu_dist_val_partitioner_R -i ./val.conn.r.dat -ig connectivity.dat -n 2

cp ${INPC}/val.conn.c.dat ./
../bin/gedatsu_dist_val_partitioner_C -i ./val.conn.c.dat -ig connectivity.dat -n 2

cp ${INPC}/bc.r.dat ./
../bin/gedatsu_bc_partitioner_R -i ./bc.r.dat -ig graph.dat -n 2

cp ${INPC}/bc.c.dat ./
../bin/gedatsu_bc_partitioner_C -i ./bc.c.dat -ig graph.dat -n 2

cp ${INPC}/node.beam.dat ./
cp ${INPC}/elem.beam.dat ./
../bin/gedatsu_simple_mesh_partitioner -in ./node.beam.dat -ie ./elem.beam.dat -n 2

mv parted.0 parted.0.c

./gedatsu_test | tee test_list.dat

#rm -r parted.0.f

#rm -r parted.0.c

rm graph.dat

rm connectivity.dat

rm val.*

rm bc.*

rm node.*

rm elem.*
