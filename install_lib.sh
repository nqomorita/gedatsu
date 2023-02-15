#!/bin/bash

git submodule update --init --recursive

BASE_DIR=$(pwd)

#> monolis_utils
cd submodule/monolis_utils
make
#make FLAGS=INTEL
cd ../..
cp submodule/monolis_utils/lib/libmonolis_utils.a ./lib/
cp submodule/monolis_utils/include/*.mod ./include/

#> metis
#cd submodule/METIS
#make config prefix=$BASE_DIR
#make config i64=1 prefix=$BASE_DIR
#make install
#cd ../..
