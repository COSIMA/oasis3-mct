#!/bin/bash

cd util/make_dir
echo "include $(pwd)/make.nci" > make.inc
source config.nci
make -j 4 -f TopMakefileOasis3
