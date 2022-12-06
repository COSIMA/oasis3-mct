#!/bin/bash
. /root/spack/share/spack/setup-env.sh
spack load gcc intel-oneapi-compilers openmpi netcdf-fortran
./build.sh
