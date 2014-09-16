#!/bin/ksh
#
grid_file=$1
mask_file=$2
srcgrid=$3
tgtgrid=$4

    ncrename -v ${srcgrid}_lon,${srcgrid}.lon ${grid_file}
    ncrename -v ${srcgrid}_lat,${srcgrid}.lat ${grid_file}
    ncrename -v ${srcgrid}_clo,${srcgrid}.clo ${grid_file}
    ncrename -v ${srcgrid}_cla,${srcgrid}.cla ${grid_file}
    ncrename -v ${srcgrid}_msk,${srcgrid}.msk ${mask_file}
#
    ncrename -v ${tgtgrid}_lon,${tgtgrid}.lon ${grid_file}
    ncrename -v ${tgtgrid}_lat,${tgtgrid}.lat ${grid_file}
    ncrename -v ${tgtgrid}_clo,${tgtgrid}.clo ${grid_file}
    ncrename -v ${tgtgrid}_cla,${tgtgrid}.cla ${grid_file}
    ncrename -v ${tgtgrid}_msk,${tgtgrid}.msk ${mask_file}
