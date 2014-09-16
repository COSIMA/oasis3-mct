#!/bin/ksh
#
grid_file=$1
mask_file=$2
srcgrid=$3
tgtgrid=$4

    ncrename -v ${srcgrid}.lon,${srcgrid}_lon ${grid_file}
    ncrename -v ${srcgrid}.lat,${srcgrid}_lat ${grid_file}
    ncrename -v ${srcgrid}.clo,${srcgrid}_clo ${grid_file}
    ncrename -v ${srcgrid}.cla,${srcgrid}_cla ${grid_file}
    ncrename -v ${srcgrid}.msk,${srcgrid}_msk ${mask_file}
#
    ncrename -v ${tgtgrid}.lon,${tgtgrid}_lon ${grid_file}
    ncrename -v ${tgtgrid}.lat,${tgtgrid}_lat ${grid_file}
    ncrename -v ${tgtgrid}.clo,${tgtgrid}_clo ${grid_file}
    ncrename -v ${tgtgrid}.cla,${tgtgrid}_cla ${grid_file}
    ncrename -v ${tgtgrid}.msk,${tgtgrid}_msk ${mask_file}
