#!/bin/ksh
#
HOMEDIR=`pwd`
#
# Name of the grids in the initial files of each grid 
srcgridini=bt42
tgtgridini=lmdz
# Name of the files containing each grid
srcgridfile=grid_model1.nc
tgtgridfile=grid_model2.nc
# Name of the grids in the final files grids_und.nc and masks_und.nc (4 characters)
srcgridend=bt42
tgtgridend=lmde
#
# WARNING : the coordinates must be double and the masks must be integer 
#
RUNDIR=$HOMEDIR/wkdir_${srcgridend}_${tgtgridend}
DATAGRIDS=$HOMEDIR/data
#
[ -d $RUNDIR ] || mkdir $RUNDIR
cd $RUNDIR
#
# Copy grid and mask files for each grid in $RUNDIR
ln -sf $DATAGRIDS/${srcgridfile} $RUNDIR/${srcgridfile}
ln -sf $DATAGRIDS/${tgtgridfile} $RUNDIR/${tgtgridfile}
#
# In case variables are defined with "." and not with "_"
ncrename -v ${srcgridini}.lon,${srcgridini}_lon $srcgridfile
ncrename -v ${srcgridini}.lat,${srcgridini}_lat $srcgridfile
ncrename -v ${srcgridini}.clo,${srcgridini}_clo $srcgridfile
ncrename -v ${srcgridini}.cla,${srcgridini}_cla $srcgridfile
ncrename -v ${srcgridini}.msk,${srcgridini}_msk $srcgridfile
#
ncrename -v ${tgtgridini}.lon,${tgtgridini}_lon $tgtgridfile
ncrename -v ${tgtgridini}.lat,${tgtgridini}_lat $tgtgridfile 
ncrename -v ${tgtgridini}.clo,${tgtgridini}_clo $tgtgridfile
ncrename -v ${tgtgridini}.cla,${tgtgridini}_cla $tgtgridfile
ncrename -v ${tgtgridini}.msk,${tgtgridini}_msk $tgtgridfile
#
# Construct grids_und.nc and masks_und.nc files (with _) 
echo "Running create_aux_files.ncl"
cp $HOMEDIR/create_aux_files.ncl $RUNDIR/create_aux_files.ncl
ncl 'srcgridfile="'${srcgridfile}'"' 'tgtgridfile="'${tgtgridfile}'"' 'srcgridini="'${srcgridini}'"' 'tgtgridini="'${tgtgridini}'"' 'srcgridend="'${srcgridend}'"' 'tgtgridend="'${tgtgridend}'"' create_aux_files.ncl
#
# Change names of variable and files for OASIS3-MCT
cp grids_und.nc grids.nc
cp masks_und.nc masks.nc

ncrename -v ${srcgridend}_lon,${srcgridend}.lon grids.nc
ncrename -v ${srcgridend}_lat,${srcgridend}.lat grids.nc
ncrename -v ${srcgridend}_clo,${srcgridend}.clo grids.nc
ncrename -v ${srcgridend}_cla,${srcgridend}.cla grids.nc
ncrename -v ${srcgridend}_msk,${srcgridend}.msk masks.nc
#
ncrename -v ${tgtgridend}_lon,${tgtgridend}.lon grids.nc
ncrename -v ${tgtgridend}_lat,${tgtgridend}.lat grids.nc 
ncrename -v ${tgtgridend}_clo,${tgtgridend}.clo grids.nc
ncrename -v ${tgtgridend}_cla,${tgtgridend}.cla grids.nc
ncrename -v ${tgtgridend}_msk,${tgtgridend}.msk masks.nc
