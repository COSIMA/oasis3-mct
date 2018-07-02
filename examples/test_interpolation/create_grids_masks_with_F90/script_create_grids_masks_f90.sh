#!/bin/ksh
#
HOMEDIR=`pwd`
#
# Name of the grids in the initial files of each grid 
export srcgridini=torc
export tgtgridini=lmdz
# Name of the files containing each grid
export srcgridfile=grid_model1.nc
export tgtgridfile=grid_model2.nc
# Name of the grids in the final files grids_und.nc and masks_und.nc (4 characters)
export srcgridend=tore
export tgtgridend=lmde
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
# In case the variables are defined with "_" and not "."
ncrename -v ${srcgridini}_lon,${srcgridini}.lon $srcgridfile
ncrename -v ${srcgridini}_lat,${srcgridini}.lat $srcgridfile
ncrename -v ${srcgridini}_clo,${srcgridini}.clo $srcgridfile
ncrename -v ${srcgridini}_cla,${srcgridini}.cla $srcgridfile
ncrename -v ${srcgridini}_msk,${srcgridini}.msk $srcgridfile
#
ncrename -v ${tgtgridini}_lon,${tgtgridini}.lon $tgtgridfile
ncrename -v ${tgtgridini}_lat,${tgtgridini}.lat $tgtgridfile 
ncrename -v ${tgtgridini}_clo,${tgtgridini}.clo $tgtgridfile
ncrename -v ${tgtgridini}_cla,${tgtgridini}.cla $tgtgridfile
ncrename -v ${tgtgridini}_msk,${tgtgridini}.msk $tgtgridfile
#
# Construct grids_und.nc and masks_und.nc files (with _) 
echo "Running create_aux_files.ncl"
cp $HOMEDIR/create_aux_files $RUNDIR/create_aux_files
./create_aux_files $srcgridfile $tgtgridfile $srcgridini $tgtgridini $srcgridend $tgtgridend
#
