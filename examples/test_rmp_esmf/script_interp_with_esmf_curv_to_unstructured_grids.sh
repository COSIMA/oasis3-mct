#!/bin/ksh
#
HOMEDIR=`pwd`
#
# Name of the grids (4 characters)
srcgrid=lmdz
tgtgrid=bt42
# Remapping (done with ESMF in an NCL program see below)
interp_method=bilinear
#interp_method=patch
#interp_method=neareststod
# DOES NOT WORK YET : interp_method=conserve
#
RUNDIR=$HOMEDIR/wkdir_esmf_${srcgrid}_${tgtgrid}_${interp_method}
DATAGRIDS=$HOMEDIR/data_esmf
REGRID_ESMF_FILE=$HOMEDIR/esmf_regridding_unstructured
[ -d $RUNDIR ] || mkdir $RUNDIR
cd $RUNDIR
#
# Copy grids and masks files 
ln -sf $DATAGRIDS/grids_all.nc $RUNDIR/grids.nc
ln -sf $DATAGRIDS/masks_all.nc $RUNDIR/masks.nc
#
# Remapping using local ESMF ESMF_regridding.ncl with NCL
echo "Running interp_esmf_curv_to_unstructured_one_step.ncl"
cp $HOMEDIR/interp_esmf_curv_to_unstructured_one_step.ncl $RUNDIR/interp_esmf_curv_to_unstructured_one_step.ncl
cp $REGRID_ESMF_FILE/ESMF_regridding.ncl $RUNDIR/ESMF_regridding.ncl
ncl 'interp_method="'${interp_method}'"' 'srcgrid="'${srcgrid}'"' 'tgtgrid="'${tgtgrid}'"' interp_esmf_curv_to_unstructured_one_step.ncl
