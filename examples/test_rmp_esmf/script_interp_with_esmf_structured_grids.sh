#!/bin/ksh
#
# Name of the grids (4 characters)
srcgrid=torc
tgtgrid=lmdz
# Remapping (done with SEMF in an NCL program see below)
interp_method=bilinear
#interp_method=conserve
#interp_method=patch
#interp_method=neareststod
#
HOMEDIR=/home/globc/coquart/oasis3-mct_perso_2.0/examples/test_rmp_esmf
RUNDIR=$HOMEDIR/wkdir_esmf_${srcgrid}_${tgtgrid}_${interp_method}
DATAGRIDS=$HOMEDIR/data_esmf
[ -d $RUNDIR ] || mkdir $RUNDIR
cd $RUNDIR
#
ln -sf $DATAGRIDS/grids_all.nc $RUNDIR/grids.nc
ln -sf $DATAGRIDS/masks_all.nc $RUNDIR/masks.nc
#
# Remapping using ESMF with NCL
cp $HOMEDIR/interp_esmf_structured_one_step.ncl $RUNDIR/interp_esmf_structured_one_step.ncl
ncl 'interp_method="'${interp_method}'"' 'srcgrid="'${srcgrid}'"' 'tgtgrid="'${tgtgrid}'"' interp_esmf_structured_one_step.ncl
