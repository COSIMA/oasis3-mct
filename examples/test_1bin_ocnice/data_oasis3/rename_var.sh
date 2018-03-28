#!/bin/ksh
ncrename -v torc_lat,torc.lat grids.nc
ncrename -v torc_lon,torc.lon grids.nc
ncrename -v torc_cla,torc.cla grids.nc
ncrename -v torc_clo,torc.clo grids.nc
ncrename -v lmdz_lat,lmdz.lat grids.nc
ncrename -v lmdz_lon,lmdz.lon grids.nc
ncrename -v lmdz_cla,lmdz.cla grids.nc
ncrename -v lmdz_clo,lmdz.clo grids.nc
ncrename -v lmd2_lat,lmd2.lat grids.nc
ncrename -v lmd2_lon,lmd2.lon grids.nc
ncrename -v lmd2_cla,lmd2.cla grids.nc
ncrename -v lmd2_clo,lmd2.clo grids.nc

ncrename -v torc_msk,torc.msk masks.nc
ncrename -v lmdz_msk,lmdz.msk masks.nc
ncrename -v lmd2_msk,lmd2.msk masks.nc

ncrename -v torc_srf,torc.srf areas.nc
ncrename -v lmdz_srf,lmdz.srf areas.nc
ncrename -v lmd2_srf,lmd2.srf areas.nc
