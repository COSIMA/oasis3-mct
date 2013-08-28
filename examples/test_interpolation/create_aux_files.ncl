load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

; ================================================;
begin

dir_in = "/space/coquart/oasis3-mct_buildbot/examples/test_interpolation/data_oasis3/"
dir_out = "/space/coquart/oasis3-mct_buildbot/examples/test_interpolation/"
system("/bin/rm -f grids.nc")   ; remove any pre-existing file
system("/bin/rm -f masks.nc")   ; remove any pre-existing file
system("/bin/rm -f areas.nc")   ; remove any pre-existing file

; == read the data of each model

fi1 = addfile(dir_in+"grid_model1.nc","r")
fi2 = addfile(dir_in+"grid_model2.nc","r")

; == write grids.nc ==

fo1 = addfile(dir_out+"grids.nc","c")
fo1@creation_date=systemfunc("date")
fo1@title="Training OASIS3-MCT CERFACS"

namelatgrd1="torc_lat"
namelongrd1="torc_lon"
namelatgrd2="lmdz_lat"
namelongrd2="lmdz_lon"

fo1->$namelatgrd1$=fi1->lat
fo1->$namelongrd1$=fi1->lon


fo1->$namelatgrd2$=fi2->lat
fo1->$namelongrd2$=fi2->lon


fo2 = addfile(dir_out+"masks.nc","c")
fo2@creation_date=systemfunc("date")
fo2@title="Training OASIS3-MCT CERFACS"

namemaskgrd1="torc_msk"
namemaskgrd2="lmdz_msk"

fo2->$namemaskgrd1$=fi1->imask
fo2->$namemaskgrd2$=fi2->imask

fo3 = addfile(dir_out+"areas.nc","c")
fo3@creation_date=systemfunc("date")
fo3@title="Training OASIS3-MCT CERFACS"


namesrfgrd1="torc_srf"
namesrfgrd2="lmdz_srf"

fo3->$namesrfgrd1$=fi1->srf
fo3->$namesrfgrd2$=fi2->srf

end
