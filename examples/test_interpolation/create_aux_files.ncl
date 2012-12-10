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
fo1@title="Training November 2012 CERFACS"

fo1->$"torc_lat"$=fi1->lat
fo1->$"torc_lon"$=fi1->lon
fo1->$"torc_cla"$=fi1->cla
fo1->$"torc_clo"$=fi1->clo

fo1->$"lmdz_lat"$=fi2->lat
fo1->$"lmdz_lon"$=fi2->lon
fo1->$"lmdz_cla"$=fi2->cla
fo1->$"lmdz_clo"$=fi2->clo

fo2 = addfile(dir_out+"masks.nc","c")
fo2@creation_date=systemfunc("date")
fo2@title="Training November 2012 CERFACS"

fo2->$"torc_msk"$=fi1->imask

fo2->$"lmdz_msk"$=fi2->imask

fo3 = addfile(dir_out+"areas.nc","c")
fo3@creation_date=systemfunc("date")
fo3@title="Training November 2012 CERFACS"

fo3->$"torc_srf"$=fi1->srf

fo3->$"lmdz_srf"$=fi2->srf

end
