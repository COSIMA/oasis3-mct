load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

; ================================================;
begin

PATH1 = "./"
PATH2 = "./"

;--------------------------------------------
;--- Source and Target grid Files
;--------------------------------------------
  print("Source grid "+srcgridfile) 
  print("Target grid "+tgtgridfile) 

;--------------------------------------------
;--- Coordinate names
;--------------------------------------------
  srcgrdlonini = srcgridini+"_lon"
  srcgrdlatini = srcgridini+"_lat"
  srcgrdcloini = srcgridini+"_clo"
  srcgrdclaini = srcgridini+"_cla"
  srcgrdmskini = srcgridini+"_msk"
  tgtgrdlonini = tgtgridini+"_lon"
  tgtgrdlatini = tgtgridini+"_lat"
  tgtgrdcloini = tgtgridini+"_clo"
  tgtgrdclaini = tgtgridini+"_cla"
  tgtgrdmskini = tgtgridini+"_msk"
;
  srcgrdlonend = srcgridend+"_lon"
  srcgrdlatend = srcgridend+"_lat"
  srcgrdcloend = srcgridend+"_clo"
  srcgrdclaend = srcgridend+"_cla"
  srcgrdmskend = srcgridend+"_msk"
  tgtgrdlonend = tgtgridend+"_lon"
  tgtgrdlatend = tgtgridend+"_lat"
  tgtgrdcloend = tgtgridend+"_clo"
  tgtgrdclaend = tgtgridend+"_cla"
  tgtgrdmskend = tgtgridend+"_msk"

;--------------------------------------------
;--- Output Files
;--------------------------------------------
  grids  = PATH2+"grids_und.nc"     ; File with the source and target grids
  masks  = PATH2+"masks_und.nc"     ; File with the source and target masks

;--------------------------------------------
;--- Cleaning: remove any pre-existing file
;-------------------------------------------- 
  system("/bin/rm -f "+grids)
  system("/bin/rm -f "+masks)

;------------------------------------------------------------
;--- read the data of each model and write the output files
;------------------------------------------------------------

fi1 = addfile(srcgridfile,"r")
fi2 = addfile(tgtgridfile,"r")

fo1 = addfile(grids,"c")
fo1@creation_date=systemfunc("date")
fo1@title="Training OASIS3-MCT CERFACS"

fo2 = addfile(masks,"c")
fo2@creation_date=systemfunc("date")
fo2@title="Training OASIS3-MCT CERFACS"

fo1->$srcgrdlonend$=fi1->$srcgrdlonini$
fo1->$srcgrdlatend$=fi1->$srcgrdlatini$
fo1->$srcgrdcloend$=fi1->$srcgrdcloini$
fo1->$srcgrdclaend$=fi1->$srcgrdclaini$

fo1->$tgtgrdlonend$=fi2->$tgtgrdlonini$
fo1->$tgtgrdlatend$=fi2->$tgtgrdlatini$
fo1->$tgtgrdcloend$=fi2->$tgtgrdcloini$
fo1->$tgtgrdclaend$=fi2->$tgtgrdclaini$

;--- In the initial files, the mask is 1 for valid points (ocean) and 0 for masked points (land)
;--- For OASIS3-MCT, it is necessary to have 0 for valid points (ocean) and 1 for masked points (land) (old convention)

srcmsk=fi1->$srcgrdmskini$
tgtmsk=fi2->$tgtgrdmskini$

fo2->$srcgrdmskend$=1-srcmsk
fo2->$tgtgrdmskend$=1-tgtmsk

end
