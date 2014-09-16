;--------------------------------------------
;---- Predefined ESMF modules
;--------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

begin

;--------------------------------------------
;--- PATHS
;--------------------------------------------
  RUNDIR="wkdir_esmf_"+srcgrid+"_"+tgtgrid+"_"+interp_method
  PATH1="/home/evian/coquart/ESMF/ESMF_with_NCL/toy_calcul_weights_with_esmf/RUNDIR/"
  PATH2="/home/evian/coquart/ESMF/ESMF_with_NCL/toy_calcul_weights_with_esmf/RUNDIR/" 
  PATH1="./"
  PATH2="./"

;--------------------------------------------
;--- Files used with oasis3-mct for SCRIP
;--------------------------------------------
  gridsfile = PATH1+"grids.nc"    ; File with grids at OASIS3-MCT description
  masksfile = PATH1+"masks.nc"    ; File with masks at OASIS3-MCT description

;--------------------------------------------
;--- Files 
;--------------------------------------------  
  funcini     = PATH2+"fldin.nc"     ; File with the initial field
  funcinterp  = PATH2+"fldou.nc"     ; File with the remapping field, error
  srcGridName = PATH2+"source_grid_esmf.nc"
  tgtGridName = PATH2+"target_grid_esmf.nc" 
  wgtFile     = PATH2+"rmp_"+srcgrid+"to"+tgtgrid+"_" + interp_method + "_withfillValue.nc"
  wgtFile1    = PATH2+"rmp_"+srcgrid+"to"+tgtgrid+"_neareststod.nc"
  wgtFilennei = PATH2+"rmp_"+srcgrid+"to"+tgtgrid+"_" + interp_method + ".nc"
  oa3wgtFile  = PATH2+"rmp_esmf_for_oa3mct_"+srcgrid+"to"+tgtgrid+"_" + interp_method + ".nc"

;--------------------------------------------
;--- Constants
;-------------------------------------------- 
  msk_field_value = -1
  msk_err_value = -10000
  weight_fill_val = -50000
  pi = 3.14159265359
  dp_length = 1.2*pi
  dp_conv   = pi/180.

;--------------------------------------------
;--- Cleaning: remove any pre-existing file
;-------------------------------------------- 
  system("/bin/rm -f "+funcini)
  system("/bin/rm -f "+funcinterp)
  system("/bin/rm -f "+srcGridName)
  system("/bin/rm -f "+tgtGridName)
  system("/bin/rm -f "+wgtFile)  
  system("/bin/rm -f "+wgtFile1)  
  system("/bin/rm -f "+wgtFilennei) 
  system("/bin/rm -f "+oa3wgtFile)
  system("/bin/rm -f PET0.RegridWeightGen.Log")

;--------------------------------------------
;--- Coordinate names
;--------------------------------------------
  srcgrdlon = srcgrid+"_lon"
  srcgrdlat = srcgrid+"_lat"
  srcgrdclo = srcgrid+"_clo"
  srcgrdcla = srcgrid+"_cla"
  srcgrdmsk = srcgrid+"_msk"
  srcgrdsrf = srcgrid+"_srf"
  tgtgrdlon = tgtgrid+"_lon"
  tgtgrdlat = tgtgrid+"_lat"
  tgtgrdclo = tgtgrid+"_clo"
  tgtgrdcla = tgtgrid+"_cla"
  tgtgrdmsk = tgtgrid+"_msk"
  tgtgrdsrf = tgtgrid+"_srf"

;--------------------------------------------
;--- Read grids, masks, areas arrays
;--------------------------------------------
  gfile   = addfile(gridsfile,"r")
  mfile   = addfile(masksfile,"r")
  
  src_lon  = gfile->$srcgrdlon$
  src_lat  = gfile->$srcgrdlat$
  src_msk  = mfile->$srcgrdmsk$
  tgt_lon  = gfile->$tgtgrdlon$
  tgt_lat  = gfile->$tgtgrdlat$
  tgt_msk  = mfile->$tgtgrdmsk$

  nlatsrc=dimsizes(src_lat(:,0))
  nlonsrc=dimsizes(src_lon(0,:))
  nlattgt=dimsizes(tgt_lat(:,0))
  nlontgt=dimsizes(tgt_lon(0,:))

;--------------------------------------------
;--- Calculate the function to regrid
;--------------------------------------------
  func_ana_src = 2. - cos(pi*(acos(cos(src_lat*dp_conv)*cos(src_lon*dp_conv))/dp_length))

;--------------------------------------------
;--- Calculate the function on the destination grid for the error
;--------------------------------------------
  func_ana_tgt = 2. - cos(pi*(acos(cos(tgt_lat*dp_conv)*cos(tgt_lon*dp_conv))/dp_length))

;----------------------------------------------------------------------
;--- Regridding all in one step
;----------------------------------------------------------------------
  Opt             = True
  Opt@SrcGridName = srcGridName 
  Opt@DstGridName = tgtGridName
  Opt@WgtFileName = wgtFile
  Opt@SrcGridLat  = src_lat
  Opt@SrcGridLon  = src_lon
  Opt@DstGridLat  = tgt_lat
  Opt@DstGridLon  = tgt_lon

  if ( interp_method .eq. "conserve" ) then
    lat_name  = "y_" + srcgrid
    lon_name  = "x_" + srcgrid
    size_name = "crn_" + srcgrid
;---Make sure grid is nlat x nlon x 4
    Opt@SrcGridCornerLat  = gfile->$srcgrdcla$($lat_name$|:,$lon_name$|:,$size_name$|:)     
    Opt@SrcGridCornerLon  = gfile->$srcgrdclo$($lat_name$|:,$lon_name$|:,$size_name$|:)
    printMinMax(Opt@SrcGridCornerLat,0)
    printMinMax(Opt@SrcGridCornerLon,0)
  end if	 

  if ( interp_method .eq. "conserve" ) then
    lat_name  = "y_" + tgtgrid
    lon_name  = "x_" + tgtgrid
    size_name = "crn_" + tgtgrid
;---Make sure grid is nlat x nlon x 4
    Opt@DstGridCornerLat  = gfile->$tgtgrdcla$($lat_name$|:,$lon_name$|:,$size_name$|:)
    Opt@DstGridCornerLon  = gfile->$tgtgrdclo$($lat_name$|:,$lon_name$|:,$size_name$|:)
    printMinMax(Opt@DstGridCornerLat,0)
    printMinMax(Opt@DstGridCornerLon,0)
  end if	 

  Opt@DstMask2D      = 1-tgt_msk
  Opt@SrcMask2D      = 1-src_msk

  Opt@InterpMethod   = interp_method
;  Opt@Pole           = "none"
  Opt@ForceOverwrite = True
  Opt@PrintTimings   = True
  Opt@Debug          = True

;---You can choose to skip any of these three steps.
; Opt@SkipSrcGrid   = True    ; Will assume source grid file has been generated
; Opt@SkipDstGrid   = True    ; Will assume target grid file has been generated
; Opt@SkipWgtGen    = True    ; Will assume weights file has been generated
  
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Points that receive no values are at _FillValue=9.96920996838687e+36 (blank points)
  func_regrid = ESMF_regrid(func_ana_src,Opt)
  printVarSummary(func_regrid)
  undef_val=func_regrid@_FillValue
  printVarSummary(undef_val)
  delete (Opt)
  func_regrid_msk_fillvalue  = where((1-tgt_msk) .eq. 1, func_regrid, msk_field_value)
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;--------------------------------------------------------------------------
;   Do the regridding "neareststod" all in one step to fill blank points
;--------------------------------------------------------------------------
  Opt=True
  Opt@SrcGridName    = srcGridName 
  Opt@DstGridName    = tgtGridName
  Opt@WgtFileName    = wgtFile1
  Opt@SrcGridLat     = src_lat
  Opt@SrcGridLon     = src_lon
  Opt@DstGridLat     = tgt_lat
  Opt@DstGridLon     = tgt_lon
  Opt@DstMask2D      = 1-tgt_msk
  Opt@SrcMask2D      = 1-src_msk

  Opt@InterpMethod   = "neareststod"
;  Opt@Pole           = "none"
  Opt@ForceOverwrite = True
  Opt@PrintTimings   = True
  Opt@Debug          = True

;---You can choose to skip any of these three steps.
;  Opt@SkipSrcGrid   = True    ; Will assume source grid file has been generated
;  Opt@SkipDstGrid   = True    ; Will assume target grid file has been generated
;  Opt@SkipWgtGen    = True    ; Will assume weights file has been generated

  func_neareststod = ESMF_regrid(func_ana_src,Opt)
  func_neareststod = where((1-tgt_msk) .eq. 1, func_neareststod, msk_field_value)

;----------------------------------------------------------------------
;--- Combine interp_method + neareststod
;----------------------------------------------------------------------
  wfile          = addfile(wgtFile,"r")  
;
  S_regrid       = wfile->$"S"$
  dst_lat_w      = wfile->$"yc_b"$
  dst_lon_w      = wfile->$"xc_b"$
  src_lat_w      = wfile->$"yc_a"$
  src_lon_w      = wfile->$"xc_a"$
  dst_address    = wfile->$"row"$
  src_address    = wfile->$"col"$
  frac_dst       = wfile->$"frac_b"$
  numlinks      = dimsizes(S_regrid)
  dst_size      = dimsizes(dst_lat_w)
  src_size      = dimsizes(src_lat_w)
  print("dst_size_w")
  print(dst_size)
;
  wfile1            = addfile(wgtFile1,"r")
  S_neareststod     = wfile1->$"S"$
  dst_lat_n         = wfile1->$"yc_b"$
  dst_lon_n         = wfile1->$"xc_b"$
  src_lat_n         = wfile1->$"yc_a"$
  dst_neareststod   = wfile1->$"row"$
  src_neareststod   = wfile1->$"col"$
  links_neareststod = dimsizes(S_neareststod)
  dst_size_n    = dimsizes(dst_lat_n)
  print("dst_size_n")
  print(dst_size_n)
;
  if (interp_method .eq. "neareststod") then
;--- Just write the new weights into the remapping file
     print("Nothing to do")
  else 
      func_oned                = ndtooned(func_regrid_msk_fillvalue)
      dst_missing              = new((/dst_size/),"integer")
      dst_missing@_FillValue   = doubletointeger(undef_val)

      count = -1
      do i=0,dst_size-1
         if (ismissing(func_oned(i))) then
            count = count+1
            dst_missing(count) = i+1
         end if
      end do
      if (count .ge. 0) then
      dst_missing_good    = new((/count+1/),"integer")
      do i=0,count
         dst_missing_good(i) = dst_missing(i)
      end do
      delete(dst_missing)
      print("count")
      print(count)
      print("dst_missing_good")
;      print(dst_missing_good)
;
;--- Replace weights where function = _FillValue by 1. (neareststod)
      dst_address_new    = new((/numlinks+count+1/),"integer")
      src_address_new    = new((/numlinks+count+1/),"integer")
      S_regrid_new       = new((/numlinks+count+1/),double)
      print("New size numlinks+count")
      print(numlinks+count+1)
      dst_address_new(0:numlinks-1)  = dst_address
      src_address_new(0:numlinks-1)  = src_address
      S_regrid_new(0:numlinks-1)     = S_regrid
      do j=0,count
         do i=0,links_neareststod-1
            if ( dst_neareststod(i) .eq. dst_missing_good(j) ) then
                dst_address_new(numlinks+j) = dst_missing_good(j)
                src_address_new(numlinks+j) = src_neareststod(i)
                S_regrid_new(numlinks+j)    = S_neareststod(i)
                break
            end if
         end do
     end do
     delete(dst_missing_good)
     end if

     delete(S_regrid)
     delete(dst_address)
     delete(src_address)

     igood = ind (.not. ismissing(S_regrid_new))
     S_regrid = S_regrid_new(igood)
     src_address = src_address_new(igood)
     dst_address = dst_address_new(igood)

; endif neareststod
     delete(S_regrid_new)
     delete(dst_address_new)
     delete(src_address_new)
     end if

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Recalculate the interpolated function with the new weights with ESMF
; Points that receive no values are at _FillValue=9.96920996838687e+36 (blank points)
  if (interp_method .eq. "neareststod") then
;--- Just write the new weights into the remapping file
     print("Nothing to do")
  else 
  filennei = addfile(wgtFilennei,"c")
  globalAtt               = True
  globalAtt@title         = "ESMF Offline Regridding Weight Generator"
  globalAtt@normalization = "destarea"
  if (interp_method .eq. "conserve") then
     globalAtt@map_method = "Conservative remapping"
     globalAtt@ESMF_regrid_method = "First-order Conservative"
  end if
  if (interp_method .eq. "bilinear") then
     globalAtt@map_method = "Bilinear remapping"
     globalAtt@ESMF_regrid_method = "Bilinear"
  end if
  if (interp_method .eq. "patch") then
     globalAtt@map_method = "Bilinear remapping"
     globalAtt@ESMF_regrid_method = "Higher-order Patch"
  end if
  if (interp_method .eq. "neareststod") then
     globalAtt@map_method = "Bilinear remapping"
     globalAtt@ESMF_regrid_method = "Nearest source to destination"
  end if
  globalAtt@conventions   = "NCAR-CSM"
  globalAtt@domain_a      = "source_grid_file.nc"
  globalAtt@domain_b      = "destination_grid_file.nc"
  globalAtt@grid_file_src = "source_grid_file.nc"
  globalAtt@grid_file_dst = "destination_grid_file.nc"
  globalAtt@CVS_revision  = "6.3.0r"
;
  fileattdef( filennei, globalAtt )
  vars = getfilevarnames (wfile)
  nv=dimsizes(vars)
  print(nv)
  S_nnei=S_regrid
  num_links= dimsizes(S_regrid)
  var_temp=new((/1/),integer)
  var_temp(0)=1
  src_address!0="n_s"
  dst_address!0="n_s"
  S_nnei!0="n_s"
  var_temp!0="num_wgts"
  delete(src_address@_FillValue)
  delete(dst_address@_FillValue)
  delete(S_nnei@_FillValue)
  do i = 0,nv-4
     filennei->$vars(i)$=wfile->$vars(i)$
  end do
  filennei->$"col"$=src_address
  filennei->$"row"$=dst_address
  filennei->$"S"$=S_nnei
  filennei->$"var_temp"$=var_temp

  Opt                = True
  Opt@Debug          = True
  Opt@PrintTimings   = True
  func_regrid_new = ESMF_regrid_with_weights(func_ana_src,wgtFilennei,Opt)
;  func_regrid_new = ESMF_regrid_with_weights(func_ana_src,wgtFile,Opt)
  printVarSummary(func_regrid_new)

  func_regrid_msk_new  = where((1-tgt_msk) .eq. 1, func_regrid_new, msk_field_value)

  delete(var_temp)
  end if
;--------------------------------------------
;--- Calculate the error on the target grid
;--------------------------------------------
  func_ana_tgt       = where((1-tgt_msk) .eq. 1, func_ana_tgt, msk_field_value)
  if ( (interp_method .eq. "conserve") .or. (interp_method .eq. "bilinear") .or. (interp_method .eq. "patch") ) then
       error = where((1-tgt_msk) .eq. 1, abs(((func_ana_tgt - func_regrid_msk_new)/func_regrid_msk_new))*100, msk_err_value)
  else
       error = where((1-tgt_msk) .eq. 1, abs(((func_ana_tgt - func_regrid_msk_fillvalue)/func_regrid_msk_fillvalue))*100, msk_err_value)
  end if
  print("Max of the error (equal -10000 on land)")
  printMinMax(error,True)
  error_min = where(error .eq. msk_err_value, - msk_err_value, error)
  print("Min of the error (equal 10000 on land)")
  printMinMax(error_min,True)

;--------------------------------------------
;--- Output files: to plot ESMF results and create the rmp file for OASIS3-MCT
;--------------------------------------------
  ifile = addfile(funcini,"c")
  ifile->$srcgrdlon$=src_lon
  ifile->$srcgrdlat$=src_lat
  ifile->$"field_in"$=(func_ana_src)

  ofile = addfile(funcinterp,"c")
  ofile->$tgtgrdlon$=tgt_lon
  ofile->$tgtgrdlat$=tgt_lat
  ofile->$"field_ana"$=(func_ana_tgt)
  ofile->$"field_undef"$=(func_regrid)
  ofile->$"field_msk_undef"$=(func_regrid_msk_fillvalue)
  if ( (interp_method .eq. "conserve") .or. (interp_method .eq. "bilinear") .or. (interp_method .eq. "patch") ) then
     ofile->$"field_ou"$=(func_regrid_msk_new)
  end if
  ofile->$"error"$=(error)

;---- Write the rmp file for OASIS3-MCT
; if bilinear interpolation : num_wgts=1
; if conservative interpolation : num_wgts=3
; if nearest neighbour : num_wgts=1

;--- Normalize to obtain Fracarea option for OASIS3-MCT for conservative interpolation
  if (interp_method .eq. "conserve") then
     do i=0,num_links-1
        if (frac_dst(dst_address(i)-1) .ne. 0.0) then
           S_regrid(i)=S_regrid(i)/frac_dst(dst_address(i)-1)
        end if
     end do
  end if

    oa3file = addfile(oa3wgtFile,"c")
    dst_lat_w!0="dst_grid_size"
    src_lat_w!0="src_grid_size"
    src_address!0="num_links"
    dst_address!0="num_links"
    oa3file->$"src_grid_center_lat"$=src_lat_w 
    oa3file->$"dst_grid_center_lat"$=dst_lat_w 
    oa3file->$"src_address"$=src_address
    oa3file->$"dst_address"$=dst_address
    if (interp_method .eq. "conserve") then
       var_temp=S_regrid
       num_links=dimsizes(var_temp)
       num_wgts=3
       var = new ((/num_links,num_wgts/),double)
       var(:,0) = var_temp
       var(:,1) = 0. 
       var(:,2) = 0. 
       var!0="num_links"
       var!1="num_wgts"
       oa3file->$"remap_matrix"$=var
  else
       var_temp=S_regrid
       num_links=dimsizes(var_temp)
       num_wgts=1
       var = new ((/num_links,num_wgts/),double)
       var(:,0) = var_temp 
       var!0="num_links"
       var!1="num_wgts"
       oa3file->$"remap_matrix"$=var
  end if

end
