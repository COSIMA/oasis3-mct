from XDR import *


###########################
# INITIALIZATION

init()

nam = WriteAsciiFile("namcouple")

nam.writeLine( """# This is a typical input file for OASIS3-MCT, using NetCDF 
# format for restart input files. OASIS3-MCT reads in this file 
# at run time. Don't hesitate to ask precisions or make 
# suggestions (oasishelp@cerfacs.fr)
#
# Any line beginning with # is ignored. Blank lines are not allowed
####################################################################""")

###########################
# NFIELDS
fieldsline = ""
nb_fields = 0
nam.writeLine( """ $NFIELDS """)

fieldsline += "-to_be_replaced_nfields-"
for fields in getChildrenName("flddef"):
    nb_fields += 1

fieldsline = fieldsline.replace("-to_be_replaced_nfields-",str(nb_fields))

nam.writeLine( fieldsline )
nam.writeLine( """ $END
############################################""")

###########################
# RUNTIME
nam.writeLine( """ $RUNTIME """)
nam.writeLine( getValue("duration","runtime") )
nam.writeLine( """ $END
############################################""")

###########################
# DEBUG
nam.writeLine( """ $NLOGPRT """)

debugline = ""

# Model : debugging options and time statistics
if getValue("nlogprt") == "none" :
    debugline+="0"
if getValue("nlogprt") == "ptr_1" :
    debugline+="1"
if getValue("nlogprt") == "ptr_2" :
    debugline+="2"
if getValue("nlogprt") == "ptr_5" :
    debugline+="5"
if getValue("nlogprt") == "ptr_10" :
    debugline+="10"
if getValue("nlogprt") == "ptr_12" :
    debugline+="12"
if getValue("nlogprt") == "ptr_15" :
    debugline+="15"
if getValue("nlogprt") == "ptr_20" :
    debugline+="20"
if getValue("nlogprt") == "ptr_30" :
    debugline+="30"


debugline+=" "

if getValue("opttime") == "none" :
    debugline+="0"
if getValue("opttime") == "opt_1" :
    debugline+="1"
if getValue("opttime") == "opt_2" :
    debugline+="2"
if getValue("opttime") == "opt_3" :
    debugline+="3"

nam.writeLine(debugline)
nam.writeLine( """ $END
############################################""")


###########################
# STRINGS
nam.writeLine( """ $STRINGS """)

# Everything will be stored in the variable gridline
# the variable will be write a the end

multiple_list= []
mult_fields_name=[]
mult_array={}
grid_array={}
grid_names_list=[]
gridline=""
new_field_names_list=[]
before_field_name=""
after_field_name=""

# Get the list of multiple fields
# Variable mult_fields_name is defined in the 
# script process_multiple.py : contains the 
# values single or multiple 
mult_fields_name=getListValue("mult_fields_name")
grid_names_list=getListValue("grid_names_list")

for mult in mult_fields_name:
    if mult != "single" :
        sendnames = []
        recvnames = []
        for fields in getChildrenName("flddef"):
            if getValue("fldmult",fields) == mult:
                send =getValue("fldnamebef",fields)   
                recv =getValue("fldnameaft",fields)
                field_label = getValue(fields)
                for fields2 in getChildrenName("crs"):
                   field_label2 = getValue(fields2,"crs")
#                   print(field_label2) 
                   if field_label2 == field_label :
                       statusf= getValue("status",fields2).upper()
# Treat the case where the field is INPUT or OUTPUT :
# source and target symbolic names must be the same
                       if ( (statusf == "OUTPUT") or (statusf == "INPUT") ) :
                           send = getValue("fldnamebef",fields)
                           recv = getValue("fldnamebef",fields)    
                sendnames.append(send)
                recvnames.append(recv)
                mult_array[mult,"sendnames"] = sendnames
                mult_array[mult,"recvnames"] = recvnames
#                print(sendnames)
#                print(recvnames)

# Create a dictionnary with the characteristics of the 
# different grids defined in Grids and used in Fields 
for gridid in getChildrenName("gridsline"):
    grid_name=getValue(gridid)
    grid_array[grid_name,"grid_name"] = grid_name 
    grid_array[grid_name,"grdstatus"] = getValue("gridstatus",gridid)
    grid_array[grid_name,"overlap"] = getValue("grid_overlap",gridid)
    grid_array[grid_name,"grddims"] = getValue("dimsgrid",gridid)
    if grid_array[grid_name,"grddims"] == "dims_on":
        grid_array[grid_name,"ni"] = getValue("ni_grid",gridid)
        grid_array[grid_name,"nj"] = getValue("nj_grid",gridid)

# Begin to store the data in gridline :
# Get the field names to write the first line of STRING
for fields in getChildrenName("flddef"):
    mult_cat = getValue("fldmult",fields)
#    print("mult_cat",mult_cat)
    if (mult_cat == "single") :
        field_label = getValue(fields)
        for fields2 in getChildrenName("crs"):
            field_label2 = getValue(fields2,"crs")
            if field_label == field_label2 :
                statusf= getValue("status",fields2).upper()
                if ( (statusf == "OUTPUT") or (statusf == "INPUT") ) :
                    gridline+=getValue("fldnamebef",fields)
                    gridline+=" "
                    gridline+=getValue("fldnamebef",fields)
                    gridline+=" " 
                    gridline+="1"
                    gridline+=" "
                else :
                    gridline+=getValue("fldnamebef",fields)
                    gridline+=" "
                    gridline+=getValue("fldnameaft",fields)
                    gridline+=" " 
                    gridline+="1"
                    gridline+=" "
    else: 
        if mult_cat in multiple_list:
            print ("Multiple field alreay treated")
            field_label="nothing"
        else :
            multiple_list.append(mult_cat)
            field_label=mult_cat
            gridline+=":".join(mult_array[mult_cat,"sendnames"]) 
            gridline+=" "
            gridline+=":".join(mult_array[mult_cat,"recvnames"])
            gridline+=" "
            gridline+="1"
            gridline+=" "

    nb_transfo = 0

# Get the coupling period, the restart file and the status
# to write the first line of STRING
    for fields2 in getChildrenName("crs"):
        field_label2 = getValue(fields2,"crs")
        if field_label == field_label2 :
            gridline += getValue("cpl",fields2)
            gridline+=" " 
            gridline += "-to_be_replaced_ntransfo-"
            gridline+=" " 
            gridline += getValue("restart",fields2)
            gridline+=" " 
            gridline += getValue("status",fields2).upper()
            gridline+="\n" 
    
# Get the grids informations to write the second line of STRING    
# If the field is INPUT or OUTPUT no dimensions must be written 
    if (field_label != "nothing"):
         src_name=getValue("srcgrid",fields)
         tgt_name=getValue("tgtgrid",fields)
         for fields2 in getChildrenName("crs"):
             field_label2 = getValue(fields2,"crs")
             if field_label == field_label2 :
                  if (getValue("status",fields2) == "input"):
                      gridline+=""
                  if (getValue("status",fields2) == "output"):
                       gridline+=src_name
                       gridline+=" "
                       gridline+=src_name
                  if ((getValue("status",fields2) == "exported" or getValue("status",fields2) == "expout")):
                      if (grid_array[src_name,"grddims"] == "dims_on"):
                           gridline+=grid_array[src_name,"ni"]
                           gridline+=" "
                           gridline+=grid_array[src_name,"nj"]
                           gridline+=" "
                           gridline+=grid_array[tgt_name,"ni"]
                           gridline+=" "
                           gridline+=grid_array[tgt_name,"nj"]
                           gridline+=" "
                           gridline+=src_name
                           gridline+=" "
                           gridline+=tgt_name
                      else :
                           gridline+=src_name
                           gridline+=" "
                           gridline+=tgt_name

# Get the SEG and LAG values for the second line of STRING
# If the field id INPUT or OUTPUT no LAG and no SEG
    if (field_label != "nothing"):
         for fields2 in getChildrenName("crs"):
             field_label2 = getValue(fields2,"crs")
             if field_label == field_label2 :
                  if (getValue("status",fields2) == "exported" or getValue("status",fields2) == "expout"):
                      for fields3 in getChildrenName("seqlag","defseqlag"):
                          field_label3 = getValue(fields3,"seqlag")
                          if field_label == field_label3 : 
                              if getValue("seq",fields3) == "seq_on" :
                                   gridline+=" " 
                                   gridline+="SEQ="+getValue("seq_fld",fields3)
                              if getValue("lag",fields3) == "lag_on" :
                                   gridline+=" " 
                                   gridline+="LAG="+getValue("lag_fld",fields3)
                              gridline+="\n"
                  if (getValue("status",fields2) == "input"):
                      gridline+=""
                  if (getValue("status",fields2) == "output") :
                      gridline+="\n"        

# Write the third line of STRING : P or R and overlapping points
    if (field_label != "nothing"):
         for fields2 in getChildrenName("crs"):
             field_label2 = getValue(fields2,"crs")
             if field_label == field_label2 :
                  if (getValue("status",fields2) == "exported" or getValue("status",fields2) == "expout"):
                      src_status=grid_array[src_name,"grdstatus"]
                      src_overlap=grid_array[src_name,"overlap"]
                      tgt_status=grid_array[tgt_name,"grdstatus"]
                      tgt_overlap=grid_array[tgt_name,"overlap"]
                      if src_status == "periodical_grid" : 
                          gridline+="P"
                          gridline+=" "
                          gridline+=src_overlap
                          gridline+=" "
                      elif src_status == "regional_grid" :
                          gridline+="R"
                          gridline+=" "
                          gridline+=src_overlap
                          gridline+=" "
                      if tgt_status == "periodical_grid" : 
                          gridline+="P"
                          gridline+=" "
                          gridline+=tgt_overlap
                          gridline+=" "
                      elif tgt_status == "regional_grid" :
                          gridline+="R"
                          gridline+=" "
                          gridline+=tgt_overlap
                          gridline+=" "
                      gridline+="\n"

# Transformations : write the fourth line of STRING with all the
# transformations that will be applied to the coupling field
# If the field id INPUT or OUTPUT only LOCTRANS or no transfo are possible
    if (field_label != "nothing"):
         for fields100 in getChildrenName("crs"):
             field_label100 = getValue(fields100,"crs")
             if field_label == field_label100 :
                  if (getValue("status",fields100) == "exported" or getValue("status",fields100) == "expout"):
                      for fields2 in getChildrenName("time"):
                          field_label2 = getValue(fields2,"time")
                          if field_label == field_label2 :
                              if getValue("loctrans",fields2) == "loctrans_on" :
                                  nb_transfo += 1
                                  gridline+="LOCTRANS"
                                  gridline+=" "
                      for fields2 in getChildrenName("checkinoutminmax"):
                          field_label2 = getValue(fields2,"checkinoutminmax")
                          if field_label == field_label2 :
                              if getValue("checkin",fields2) == "checkin_on" :
                                  nb_transfo += 1
                                  gridline+="CHECKIN"
                                  gridline+=" "
                      for fields2 in getChildrenName("multaddoldnew"):
                          field_label2 = getValue(fields2,"multaddoldnew")
                          if field_label == field_label2 :
                              if getValue("blasold",fields2) == "blasold_on" :
                                  nb_transfo += 1
                                  gridline+="BLASOLD"
                                  gridline+=" "
                      for fields2 in getChildrenName("interp"):
                          field_label2 = getValue(fields2,"interp")
                          if field_label == field_label2 :
                              if getValue("scrip",fields2) == "scrip_on" :
                                  nb_transfo += 1
                                  gridline+="SCRIPR"
                                  gridline+=" "
                      for fields2 in getChildrenName("map"):
                          field_label2 = getValue(fields2,"map")
                          if field_label == field_label2 :
                              if getValue("mapremapping",fields2) == "mapping_on" :
                                  nb_transfo += 1
                                  gridline+="MAPPING"
                                  gridline+=" "
                      for fields2 in getChildrenName("conservation"):
                          field_label2 = getValue(fields2,"conservation")
                          if field_label == field_label2 :
                              if getValue("conservopt",fields2) == "conserv_on" :
                                  nb_transfo += 1
                                  gridline+="CONSERV"
                                  gridline+=" "
                      for fields2 in getChildrenName("multaddoldnew"):
                          field_label2 = getValue(fields2,"multaddoldnew")
                          if field_label == field_label2 :
                              if getValue("blasnew",fields2) == "blasnew_on" :
                                  nb_transfo += 1
                                  gridline+="BLASNEW"
                                  gridline+=" "
                      for fields2 in getChildrenName("checkinoutminmax"):
                          field_label2 = getValue(fields2,"checkinoutminmax")
                          if field_label == field_label2 :
                              if getValue("checkout",fields2) == "checkout_on" :
                                  nb_transfo += 1
                                  gridline+="CHECKOUT"
                                  gridline+=" "
                              gridline+="\n"

                      if (nb_transfo == 0 and field_label != "nothing"):
                          error("Number of transformations cannot be 0")

                  else : 
                      print("Only LOCTRANS transformation is possible for INPUT and OUTPUT coupling fields") 
                      for fields2 in getChildrenName("time"):
                          field_label2 = getValue(fields2,"time")
                          if field_label == field_label2 :
                              if getValue("loctrans",fields2) == "loctrans_on" :
                                  nb_transfo += 1
                                  gridline+="LOCTRANS"
                                  gridline+="\n"
 
    gridline = gridline.replace("-to_be_replaced_ntransfo-",str(nb_transfo))
#
# Write characteristics of the different transformations  
# on the last lines of STRING for each field 

    if (field_label != "nothing"):
         for fields100 in getChildrenName("crs"):
             field_label100 = getValue(fields100,"crs")
             if field_label == field_label100 :
                  if (getValue("status",fields100) == "exported" or getValue("status",fields100) == "expout"):
# LOCTRANS          
                      for fields2 in getChildrenName("time"):
                          field_label2 = getValue(fields2,"time")
                          if field_label == field_label2 :
                              if getValue("loctrans",fields2) == "loctrans_on" :
                                  myvalue=""
                                  myvalue+= getValue("loctransdef",fields2)
                                  gridline+= myvalue.upper()
                                  gridline+=" "
                                  gridline+="\n"
# CHECKIN
                      for fields2 in getChildrenName("checkinoutminmax"):
                          field_label2 = getValue(fields2,"checkinoutminmax")
                          if field_label == field_label2 :
                              if getValue("checkin",fields2) == "checkin_on" :
                                  gridline+="INT=1"
                                  gridline+="\n"
# BLASOLD
                      for fields2 in getChildrenName("multaddoldnew"):
                          field_label2 = getValue(fields2,"multaddoldnew")
                          if field_label == field_label2 :
                              if getValue("blasold",fields2) == "blasold_on" :
                                  gridline+= getValue("xmult_blso","blasold_on",fields2)
                                  gridline+=" "
                                  if getValue("xadd_blso","blasold_on",fields2) == "0." :
                                      gridline+="0"
                                      gridline+="\n"
                                  else:
                                      gridline+="1"
                                      gridline+="\n"
                                      gridline+= "CONSTANT"+" "+getValue("xadd_blso","blasold_on",fields2)
                                      gridline+=" "
                                      gridline+="\n"

# SCRIPR 
                      for fields2 in getChildrenName("interp"):
                          field_label2 = getValue(fields2,"interp")
                          if field_label == field_label2 :
                              if getValue("scrip",fields2) == "scrip_on" :
                                  if getValue("scriprinterp",fields2) == "distwgt" :
                                      gridline+="DISTWGT"
                                      gridline+=" "
                                      if getValue("cgrs_dist",fields2) == "lr_dist":
                                          gridline+="LR"
                                          gridline+=" "
                                      elif getValue("cgrs_dist",fields2) == "d_dist":
                                          gridline+="D"
                                          gridline+=" "
                                      elif getValue("cgrs_dist",fields2) == "u_dist":
                                          gridline+="U"
                                          gridline+=" "
                                      gridline+="SCALAR"
                                      gridline+=" "
                                      if  getValue("rest_dist",fields2) == "latlon_dist":
                                           gridline+= "LATLON"
                                           gridline+= " "
                                      elif getValue("rest_dist",fields2) == "latitude_dist":
                                           gridline+= "LATITUDE"
                                           gridline+= " "
                                      gridline+= getValue("nbin_dist",fields2)
                                      gridline+=" "
                                      gridline+= getValue("nv_dist",fields2)
                                      gridline+= "\n"
                                  if getValue("scriprinterp",fields2) == "gauswgt" :
                                      gridline+="GAUSWGT"
                                      gridline+=" "
                                      if getValue("cgrs_gaus",fields2) == "lr_gaus":
                                          gridline+="LR"
                                          gridline+=" "
                                      elif getValue("cgrs_gaus",fields2) == "d_gaus":
                                          gridline+="D"
                                          gridline+=" "
                                      elif getValue("cgrs_gaus",fields2) == "u_gaus":
                                          gridline+="U"
                                          gridline+=" "
                                      gridline+="SCALAR"
                                      gridline+=" "
                                      if  getValue("rest_gaus",fields2) == "latlon_gaus":
                                           gridline+= "LATLON"
                                           gridline+= " "
                                      elif getValue("rest_gaus",fields2) == "latitude_gaus":
                                           gridline+= "LATITUDE"
                                           gridline+= " "
                                      gridline+= getValue("nbin_gaus",fields2)
                                      gridline+=" "
                                      gridline+= getValue("nv_gaus",fields2)
                                      gridline+=" "
                                      gridline+= getValue("var_gaus",fields2)
                                      gridline+= "\n"
                                  if getValue("scriprinterp",fields2) == "bilinear" :
                                      gridline+="BILINEAR"
                                      gridline+=" "
                                      if getValue("cgrs_bili",fields2) == "lr_bili":
                                          gridline+="LR"
                                          gridline+=" "
                                      elif getValue("cgrs_bili",fields2) == "d_bili":
                                          gridline+="D"
                                          gridline+=" "
                                      gridline+="SCALAR"
                                      gridline+=" "
                                      if  getValue("rest_bili",fields2) == "latlon_bili":
                                           gridline+= "LATLON"
                                           gridline+= " "
                                      elif getValue("rest_bili",fields2) == "latitude_bili":
                                           gridline+= "LATITUDE"
                                           gridline+= " "
                                      gridline+= getValue("nbin_bili",fields2)
                                      gridline+="\n"
                                  if getValue("scriprinterp",fields2) == "bicubic" :
                                      gridline+="BICUBIC"
                                      gridline+=" "
                                      if getValue("cgrs_bicu",fields2) == "lr_bicu":
                                          gridline+="LR"
                                          gridline+=" "
                                      elif getValue("cgrs_bicu",fields2) == "d_bicu":
                                          gridline+="D"
                                          gridline+=" "
                                      gridline+="SCALAR"
                                      gridline+=" "
                                      if  getValue("rest_bicu",fields2) == "latlon_bicu":
                                           gridline+= "LATLON"
                                           gridline+= " "
                                      elif getValue("rest_bicu",fields2) == "latitude_bicu":
                                           gridline+= "LATITUDE"
                                           gridline+= " "
                                      gridline+= getValue("nbin_bicu",fields2)
                                      gridline+="\n"
                                  if getValue("scriprinterp",fields2) == "conserv_interp" :
                                      gridline+="CONSERV"
                                      gridline+=" "
                                      if getValue("cgrs_conserv",fields2) == "lr_conserv":
                                          gridline+="LR"
                                          gridline+=" "
                                      elif getValue("cgrs_conserv",fields2) == "d_conserv":
                                          gridline+="D"
                                          gridline+=" "
                                      elif getValue("cgrs_conserv",fields2) == "u_conserv":
                                          gridline+="U"
                                          gridline+=" "
                                      gridline+="SCALAR"
                                      gridline+=" "
                                      if  getValue("rest_conserv",fields2) == "latlon_conserv":
                                           gridline+= "LATLON"
                                           gridline+= " "
                                      elif getValue("rest_conserv",fields2) == "latitude_conserv":
                                           gridline+= "LATITUDE"
                                           gridline+= " "
                                      gridline+= getValue("nbin_conserv",fields2)
                                      gridline+=" "
                                      if getValue("norm_conserv",fields2) == "fracnnei_conserv" :
                                          gridline+= "FRACNNEI"
                                          gridline+= " "
                                      elif getValue("norm_conserv",fields2) == "fracarea_conserv" :
                                          gridline+= "FRACAREA"
                                          gridline+= " "
                                      elif getValue("norm_conserv",fields2) == "destarea_conserv" :
                                          gridline+= "DESTAREA"
                                          gridline+= " "
                                      if getValue("order_conserv",fields2) == "first_conserv" :
                                          gridline+= "FIRST"
                                          gridline+= "\n"
                                      elif getValue("order_conserv",fields2) == "second_conserv" :
                                          gridline+= "SECOND"
                                          gridline+= "\n"

# MAPPING
                      for fields2 in getChildrenName("map"):
                          field_label2 = getValue(fields2,"map")
                          if field_label == field_label2 :
                              if getValue("mapremapping",fields2) == "mapping_on" :
                                  gridline+= getValue("filename",fields2)
                                  gridline+=" "
                                  gridline+= getValue("maploc",fields2)
                                  gridline+=" "
                                  gridline+= getValue("mapstrategy",fields2)
                                  gridline+=" "
                                  gridline+="\n"

# CONSERV
                      for fields2 in getChildrenName("conservation"):
                          field_label2 = getValue(fields2,"conservation")
                          if field_label == field_label2 :
                              if getValue("conservopt",fields2) == "conserv_on" :
                                  gridline+= getValue("conservstrategy",fields2).upper()
                                  gridline+=" "
                                  if getValue("conservalgo",fields2) == "bfb_conserv_on" :
                                      gridline+="bfb"
                                      gridline+=" "
                                      gridline+="\n"
                                  elif getValue("conservalgo",fields2) == "opt_conserv_on" :
                                      gridline+="opt"
                                      gridline+=" "
                                      gridline+="\n"

# BLASNEW
                      for fields2 in getChildrenName("multaddoldnew"):
                          field_label2 = getValue(fields2,"multaddoldnew")
                          if field_label == field_label2 :
                              if getValue("blasnew",fields2) == "blasnew_on" :
                                  gridline+= getValue("xmult_blsn","blasnew_on",fields2)
                                  gridline+=" "
                                  if getValue("xadd_blsn","blasnew_on",fields2) == "0." :
                                      gridline+="0"
                                      gridline+="\n"
                                  else:
                                      gridline+="1"
                                      gridline+="\n"
                                      gridline+= "CONSTANT"+" "+getValue("xadd_blsn","blasnew_on",fields2)
                                      gridline+=" "
                                      gridline+="\n"
# CHECKOUT
                      for fields2 in getChildrenName("checkinoutminmax"):
                          field_label2 = getValue(fields2,"checkinoutminmax")
                          if field_label == field_label2 :
                              if getValue("checkout",fields2) == "checkout_on" :
                                  gridline+="INT=1"
                                  gridline+="\n"
#### HERE ELSE FOR INPUT and OUTPUT
                  else :    
# LOCTRANS          
                      for fields2 in getChildrenName("time"):
                          field_label2 = getValue(fields2,"time")
                          if field_label == field_label2 :
                              if getValue("loctrans",fields2) == "loctrans_on" :
                                  myvalue=""
                                  myvalue+= getValue("loctransdef",fields2)
                                  gridline+= myvalue.upper()
                                  gridline+=" "
                                  gridline+="\n"

    if (field_label == "nothing" ):
        gridline+=""
    else:
        gridline+="####"
        gridline+="\n"

gridline += " $END"
nam.writeLine(gridline)
nam.writeLine("############################################")

nam.close()

# Write the namcouple in a window of the interface
totaltime=getValue("duration","runtime")
gridline2=gridline
namcoupleline="$NFIELDS \n"+fieldsline+"\n"+"$END \n"+"####"+"\n"+"$RUNTIME \n"+totaltime+"\n"+"$END \n"+"####"+"\n"+"$NLOGPRT \n"+debugline+"\n"+"$END \n"+"####"+"\n"+"$STRINGS \n"+gridline2
setValue(namcoupleline,"namcouplescript")

#########################
# FINISHING
finish()




