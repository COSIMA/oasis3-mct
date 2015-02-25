from XDR import *


###########################
# INITIALIZATION

init()


gridline="******************************************************************"
gridline+="\n"
gridline+="*** Summary of the coupling fields and their transformations: \n"
gridline+="******************************************************************"
gridline+="\n"
gridline+="******************************************************************"
gridline+="\n"
multiple_list= []
mult_fields_name=[]
mult_array={}
grid_array={}
grid_names_list=[]

# Get the possible type of fields (single, multiple1, multiple2 ...)
# depending on nb_mult (by default list initialized to (single,multiple1))
mult_fields_name=getListValue("mult_fields_name")
grid_names_list=getListValue("grid_names_list")
#print("nb_mult",nb_mult)
#print("mult_fields_name",mult_fields_name)

# Construction of the real names of the sent fields and the received
# fields for each multiple and store them in the array mult_array
for mult in mult_fields_name:
    if mult != "single" :
#        print(mult)
        sendnames = []
        recvnames = []
        for fields in getChildrenName("flddef"):
            if getValue("fldmult",fields) == mult:
                send =getValue("fldnamebef",fields)     
                recv =getValue("fldnameaft",fields)     
                field_label = getValue(fields)
                for fields2 in getChildrenName("group"):
                    field_label2 = getValue(fields2,"group")
                    for fields3 in getChildrenName("crs"):
                        field_label3 = getValue(fields3,"crs")
#                        print(field_label3,field_label2) 
                        if field_label3 == field_label2 :
                            statusf= getValue("status",fields3).upper()
# Treat the case where the field is INPUT or OUTPUT :
# source and target symbolic names must be the same
                            if ( (statusf == "OUTPUT") or (statusf == "INPUT") ) :
                                send =getValue("fldnamebef",fields)
                                recv = getValue("fldnamebef",fields)    
                sendnames.append(send)
                recvnames.append(recv)
                mult_array[mult,"sendnames"] = sendnames
                mult_array[mult,"recvnames"] = recvnames
#                print(sendnames)
#                print(recvnames)


# Create a tab with the characteristics of the 
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


# Loop over all the coupling fields to write the transformations
# associated to each field.
# treat the multiple fields only once. 
for fields in getChildrenName("flddef"):
    mult_cat = getValue("fldmult",fields)
#    print("mult_cat",mult_cat)
    if (mult_cat == "single") :
        field_label = getValue(fields)
        for fields3 in getChildrenName("crs"):
             field_label3 = getValue(fields3,"crs")
#             print(field_label3,field_label) 
             if field_label3 == field_label :
                 statusf= getValue("status",fields3).upper()
# Treat the case where the field is INPUT or OUTPUT :
# source and target symbolic names must be the same
                 if ( (statusf == "OUTPUT") or (statusf == "INPUT") ) :
                     gridline+=getValue("fldnamebef",fields)+"-"+getValue("fldnamebef",fields)+":"
                 else :
                     gridline+=getValue("fldnamebef",fields)+"-"+getValue("fldnameaft",fields)+":"
        gridline+="\n" 
    else: 
        if mult_cat in multiple_list:
            print ("Multiple field alreay treated")
            field_label="nothing"
        else :
            multiple_list.append(mult_cat)
            field_label=mult_cat
            gridline+=":".join(mult_array[mult_cat,"sendnames"]) + "-"
            gridline+=":".join(mult_array[mult_cat,"recvnames"]) 
            gridline+="\n"

# Get the grids informations   
    if (field_label != "nothing"):
         src_name=getValue("srcgrid",fields)
         tgt_name=getValue("tgtgrid",fields)
         for fields2 in getChildrenName("crs"):
             field_label2 = getValue(fields2,"crs")
             if field_label == field_label2 :
                  if (getValue("status",fields2) == "input"):
                      gridline+=""
                  if (getValue("status",fields2) == "output"):
                      if grid_array[src_name,"grddims"] == "dims_on":
                          gridline+="Source grid :"
                          gridline+=" "
                          gridline+=src_name
                          gridline+="\n"
                          gridline+="Dimensions :"
                          gridline+=" "
                          gridline+=grid_array[src_name,"ni"]+"x"+grid_array[src_name,"nj"]
                          gridline+="\n"
                          gridline+="Target grid :"
                          gridline+=" "
                          gridline+=src_name
                          gridline+="\n"
                          gridline+="Dimensions :"
                          gridline+=" "
                          gridline+=grid_array[src_name,"ni"]+"x"+grid_array[src_name,"nj"]
                          gridline+="\n"
                  if ((getValue("status",fields2) == "exported" or getValue("status",fields2) == "expout")):
                      if (grid_array[src_name,"grddims"] == "dims_on"):
                          gridline+="Source grid :"
                          gridline+=" "
                          gridline+=src_name
                          gridline+="\n"
                          gridline+="Dimensions :"
                          gridline+=" "
                          gridline+=grid_array[src_name,"ni"]+"x"+grid_array[src_name,"nj"]
                          gridline+="\n"
                          gridline+="Target grid :"
                          gridline+=" "
                          gridline+=tgt_name
                          gridline+="\n"
                          gridline+="Dimensions :"
                          gridline+=" "
                          gridline+=grid_array[tgt_name,"ni"]+"x"+grid_array[tgt_name,"nj"]
                          gridline+="\n"
                      else :
                         gridline+="Source grid :"
                         gridline+=" "
                         gridline+=src_name
                         gridline+="\n"
                         gridline+="Target grid :"
                         gridline+=" "
                         gridline+=tgt_name
                         gridline+="\n"

         for fields2 in getChildrenName("crs"):
             field_label2 = getValue(fields2,"crs")
             if field_label == field_label2 :
                 src_status=grid_array[src_name,"grdstatus"]
                 src_overlap=grid_array[src_name,"overlap"]
                 tgt_status=grid_array[tgt_name,"grdstatus"]
                 tgt_overlap=grid_array[tgt_name,"overlap"]
                 if src_status == "periodical_grid" : 
                     gridline+="Source grid periodical with "
                     gridline+=" "
                     gridline+=src_overlap+" overlapping points"
                     gridline+="\n"
                 elif src_status == "regional_grid" :
                     gridline+="Source grid regional"
                     gridline+="\n"
                 if tgt_status == "periodical_grid" : 
                     gridline+="Target grid periodical with "
                     gridline+=" "
                     gridline+=tgt_overlap+" overlapping points"
                     gridline+=" "
                 elif tgt_status == "regional_grid" :
                     gridline+="Target grid regional"
                     gridline+=" "
                 gridline+="\n"

# Get the informations concerning the coupling period, the restart file and the
# status of the field 
    for fields2 in getChildrenName("crs"):
        field_label2 = getValue(fields2,"crs")
#        print(field_label, field_label2)
        if field_label == field_label2 :
            gridline+="Coupling period="+getValue("cpl",fields2)
            gridline+="\n" 
            gridline+="Restart file: "+getValue("restart",fields2)
            gridline+="\n" 
            gridline+="Field status: "+getValue("status",fields2).upper()
            gridline+="\n" 

# Get the SEG and LAG values for the second line of STRING
# If the field id INPUT or OUTPUT no LAG and no SEG
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
    

# Transformations done on the coupling fields
    for fields100 in getChildrenName("crs"):
        field_label100 = getValue(fields100,"crs")
        if field_label == field_label100 :
             if (getValue("status",fields100) == "exported" or getValue("status",fields100) == "expout"):
                 for fields2 in getChildrenName("time"):
                     field_label2 = getValue(fields2,"time")
                     if field_label == field_label2 :
                         if getValue("loctrans",fields2) == "loctrans_on" :
                             gridline+="Time transformation:"+getValue("loctransdef",fields2).upper()
                             gridline+="\n"

                 for fields2 in getChildrenName("checkinoutminmax"):
                     field_label2 = getValue(fields2,"checkinoutminmax")
                     if field_label == field_label2 :
                         if getValue("checkin",fields2) == "checkin_on" :
                             gridline+="Min and max of the field sent calculated"
                             gridline+="\n"

                 for fields2 in getChildrenName("checkinoutminmax"):
                     field_label2 = getValue(fields2,"checkinoutminmax")
                     if field_label == field_label2 :
                         if getValue("checkout",fields2) == "checkout_on" :
                             gridline+="Min and max of the received field calculated"
                             gridline+="\n"

                 for fields2 in getChildrenName("multaddoldnew"):
                     field_label2 = getValue(fields2,"multaddoldnew")
                     if field_label == field_label2 :
                         if getValue("blasold",fields2) == "blasold_on" :
                             gridline+="Multiply by "+getValue("xmult_blso","blasold_on",fields2)+" the field sent"
                             gridline+="\n"
                             if getValue("xadd_blso","blasold_on",fields2) == "0." :
                                 gridline+=" "
                             else:
                                 gridline+="Add "+getValue("xadd_blso","blasold_on",fields2)+" to the field sent"
                                 gridline+="\n"

                 for fields2 in getChildrenName("multaddoldnew"):
                     field_label2 = getValue(fields2,"multaddoldnew")
                     if field_label == field_label2 :
                         if getValue("blasnew",fields2) == "blasnew_on" :
                             gridline+="Multiply by "+getValue("xmult_blsn","blasnew_on",fields2)+" the received field"
                             gridline+="\n"
                             if getValue("xadd_blsn","blasnew_on",fields2) == "0." :
                                 gridline+=" "
                             else:
                                 gridline+="Add "+getValue("xadd_blsn","blasnew_on",fields2)+" to the received field"
                                 gridline+="\n"


                 for fields2 in getChildrenName("map"):
                     field_label2 = getValue(fields2,"map")
                     if field_label == field_label2 :
                         if getValue("mapremapping",fields2) == "mapping_on" :
                             gridline+="MAPPING is used with options: "+getValue("maploc",fields2)+" and "+getValue("mapstrategy",fields2)
                             gridline+="\n"

                 for fields2 in getChildrenName("interp"):
                     field_label2 = getValue(fields2,"interp")
                     if field_label == field_label2 :
                         if getValue("scrip",fields2) == "scrip_on" :
                             gridline+="SCRIPR is used: "
                             gridline+="\n"
                             if getValue("scriprinterp",fields2) == "distwgt" :
                                 gridline+="DISTWGT interpolation with "+getValue("nv_dist",fields2)+" neighbours"
                                 gridline+="\n"
                             if getValue("scriprinterp",fields2) == "gauswgt" :
                                 gridline+="GAUSWGT interpolation with "+getValue("nv_gaus",fields2)+" neighbours"
                                 gridline+="\n"
                             if getValue("scriprinterp",fields2) == "bilinear" :
                                 gridline+="BILINEAR interpolation"
                                 gridline+="\n"
                             if getValue("scriprinterp",fields2) == "bicubic" :
                                 gridline+="BICUBIC interpolation"
                                 gridline+="\n"
                             if getValue("scriprinterp",fields2) == "conserv_interp" :
                                 gridline+="CONSERVATIVE interpolation of: "
                                 gridline+="\n"
                                 if getValue("order_conserv",fields2) == "first_conserv" :
                                     gridline+= "FIRST ORDER"
                                     gridline+= "\n"
                                 elif getValue("order_conserv",fields2) == "second_conserv" :
                                     gridline+= "SECOND ORDER"
                                     gridline+= "\n"
                                 if getValue("norm_conserv",fields2) == "fracnnei_conserv" :
                                     gridline+= "using FRACNNEI option"
                                     gridline+= " "
                                 elif getValue("norm_conserv",fields2) == "fracarea_conserv" :
                                     gridline+= "FRACAREA"
                                     gridline+= " "
                                 elif getValue("norm_conserv",fields2) == "destarea_conserv" :
                                     gridline+= "DESTAREA"
                                     gridline+= " "


                 for fields2 in getChildrenName("conservation"):
                     field_label2 = getValue(fields2,"conservation")
                     if field_label == field_label2 :
                         if getValue("conservopt",fields2) == "conserv_on" :
                             gridline+="Redistribution after interpolation (CONSERV)"
                             gridline+="\n"
                             gridline+= "with conserv option: "+getValue("conservstrategy",fields2).upper()
                             gridline+="\n"
                             if getValue("conservalgo",fields2) == "bfb_conserv_on" :
                                 gridline+="and bit-for-bit reproductibility option"
                                 gridline+="\n"
                             elif getValue("conservalgo",fields2) == "opt_conserv_on" :
                                 gridline+="and optimize option"
                                 gridline+="\n"
             else : 
                 print("Only LOCTRANS transformation is possible for INPUT and OUTPUT coupling fields") 
                 for fields2 in getChildrenName("time"):
                     field_label2 = getValue(fields2,"time")
                     if field_label == field_label2 :
                          if getValue("loctrans",fields2) == "loctrans_on" :
                              gridline+="Time transformation"
                              gridline+="\n"
                              myvalue=""
                              myvalue+= getValue("loctransdef",fields2)
                              gridline+= myvalue.upper()
                              gridline+=" "
                              gridline+="\n" 

    if (field_label == "nothing" ):
        gridline+=""
    else:
        gridline+="********************************************************"
        gridline+="\n"
        gridline+="********************************************************"
        gridline+="\n"


summaryline=gridline
# Write the informations in a window of the interface
# not at the namcouple format
setValue(summaryline,"summaryscript")

#########################
# FINISHING
finish()

