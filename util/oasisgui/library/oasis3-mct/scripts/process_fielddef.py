from XDR import *

###########################
# INITIALIZATION

init()

##
# 
# Create the list (names of fields only used by the GUI) of the fields (single or multiple)
# that will be used in the definition of transformations
# for single fields, the label will be the field name entered by the user
# for multiple fields, the label will be the name of the multiple  
field_names_list = []
after_field_names_list = []
multiple_list = []
for field in getChildrenName("flddef"):
    if (getValue("fldmult",field) == "single") :
        after_field_name = getValue("fldnameaft",field)
#        field_name=getValue("fldnamebef",field)+"-"+after_field_name
        field_name=getValue(field)
        #field_names_list.append(field_name)
        field_names_list.append(getValue(field))
        if after_field_name in after_field_names_list:
            error("Name "+after_field_name+" is already in use")
        else:
            after_field_names_list.append(after_field_name)
        if field_name in after_field_names_list:
            error("Name "+field_name+" is already in use")
        else:
            after_field_names_list.append(field_name)
    else : 
        if getValue("fldmult",field) in multiple_list:
            print ("Name of the multiple alreay define in the list")
        else :
            field_name=getValue("fldmult",field)
            field_names_list.append(field_name)
            multiple_list.append(field_name)
    setValue(field_name,"namefldgui",field)


print(field_names_list, "field_names_list")
setValue(field_names_list, "field_names_list")

finish()






