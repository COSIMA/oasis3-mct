from XDR import *

###########################
# INITIALIZATION

init()

##
# 
mult_fields_name = ["single"]
after_group_names_list = []

# Create the multiple fields names 
# in function of the nb_mult value

index = 1

for group_id in getChildrenName("group"):
    group_name = getValue(group_id)
    mult_fields_name.append(group_name)
    if group_name == "no label":
        error("You must give a name to your group or delete it")


#print(mult_fields_name)
setValue(mult_fields_name, "mult_fields_name")

finish()


