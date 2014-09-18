from XDR import *

###########################
# INITIALIZATION

init()

##
# 
model_names_list = []
after_model_names_list = []

# Create the list with the names of the models
# Verify that either all the maximum fortran are given for all the models
# either none fortran units are given
nb_models = 0
nb_units = 0
for model_id in getChildrenName("models"):
    model_name = getValue(model_id)
    model_names_list.append(model_name)
    nb_models += 1
    if model_name == "no label":
        error("You must give a name to your model or delete it")
    if getValue("maxunits",model_id) == "units_on" :
        nb_units += 1

if  nb_models != nb_units and nb_units != 0 :
    error("The maximum fortan units must be given for none or for all the models")


#print model_names_list
setValue(model_names_list, "model_names_list")

finish()


