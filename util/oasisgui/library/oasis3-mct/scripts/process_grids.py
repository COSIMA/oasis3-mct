from XDR import *

###########################
# INITIALIZATION

init()

##
# 
grid_names_list = []
grid_dims_list = []

# Create the list of grid names 
# Verify that either all the dimensions are given for all grids
# either none dimensions are given
nb_grids = 0
nb_dims = 0
for grid_id in getChildrenName("gridsline"):
    grid_name = getValue(grid_id)
    grid_names_list.append(grid_name)
    nb_grids += 1
    if grid_name == "no label":
        error("You must give a name to your grid or delete it")
    if (len(grid_name) != 4):
        error("Grid name must be 4 characters")
    if getValue("dimsgrid",grid_id) == "dims_on" :
        nb_dims += 1

if nb_grids != nb_dims and nb_dims != 0:
    error("The dimensions of the grids must be given for none or for all the grids")

#print(grid_names_list)
setValue(grid_names_list, "grid_names_list")

finish()


