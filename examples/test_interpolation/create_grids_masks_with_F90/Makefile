#
##### User configurable options #####
#
F90=pgf90
F90FLAGS=-C -g -Mbounds -byteswapio
LD=pgf90
NETCDF_INC=-I/usr/local/include
NETCDF_LIB=-L/usr/local/lib -lnetcdff -lnetcdf -L/usr/local/HDF5-1.8.6/lib -lhdf5_hl -lhdf5 -lz -lm -lhdf5_hl -lhdf5 -lz -lcurl 
### End User configurable options ###
#
OBJ =  routine_hdlerr.o read_dimgrid.o read_grid_mask.o write_grids_masks.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: create_aux_files
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
create_aux_files: 	$(OBJ) create_aux_files.o Makefile
			$(LD) -o $@ $(OBJ) $(NETCDF_LIB) create_aux_files.o 
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :		routine_hdlerr.F90
				$(F90) $(F90FLAGS) -c routine_hdlerr.F90 $(NETCDF_INC)
read_dimgrid.o :		read_dimgrid.F90
				$(F90) $(F90FLAGS) -c read_dimgrid.F90 $(NETCDF_INC)
read_grid_mask.o :		read_grid_mask.F90
				$(F90) $(F90FLAGS) -c read_grid_mask.F90 $(NETCDF_INC)
write_grids_masks.o :		write_grids_masks.F90
				$(F90) $(F90FLAGS) -c write_grids_masks.F90 $(NETCDF_INC)

create_aux_files.o :	create_aux_files.F90 Makefile
			$(F90) $(F90FLAGS) -c create_aux_files.F90 $(NETCDF_INC)
#
#-------------------------------------------------------------------------------
# Utilities
#-------------------------------------------------------------------------------
#
help:
	more Make.help
#
# Clean directory
#
clean:
	-rm -f *.o *.mod *.MOD create_aux_files
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
#
