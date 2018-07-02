#
include ../../util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys for model 1 (_M1) and for model 2 (_M2)
# type of decomposition :
# DECOMP_APPLE for 1D decomposition
# DECOMP_BOX for 2D decomposition
CPPKEYDECOMP_M1=DECOMP_APPLE
CPPKEYDECOMP_M2=DECOMP_APPLE
#
# Precision for REAL: USE_DOUBLE_PRECISION or NO_USE_DOUBLE_PRECISION
CPPKEYDP=USE_DOUBLE_PRECISION
#
### End User configurable options ###
#
# For single precision, add -DNO_USE_DOUBLE_PRECISION in CPPLOCAL_M1/M2
CPPLOCAL_M1 =  -D${CPPKEYDECOMP_M1} -D${CPPKEYDP}
CPPLOCAL_M2 = -D${CPPKEYDECOMP_M2} -D${CPPKEYDP}
#
OBJ_M1 =  routine_hdlerr.o read_dimgrid.o  \
          read_grid.o decomp_def.o \
          function_sent.o 
OBJ_M2 =  routine_hdlerr.o read_dimgrid.o  \
          read_grid.o decomp_def_m2.o \
          function_sent_m2.o 
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: oasis3_psmile model1 model2 
#
# Compile and link libaries
oasis3_psmile:
	(cd ../../util/make_dir ; $(MAKE) oasis3_psmile -f TopMakefileOasis3)
#
#
#-------------------------------------------------------------------------------
# Rules for executables
#-------------------------------------------------------------------------------
#
model1: $(OBJ_M1) model1.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) model1.o $(LIBPSMILE) $(FLIBS)
model2: $(OBJ_M2) model2.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M2) model2.o $(LIBPSMILE) $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :	routine_hdlerr.F90
				$(F90) $(F90FLAGS) -c routine_hdlerr.F90
read_dimgrid.o :	read_dimgrid.F90
				$(F90) $(F90FLAGS) -c read_dimgrid.F90
read_grid.o :		read_grid.F90
				$(F90) $(F90FLAGS) -c read_grid.F90
decomp_def.o : 		decomp_def.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c decomp_def.F90
function_sent.o :	function_sent.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c function_sent.F90
decomp_def_m2.o : 	decomp_def.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o decomp_def_m2.o -c decomp_def.F90
function_sent_m2.o :	function_sent.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o function_sent_m2.o -c function_sent.F90

model1.o :	model1.F90 Makefile
		$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c model1.F90
model2.o :	model2.F90 Makefile
		$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -c model2.F90
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
	-rm -f *.o *.mod *.MOD model1 model2
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
#
# Make sure to recompile everything
#
realclean: clean remove
remove:
	(cd ../../util/make_dir ; $(MAKE) realclean -f TopMakefileOasis3)
