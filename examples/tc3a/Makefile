#
include ../../util/make_dir/make.inc
#LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmpp_io.a $(ARCHDIR)/lib/libpio.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmpp_io.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a
INCPSMILE = -I$(LIBBUILD)/psmile.$(CHAN) -I$(LIBBUILD)/pio  -I$(LIBBUILD)/mct 
#
##### User configurable options #####
#
# CPP keys for model 1 (_M1) and for model 2 (_M2)
# type of decomposition :
# DECOMP_APPLE for 1D decomposition
# DECOMP_BOX for 2D decomposition
CPPKEYDECOMP_M1=DECOMP_APPLE
CPPKEYDECOMP_M2=DECOMP_APPLE
CPPKEYDECOMP_M3=DECOMP_APPLE
#
### End User configurable options ###
#
CPPKEYDP=USE_DOUBLE_PRECISION
CPPKEYOA=OASIS3
CPPLOCAL_M1 = -D${CPPKEYDP} -D${CPPKEYOA} -D${CPPKEYDECOMP_M1}
CPPLOCAL_M2 = -D${CPPKEYDP} -D${CPPKEYOA} -D${CPPKEYDECOMP_M2}
CPPLOCAL_M3 = -D${CPPKEYDP} -D${CPPKEYOA} -D${CPPKEYDECOMP_M3}
#
OBJ_M1 =  routine_hdlerr.o read_dim_irreg.o  \
          read_grid_irreg.o decomp_def.o \
          oasis3_local_grid.o function_sent.o flddiag.o
OBJ_M2 =  routine_hdlerr.o read_dim_reg.o  \
          read_grid_reg.o decomp_def_m2.o \
          oasis3_local_grid_m2.o function_sent_m2.o flddiag.o
OBJ_M3 =  routine_hdlerr.o read_dim_reg.o  \
          read_grid_reg.o decomp_def_m2.o \
          oasis3_local_grid_m2.o function_sent_m2.o flddiag.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: oasis3_psmile model1 model2 model3
#
# Compile and link oasis3 and libaries
oasis3_psmile:
	-rm -f oasis3.$(CHAN).x
	(cd ../../util/make_dir ; $(MAKE) oasis3_psmile -f TopMakefileOasis3)
	ln -sf $(BINDIR)/oasis3.$(CHAN).x oasis3.$(CHAN).x
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
model3: $(OBJ_M3) model3.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M3) model3.o $(LIBPSMILE) $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :		routine_hdlerr.F90
				$(F90) $(F90FLAGS) -c routine_hdlerr.F90
read_dim_irreg.o :	read_dim_irreg.F90
				$(F90) $(F90FLAGS) -c read_dim_irreg.F90
read_grid_irreg.o :	read_grid_irreg.F90
				$(F90) $(F90FLAGS) -c read_grid_irreg.F90
read_dim_reg.o :	read_dim_reg.F90
				$(F90) $(F90FLAGS) -c read_dim_reg.F90
read_grid_reg.o :	read_grid_reg.F90 
				$(F90) $(F90FLAGS) -c read_grid_reg.F90
decomp_def.o : 		decomp_def.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c decomp_def.F90
oasis3_local_grid.o :		oasis3_local_grid.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c oasis3_local_grid.F90
function_sent.o :		function_sent.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M1) -c function_sent.F90
flddiag.o:			flddiag.F90
				$(F90) $(F90FLAGS) $(INCPSMILE) $(CPPLOCAL_M1) -c flddiag.F90
decomp_def_m2.o : 		decomp_def.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o decomp_def_m2.o -c decomp_def.F90
oasis3_local_grid_m2.o :		oasis3_local_grid.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o oasis3_local_grid_m2.o -c oasis3_local_grid.F90
function_sent_m2.o :		function_sent.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL_M2) -o function_sent_m2.o -c function_sent.F90

model1.o :	model1.F90 Makefile
		$(F90) $(F90FLAGS) $(INCPSMILE) $(CPPLOCAL_M1) -c model1.F90
model2.o :	model2.F90 Makefile
		$(F90) $(F90FLAGS) $(INCPSMILE) $(CPPLOCAL_M2) -c model2.F90
model3.o :	model3.F90 Makefile
		$(F90) $(F90FLAGS) $(INCPSMILE) $(CPPLOCAL_M3) -c model3.F90
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
	-rm -f *.o *.x *.mod *.MOD model1 model2 model3
	-rm -f i.*.F90 *.L
	-rm -f core core.* 

