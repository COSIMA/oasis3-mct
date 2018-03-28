#
include ../../util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
##### User configurable options #####
#
# CPP keys 
CPPKEYDECOMP=DECOMP_APPLE
#CPPKEYDECOMP=DECOMP_BOX
#
CPPLOCAL = -D${CPPKEYDECOMP} 
#
### End User configurable options ###
#
OBJ_M1 =  routine_hdlerr.o read_dimgrid.o flddiag.o \
          read_grid.o decomp_def.o  
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: oasis3_psmile ocnice 
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
ocnice: $(OBJ_M1) ocnice.o $(LIBPSMILE) Makefile
	$(LD) $(LDFLAGS) -o $@ $(OBJ_M1) ocnice.o $(LIBPSMILE) $(FLIBS)
#
#-------------------------------------------------------------------------------
# Rules for compilation
#-------------------------------------------------------------------------------
#
routine_hdlerr.o :		routine_hdlerr.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL) -c routine_hdlerr.F90
read_dimgrid.o :		read_dimgrid.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL) -c read_dimgrid.F90
read_grid.o :			read_grid.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL) -c read_grid.F90
decomp_def.o : 			decomp_def.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL) -c decomp_def.F90
flddiag.o : 			flddiag.F90
				$(F90) $(F90FLAGS) $(CPPLOCAL) -c flddiag.F90


ocnice.o :	ocnice.F90 Makefile
		$(F90) $(F90FLAGS) $(CPPLOCAL) -c ocnice.F90
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
	-rm -f *.o *.mod *.MOD ocnice 
	-rm -f i.*.F90 *.L
	-rm -f core core.* 
#
# Make sure to recompile everything
#
realclean: clean remove
remove:
	(cd $(O3DIR)/util/make_dir ; $(MAKE) realclean -f TopMakefileOasis3)
