#
# OASIS3-MCT_3.0 directory
O3DIR = $(HOME)/oasis3-mct
#
include $(O3DIR)/util/make_dir/make.inc
LIBPSMILE = $(ARCHDIR)/lib/libpsmile.${CHAN}.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libscrip.a
#
OBJ_M1 =  routine_hdlerr.o read_all_data.o \
          gradient_bicubic.o  gradient_conserv.o
OBJ_M2 =  routine_hdlerr.o read_all_data.o \
          write_all_fields.o
#-------------------------------------------------------------------------------
# General rules
#-------------------------------------------------------------------------------
#
default: all
#
all: model1 model2
#
# Compile and link libaries
#oasis3_psmile:
#	(cd $(O3DIR)/util/make_dir ; $(MAKE) oasis3_psmile -f TopMakefileOasis3)
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
routine_hdlerr.o :		routine_hdlerr.f90
				$(F90) $(F90FLAGS) -c routine_hdlerr.f90
read_all_data.o :		read_all_data.F90
				$(F90) $(F90FLAGS) -c read_all_data.F90
write_all_fields.o :		write_all_fields.F90
				$(F90) $(F90FLAGS) -c write_all_fields.F90
gradient_bicubic.o :		gradient_bicubic.f90
				$(F90) $(F90FLAGS) -c gradient_bicubic.f90
gradient_conserv.o :		gradient_conserv.f90
				$(F90) $(F90FLAGS) -c gradient_conserv.f90

model1.o :	model1.f90 Makefile
		$(F90) $(F90FLAGS) -c model1.f90
model2.o :	model2.f90 Makefile
		$(F90) $(F90FLAGS) -c model2.f90
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
	(cd $(O3DIR)/util/make_dir ; $(MAKE) realclean -f TopMakefileOasis3)
