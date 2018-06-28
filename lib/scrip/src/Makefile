
SRCS1 =  $(shell ls $(COUPLE)/lib/scrip/src/*.F90)
OBJS1 =  $(patsubst $(COUPLE)/lib/scrip/src/%.F90, %.o, $(SRCS1))

SRCS3   = $(shell ls $(COUPLE)/lib/scrip/src/*.f90)
OBJS3   = $(patsubst $(COUPLE)/lib/scrip/src/%.f90, %.o, $(SRCS3))

SRCS4 =  $(shell ls $(COUPLE)/lib/scrip/src/*.f)
OBJS4 =  $(patsubst $(COUPLE)/lib/scrip/src/%.f, %.o, $(SRCS4))

VPATH =	.:$(COUPLE)/lib/scrip/src:$(COUPLE)/src

LIBRARY	= $(ARCHDIR)/lib/libscrip.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1)  $(OBJS3) $(OBJS4)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS3) $(OBJS4)

.SUFFIXES:
.SUFFIXES: .o .f .F .f90 .F90 .c

%.o: %.F90
	$(F90) $(F90FLAGS) -c   $<

%.o: %.f90
	$(f90) $(f90FLAGS) -c   $<

%.o: %.F
	$(F) $(FFLAGS) -c   $<

%.o: %.f
	$(f) $(fFLAGS) -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) -c   $<

#
# ALL dependencies ...
#
mod_oasis_flush.o : kinds_mod.o
remap_bicubic_reduced.o: constants.o grids.o remap_vars.o timers.o
netcdf.o: constants.o kinds_mod.o 
remap_conserv.o: constants.o grids.o kinds_mod.o remap_vars.o timers.o
remap_dist_gaus_wgt.o: constants.o grids.o kinds_mod.o remap_vars.o timers.o
remap_vars.o: constants.o grids.o kinds_mod.o
scrip.o: constants.o grids.o iounits.o kinds_mod.o \
	remap_bicubic_reduced.o remap_bi_interp.o \
	remap_conserv.o remap_dist_gaus_wgt.o remap_vars.o \
	remap_write.o timers.o fracnnei.o
constants.o: kinds_mod.o
distance.o: constants.o kinds_mod.o
fracnnei.o: constants.o grids.o kinds_mod.o remap_vars.o 
gradient_bicubic.o: constants.o kinds_mod.o 
grids.o: constants.o iounits.o kinds_mod.o 
iounits.o: mod_oasis_flush.o kinds_mod.o 
kinds_mod.o: 
remap_bi_interp.o: constants.o grids.o kinds_mod.o remap_vars.o timers.o
remap_write.o: constants.o grids.o kinds_mod.o netcdf.o remap_vars.o 
timers.o: kinds_mod.o
