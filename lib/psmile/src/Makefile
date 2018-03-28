
SRCS1	= $(shell ls $(COUPLE)/lib/psmile/src/*.F90)
OBJS0	= $(patsubst $(COUPLE)/lib/psmile/src/%.F90, %.o, $(SRCS1))
OBJS1   = $(OBJS0) GPTLget_memusage.o

VPATH 	= .:$(COUPLE)/lib/psmile/src:\
	     $(COUPLE)/lib/psmile/include:\
	     $(NETCDF_INCLUDE):$(MPI_INCLUDE)

LIBRARY = $(ARCHDIR)/lib/libpsmile.${CHAN}.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1) 
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) 

INCLS = -I$(COUPLE)/lib/psmile/include \
        -I$(ARCHDIR)/build/lib/mct \
        -I$(ARCHDIR)/build/lib/scrip \
        -I$(ARCHDIR)/build/lib/pio

INCLSC = -I$(COUPLE)/lib/psmile/include
.SUFFIXES:
.SUFFIXES: .o .F90 .c

%.o: %.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

GPTLget_memusage.o: GPTLget_memusage.c
	$(CC) $(CCFLAGS) -DHAVE_SLASHPROC -c   $<

mod_psmile_io.o: mod_psmile_io.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

# problem is that mct_mod always has a new timestamp even when it shouldn't
# so just copy the file, but don't create a dependency for now.
# need to add a check whether mct_mod.o has changed and only copy if it has.
#mct_mod.o: $(ARCHDIR)/build/lib/mct/mct_mod.o
#	cp -p $(ARCHDIR)/build/lib/mct/mct_mod.o .
mct_mod.o: 
	cp -p $(ARCHDIR)/build/lib/mct/mct_mod.o .

#
# ALL dependencies ...
#

GPTLget_memusage.o:
mod_oasis_kinds.o:
mod_oasis_parameters.o: mod_oasis_kinds.o
mod_oasis_data.o: mod_oasis_kinds.o 
mod_oasis_sys.o: mod_oasis_kinds.o mod_oasis_data.o
mod_oasis_mem.o: mod_oasis_kinds.o GPTLget_memusage.o mod_oasis_sys.o
mod_oasis_timer.o: mod_oasis_kinds.o mod_oasis_sys.o mod_oasis_data.o
mod_oasis_mpi.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o mod_oasis_timer.o
mod_oasis_string.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o
mod_oasis_namcouple.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_mpi.o mod_oasis_string.o
mod_oasis_part.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_timer.o mod_oasis_mpi.o mct_mod.o
mod_oasis_var.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_timer.o mod_oasis_mpi.o mod_oasis_part.o
mod_oasis_ioshr.o:  mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_mpi.o mod_oasis_string.o mct_mod.o
mod_oasis_io.o:  mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_ioshr.o mod_oasis_mpi.o mct_mod.o
mod_oasis_grid.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_io.o mod_oasis_part.o mod_oasis_mpi.o mod_oasis_timer.o mct_mod.o
mod_oasis_map.o: mod_oasis_kinds.o  mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_mpi.o mod_oasis_var.o mod_oasis_part.o  \
        mod_oasis_string.o mod_oasis_namcouple.o mod_oasis_timer.o mod_oasis_io.o  mct_mod.o
mod_oasis_coupler.o: mod_oasis_kinds.o  mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_map.o mod_oasis_parameters.o mod_oasis_mpi.o mod_oasis_var.o mod_oasis_part.o  \
        mod_oasis_string.o mod_oasis_namcouple.o mod_oasis_timer.o mod_oasis_io.o \
        mod_oasis_mem.o mct_mod.o
mod_oasis_advance.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_var.o mod_oasis_part.o mod_oasis_mpi.o \
        mod_oasis_coupler.o mod_oasis_timer.o mod_oasis_io.o mod_oasis_mem.o mct_mod.o \
        mod_oasis_map.o
mod_oasis_method.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_coupler.o mod_oasis_namcouple.o \
        mod_oasis_timer.o mod_oasis_ioshr.o mod_oasis_advance.o mod_oasis_grid.o \
        mod_oasis_mpi.o mod_oasis_part.o mod_oasis_var.o mod_oasis_mem.o mct_mod.o
mod_oasis_getput_interface.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
        mod_oasis_parameters.o mod_oasis_var.o mod_oasis_advance.o mct_mod.o
mod_oasis_auxiliary_routines.o: mod_oasis_kinds.o mod_oasis_data.o mod_oasis_sys.o \
	mod_oasis_parameters.o mod_oasis_var.o mod_oasis_mpi.o \
	mod_oasis_coupler.o mod_oasis_timer.o mod_oasis_io.o mct_mod.o
mod_prism.o: mod_oasis_kinds.o mod_oasis_part.o mod_oasis_sys.o \
	mod_oasis_getput_interface.o mod_oasis_parameters.o \
	mod_oasis_grid.o mod_oasis_method.o mod_oasis_var.o
mod_oasis.o: mod_oasis_kinds.o mod_oasis_part.o mod_oasis_sys.o \
	mod_oasis_getput_interface.o mod_oasis_parameters.o mod_oasis_auxiliary_routines.o \
	mod_oasis_grid.o mod_oasis_method.o mod_oasis_var.o

