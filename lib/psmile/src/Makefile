
SRCS1	= $(shell ls $(COUPLE)/lib/psmile/src/*.F90)
OBJS1	= $(patsubst $(COUPLE)/lib/psmile/src/%.F90, %.o, $(SRCS1))

SRCS2 =  $(shell ls $(COUPLE)/lib/psmile/src/*.f90)
OBJS2 =  $(patsubst $(COUPLE)/lib/psmile/src/%.f90, %.o, $(SRCS2))

SRCS3 	= $(shell ls $(COUPLE)/lib/psmile/src/*.F)
OBJS3 	= $(patsubst $(COUPLE)/lib/psmile/src/%.F, %.o, $(SRCS3))

VPATH 	= .:$(COUPLE)/lib/psmile/src:\
	     $(COUPLE)/lib/psmile/include:\
	     $(NETCDF_INCLUDE):$(MPI_INCLUDE)

LIBRARY = $(ARCHDIR)/lib/libpsmile.${CHAN}.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1) $(OBJS2) $(OBJS3) 
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS2) $(OBJS3)

INCLS = -I$(COUPLE)/lib/psmile/include \
        -I$(ARCHDIR)/build/lib/mct \
        -I$(ARCHDIR)/build/lib/pio

INCLSC = -I$(COUPLE)/lib/psmile/include
.SUFFIXES:
.SUFFIXES: .o .f .F .f90 .F90 .c

%.o: %.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

%.o: %.f90
	$(f90) $(f90FLAGS) $(INCLS)  -c   $<

%.o: %.F
	$(F) $(FFLAGS) $(INCLS)  -c   $<

%.o: %.f
	$(f) $(fFLAGS) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

mod_psmile_io.o: mod_psmile_io.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

#
# ALL dependencies ...
#

mod_prism_kinds.o: 
mod_prism_parameters.o: mod_prism_kinds.o
mod_prism_data.o: mod_prism_kinds.o 
mod_prism_sys.o: mod_prism_kinds.o mod_prism_data.o
mod_prism_mpi.o: mod_prism_kinds.o mod_prism_sys.o
mod_prism_timer.o: mod_prism_kinds.o mod_prism_sys.o mod_prism_mpi.o mod_prism_data.o
mod_prism_string.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o
mod_prism_namcoupler.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_mpi.o mod_prism_string.o
mod_prism_var.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o
mod_prism_part.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_timer.o
mod_prism_ioshr.o:  mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_mpi.o
mod_prism_io.o:  mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_ioshr.o mod_prism_mpi.o
mod_prism_grid.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_io.o
mod_prism_coupler.o: mod_prism_kinds.o  mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_mpi.o mod_prism_var.o mod_prism_part.o  \
        mod_prism_string.o mod_prism_namcouple.o mod_prism_timer.o mod_prism_io.o
mod_prism_advance.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_var.o mod_prism_part.o \
        mod_prism_coupler.o mod_prism_timer.o mod_prism_io.o
mod_prism_method.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_coupler.o mod_prism_namcouple.o \
        mod_prism_timer.o mod_prism_ioshr.o mod_prism_advance.o mod_prism_grid.o
mod_prism_getput_interface.o: mod_prism_kinds.o mod_prism_data.o mod_prism_sys.o \
        mod_prism_parameters.o mod_prism_var.o mod_prism_advance.o
mod_prism.o: mod_prism_kinds.o mod_prism_part.o mod_prism_sys.o \
	mod_prism_getput_interface.o mod_prism_parameters.o \
	mod_prism_grid.o mod_prism_method.o mod_prism_var.o
mod_oasis.o: mod_prism_kinds.o mod_prism_part.o mod_prism_sys.o \
	mod_prism_getput_interface.o mod_prism_parameters.o \
	mod_prism_grid.o mod_prism_method.o mod_prism_var.o


