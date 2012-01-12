
#DEPLIBS=$(ARCHDIR)/lib/libscrip.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libpsmile.$(CHAN).a
#DEPLIBS=$(ARCHDIR)/lib/libpio.a $(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libpsmile.$(CHAN).a
DEPLIBS=$(ARCHDIR)/lib/libmct.a $(ARCHDIR)/lib/libmpeu.a $(ARCHDIR)/lib/libpsmile.$(CHAN).a
LIBS=$(ARCHDIR)/lib/oasis3/liboasis3.$(CHAN).a $(DEPLIBS) $(FLIBS)
#
SRCS1 	= $(shell ls $(COUPLE)/src/*.F90)
OBJS1 	= $(patsubst $(COUPLE)/src/%.F90, %.o, $(SRCS1))
SRCS2 	= $(shell ls $(COUPLE)/src/*.f90)
OBJS2 	= $(patsubst $(COUPLE)/src/%.f90, %.o, $(SRCS2))
SRCS3 	= $(shell ls $(COUPLE)/src/*.F)
OBJS3 	= $(patsubst $(COUPLE)/src/%.F, %.o, $(SRCS3))
SRCS4 	= $(shell ls $(COUPLE)/src/*.f)
OBJS4 	= $(patsubst $(COUPLE)/src/%.f, %.o, $(SRCS4))
SRCS5 	= $(shell ls $(COUPLE)/src/*.c)
OBJS5 	= $(patsubst $(COUPLE)/src/%.c, %.o, $(SRCS5))

VPATH = .:$(COUPLE)/src:\
	  $(NETCDF_INCLUDE):$(MPI_INCLUDE)

LIBRARY	= $(ARCHDIR)/lib/oasis3/liboasis3.${CHAN}.a

PROG =  $(ARCHDIR)/bin/oasis3.${CHAN}.x

INCLS   = -I$(ARCHDIR)/build/lib/psmile.${CHAN} \
          -I$(ARCHDIR)/build/lib/pio \
          -I$(ARCHDIR)/build/lib/mct


INCLSC  = -I$(ARCHDIR)/build/lib/psmile.${CHAN} \
          -I$(ARCHDIR)/build/lib/pio \
          -I$(ARCHDIR)/build/lib/mct


clean:
	rm -f $(LIBRARY)
	rm -f i.* *.o *.mod
	rm -f Top_Makefile_*

all: $(PROG)

lib: $(LIBRARY)

$(PROG): $(DEPLIBS) $(LIBRARY)
	$(F90) $(LDFLAGS) -o $@ couple.o $(LIBS) $(LIBS)

$(LIBRARY): $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4)
	$(AR) $(ARFLAGS) $(LIBRARY) *.o

.SUFFIXES:
.SUFFIXES: .o .c .f .F .f90 .F90

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

#
# All dependencies ...
#

couple.o: 
