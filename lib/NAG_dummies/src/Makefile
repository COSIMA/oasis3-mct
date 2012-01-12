
SRCS1 =  $(shell ls $(COUPLE)/lib/NAG_dummies/src/*.F90)
OBJS1 =  $(patsubst $(COUPLE)/lib/NAG_dummies/src/%.F90, %.o, $(SRCS1))

VPATH =	.:$(COUPLE)/lib/NAG_dummies/src

LIBRARY	= $(ARCHDIR)/lib/libNAG_dummies.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1)

.SUFFIXES:
.SUFFIXES: .o  .F90

%.o: %.F90
	$(F90) $(F90FLAGS) -c   $<



#
# ALL dependencies ...
#

abort.o: f90_unix_proc.o
flush.o: f90_unix_io.o
