PROG =	Bispec

SRCS =	numread.for rnumread.for charead.for readhead.for cdchi.for cdg.for number.for sort.for head.for far.for dcholdc.for dcholsl.for dinverse.for dfft.for outar.for taper.for p2chi.for zoots.for port.for statd.for Bispec.for 

OBJS =	numread.o rnumread.o charead.o readhead.o cdchi.o cdg.o number.o sort.o head.o far.o dcholdc.o dcholsl.o dinverse.o dfft.o outar.o taper.o p2chi.o zroots.o port.o  statd.o Bispec.o

F90 = gfortran
F90FLAGS = -g 
LDFLAGS = 

#
# ... Cancel .mod.o rule
#
%.o : %.mod
# 

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(INCS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

install:
	mv -f $(PROG) /home/eduardo/bin

.SUFFIXES: $(SUFFIXES) .for

.for.o:
	$(F90) $(F90FLAGS) -c $<

