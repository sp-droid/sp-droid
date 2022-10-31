# FORTRAN-77 compiler given below
FC=gfortran

# LINKER GIVEN BELOW
LD=gfortran

# COMPILER FLAGS GIVEN BELOW
FFLAGS= -O3 -fbounds-check

.SUFFIXES: .o .f95 

RM=del

# OBJECTS GIVEN BELOW
OBJS= generador.o\
main.o

Hundirlaflota: $(OBJS)
	$(LD) -o $@  $(OBJS) 

.f95.o:
	$(FC) -c $< $(FFLAGS) -o $@

clean:
	$(RM) $(OBJS) *.mod *.exe