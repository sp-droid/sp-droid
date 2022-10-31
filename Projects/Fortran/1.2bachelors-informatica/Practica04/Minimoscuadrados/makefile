# FORTRAN-77 compiler given below
FC=gfortran

# LINKER GIVEN BELOW
LD=gfortran

# COMPILER FLAGS GIVEN BELOW
FFLAGS= -O3 -fbounds-check

# Libraries 
MY_LIBRARY_PATH="..\..\Librerias"
LIBRARIES = \
	$(MY_LIBRARY_PATH)\libalgebra_lineal.a

.SUFFIXES: .o .f95

RM=del

# OBJECTS GIVEN BELOW
OBJS= main.o

execute: $(OBJS)
	$(LD) -o $@  $(OBJS) $(LIBRARIES)

.f95.o:
	$(FC) -c $< $(FFLAGS) -o $@

clean:
	$(RM) $(OBJS) *.mod *.exe