# Compilador Fortran
FC=gfortran

# LINKER
LD=gfortran

# Opciones del Compilador
FFLAGS= -O3 -fbounds-check

# Extensiones de los ficheros
.SUFFIXES: .o .f90 .f95

# Objetos
OBJS= profesor.o grupo12.o auxiliar.o	principal.o

# Borrar archivos
RM=del

# Ejecucion
programa : $(OBJS)
	$(LD) -o $@  $(OBJS)
.f95.o:
	$(FC) -c $< $(FFLAGS) -o $@

# Limpieza
clean:
	$(RM) $(OBJS) *.mod *.exe
