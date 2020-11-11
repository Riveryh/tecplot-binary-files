FC=gfortran

obj = string.o tecplot.o examples.o

.SUFFIXES: .o .f90

%.o: %.f90
	$(FC) -c $(FFLAGS) $<

all: $(obj)
	$(FC) $^ 

clean:
	rm -rf *.o *.mod a.out *~ *.plt

