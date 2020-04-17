# FC = gfortran-9
# FFLAGS = -Wall -Wextra -pedantic -Wimplicit-interface -fPIC -g -fcheck=all


FC = ifort
FFLAGS = -warn all -traceback -std08

#-------------------------------------------------------------
# This section contains default rule for creating Fortran
# object files.
#-------------------------------------------------------------

test_ascii: test_ascii.o fortran_ascii.o fortran_ascii_selectcase.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

%.o: %.f
	$(FC) $(FFLAGS) -c $<
%.o: %.f90
	$(FC) $(FFLAGS) -c $<


fortran_ascii_bit.o: fortran_ascii.o
fortran_ascii_cctype.o: fortran_ascii.o cctype.o
fortran_ascii_pure.o: fortran_ascii.o
fortran_ascii_selectcase.o: fortran_ascii.o

test_ascii.o: fortran_ascii.o

#----------------------------------------------
# This section shows how to clean up afterward.
#----------------------------------------------

.PHONY : clean cleanobj cleanmod
clean : cleanobj cleanmod
	rm -f $(PROGRAMS)
cleanobj :
	rm -f *.o
cleanmod :
	rm -f *.mod *__genmod.f90 *.smod