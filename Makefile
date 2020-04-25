# FC = gfortran-9
# FFLAGS = -Wall -Wextra -pedantic -Wimplicit-interface -fPIC -g -fcheck=all

# FC = ifort
# FFLAGS = -warn all -traceback -std08
# FFLAGS = -warn all -O3
CC=gcc-9
CCFLAGS= -Wall -O3

CPP = g++-9
CPPFLAGS = -Wall -O3

EXECS = test_ascii generate_ascii_table reduce_ascii_table benchmark_f90 benchmark_cpp generate_characters

.PHONY.: all
all: $(EXECS)

test_ascii: test_ascii.o fortran_ascii.o $(SM)
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

generate_ascii_table: generate_ascii_table.o fortran_ascii.o fortran_ascii_pure.o mod_functional.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

reduce_ascii_table: reduce_ascii_table.o mod_functional.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

print_table: print_table.o fortran_ascii.o fortran_ascii_bit.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

benchmark_f90: benchmark_f90.o fortran_ascii.o $(SM)
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

benchmark_cpp: benchmark_cpp.o
	$(CPP) $(CPPFLAGS) -o $@ $^

generate_characters: generate_characters.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

#-------------------------------------------------------------
# This section contains default rule for creating Fortran
# object files.
#-------------------------------------------------------------

%.o: %.c
	$(CC) $(CCFLAGS) -c $<
%.o: %.cpp
	$(CPP) $(CPPFLAGS) -c $<
%.o: %.f
	$(FC) $(FFLAGS) -c $<
%.o: %.f90
	$(FC) $(FFLAGS) -c $<


fortran_ascii_bit.o: fortran_ascii.o
fortran_ascii_cctype.o: fortran_ascii.o cctype.o
fortran_ascii_pure.o: fortran_ascii.o
fortran_ascii_selectcase.o: fortran_ascii.o

test_ascii.o: fortran_ascii.o
generate_ascii_table.o: fortran_ascii.o mod_functional.o
reduce_ascii_table.o: mod_functional.o
benchmark_f90.o: fortran_ascii.o
mod_functional.o: mod_interfaces.o
print_table.o: fortran_ascii.o

#----------------------------------------------
# This section shows how to clean up afterward.
#----------------------------------------------

.PHONY : clean cleanobj cleanmod
clean : cleanobj cleanmod
	rm -f $(EXECS)
cleanobj :
	rm -f *.o
cleanmod :
	rm -f *.mod *__genmod.f90 *.smod