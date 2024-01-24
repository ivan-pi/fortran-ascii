FC = gfortran
FFLAGS = -Wall -O2 -march=native -fopenmp-simd
# FFLAGS=-Wall -Wextra -pedantic -Wimplicit-interface -fPIC -g -fcheck=all
FLFLAGS =

# FC = ifort
# FFLAGS = -warn all -traceback -std08
# FFLAGS = -warn all -O3

CC = gcc
CFLAGS= -Wall -O2 -march=native

CPP = g++
CPPFLAGS = -Wall -O2 -march=native

SM=pure

EXECS = test_ascii \
		benchmark_f90 \
		benchmark_cpp \
 		benchmark_case \
 		benchmark_scan \
		generate_characters \
		generate_ascii_table \
		generate_ascii_byte_table \
		print_table

stream: stream.c
	$(CC) $(CFLAGS) -o $@ $<

test_ascii: test_ascii.o fortran_ascii.o fortran_ascii_$(SM).o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

generate_ascii_table: generate_ascii_table.o fortran_ascii.o fortran_ascii_pure.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

generate_ascii_byte_table: generate_ascii_byte_table.o fortran_ascii.o fortran_ascii_pure.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

print_table: print_table.o fortran_ascii.o fortran_ascii_bit.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

benchmark_f90: benchmark_f90.o fortran_ascii.o fortran_ascii_$(SM).o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^

benchmark_cpp: benchmark_cpp.o
	$(CPP) $(CPPFLAGS) -o $@ $^

benchmark_case: benchmark_case.o ascii_simd.o
	$(FC) $(FFLAGS) -o $@ $^
	
benchmark_scan: benchmark_scan.o ascii_simd.o
	$(FC) $(FFLAGS) -o $@ $^

generate_characters: generate_characters.o
	$(FC) $(FFLAGS) -o $@ $(FLFLAGS) $^


#-------------------------------------------------------------
# This section list dependencies between object files
#-------------------------------------------------------------

#
# Submodules and any modules
#
fortran_ascii_bit.o: fortran_ascii.o
fortran_ascii_byte.o: fortran_ascii.o
fortran_ascii_cctype.o: fortran_ascii.o cctype.o
fortran_ascii_pure.o: fortran_ascii.o
fortran_ascii_selectcase.o: fortran_ascii.o

#
# Executables
#
test_ascii.o: fortran_ascii.o
generate_ascii_table.o: fortran_ascii.o
generate_ascii_byte_table.o: fortran_ascii.o
benchmark_f90.o: fortran_ascii.o
benchmark_case.o: ascii_simd.o
benchmark_scan.o: ascii_simd.o
print_table.o: fortran_ascii.o

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
%.o: %.F90
	$(FC) $(FFLAGS) -c $<

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