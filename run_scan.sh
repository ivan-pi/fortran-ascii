
run_benchmark () {
    FC=$1
    FFLAGS=$2
    make clean
    make benchmark_scan FC="$FC" FFLAGS="$FFLAGS"
    ./benchmark_scan > "out_$FC.txt"
    gnuplot -c plot_scan.gp "out_$FC.txt" "scan_$FC" "$FC $FFLAGS"
}

run_benchmark ifx "-O2 -xcore-avx2 -qopenmp-simd"
run_benchmark ifort "-O2 -xcore-avx2 -qopenmp-simd"
run_benchmark gfortran-13 "-O2 -march=core-avx2 -fopenmp-simd"
run_benchmark nvfortran "-O3 -fast -tp=skylake -mp=multicore"
