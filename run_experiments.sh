
run_vector_width () {
    echo "EXPERIMENT -mprefer-vector-width=$1"
    make clean
    make benchmark_case FC=gfortran FFLAGS="-O2 -march=native -mprefer-vector-width=$1 -fopenmp-simd"
    ./benchmark_case > "vector_$1.out"
}

if [ $1 = "width" ]; then
run_vector_width 128
run_vector_width 256
run_vector_width 512
fi

run_On () {
    echo "EXPERIMENT -O$1"
    make clean
    make benchmark_case FC=gfortran FFLAGS="-O$1 -march=native -fopenmp-simd"
    ./benchmark_case > "opt_$1.out"
}

if [ $1 = "opt" ]; then
run_On 1
run_On 2
run_On 3
fi

run_extension () {
    echo "EXPERIMENT $1"
    make clean
    make benchmark_case FC=gfortran FFLAGS="-O2 -ftree-vectorize $1 -fopenmp-simd"
    ./benchmark_case > "vector$1.out"
}

if [ $1 = "ext" ]; then
run_extension -msse
run_extension -msse2
run_extension -msse3
run_extension -mssse3
run_extension -msse4
run_extension -msse4a
run_extension -mavx
run_extension -mavx2
fi


run_simdlen () {
    echo "EXPERIMENT simdlen($1)"
    make clean
    make benchmark_case FC=gfortran FFLAGS="-O3 -march=native -fopenmp-simd -DUSE_VLEN=$1"
    ./benchmark_case > "simdlen_$1.out"
}

if [ $1 = "simdlen" ]; then
run_simdlen 8
run_simdlen 16
run_simdlen 32
run_simdlen 64
run_simdlen 128
run_simdlen 256
fi