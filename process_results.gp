
gb_per_s(y) = y/1024.0

l2cachesize = 262144
l3cachesize = 12582912

set terminal pngcairo enhanced font ",12"
set output "vector.png"

set arrow from first l2cachesize, graph 0 to first l2cachesize, graph 1 nohead lc rgb 'red' dt 2 lw 1.5
set arrow from first l3cachesize, graph 0 to first l3cachesize, graph 1 nohead lc rgb 'red' dt 2 lw 1.5

set label "L2 Cache" at first l2cachesize, graph 0.05 left offset 1,0 tc rgb "red"
set label "L3 Cache" at first l3cachesize, graph 0.05 left offset 1,0 tc rgb "red"


file(i) = sprintf("vector_%d.out",2**(6+i))
label(i) = sprintf("-mprefer-vector-width=%d",2**(6+i))

set title "-O2 -march=native -mprefer-vector-width=<X> -fopenmp-simd"
set xlabel "N"
set ylabel "GB/s"
set log x

plot for [i = 1:3] file(i) using 1:(gb_per_s($3)) w lp lw 2 title label(i)


set output "opt.png"

file(i) = sprintf("opt_%d.out",i)
label(i) = sprintf("-O%d",i)

set title "-O<X> -march=native -fopenmp-simd"

plot for [i = 1:3] file(i) using 1:(gb_per_s($3)) w lp lw 2 title label(i)

set output "vector-ext.png"

next = 8
exts = "sse sse2 sse3 ssse3 sse4 sse4a avx avx2"
file(i) = sprintf("vector-m%s.out",word(exts,i))
label(i) = sprintf("-m%s",word(exts,i))

set title "-O2 -ftree-vectorize -m<Y> -fopenmp-simd"

plot for [i = 1:next] file(i) using 1:(gb_per_s($3)) w lp lw 2 title label(i)

set output "simdlen.png"

file(i) = sprintf("simdlen_%d.out",2**(2 + i))
label(i) = sprintf("simdlen(%4d)",2**(2 + i))

set title "Effect of simdlen clause (set via preprocessor)\n-O3 -march=native -fopenmp-simd; simdlen(<X>)"

plot for [i = 1:6] file(i) using 1:(gb_per_s($3)) w lp lw 2 title label(i)

set output