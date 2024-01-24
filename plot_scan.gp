
gb_per_s(y) = y/1024.0

l2cachesize = 524288 
l3cachesize = 16777216 

set terminal pngcairo enhanced font ",12" size 800,400
set output ARG2.".png"

set arrow from first l2cachesize, graph 0 to first l2cachesize, graph 1 nohead lc rgb 'red' dt 2 lw 1.5
set arrow from first l3cachesize, graph 0 to first l3cachesize, graph 1 nohead lc rgb 'red' dt 2 lw 1.5

set label "L2 Cache" at first l2cachesize, graph 0.05 left offset 1,0 tc rgb "red"
set label "L3 Cache" at first l3cachesize, graph 0.05 left offset 1,0 tc rgb "red"

file = ARG1
print(file)

label(i) = word("scan index findloc custom-1 custom-2 custom-3",i+1)

set colorsequence classic

set title ARG3

set key outside;
set key right top;

set xlabel "N"
set ylabel "GB/s"
set log x
set log y

stream_copy = 37647.1
stream_triad = 28179.0

plot for [i = 0:5] file index i using 1:(gb_per_s($3)) w lp lw 2 pt 7 ps 1 title label(i),\
	gb_per_s(stream_copy) dt 7 lw 3 title "Copy",\
	gb_per_s(stream_triad) dt 7 lw 3 title "Triad"
