set terminal postscript eps color enhanced "Helvetica" 30
set size 1.4,1.8
set output 'oasis_balance.eps'
set boxwidth 0.3
set xtics nomirror rotate by -45 font "Helvetica,40"
set style line 1 lt 1 lc rgb "red"
set style line 2 lt 1 lc rgb "green"
set style fill solid
#set key on outside bottom
#set key box 
unset key
set ylabel 'Elapsed time (s)' font "Helvetica,40"
set title "OASIS coupled model components\n \nCalculation time (green) vs coupling exchange duration\n including time spent to wait slower models (red)" font "Helvetica,30"
plot [0:] [0:] "info.dat" using ($1-.6):2:xtic(4) title "Computations" w boxes fs 2, '' u ($1-.2):3 title "Coupling related time" w boxes fs 1
