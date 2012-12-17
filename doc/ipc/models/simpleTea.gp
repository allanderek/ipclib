set terminal postscript eps colour dash 20
set output "models/simpleTea_cdf.eps"

set title "The CDF and PDF for the simple tea passage analysis"
set xlabel "Time"
set key bottom right
set ylabel "Probability"

plot "models/simpleTea__cdf.csv" title '' with lines lw 3.0
