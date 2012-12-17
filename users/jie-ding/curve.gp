set terminal png; set output "number_pdes.png"
plot  "curve0.csv" using 1:2 with lines linewidth 2.0 title "1", "curve1.csv" using 1:2 with lines linewidth 2.0 title "2", "curve2.csv" using 1:2 with lines linewidth 2.0 title "3", "curve3.csv" using 1:2 with lines linewidth 2.0 title "4"
