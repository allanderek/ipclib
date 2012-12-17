set terminal postscript eps color;set output "simpleTea-pdf.eps"
set title "Passage Time Results"
plot  "curve0.csv" using 1:2 with lines linewidth 1.0 title "pdf"
