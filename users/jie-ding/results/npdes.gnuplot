set terminal postscript color
set output "npdes.eps"

set key bottom right

plot "npdes1/ICC2009_T.CDF_RESULTS" w l, \
     "npdes2/ICC2009_T.CDF_RESULTS" w l, \
     "npdes3/ICC2009_T.CDF_RESULTS" w l, \
     "npdes4/ICC2009_T.CDF_RESULTS" w l, \
     "npdes5/ICC2009_T.CDF_RESULTS" w l, \
     "npdes6/ICC2009_T.CDF_RESULTS" w l, \
     "npdes7/ICC2009_T.CDF_RESULTS" w l


     