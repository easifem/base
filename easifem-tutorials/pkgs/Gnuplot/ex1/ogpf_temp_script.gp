# ogpf libray
# Rev. 0.22 of March 9th, 2018
# Licence: MIT

# gnuplot global setting
set term qt size 640,480 enhanced font "Helvetica,10" title "ogpf libray: Rev. 0.22 of March 9th, 2018"

# ogpf extra configuration
# -------------------------------------------
# color definitions
set style line 1 lc rgb "#800000" lt 1 lw 2
set style line 2 lc rgb "#ff0000" lt 1 lw 2
set style line 3 lc rgb "#ff4500" lt 1 lw 2
set style line 4 lc rgb "#ffa500" lt 1 lw 2
set style line 5 lc rgb "#006400" lt 1 lw 2
set style line 6 lc rgb "#0000ff" lt 1 lw 2
set style line 7 lc rgb "#9400d3" lt 1 lw 2

# Axes
set border linewidth 1.15
set tics nomirror

# grid
# Add light grid to plot
set style line 102 lc rgb "#d6d7d9" lt 0 lw 1
set grid back ls 102

# plot style
set style data linespoints

# -------------------------------------------

 
# plot scale
 
# Annotation: title and labels
set title "Example 1. A simple xy plot" tc "#990011" font "Helvetica,10"
set xlabel "my x axis ..." tc "#99aa33" font "Helvetica,10"
set ylabel "my y axis ..." tc "#99aa33" font "Helvetica,10"
 
# axes setting

plot "-" notitle
  -8.0000000000000000        66.000000000000000     
  -7.0000000000000000        51.000000000000000     
  -6.0000000000000000        38.000000000000000     
  -5.0000000000000000        27.000000000000000     
  -4.0000000000000000        18.000000000000000     
  -3.0000000000000000        11.000000000000000     
  -2.0000000000000000        6.0000000000000000     
  -1.0000000000000000        3.0000000000000000     
   0.0000000000000000        2.0000000000000000     
   1.0000000000000000        3.0000000000000000     
   2.0000000000000000        6.0000000000000000     
   3.0000000000000000        11.000000000000000     
   4.0000000000000000        18.000000000000000     
   5.0000000000000000        27.000000000000000     
   6.0000000000000000        38.000000000000000     
   7.0000000000000000        51.000000000000000     
   8.0000000000000000        66.000000000000000     
e
