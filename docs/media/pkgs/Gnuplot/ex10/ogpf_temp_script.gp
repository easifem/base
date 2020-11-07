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
set title "Reset to initial setting"
 
# axes setting

plot "-" notitle
  0.10000000000000001        2.0000000000000004E-002
   7.2357142857142858        104.71112244897959     
   14.371428571428572        413.07591836734696     
   21.507142857142860        925.11438775510226     
   28.642857142857146        1640.8265306122453     
   35.778571428571432        2560.2123469387761     
   42.914285714285718        3683.2718367346947     
   50.050000000000004        5010.0050000000010     
   57.185714285714290        6540.4118367346946     
   64.321428571428569        8274.4923469387759     
   71.457142857142856        10212.246530612245     
   78.592857142857142        12353.674387755102     
   85.728571428571428        14698.775918367346     
   92.864285714285714        17247.551122448978     
   100.00000000000000        20000.000000000000     
e
