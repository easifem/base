program main
use easifem
implicit none

type(gpf):: mp
real(dfp):: x(15)
real(dfp):: y(15)

!Options is a dynamic length string and can set all kind of gnuplot
call mp%options('set style line 1 lc rgb "#0060ad" lt 1 lw 2 pt 5 ps 1.5 # --- blue')
call mp%options('set style line 2 lc rgb "#ad6000" lt 2 lw 2 pt 6 ps 1.5 # --- red')
call mp%options('set style line 3 lc rgb "#00ad00" lt 2 lw 2 pt 7 ps 1.5 # --- green')
! this is a multipart string spanned over several lines
call mp%options('&
    &set style data linespoints;&
    &set xrange [0.1:100];&
    &set yrange [0.01:10000];&
    &set autoscale')
call mp%options('set key top left') ! set the key location

x=linspace(0.1d0,100d0,15);
y=x**2;
call mp%title("Example 10. x vs. x^2")
call mp%plot(x1=x, y1=1.50*y, ls1='t "y=1.5x^2" ls 1', &
    x2=x, y2=2.00*y, ls2='t "y=2.0x^2" ls 2', &
    x3=x, y3=2.50*y, ls3='t "y=2.5x^2" ls 3')
call mp%reset()
call mp%title('Reset to initial setting')
call mp%plot(x,2*y)
end program main