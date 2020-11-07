program main
use easifem
implicit none
type(gpf):: matplot
integer, parameter:: n=25, m=6
integer :: i
real(dfp):: tf
real(dfp):: vo
real(dfp):: g
real(dfp):: t(n)
real(dfp):: y(n,m)

!Create data
tf=10.d0
g=32.d0;
t=linspace(0.d0,tf,n)
do i = 1, m
  vo = 25.0d0 * i
  y(:, i) = vo*t-0.5d0*g*t**2
end do

!Draw the matrix y againest vector x
call matplot%title('Example 8. Plotting a Matrix against a vector')
call matplot%xlabel ('t, sec')
call matplot%ylabel ('y, feet')
call matplot%options('set xrange[0:10];set yrange [0:400];')
call matplot%plot(t, y)

!Another Matrix plot with legends and line specification
call matplot%title('Example 8.2: Matrix plot, legends and linespec')
call matplot%plot(t, 2.0d0*y(:,3:4), &
    lspec='t "vo=100" w lp lt 6 ps 3 lw 2;&
    & t "v=125" w lp lt 7 ps 3 lw 2 lc rgb "#ad6000"')
end program main