program main
use easifem
implicit none

type(gpf):: gplot
integer, parameter:: n=50
real(dfp):: x(n)
real(dfp):: ys(n)
real(dfp):: yc(n)
real(dfp):: ysc(n)
! Input data
x=linspace(-2.d0*pi,2.d0*pi,n)  !linspace is a utility function from module Utils
ys=  sin(x)
yc=  exp(-0.1d0*x)*cos(x)
ysc= sin(x/2.d0)*cos(2.d0*x)

! Annotation, set title, xlabel, ylabel
call gplot%title('Example 5. A sample with customized line style')
call gplot%xlabel('x, rad')
call gplot%ylabel('y, dimensionless')

! Plot to draw three set of data
call gplot%plot( &
    x,ys, 'title "sin" with lines lt 5 lc rgb "#0008B0"', '', &
    x,yc, 'title "cos" with points lt 6 lc rgb "#FF1100"', '', &
    x,ysc,'title "sin(x/2)cos(2x)" with lp lt 7 lc rgb "#00AA04"' )
end program main