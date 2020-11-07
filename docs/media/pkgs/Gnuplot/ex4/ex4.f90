!Example 3: A very basic example
program main
use easifem
implicit none

type(gpf):: gp
integer, parameter:: n=50
integer, parameter:: m=65
real(dfp):: x(n)
real(dfp):: y(n)
real(dfp):: xv(m)
real(dfp):: yv(m)
! Input data
x=linspace(-pi,pi,n)  !linspace is a utility function from module ogpf
y=sin(x)

xv=linspace(0.d0, 2.d0*pi,m)
yv=cos(2.d0*xv)
!           This is the maximum number of plot can be drawn at the same time
!           If you have more data see, you can plot can be used with matrices!
call gp%title('Example 4. Plot four data sets using gnuplot')
call gp%options('set key top left; set grid')

call gp%plot(x,y, 'title "sin(x)"', '', &
    xv,yv, 'with lp lt 6 title "cos(2x)"', '', &
    xv, 2.d0*yv, 'title "2cos(2x)" lt 7', '', &
    xv, 0.5d0*yv, 'title "0.5cos(2x)" with points pt 8')

! Another example with keyboard arguments
call gp%plot(x1=x,y1=y,x2=xv,y2=yv)
end program main