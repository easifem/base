!Example 3: A very basic example
program main
use easifem
implicit none
type(gpf):: g
integer, parameter:: n = 50
integer, parameter:: m = 65
real(dfp):: x(n)
real(dfp):: y(n)
real(dfp):: xv(m)
real(dfp):: yv(m)

! Input data
x=linspace(-pi,pi,n)  !linspace is a utility function in ogpf module
y=sin(x)              !linspace(a,b,n) create a linear vector in [a,b] with n elements

xv=linspace(0.d0, 2.d0*pi,m)
yv=cos(2.d0*xv)

! Annotation, set title, xlabel, ylabel
call g%title('Example 3. Plot two data series using gnuplot')
call g%xlabel(' x axis ...')
call g%ylabel(' y axis ...')
call g%options('set key top left')

! Sample 1: Plot to draw two set of data
call g%plot(x1=x,y1=y,ls1='title "sin"',axes1='', &
  & x2=xv,y2=yv,ls2='title "cos"')

!Sample 2: Use keyword arguments to plot the same example as above
call g%title('Example 3. Another plot using keyword arguments...')
call g%plot(x1=x,y1=y,ls1='pt 8',x2=xv,y2=yv,ls2='title "cos(2x)"')

! Sample 3: An xy plot with line specification no legends
call g%title('Example 3. Another plot without keyword arguments...')
call g%plot(x,y,'title "sin(x)" with points pt 6 lc "blue"', '',xv,yv,'title "cos(2x)" pt 7 lc rgb "#993300"')
end program main