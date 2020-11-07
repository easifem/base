program main
use easifem
implicit none
type(gpf):: gplot
integer, parameter:: n=125
real(dfp):: x(n)
real(dfp):: y(n)

! Input data
x=linspace(0.d0,pi*2.d0,n)  !linspace is a utility function from module Utils
y=sin(6.d0*x)*exp(-x)


! Annotation, set title, xlabel, ylabel
call gplot%title('Example 6. A sample shows f(x) and its zero on the plot')
call gplot%xlabel('x, rad')
call gplot%ylabel('f(x) = sin(6x)exp(-x) dimensionless')
call gplot%options('set grid')

! Plot to draw two set of data, a series and a single point
call gplot%plot(x,y,'title "sin(6x)exp(-x)" with lines lt 2 lw 3', '', &
    & [pi],[0.d0],'title "zero" with points pt 7 ps 3 lc rgb "#FF0000"')
end program main