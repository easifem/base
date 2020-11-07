program main
use easifem
implicit none
type(gpf):: g
integer, parameter:: n=25
real(dfp):: x(n)
real(dfp):: y(n,6)

!Create data
x=linspace(-pi,pi,n)
y(:,1)=sin(x)
y(:,2)=cos(x)
y(:,3)=cos(0.5d0*x)
y(:,4)=sin(0.5d0*x)
y(:,5)=sin(x)*cos(x)
y(:,6)=sin(x)*exp(-x**2)

!Draw the matrix y against vector x
call g%title  ('Example 7. Plotting a Matrix against a vector')
call g%xlabel ('my x axis')
call g%ylabel ('my y axis')
call g%plot   (x, y)
end program main