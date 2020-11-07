!Example 2: A very basic example
program main
use easifem
implicit none
  type(gpf):: gp
  integer, parameter:: n=17
  real(dfp):: x(n)
  real(dfp):: y(n)

  x=dble([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8])
  y=dble([66,51,38,27,18,11,6,3,2,3,6,11,18,27,38,51,66])

  ! Annotation: set title, xlabel, ylabel
  call gp%title( &
    & string='Example 1. A simple xy plot', &
    & textcolor='#990011', &
    & font_size=10, &
    & font_name="Helvetica" )

  call gp%xlabel( &
    & string='my x axis ...',&
    & textcolor='#99aa33',&
    & font_size=10,&
    & font_name="Helvetica")

  call gp%ylabel( &
    & string='my y axis ...',&
    & textcolor='#99aa33',&
    & font_size=10,&
    & font_name="Helvetica")
  call gp%plot(x,y,'with linespoints lt 2 pt 4')
end program main