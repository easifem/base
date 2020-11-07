program main
use easifem
implicit none

type(gpf):: gp
integer, parameter::   n  = 35
real(dfp):: x(n)
real(dfp):: y(n), z(n)
integer :: i

x=linspace(-pi, pi,n)
y = 0.0
z = 0.0
call gp%animation_start(1) ! start animation, set delay is one second between frames
call gp%axis([-pi, pi, -1.2_dfp, 1.2_dfp])
call gp%title('A beautiful animation using ogpf library', textcolor='#aa5500')
! add frames
do i=1, n, 1
  y(i) = sin(x(i))
  z(i) = cos(x(i))
  ! each plot command adds one frame
  call gp%plot(x(1:i),y(1:i), 'w lines lc "red" lw 2','', &
      x(i:i), y(i:i),'w points ps 3 pt 7 lc "red"','', &
      x(1:i),z(1:i), 'w lines lc "blue" lw 2', '',&
      x(i:i), z(i:i), 'w points ps 3 pt 7 lc "blue"' )
end do
! finalize and show frames one by one with delay between them
! as set by animation_start
call gp%animation_show()
end program main