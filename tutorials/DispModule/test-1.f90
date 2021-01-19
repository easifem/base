program main
use easifem
implicit none


call disp( 'scalars' )
call disp( title = 'str=', x = "hello world   ", FMT = 'a', unit = stdout, style = 'left', advance = 'double' )
call disp( title = 'real=', x = 1.0_dfp, unit = stdout, style = 'left', advance = 'double' )


! block
!   real( dfp ) :: vec( 20 )
!   call random_number( vec )


!   call disp( title = 'vector', &
!     & x= vec, &
!     & unit = stdout, &
!     & style = 'underline & pad', &
!     & sep = ' ---> ' )
! end block


! block
!   real( dfp ) :: mat( 10, 10 )

!   INTEGER( I4B ) :: nx, ny

!   nx = 5; ny = 5


!   call random_number( mat )

!   call disp( title = 'matrix', &
!     & x= mat( 5:10, : ), &
!     & unit = stdout, &
!     & style = 'underline & pad', &
!     & sep = ', ', advance = 'double' )

! end block

block
  real( dfp ) :: A( 20 )
  INTEGER( I4B ) :: JA( 20 ), IA( 6 )

  IA = arange( 1, 6 )
  JA = arange( 1, 20 )
  A = arange( 1.0, 20.0 )

  call disp( title = 'IA', &
  & x = IA, &
  & unit = stdout, &
  & style = 'underline', &
  & advance = 'no' )

  call disp( title = 'JA', &
  & x = JA, &
  & unit = stdout, &
  & style = 'underline', &
  & advance = 'no' )

  call disp( title = 'A', &
  & x = A, &
  & unit = stdout, &
  & style = 'underline', &
  & advance = 'double' )
end block

end program main
