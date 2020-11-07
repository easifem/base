program main
  use easifem
  implicit none


!% test-1
BLOCK
  type( dof_ ) :: obj
  real( dfp ), allocatable :: v( : ), ve( : )

  call display( 'test-1' )
  call initiate( obj, [10], ['V'], [3], [2], nodes_FMT )
  call display( obj, "obj" )
  call initiate( v, obj )
  call getArrayValues( ve, v, obj, arange(1,6), nodes_FMT, [2, 4, 6] )
  ve = 1.0; call display( size(ve), 've')
  call setValue( v, obj, [2,4,6], ve, [na] )
  call display( v, 'v', stdout, .true. )
END BLOCK

! !% test-2
! BLOCK
!   type( dof_ ) :: obj
!   real( dfp ), allocatable :: V( : ), Vx( : )
!   call initiate( obj, [10], ['V'], [3], [2] )

!   call initiate( V, obj )

!   call display( V, "V" )

! END BLOCK

end program main