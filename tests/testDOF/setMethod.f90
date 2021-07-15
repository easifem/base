! This example shows how to set values in vector by using DOF
!
program main
  use basetype
  use basemethod
  implicit none

  ! test-1
  ! BLOCK
  !   type( dof_ ) :: obj
  !   real( dfp ), allocatable :: V( : ), Val( : )
  !   integer( i4b ), allocatable :: nptrs( : )

  !   call display( 'test-1' )
  !   call initiate( obj, [10], ['V'], [3], [2], Nodes_FMT )
  !   call initiate( V, obj )

  !   nptrs = [2,3,4]
  !   Val = [1,2,3];
  !   call setValue( V, obj, nptrs, val, [na] )
  !   call display( V, 'V', stdout, .true. )
  ! END BLOCK

  !   ! test-2
  ! BLOCK
  !   type( dof_ ) :: obj
  !   real( dfp ), allocatable :: V( : ), Val( : )
  !   integer( i4b ), allocatable :: nptrs( : )

  !   call display( 'test-1' )
  !   call initiate( obj, [10], ['V'], [3], [2], DOF_FMT )
  !   call display( obj, 'obj' )
  !   call initiate( V, obj )

  !   nptrs = [2,3,4]
  !   Val = [1,2,3];
  !   call setValue( V, obj, nptrs, val, [na] )
  !   call display( V, 'V', stdout, .true. )
  ! END BLOCK

  ! BLOCK
  !   type( dof_ ) :: obj
  !   real( dfp ), allocatable :: V( : ), Val( : )
  !   integer( i4b ), allocatable :: nptrs( : )

  !   call initiate( obj, [10], ['V'], [3], [1] )
  !   call initiate( V, obj )
  !   call display( V, "V" )
  !   nptrs = [2,3,4]
  !   val = [1.0]
  !   call setValue( V, obj, nptrs, val )
  !   write( *, "(10F12.6)" ) V
  ! END BLOCK

  ! BLOCK
  !   type( dof_ ) :: obj
  !   real( dfp ), allocatable :: V( : ), Val( : )
  !   integer( i4b ), allocatable :: nptrs( : )

  !   call initiate( obj, [10], ['V'], [3], [1] )
  !   call initiate( V, obj )
  !   call display( V, "V" )
  !   nptrs = [2,3,4]
  !   val = [1.0]
  !   call setValue( V, obj, nptrs, val, 3 )
  !   write( *, "(10F12.6)" ) V
  ! END BLOCK

  ! !% test-3
  ! BLOCK
  !   type( dof_ ) :: obj
  !   type( RealVector_ ), allocatable :: Val( : )
  !   call initiate( obj, [10], ['V'], [3], [2] )
  !   call initiate( Val, obj )
  !   call display( Val, "Val" )
  ! END BLOCK

  end program main