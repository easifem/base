! This example shows how to construct the DOF
!
program main
use easifem
implicit none

! test-1
BLOCK
  type( dof_ ) :: obj

  call display( "test-1" )
  obj = dof( tNodes = [20, 10], Names = ['V', 'P'], SpaceCompo = [3, -1], &
    & Timecompo = [2, 2], StorageFMT = dof_FMT )
  call display( obj, "obj" )
  call display( obj%map, "obj%map")

END BLOCK

! ! test-2
! BLOCK
!   type( dof_ ) :: obj

!   call display( "test 2" )
!   obj = dof( tNodes = [10, 10], Names = ['V', 'U'], SpaceCompo = [3, 2], &
!     & Timecompo = [2, 2], StorageFMT = nodes_FMT )
!   call display( obj, "obj" )
! END BLOCK

! !% test-3
! BLOCK
!   type( dof_ ) :: obj
!   real( dfp ), allocatable :: V( : ), Vx( : )

!   call display( 'test-3' )
!   call initiate( obj, [10], ['V'], [3], [2], nodes_FMT )
!   call initiate( V, obj )
!   call display( V, "V" )
! END BLOCK

! !% test-4
! BLOCK
!   type( dof_ ) :: obj
!   type( RealVector_ ), allocatable :: Val( : )

!   call display( 'test-4' )
!   call initiate( obj, [10], ['V'], [3], [2], nodes_FMT )
!   call initiate( Val, obj )
!   call display( Val, "Val" )
! END BLOCK

end program main