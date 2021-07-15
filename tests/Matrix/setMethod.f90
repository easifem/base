program main
use basetype
use basemethod
implicit none

! block
! type(realmatrix_) :: obj
! real(dfp), allocatable :: Val( :, : )

! call initiate( obj, [4, 4] )
! allocate( Val( 4, 4 ) )
! call RANDOM_NUMBER( Val )
! call display( val, "val" )

! call setValues( obj, Val )
! call display( obj, "obj" )
! end block

! block
!   type(realmatrix_) :: obj
!   real(dfp), allocatable :: Val( :, : )
!   call initiate( obj, [4, 4] ); allocate( Val( 4, 4 ) )
!   call RANDOM_NUMBER( Val )
!   call setValues( obj, Val ); call display( obj, "obj" )
!   call setValues( obj, 0.0_DFP, 1, 1 );   call display( obj, "obj" )
! end block

! block
!   type(realmatrix_) :: obj
!   real(dfp), allocatable :: Val( :, : )
!   call initiate( obj, [4, 4] ); allocate( Val( 4, 4 ) ); call RANDOM_NUMBER( Val )
!   call setValues( obj, Val ); call display( obj, "obj" ); Deallocate( Val )
!   Allocate( Val( 2, 2 ) ); Val = 0.0_DFP
!   call setValues( obj, Val, [3,4], [3,4] ); call display( obj, "obj" )
! end block

! block
!   type(realmatrix_) :: obj
!   real(dfp), allocatable :: Val( :, : )
!   call initiate( obj, [4, 4] ); allocate( Val( 4, 4 ) ); call RANDOM_NUMBER( Val )
!   call setValues( obj, Val ); call display( obj, "obj" )
!   call setValues( obj, [0.0_dfp, 0.0_dfp, 0.0_dfp, 0.0_dfp], 0, 0 ); call display( obj, "obj" )
!   call setValues( obj, [0.0_dfp, 1.0_dfp, 2.0_dfp], 1, 0 ); call display( obj, "obj" )
!   call setValues( obj, [0.0_dfp, 1.0_dfp, 2.0_dfp], -1, 0 ); call display( obj, "obj" )
!   call setValues( obj, [1.0_dfp, 1.0_dfp, 1.0_dfp, 1.0_dfp], 1, 1 ); call display( obj, "obj" )
!   call setValues( obj, [1.0_dfp, 1.0_dfp, 1.0_dfp, 1.0_dfp], 1, 2 ); call display( obj, "obj" )
! end block

end program main