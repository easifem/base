program main
use basetype 
use basemethod
implicit none

! block
! type(realmatrix_) :: Obj
! real(dfp), allocatable :: Val( :, : )
! call initiate( Obj, [4, 4] ); allocate( Val( 4, 4 ) ); call RANDOM_NUMBER( Val )
! call setValues( Obj, Val ); call display( Obj, "Obj" )
! ! call addContribution( Obj, Val, 1.0_DFP, "+")
! ! call display( Obj, "Obj" )
! ! call addContribution( Obj, Val, 1.0_DFP, "-")
! ! call display( Obj, "Obj" )
! call addContribution( Obj, Val, 1.0_DFP, "*")
! call display( Obj, "Obj" )
! end block

! block
!   type(realmatrix_) :: Obj
!   real(dfp), allocatable :: Val( :, : )
!   call initiate( Obj, [4, 4] ); allocate( Val( 4, 4 ) )
!   call RANDOM_NUMBER( Val )
!   call setValues( Obj, Val ); call display( Obj, "Obj" )
!   call addContribution( Obj, 1.0_DFP, 1, 1, 1.0_DFP, "+" ); call display( Obj, "Obj" )
! end block

! block
!   type(realmatrix_) :: Obj
!   real(dfp), allocatable :: Val( :, : )
!   call initiate( Obj, [4, 4] ); allocate( Val( 4, 4 ) ); call RANDOM_NUMBER( Val )
!   call setValues( Obj, Val ); call display( Obj, "Obj" ); Deallocate( Val )
!   Allocate( Val( 2, 2 ) ); Val = 1.0_DFP
!   call setValues( Obj, Val, [3,4], [3,4] ); call display( Obj, "Obj" )
! end block

block
  type(realmatrix_) :: Obj
  real(dfp), allocatable :: Val( :, : )
  call initiate( Obj, [4, 4] ); allocate( Val( 4, 4 ) ); call RANDOM_NUMBER( Val )
  call setValues( Obj, Val ); call display( Obj, "Obj" )
  ! call addContribution( Obj, [1.0_dfp, 1.0_dfp, 1.0_dfp, 1.0_dfp], 0, 0, 1.0_dfp, "+" ); call display( Obj, "Obj" )
  ! call addContribution( Obj, [1.0_dfp, 1.0_dfp, 1.0_dfp], 1, 0, 1.0_dfp, "+" ); call display( Obj, "Obj" )
  ! call addContribution( Obj, [1.0_dfp, 1.0_dfp, 1.0_dfp], -1, 0, 1.0_dfp, "+" ); call display( Obj, "Obj" )
  ! call addContribution( Obj, [1.0_dfp, 1.0_dfp, 1.0_dfp, 1.0_dfp], 2, 1, 1.0_dfp, "+" ); call display( Obj, "Obj" )
  call addContribution( Obj, [1.0_dfp, 1.0_dfp, 1.0_dfp, 1.0_dfp], 2, 2, 1.0_dfp, "+" ); call display( Obj, "Obj" )
end block

end program main