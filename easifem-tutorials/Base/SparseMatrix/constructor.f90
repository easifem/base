program main
use easifem
implicit none

! block
!   type( sparsematrix_ ) :: obj
!   integer( i4b ) :: i, tdof, tnodes
!   tdof = 1; tnodes = 3
!   call initiate( obj, tdof, [tnodes] )
!   do i = 1, tnodes
!     call setsparsity( obj, i, [1,2,3,4] )
!   end do
!   call display( obj, "obj" )
! end block

! block
!   type( sparsematrix_ ) :: obj
!   type( intvector_ ), allocatable :: col( : )
!   integer( i4b ), allocatable :: row( : )
!   integer( i4b ) :: i, tdof, tnodes

!   tdof = 1; tnodes = 3
!   call initiate( obj, tdof, [tnodes] )

!   allocate( col( tnodes ), row( tnodes ) )
!   row = [1,2,3]
!   do i = 1, tnodes
!     col( i ) % val = [1,2,3,4]
!   end do
!   call setsparsity( obj, Row, Col )
!   call display( obj, "obj" )
! end block

block
  type( sparsematrix_ ) :: obj
  type( intvector_ ), allocatable :: col( : )
  integer( i4b ), allocatable :: row( : )
  integer( i4b ) :: i, tdof, tnodes

  tdof = 1; tnodes = 3
  call initiate( obj, tdof, [tnodes] )

  allocate( col( tnodes ), row( tnodes ) )
  row = [1,2,3]
  do i = 1, tnodes
    col( i ) % val = [1,2,3,4]
  end do
  call setsparsity( obj, Row, Col )
  call display( obj, "obj" )
end block

end program main