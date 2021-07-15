program main
use basetype
use basemethod
implicit none

!  block
!    type(realmatrix_) :: obj
!    real(dfp) :: mat( 4, 4 )
!    real( dfp ), allocatable :: val( :, : )
!
!    call random_number( mat )
!    call Initiate(obj, mat )
!    call display( obj, "obj")
!
!    val = ArrayValues( obj, TypeDFP )
!    call display( val, "val" )
!  end block
!
!  block
!    type(realmatrix_) :: obj
!    real(dfp) :: mat( 4, 4 )
!    real( dfp ), allocatable :: val( :, : )
!
!    call random_number( mat )
!    call Initiate(obj, mat )
!    call display( obj, "obj")
!
!    val = ArrayValues( obj, [1,2], [2,3] ,TypeDFP )
!    call display( val, "val" )
!  end block
!
!  block
!    type(realmatrix_) :: obj
!    real(dfp) :: mat( 4, 4 )
!    real( dfp ), allocatable :: val( :, : )
!
!    call random_number( mat )
!    call Initiate(obj, mat )
!    call display( obj, "obj")
!
!    val = ArrayValues( obj, 1,4,2 ,TypeDFP )
!    call display( val, "val" )
!  end block

! block
!   REAL( DFP ), ALLOCATABLE :: Ans( :, : )
!   TYPE( RealMatrix_ ) :: obj( 2, 2 )
!   INTEGER( I4B ):: i, j
! !
!   DO j = 1, SIZE( obj, 2 )
!     DO i = 1, SIZE( obj, 1 )
!       ALLOCATE( Ans( i+2, j+2 ) )
!       CALL Random_Number( Ans )
!       CALL Initiate( obj( i, j ), Ans )
!       DEALLOCATE( Ans )
!     END DO
!   END DO
! !
!   Ans = ArrayValues( obj, TypeDFP )
!   call display( ans, "ans")
! end block

block
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
  TYPE( RealMatrix_ ) :: obj( 2, 2 ), Val
  INTEGER( I4B ):: i, j
!
  DO j = 1, SIZE( obj, 2 )
    DO i = 1, SIZE( obj, 1 )
      ALLOCATE( Ans( i+2, j+2 ) )
      CALL Random_Number( Ans )
      CALL Initiate( obj( i, j ), Ans )
      DEALLOCATE( Ans )
    END DO
  END DO
!
  Val = ArrayValues( obj, TypeRealMatrix )
  call display( val, "val")
end block

end program main