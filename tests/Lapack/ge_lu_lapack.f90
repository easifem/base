!> authors: Dr. Vikas Sharma
!
! This module test the lu decomposition algorithms
! The general interface for the LU decomposition is given below
!
! ```fortran
! call getLU( A=mat(:,:), LU=lu(:,:), ipiv=ipiv(:), solverName=?)
! ```
!
! The solver name has following values
! - DGETF2
! - DGETRF
! - DGETRF2
!
! There are three tests
! - test01: Uses "GETF2" solver
! - test02: Uses "GETRF" solver
! - test03: Uses "GETRF2" solver

module test
use basetype
use basemethod
implicit none
contains

!----------------------------------------------------------------------------
!                                                                   test01
!----------------------------------------------------------------------------

subroutine test01(solverName)
  character(len=*), intent(in) :: solverName

  ! define internal variables
  integer( i4b ), parameter :: m=4, n = 4
  real( dfp ) :: mat( m, n ), lu( m, n ), L(m,n), U(m,n)
  integer( i4b ) :: ipiv( m ), ii, jj

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST :: " // trim(solverName) )
  CALL EqualLine( )
  mat = testMatrix( 1 )
  call DISPLAY( mat, msg="    Matrix is :: ")

  call getLU( A = mat, LU=lu, ipiv = ipiv, solverName=solverName )

  L = 0.0; U = 0.0
  DO jj = 1, n
    L( jj, jj ) = 1.0
    DO ii = 1, m
      IF( ii .LE. jj ) THEN
        U( ii, jj ) = LU( ii, jj )
      ELSE
        L( ii, jj ) = LU( ii, jj )
      END IF
    END DO
  END DO

  CALL Blanklines( NOL = 2 )
  call display( L, msg="    L :: " )
  call display( U, msg="    U :: " )
  CALL Blanklines( NOL = 2 )
  call display( ipiv, "    IPIV :: ")
  CALL Blanklines( NOL = 2 )
end subroutine test01
end module test

program main
use test
implicit none

call test01( 'getf2')
call test01( 'getrf')
call test01( 'getrf2' )

end program main