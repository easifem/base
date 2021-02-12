MODULE LapackInterface
  IMPLICIT NONE

INTERFACE GESV
!! DGESV computes the solution to a real system of linear equations

!
!> DGESV computes the solution to a real system of linear equations
!  A * X = B,
!  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
!  The LU decomposition with partial pivoting and row interchanges is
!  used to factor A as
!     A = P * L * U,
!  where P is a permutation matrix, L is unit lower triangular, and U is
!  upper triangular.  The factored form of A is then used to solve the
!  system of equations A * X = B.
!

SUBROUTINE SGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
  INTEGER, PARAMETER :: WP = KIND(1.0E0)
  INTEGER            INFO, LDA, LDB, N, NRHS
  INTEGER            IPIV( * )
  REAL(WP)           A( LDA, * ), B( LDB, * )
END SUBROUTINE

SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
  INTEGER, PARAMETER :: WP = KIND(1.0D0)
  INTEGER            INFO, LDA, LDB, N, NRHS
  INTEGER            IPIV( * )
  REAL(WP)           A( LDA, * ), B( LDB, * )
END SUBROUTINE
END INTERFACE