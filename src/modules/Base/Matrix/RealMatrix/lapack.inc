
!----------------------------------------------------------------------------
!                                                         JacobiMethod@LAPACK
!----------------------------------------------------------------------------

INTERFACE
!! Returns all the eigenvalues of symmetric matrix

!> authors: Dr. Vikas Sharma
!
! This subroutine computes all eigenvalues and eigenvectors of a real
! symmetric N × N matrix A.
! -  On output, elements of `A` above the diagonal are destroyed.
! -  `d` is a vector of length N that returns the eigenvalues of `A`.
! -  `V` is an `N × N` matrix whose columns contain on output, the normalized
!  eigenvectors of A.
! -  `tRot` returns the number of Jacobi rotations that were required.
!
! ### Reference:: Numerical Reciepe in Fortran, Page 1225
!

MODULE PURE SUBROUTINE eig_jacobi_method( Mat, EigenValues, EigenVectors, &
  & MaxIter )
  INTEGER( I4B ), INTENT( IN ) :: MaxIter
  REAL( DFP ), INTENT( INOUT ) :: EigenValues( : )
  REAL( DFP ), INTENT( INOUT ) :: Mat( :, : )
  REAL( DFP ), INTENT( INOUT ) :: EigenVectors( :, : )
END SUBROUTINE eig_jacobi_method
END INTERFACE

!> Generic subroutine for computing eigen value of a symmetric matrix
INTERFACE JacobiMethod
  MODULE PROCEDURE eig_jacobi_method
END INTERFACE JacobiMethod

PUBLIC :: JacobiMethod
