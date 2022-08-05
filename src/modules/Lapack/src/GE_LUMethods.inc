! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

PUBLIC :: getLU
PUBLIC :: LUSolve
PUBLIC :: Inv

!----------------------------------------------------------------------------
!                                                                   getLU@LU
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 7 July 2022
! summary: DGETF2 computes an LU factorization of a general m-by-n matrix A
!
!# Introduction
!
!  Using partial pivoting with row interchanges.
!
! DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!
!  This is the right-looking Level 2 BLAS version of the algorithm.
!
! ## GETRF
!
! DGETRF computes an LU factorization of a general M-by-N matrix A
!  using partial pivoting with row interchanges
!
! - iterative version of Sivan Toledo's recursive LU algorithm
! - left-looking Level 3 BLAS version of the algorithm.
!
!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).
!

INTERFACE
MODULE SUBROUTINE getLU_1(A, LU, IPIV, RCOND, NORM, info)
  REAL( DFP ), INTENT( IN ) :: A( :, : )
    !! Matrix to be factored
  REAL( DFP ), INTENT( OUT ) :: LU( :, : )
    !! LU factorization, the unit diagonal elements of L are not stored.
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: IPIV( : )
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
    !! IPIV is INTEGER array, dimension (min(M,N))
    !! The pivot indices; for 1 <= i <= min(M,N), row i of the
    !! matrix was interchanged with row IPIV(i).
  REAL( DFP ), OPTIONAL, INTENT( OUT ) :: RCOND
    !! Condition number
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: NORM
    !! NORM "1", "0"
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE getLU_1
END INTERFACE

INTERFACE getLU
  MODULE PROCEDURE getLU_1
END INTERFACE getLU

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Compute LU factorization
!
!# Introduction
!
! This routine is same as `getLU_1`	however in this routine LU
! factorization is computed in A matrix on return.

INTERFACE
MODULE SUBROUTINE getLU_2(A, IPIV, RCOND, NORM, info)
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! Matrix to be factored, on return it contains LU factorization,
    !! the unit diagonal elements of L are not stored.
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: IPIV( : )
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
    !! IPIV is INTEGER array, dimension (min(M,N))
    !! The pivot indices; for 1 <= i <= min(M,N), row i of the
    !! matrix was interchanged with row IPIV(i).
  REAL( DFP ), OPTIONAL, INTENT( OUT ) :: RCOND
    !! If present then condition number is returned
  CHARACTER( LEN = 1 ), OPTIONAL, INTENT( IN ) :: NORM
    !! "1", "0"
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE getLU_2
END INTERFACE

INTERFACE getLU
  MODULE PROCEDURE getLU_2
END INTERFACE getLU

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE
MODULE SUBROUTINE LUSolve_1( A, B, IPIV, isTranspose, info )
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! LU decomposition of matrix A, see getLU
  REAL( DFP ), INTENT( INOUT ) :: B(:)
    !! RHS, on return solution will be in B
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! IPIV returned from getLU
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTranspose
    !! if isTranspose true then we solve A^Tx=y
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
    !! info
END SUBROUTINE LUSolve_1
END INTERFACE

INTERFACE LUSolve
  MODULE PROCEDURE LUSolve_1
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE
MODULE SUBROUTINE LUSolve_2( A, B, IPIV, isTranspose, info )
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! LU Decomposition of A returned from getLU
  REAL( DFP ), INTENT( INOUT ) :: B(:,:)
    !! Several rhs, on return solution will be in B
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! pivoting returned from getLU
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTranspose
    !! if true we solve A^Tx = y
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE LUSolve_2
END INTERFACE

INTERFACE LUSolve
  MODULE PROCEDURE LUSolve_2
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE
MODULE SUBROUTINE LUSolve_3( X, A, B, IPIV, isTranspose, info )
  REAL( DFP ), INTENT( OUT ) :: X(:)
    !! RHS, on return solution will be in B
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! LU decomposition of matrix A, see getLU
  REAL( DFP ), INTENT( IN ) :: B(:)
    !! RHS
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! IPIV returned from getLU
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTranspose
    !! if isTranspose true then we solve A^Tx=y
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
    !! info
END SUBROUTINE LUSolve_3
END INTERFACE

INTERFACE LUSolve
  MODULE PROCEDURE LUSolve_3
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE
MODULE SUBROUTINE LUSolve_4( X, A, B, IPIV, isTranspose, info )
  REAL( DFP ), INTENT( OUT ) :: X(:,:)
    !! solution
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! LU Decomposition of A returned from getLU
  REAL( DFP ), INTENT( IN ) :: B(:,:)
    !! several RHS
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! pivoting returned from getLU
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isTranspose
    !! if true we solve A^Tx = y
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE LUSolve_4
END INTERFACE

INTERFACE LUSolve
  MODULE PROCEDURE LUSolve_4
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                    getInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: get inverse of square matrix from LU decomposition

INTERFACE
MODULE SUBROUTINE Inv_1( A, invA, IPIV, info )
  REAL( DFP ), INTENT( IN ) :: A( :, : )
    !! LU Decomposition
  REAL( DFP ), INTENT( INOUT ) :: invA(:, :)
    !! inverse of A
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! returned from getLU
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE Inv_1
END INTERFACE

INTERFACE Inv
  MODULE PROCEDURE Inv_1
END INTERFACE Inv

!----------------------------------------------------------------------------
!                                                                    getInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: get inverse of square matrix from LU decomposition

INTERFACE
MODULE SUBROUTINE Inv_2( A, IPIV, info )
  REAL( DFP ), INTENT( INOUT ) :: A( :, : )
    !! LU Decomposition, inverse will be returned in A
  INTEGER( I4B ), INTENT( IN ) :: IPIV(:)
    !! returned from getLU
  INTEGER( I4B ), OPTIONAL, INTENT( OUT ) :: info
END SUBROUTINE Inv_2
END INTERFACE

INTERFACE Inv
  MODULE PROCEDURE Inv_2
END INTERFACE Inv
