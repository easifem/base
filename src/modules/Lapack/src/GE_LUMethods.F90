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

MODULE GE_LUMethods
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: GetLU
PUBLIC :: LUSolve
PUBLIC :: Inv

!----------------------------------------------------------------------------
!                                                                   GetLU@LU
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-07-07
! summary: DGetF2 computes an LU factorization of a general m-by-n matrix A
!
!# Introduction
!
!  Using partial pivoting with row interchanges.
!
! DGetF2 computes an LU factorization of a general m-by-n matrix A
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
! ## GetRF
!
! DGetRF computes an LU factorization of a general M-by-N matrix A
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

INTERFACE GetLU
  MODULE SUBROUTINE GetLU_1(A, LU, IPIV, RCOND, NORM, info)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! Matrix to be factored
    REAL(DFP), INTENT(OUT) :: LU(:, :)
    !! LU factorization, the unit diagonal elements of L are not stored.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
    !! IPIV is INTEGER array, dimension (min(M,N))
    !! The pivot indices; for 1 <= i <= min(M,N), row i of the
    !! matrix was interchanged with row IPIV(i).
    REAL(DFP), OPTIONAL, INTENT(OUT) :: RCOND
    !! Inverse of Condition number
    CHARACTER(1), OPTIONAL, INTENT(IN) :: NORM
    !! NORM "1", "0"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE GetLU_1
END INTERFACE GetLU

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Compute LU factorization
!
!# Introduction
!
! This routine is same as `GetLU_1` however in this routine LU
! factorization is computed in A matrix on return.

INTERFACE GetLU
  MODULE SUBROUTINE GetLU_2(A, IPIV, RCOND, NORM, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! Matrix to be factored, on return it contains LU factorization,
    !! the unit diagonal elements of L are not stored.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
    !! IPIV is INTEGER array,row i of the matrix was interchanged with row
    !! IPIV(i).
    !! IPIV is INTEGER array, dimension (min(M,N))
    !! The pivot indices; for 1 <= i <= min(M,N), row i of the
    !! matrix was interchanged with row IPIV(i).
    REAL(DFP), OPTIONAL, INTENT(OUT) :: RCOND
    !! If present then inverse of condition number is returned
    CHARACTER(1), OPTIONAL, INTENT(IN) :: NORM
    !! "1", "0"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE GetLU_2
END INTERFACE GetLU

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE LUSolve
  MODULE SUBROUTINE LUSolve_1(A, B, IPIV, isTranspose, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU decomposition of matrix A, see GetLU
    REAL(DFP), INTENT(INOUT) :: B(:)
    !! at entry RHS
    !! on return solution will be in B
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from GetLU
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! IF isTranspose true then we solve A^Tx=y
    !! Default is `.false.`
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE LUSolve_1
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE LUSolve
  MODULE SUBROUTINE LUSolve_2(A, B, IPIV, isTranspose, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU Decomposition of A returned from GetLU
    REAL(DFP), INTENT(INOUT) :: B(:, :)
    !! Several rhs, on return solution will be in B
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! pivoting returned from GetLU
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true we solve A^Tx = y
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE LUSolve_2
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE LUSolve
  MODULE SUBROUTINE LUSolve_3(X, A, B, IPIV, isTranspose, info)
    REAL(DFP), INTENT(OUT) :: X(:)
    !! RHS, on return solution will be in B
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU decomposition of matrix A, see GetLU
    REAL(DFP), INTENT(IN) :: B(:)
    !! RHS
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from GetLU
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if isTranspose true then we solve A^Tx=y
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE LUSolve_3
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y

INTERFACE LUSolve
  MODULE SUBROUTINE LUSolve_4(X, A, B, IPIV, isTranspose, info)
    REAL(DFP), INTENT(OUT) :: X(:, :)
    !! solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU Decomposition of A returned from GetLU
    REAL(DFP), INTENT(IN) :: B(:, :)
    !! several RHS
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! pivoting returned from GetLU
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! if true we solve A^Tx = y
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE LUSolve_4
END INTERFACE LUSolve

!----------------------------------------------------------------------------
!                                                                    GetInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Get inverse of square matrix from LU decomposition

INTERFACE Inv
  MODULE SUBROUTINE Inv_1(A, invA, IPIV, info)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! LU Decomposition
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    !! inverse of A
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! returned from GetLU
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE Inv_1
END INTERFACE Inv

!----------------------------------------------------------------------------
!                                                                    GetInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Get inverse of square matrix from LU decomposition

INTERFACE Inv
  MODULE SUBROUTINE Inv_2(A, IPIV, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LU Decomposition, inverse will be returned in A
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! returned from GetLU
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE Inv_2
END INTERFACE Inv

END MODULE GE_LUMethods
