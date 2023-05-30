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

MODULE Sym_LUMethods
USE GlobalData, ONLY: DFP, I4B, LGT
IMPLICIT NONE
PRIVATE
PUBLIC :: SymGetLU
PUBLIC :: SymGetLDL
PUBLIC :: SymGetCholesky
PUBLIC :: SymLUSolve
PUBLIC :: SymGetInv

! PUBLIC :: CholeskySolve

!----------------------------------------------------------------------------
!                                                                SymGetLU
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-15
! summary: Computes an LU (LL' or U'U) factorization of a sym matrix A

INTERFACE
  MODULE SUBROUTINE SymGetLU_1(A, LU, IPIV, UPLO, info)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! Matrix to be factored
    REAL(DFP), INTENT(OUT) :: LU(:, :)
    !! L or U factorization
    !! SHAPE(LU) = [N,N]
    INTEGER(I4B), INTENT(OUT) :: IPIV(:)
    !! reverse permulation
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info=0 => success
    !! info \ne 0 => error
  END SUBROUTINE SymGetLU_1
END INTERFACE

INTERFACE SymGetLU
  MODULE PROCEDURE SymGetLU_1
END INTERFACE SymGetLU

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Compute LU factorization

INTERFACE
  MODULE SUBROUTINE SymGetLU_2(A, IPIV, UPLO, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! Matrix to be factored, on return it contains L or U factorization,
    INTEGER(I4B), INTENT(OUT) :: IPIV(:)
    !! permutation
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info = 0 ➡️ success
    !! info .ne. 0 ➡️ error
  END SUBROUTINE SymGetLU_2
END INTERFACE

INTERFACE SymGetLU
  MODULE PROCEDURE SymGetLU_2
END INTERFACE SymGetLU

!----------------------------------------------------------------------------
!                                                                SymGetLDL
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-15
! summary: Computes an LU (LL' or U'U) factorization of a sym matrix A
!
!# Introduction
!
!- Computes the LDLt or Bunch-Kaufman factorization of a symmetric/ hermitian
! matrix.
!- This function returns a block diagonal matrix D consisting blocks of
! size at most 2x2 and also a possibly permuted unit lower triangular
! matrix L such that the factorization `A = L D L^H` or
! `A = L D L^T` holds.
! If `uplo=U` (again possibly permuted) upper triangular matrices
! are returned as outer factors.
!
! The permutation array can be used to triangularize the outer factors
! simply by a row shuffle, i.e., `lu[perm, :]` is an upper/lower triangular
!  matrix. This is also equivalent to multiplication with a
! permutation matrix MATMUL(P, lu), where P is a column-permuted
! identity matrix I[:, perm].
!
! Depending on the value of the "uplo", only upper or lower triangular
! part of the input array is referenced.
! Hence, a triangular matrix on entry would give the same result
! as if the full matrix is supplied.
!
! This routine calls following routines
!
!- LACPY: Copy two matrices
!- SYTRF: Perform factorization
!- SYCONV: Convert data from SYTRF to standard form. At this point, the
! LU matrix has undergone both row and column interchange (possibly)
!- LAPMR: At this point we undo the row interchange.
!
! Finally, LU is permuted such that
!
! `MATMUL( MATMUL( LU, Diag(D) ), TRANSPOSE(LU))` returns the A matrix.
!
! ## ?SYTRF
!
! DSYTRF computes the factorization of a real symmetric matrix A using
! the Bunch-Kaufman diagonal pivoting method.
! The form of the factorization is
!
!```fortran
!     A = U**T*D*U  or  A = L*D*L**T
!```
!
!  where,
!
!- U (or L) is a product of permutation and unit upper (or lower)
!  triangular matrices
!- D is symmetric and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
!- This is the blocked version of the algorithm, calling Level 3 BLAS.
!
! Also see, SYTRF from Lapack95

INTERFACE
  MODULE SUBROUTINE SymGetLDL_1(A, LU, D, E, UPLO, IPIV, info)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! Matrix to be factored
    REAL(DFP), INTENT(OUT) :: LU(:, :)
    !! L or U factorization
    !! SHAPE(LU) = [N,N]
    REAL(DFP), INTENT(OUT) :: D(:)
    !! Diagonal entries
    !! Size(D) = N
    REAL(DFP), INTENT(OUT) :: E(:)
    !! Subdiagonal and superdiagonal entries
    !! Size(E) = N
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
    !! reverse permulation
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info=0 => success
    !! info \ne 0 => error
  END SUBROUTINE SymGetLDL_1
END INTERFACE

INTERFACE SymGetLDL
  MODULE PROCEDURE SymGetLDL_1
END INTERFACE SymGetLDL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Compute LU factorization
!
!# Introduction
!
! This routine is same as `getLU_1` however in this routine LU
! factorization is computed in A matrix on return.

INTERFACE
  MODULE SUBROUTINE SymGetLDL_2(A, D, E, UPLO, IPIV, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! Matrix to be factored, on return it contains L or U factorization,
    REAL(DFP), INTENT(OUT) :: D(:)
    !! Diagonal entries
    REAL(DFP), INTENT(OUT) :: E(:)
    !! Sub and super Diagonal entries
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: IPIV(:)
    !! permutation
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info = 0 ➡️ success
    !! info .ne. 0 ➡️ error
  END SUBROUTINE SymGetLDL_2
END INTERFACE

INTERFACE SymGetLDL
  MODULE PROCEDURE SymGetLDL_2
END INTERFACE SymGetLDL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-20
! summary: Cholesky factorization of symmetric matrix
!
!# Introduction
!
! Compute the Cholesky decomposition of a matrix.
!
! Returns the Cholesky decomposition, $A=L \cdot L^{T}$, and $A=U^{T} \cdot U$
! or  of a Hermitian positive-definite matrix A.
!
! This routine call following routines from Lapack95

INTERFACE
  MODULE SUBROUTINE SymGetCholesky_1(A, LU, UPLO, info)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! Matrix to be factored
    REAL(DFP), INTENT(OUT) :: LU(:, :)
    !! L or U factorization
    !! SHAPE(LU) = [N,N]
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info=0 => success
    !! info \ne 0 => error
  END SUBROUTINE SymGetCholesky_1
END INTERFACE

INTERFACE SymGetCholesky
  MODULE PROCEDURE SymGetCholesky_1
END INTERFACE SymGetCholesky

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-20
! summary: Cholesky factorization of symmetric matrix
!
!# Introduction
!
! Compute the Cholesky decomposition of a matrix.
!
! Returns the Cholesky decomposition, $A=L \cdot L^{T}$, and $A=U^{T} \cdot U$
! or  of a Hermitian positive-definite matrix A.
!
! This routine call following routines from Lapack95

INTERFACE
  MODULE SUBROUTINE SymGetCholesky_2(A, UPLO, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! On entry Matrix to be factored
    !! On exit: L or U factorization
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info=0 => success
    !! info \ne 0 => error
  END SUBROUTINE SymGetCholesky_2
END INTERFACE

INTERFACE SymGetCholesky
  MODULE PROCEDURE SymGetCholesky_2
END INTERFACE SymGetCholesky

!----------------------------------------------------------------------------
!                                                                SymLUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y
!
!# Introduction
!
! A and IPIV are returned from
! SymGetLU or SYTRF routine of Lapack95.

INTERFACE
  MODULE SUBROUTINE SymLUSolve_1(A, B, IPIV, UPLO, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LDLt decomposition of matrix A, see SymGetLU
    REAL(DFP), INTENT(INOUT) :: B(:)
    !! at entry RHS
    !! on return solution will be in B
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE SymLUSolve_1
END INTERFACE

INTERFACE SymLUSolve
  MODULE PROCEDURE SymLUSolve_1
END INTERFACE SymLUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y
!
!# Introduction
!
! It calls `SYTRS`

INTERFACE
  MODULE SUBROUTINE SymLUSolve_2(A, B, IPIV, UPLO, info)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LDLt decomposition of matrix A, see SymGetLU
    REAL(DFP), INTENT(INOUT) :: B(:, :)
    !! at entry RHS
    !! on return solution will be in B
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE SymLUSolve_2
END INTERFACE

INTERFACE SymLUSolve
  MODULE PROCEDURE SymLUSolve_2
END INTERFACE SymLUSolve

!----------------------------------------------------------------------------
!                                                                SymLUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y
!
!# Introduction
!
! A and IPIV are returned from
! SymGetLU or SYTRF routine of Lapack95.

INTERFACE
  MODULE SUBROUTINE SymLUSolve_3(X, A, B, IPIV, UPLO, info)
    REAL(DFP), INTENT(OUT) :: X(:)
    !! Solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LDLt decomposition of matrix A, see SymGetLU
    REAL(DFP), INTENT(IN) :: B(:)
    !!  RHS
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE SymLUSolve_3
END INTERFACE

INTERFACE SymLUSolve
  MODULE PROCEDURE SymLUSolve_3
END INTERFACE SymLUSolve

!----------------------------------------------------------------------------
!                                                                 LUSolve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Solve LUx=y
!
!# Introduction
!
! It calls `SYTRS`

INTERFACE
  MODULE SUBROUTINE SymLUSolve_4(X, A, B, IPIV, UPLO, info)
    REAL(DFP), INTENT(OUT) :: X(:, :)
    !! Solution
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! LDLt decomposition of matrix A, see SymGetLU
    REAL(DFP), INTENT(IN) :: B(:, :)
    !! at entry RHS
    !! on return solution will be in B
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! IPIV returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    !! If UPLO="U", then upper triangular part of A is used
    !! If UPLO="L", then lower triangular part of A is used
    !! Default = "U"
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info
  END SUBROUTINE SymLUSolve_4
END INTERFACE

INTERFACE SymLUSolve
  MODULE PROCEDURE SymLUSolve_4
END INTERFACE SymLUSolve

!----------------------------------------------------------------------------
!                                                                  SymGetInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-20
! summary: get inverse of square matrix from LU decomposition
!
!# Introduction
!
! It calls `SYTRI`

INTERFACE
  MODULE SUBROUTINE SymGetInv_1(A, invA, IPIV, UPLO, INFO)
    REAL(DFP), INTENT(IN) :: A(:, :)
    !! LU Decomposition from SymGetLU
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    !! Inverse of A
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! Returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE SymGetInv_1
END INTERFACE

INTERFACE SymGetInv
  MODULE PROCEDURE SymGetInv_1
END INTERFACE SymGetInv

!----------------------------------------------------------------------------
!                                                                  SymGetInv
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-20
! summary: get inverse of square matrix from LU decomposition
!
!# Introduction
!
! It calls `SYTRI`

INTERFACE
  MODULE SUBROUTINE SymGetInv_2(A, IPIV, UPLO, INFO)
    REAL(DFP), INTENT(INOUT) :: A(:, :)
    !! On entry: LU Decomposition from SymGetLU
    !! On Exit: Inverse of A
    INTEGER(I4B), INTENT(IN) :: IPIV(:)
    !! Returned from SymGetLU
    CHARACTER(1), OPTIONAL, INTENT(IN) :: UPLO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
  END SUBROUTINE SymGetInv_2
END INTERFACE

INTERFACE SymGetInv
  MODULE PROCEDURE SymGetInv_2
END INTERFACE SymGetInv

END MODULE Sym_LUMethods
