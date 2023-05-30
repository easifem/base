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

MODULE SuperLU_dgsequ_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! * Purpose
! *   =======
! *
! *   DGSEQU computes row and column scalings intended to equilibrate an
! *   M-by-N sparse matrix A and reduce its condition number. R returns the row
! *   scale factors and C the column scale factors, chosen to try to make
! *   the largest element in each row and column of the matrix B with
! *   elements B(i,j)=R(i)*A(i,j)*C(j) have absolute value 1.
! *
! *   R(i) and C(j) are restricted to be between SMLNUM = smallest safe
! *   number and BIGNUM = largest safe number.  Use of these scaling
! *   factors is not guaranteed to reduce the condition number of A but
! *   works well in practice.
! *
! *   See supermatrix.h for the definition of 'SuperMatrix' structure.
! *
! *   Arguments
! *   =========
! *
! *   A       (input) SuperMatrix*
! *           The matrix of dimension (A->nrow, A->ncol) whose equilibration
! *           factors are to be computed. The type of A can be:
! *           Stype = SLU_NC; Dtype = SLU_D; Mtype = SLU_GE.
! *
! *   R       (output) double*, size A->nrow
! *           If INFO = 0 or INFO > M, R contains the row scale factors
! *           for A.
! *
! *   C       (output) double*, size A->ncol
! *           If INFO = 0,  C contains the column scale factors for A.
! *
! *   ROWCND  (output) double*
! *           If INFO = 0 or INFO > M, ROWCND contains the ratio of the
! *           smallest R(i) to the largest R(i).  If ROWCND >= 0.1 and
! *           AMAX is neither too large nor too small, it is not worth
! *           scaling by R.
! *
! *   COLCND  (output) double*
! *           If INFO = 0, COLCND contains the ratio of the smallest
! *           C(i) to the largest C(i).  If COLCND >= 0.1, it is not
! *           worth scaling by C.
! *
! *   AMAX    (output) double*
! *           Absolute value of largest matrix element.  If AMAX is very
! *           close to overflow or very close to underflow, the matrix
! *           should be scaled.
! *
! *   INFO    (output) int*
! *           = 0:  successful exit
! *           < 0:  if INFO = -i, the i-th argument had an illegal value
! *           > 0:  if INFO = i,  and i is
! *                 <= A->nrow:  the i-th row of A is exactly zero
! *                 >  A->ncol:  the (i-M)-th column of A is exactly zero
! *
!
! void
! dgsequ(SuperMatrix *A, double *r, double *c, double *rowcnd,
!         double *colcnd, double *amax, int *info)

INTERFACE
  SUBROUTINE dgsequ(A, r, c, rowcnd, colcnd, amax, info)&
    & BIND(C, name="dgsequ")
    IMPORT :: SuperMatrix, C_DOUBLE, C_INT

    TYPE(SuperMatrix), INTENT(INOUT) :: A
    REAL(C_DOUBLE), INTENT(INOUT) :: r(*)
    REAL(C_DOUBLE), INTENT(INOUT) :: c(*)
    REAL(C_DOUBLE), INTENT(INOUT) :: rowcnd
    REAL(C_DOUBLE), INTENT(INOUT) :: colcnd
    REAL(C_DOUBLE), INTENT(INOUT) :: amax
    INTEGER(C_INT), INTENT(inout) :: info
  END SUBROUTINE dgsequ
END INTERFACE

PUBLIC :: dgsequ

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SuperLU_dgsequ_Methods
