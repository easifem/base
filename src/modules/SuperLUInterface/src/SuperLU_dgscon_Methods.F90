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

MODULE SuperLU_dgscon_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! *   Purpose
! *   =======
! *
! *   DGSCON estimates the reciprocal of the condition number of a general
! *   real matrix A, in either the 1-norm or the infinity-norm, using
! *   the LU factorization computed by DGETRF.   *
! *
! *   An estimate is obtained for norm(inv(A)), and the reciprocal of the
! *   condition number is computed as
! *      RCOND = 1 / ( norm(A) * norm(inv(A)) ).
! *
! *   See supermatrix.h for the definition of 'SuperMatrix' structure.
! *
! *   Arguments
! *   =========
! *
! *    NORM    (input) char*
! *            Specifies whether the 1-norm condition number or the
! *            infinity-norm condition number is required:
! *            = '1' or 'O':  1-norm;
! *            = 'I':         Infinity-norm.
! *
! *    L       (input) SuperMatrix*
! *            The factor L from the factorization Pr*A*Pc=L*U as computed by
! *            dgstrf(). Use compressed row subscripts storage for supernodes,
! *            i.e., L has types: Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
! *
! *    U       (input) SuperMatrix*
! *            The factor U from the factorization Pr*A*Pc=L*U as computed by
! *            dgstrf(). Use column-wise storage scheme, i.e., U has types:
! *            Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_TRU.
! *
! *    ANORM   (input) double
! *            If NORM = '1' or 'O', the 1-norm of the original matrix A.
! *            If NORM = 'I', the infinity-norm of the original matrix A.
! *
! *    RCOND   (output) double*
! *           The reciprocal of the condition number of the matrix A,
! *           computed as RCOND = 1/(norm(A) * norm(inv(A))).
! *
! *    INFO    (output) int*
! *           = 0:  successful exit
! *           < 0:  if INFO = -i, the i-th argument had an illegal value
! *
! *    =====================================================================

! void
! dgscon(char *norm, SuperMatrix *L, SuperMatrix *U,
!        double anorm, double *rcond, SuperLUStat_t *stat, int *info)

INTERFACE
  SUBROUTINE dgscon(norm, L, U, anorm, rcond, stat, info) &
    & BIND(C, name="dgscon")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
    & SuperMatrix, C_CHAR, C_DOUBLE
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: norm
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: anorm
    REAL(C_DOUBLE), INTENT(INOUT) :: rcond
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    INTEGER(C_INT), INTENT(INOUT) :: info
  END SUBROUTINE dgscon
END INTERFACE

PUBLIC :: dgscon

END MODULE SuperLU_dgscon_Methods
