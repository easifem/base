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

MODULE SuperLU_dgstrs_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!   Purpose
!   =======
!
!   DGSTRS solves a system of linear equations A*X=B or A'*X=B
!   with A sparse and B dense, using the LU factorization computed by
!   DGSTRF.
!
!   See supermatrix.h for the definition of 'SuperMatrix' structure.
!
!   Arguments
!   =========
!
!   trans   (input) trans_t
!            Specifies the form of the system of equations:
!            = NOTRANS: A * X = B  (No transpose)
!            = TRANS:   A'* X = B  (Transpose)
!            = CONJ:    A**H * X = B  (Conjugate transpose)
!
!   L       (input) SuperMatrix*
!           The factor L from the factorization Pr*A*Pc=L*U as computed by
!           dgstrf(). Use compressed row subscripts storage for supernodes,
!           i.e., L has types: Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
!
!   U       (input) SuperMatrix*
!           The factor U from the factorization Pr*A*Pc=L*U as computed by
!           dgstrf(). Use column-wise storage scheme, i.e., U has types:
!           Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_TRU.
!
!   perm_c  (input) int*, dimension (L->ncol)
!             Column permutation vector, which defines the
!           permutation matrix Pc; perm_c[i] = j means column i of A is
!           in position j in A*Pc.
!
!   perm_r  (input) int*, dimension (L->nrow)
!           Row permutation vector, which defines the permutation matrix Pr;
!           perm_r[i] = j means row i of A is in position j in Pr*A.
!
!   B       (input/output) SuperMatrix*
!           B has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
!           On entry, the right hand side matrix.
!           On exit, the solution matrix if info = 0;
!
!   stat     (output) SuperLUStat_t*
!            Record the statistics on runtime and floating-point operation count.
!            See util.h for the definition of 'SuperLUStat_t'.
!
!   info    (output) int*
!              = 0: successful exit
!             < 0: if info = -i, the i-th argument had an illegal value
!
! void
! dgstrs (trans_t trans, SuperMatrix *L, SuperMatrix *U,
!         int *perm_c, int *perm_r, SuperMatrix *B,
!         SuperLUStat_t *stat, int *info)

INTERFACE
  SUBROUTINE dgstrs(trans, L, U, perm_c, perm_r, B, stat, info) &
    & BIND(C, name="dgstrs")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
    & SuperMatrix
    INTEGER(C_INT), VALUE, INTENT(IN) :: trans
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    INTEGER(C_INT), INTENT(INOUT) :: perm_c(*)
    INTEGER(C_INT), INTENT(INOUT) :: perm_r(*)
    TYPE(SuperMatrix), INTENT(INOUT) :: B
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    INTEGER(C_INT), INTENT(INOUT) :: info
  END SUBROUTINE dgstrs
END INTERFACE

PUBLIC :: dgstrs

END MODULE SuperLU_dgstrs_Methods
