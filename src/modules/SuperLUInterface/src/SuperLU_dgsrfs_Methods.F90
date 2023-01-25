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

MODULE SuperLU_dgsrfs_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!  *   Purpose
!  *   =======
!  *
!  *   DGSRFS improves the computed solution to a system of linear
!  *   equations and provides error bounds and backward error estimates for
!  *   the solution.
!  *
!  *   If equilibration was performed, the system becomes:
!  *           (diag(R)*A_original*diag(C)) * X = diag(R)*B_original.
!  *
!  *   See supermatrix.h for the definition of 'SuperMatrix' structure.
!  *
!  *   Arguments
!  *   =========
!  *
!  * trans   (input) trans_t
!  *          Specifies the form of the system of equations:
!  *          = NOTRANS: A * X = B  (No transpose)
!  *          = TRANS:   A'* X = B  (Transpose)
!  *          = CONJ:    A**H * X = B  (Conjugate transpose)
!  *
!  *   A       (input) SuperMatrix*
!  *           The original matrix A in the system, or the scaled A if
!  *           equilibration was done. The type of A can be:
!  *           Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_GE.
!  *
!  *   L       (input) SuperMatrix*
!  *             The factor L from the factorization Pr*A*Pc=L*U. Use
!  *           compressed row subscripts storage for supernodes,
!  *           i.e., L has types: Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
!  *
!  *   U       (input) SuperMatrix*
!  *           The factor U from the factorization Pr*A*Pc=L*U as computed by
!  *           dgstrf(). Use column-wise storage scheme,
!  *           i.e., U has types: Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_TRU.
!  *
!  *   perm_c  (input) int*, dimension (A->ncol)
!  *             Column permutation vector, which defines the
!  *           permutation matrix Pc; perm_c[i] = j means column i of A is
!  *           in position j in A*Pc.
!  *
!  *   perm_r  (input) int*, dimension (A->nrow)
!  *           Row permutation vector, which defines the permutation matrix Pr;
!  *           perm_r[i] = j means row i of A is in position j in Pr*A.
!  *
!  *   equed   (input) Specifies the form of equilibration that was done.
!  *           = 'N': No equilibration.
!  *           = 'R': Row equilibration, i.e., A was premultiplied by diag(R).
!  *           = 'C': Column equilibration, i.e., A was postmultiplied by
!  *                  diag(C).
!  *           = 'B': Both row and column equilibration, i.e., A was replaced
!  *                  by diag(R)*A*diag(C).
!  *
!  *   R       (input) double*, dimension (A->nrow)
!  *           The row scale factors for A.
!  *           If equed = 'R' or 'B', A is premultiplied by diag(R).
!  *           If equed = 'N' or 'C', R is not accessed.
!  *
!  *   C       (input) double*, dimension (A->ncol)
!  *           The column scale factors for A.
!  *           If equed = 'C' or 'B', A is postmultiplied by diag(C).
!  *           If equed = 'N' or 'R', C is not accessed.
!  *
!  *   B       (input) SuperMatrix*
!  *           B has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
!  *           The right hand side matrix B.
!  *           if equed = 'R' or 'B', B is premultiplied by diag(R).
!  *
!  *   X       (input/output) SuperMatrix*
!  *           X has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
!  *           On entry, the solution matrix X, as computed by dgstrs().
!  *           On exit, the improved solution matrix X.
!  *           if *equed = 'C' or 'B', X should be premultiplied by diag(C)
!  *               in order to obtain the solution to the original system.
!  *
!  *   FERR    (output) double*, dimension (B->ncol)
!  *           The estimated forward error bound for each solution vector
!  *           X(j) (the j-th column of the solution matrix X).
!  *           If XTRUE is the true solution corresponding to X(j), FERR(j)
!  *           is an estimated upper bound for the magnitude of the largest
!  *           element in (X(j) - XTRUE) divided by the magnitude of the
!  *           largest element in X(j).  The estimate is as reliable as
!  *           the estimate for RCOND, and is almost always a slight
!  *           overestimate of the true error.
!  *
!  *   BERR    (output) double*, dimension (B->ncol)
!  *           The componentwise relative backward error of each solution
!  *           vector X(j) (i.e., the smallest relative change in
!  *           any element of A or B that makes X(j) an exact solution).
!  *
!  *   stat     (output) SuperLUStat_t*
!  *            Record the statistics on runtime and floating-point operation count.
!  *            See util.h for the definition of 'SuperLUStat_t'.
!  *
!  *   info    (output) int*
!  *           = 0:  successful exit
!  *            < 0:  if INFO = -i, the i-th argument had an illegal value
!  *
!  *    Internal Parameters
!  *    ===================
!  *
!  *    ITMAX is the maximum number of steps of iterative refinement.
!  *
! void
! dgsrfs(trans_t trans, SuperMatrix *A, SuperMatrix *L, SuperMatrix *U,
!        int *perm_c, int *perm_r, char *equed, double *R, double *C,
!        SuperMatrix *B, SuperMatrix *X, double *ferr, double *berr,
!        SuperLUStat_t *stat, int *info)

INTERFACE
  SUBROUTINE dgsrfs(trans, A, L, U, perm_c, perm_r, &
    & equed, R, C, B, X, ferr, berr, &
    &  stat, info) &
    & BIND(C, name="dgsrfs")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
      & SuperMatrix, GlobalLU_t, mem_usage_t, C_CHAR, C_DOUBLE
    !
    INTEGER(C_INT), VALUE, INTENT(IN) :: trans
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    INTEGER(C_INT), INTENT(INOUT) :: perm_c(*)
    INTEGER(C_INT), INTENT(INOUT) :: perm_r(*)
    CHARACTER(1, kind=C_CHAR), INTENT(inout) :: equed(*)
    REAL(C_DOUBLE), INTENT(inout) :: R(*)
    REAL(C_DOUBLE), INTENT(inout) :: C(*)
    TYPE(SuperMatrix), INTENT(INOUT) :: B
    TYPE(SuperMatrix), INTENT(INOUT) :: X
    REAL(C_DOUBLE), INTENT(inout) :: ferr(*)
    REAL(C_DOUBLE), INTENT(inout) :: berr(*)
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    INTEGER(C_INT), INTENT(INOUT) :: info
  END SUBROUTINE dgsrfs
END INTERFACE

PUBLIC :: dgsrfs

END MODULE SuperLU_dgsrfs_Methods
