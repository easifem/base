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

MODULE SuperLU_dgssv_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
! DGSSV solves the system of linear equations A*X=B, using the
! LU factorization from DGSTRF. It performs the following steps:
!
!   1. If A is stored column-wise (A->Stype = SLU_NC):
!
!      1.1. Permute the columns of A, forming A*Pc, where Pc
!           is a permutation matrix. For more details of this step,
!           see sp_preorder.c.
!
!      1.2. Factor A as Pr*A*Pc=L*U with the permutation Pr determined
!           by Gaussian elimination with partial pivoting.
!           L is unit lower triangular with offdiagonal entries
!           bounded by 1 in magnitude, and U is upper triangular.
!
!      1.3. Solve the system of equations A*X=B using the factored
!           form of A.
!
!   2. If A is stored row-wise (A->Stype = SLU_NR), apply the
!      above algorithm to the transpose of A:
!
!      2.1. Permute columns of transpose(A) (rows of A),
!           forming transpose(A)*Pc, where Pc is a permutation matrix.
!           For more details of this step, see sp_preorder.c.
!
!      2.2. Factor A as Pr*transpose(A)*Pc=L*U with the permutation Pr
!           determined by Gaussian elimination with partial pivoting.
!           L is unit lower triangular with offdiagonal entries
!           bounded by 1 in magnitude, and U is upper triangular.
!
!      2.3. Solve the system of equations A*X=B using the factored
!           form of A.
!
!   See supermatrix.h for the definition of 'SuperMatrix' structure.
!
! void
! dgssv(superlu_options_t *options, SuperMatrix *A, int *perm_c, int *perm_r,
!       SuperMatrix *L, SuperMatrix *U, SuperMatrix *B,
!       SuperLUStat_t *stat, int *info )

INTERFACE
  SUBROUTINE dgssv(options, A, perm_c, perm_r, L, U, B, stat, info) &
    & BIND(C, name="dgssv")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
    & SuperMatrix
    TYPE(superlu_options_t), INTENT(INOUT) :: options
    ! options (input) superlu_options_t*
    !         The structure defines the input parameters to control
    !         how the LU decomposition will be performed and how the
    !         system will be solved.
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    ! A       (input) SuperMatrix*
    !         Matrix A in A*X=B, of dimension (A->nrow, A->ncol). The number
    !         of linear equations is A->nrow. Currently, the type of A can be:
    !         Stype = SLU_NC or SLU_NR; Dtype = SLU_D; Mtype = SLU_GE.
    !         In the future, more general A may be handled.
    INTEGER(C_INT), INTENT(INOUT) :: perm_c(*)
    ! perm_c  (input/output) int*
    !         If A->Stype = SLU_NC, column permutation vector of size A->ncol
    !         which defines the permutation matrix Pc; perm_c[i] = j means
    !         column i of A is in position j in A*Pc.
    !         If A->Stype = SLU_NR, column permutation vector of size A->nrow
    !         which describes permutation of columns of transpose(A)
    !         (rows of A) as described above.
    !
    !         If options->ColPerm = MY_PERMC or options->Fact = SamePattern or
    !            options->Fact = SamePattern_SameRowPerm, it is an input argument.
    !            On exit, perm_c may be overwritten by the product of the input
    !            perm_c and a permutation that postorders the elimination tree
    !            of Pc'*A'*A*Pc; perm_c is not changed if the elimination tree
    !            is already in postorder.
    !         Otherwise, it is an output argument.
    INTEGER(C_INT), INTENT(INOUT) :: perm_r(*)
    ! perm_r  (input/output) int*
    !         If A->Stype = SLU_NC, row permutation vector of size A->nrow,
    !         which defines the permutation matrix Pr, and is determined
    !         by partial pivoting.  perm_r[i] = j means row i of A is in
    !         position j in Pr*A.
    !         If A->Stype = SLU_NR, permutation vector of size A->ncol, which
    !         determines permutation of rows of transpose(A)
    !         (columns of A) as described above.
    !
    !         If options->RowPerm = MY_PERMR or
    !            options->Fact = SamePattern_SameRowPerm, perm_r is an
    !            input argument.
    !         otherwise it is an output argument.
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    ! L       (output) SuperMatrix*
    !         The factor L from the factorization
    !             Pr*A*Pc=L*U              (if A->Stype = SLU_NC) or
    !             Pr*transpose(A)*Pc=L*U   (if A->Stype = SLU_NR).
    !         Uses compressed row subscripts storage for supernodes, i.e.,
    !         L has types: Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    ! U       (output) SuperMatrix*
    !           The factor U from the factorization
    !             Pr*A*Pc=L*U              (if A->Stype = SLU_NC) or
    !             Pr*transpose(A)*Pc=L*U   (if A->Stype = SLU_NR).
    !         Uses column-wise storage scheme, i.e., U has types:
    !         Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_TRU.
    TYPE(SuperMatrix), INTENT(INOUT) :: B
    ! B       (input/output) SuperMatrix*
    !         B has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
    !         On entry, the right hand side matrix.
    !         On exit, the solution matrix if info = 0;
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    ! stat   (output) SuperLUStat_t*
    !        Record the statistics on runtime and floating-point operation count.
    !        See util.h for the definition of 'SuperLUStat_t'.
    INTEGER(C_INT), INTENT(INOUT) :: info
    ! info    (output) int*
    !           = 0: successful exit
    !         > 0: if info = i, and i is
    !             <= A->ncol: U(i,i) is exactly zero. The factorization has
    !                been completed, but the factor U is exactly singular,
    !                so the solution could not be computed.
    !             > A->ncol: number of bytes allocated when memory allocation
    !                failure occurred, plus A->ncol.
  END SUBROUTINE dgssv
END INTERFACE

PUBLIC :: dgssv

END MODULE
