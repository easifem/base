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

#include "./include/macros.inc"

MODULE SuperLU_dUtil_Methods
USE ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE, C_CHAR
USE SuperLU_Types
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern void
! dCreate_CompCol_Matrix(SuperMatrix *, int, int, int, double *,
!                        int *, int *, Stype_t, Dtype_t, Mtype_t);

INTERFACE
  SUBROUTINE dCreate_CompCol_Matrix(A, m, n, nnz, nzval, rowind, colptr, &
    & stype, dtype, mtype) BIND(C, name="dCreate_CompCol_Matrix")
    IMPORT :: C_PTR, C_INT, C_DOUBLE, SuperMatrix
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    INTEGER(C_INT), VALUE, INTENT(IN) :: m
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: nnz
    REAL(C_DOUBLE), INTENT(INOUT) :: nzval(*)
    INTEGER(C_INT), INTENT(INOUT) :: rowind(*)
    INTEGER(C_INT), INTENT(INOUT) :: colptr(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stype
    INTEGER(C_INT), VALUE, INTENT(IN) :: dtype
    INTEGER(C_INT), VALUE, INTENT(IN) :: mtype
  END SUBROUTINE dCreate_CompCol_Matrix
END INTERFACE

PUBLIC :: dCreate_CompCol_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dCreate_CompRow_Matrix(SuperMatrix *A, int m, int n, int nnz,
!                        double *nzval, int *colind, int *rowptr,
!                        Stype_t stype, Dtype_t dtype, Mtype_t mtype)

INTERFACE
  SUBROUTINE dCreate_CompRow_Matrix(A, m, n, nnz, nzval, colind, rowptr, &
    & stype, dtype, mtype) BIND(C, name="dCreate_CompRow_Matrix")
    IMPORT :: C_PTR, C_INT, C_DOUBLE, SuperMatrix
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    INTEGER(C_INT), VALUE, INTENT(IN) :: m
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: nnz
    REAL(C_DOUBLE), INTENT(IN) :: nzval(*)
    INTEGER(C_INT), INTENT(IN) :: colind(*)
    INTEGER(C_INT), INTENT(IN) :: rowptr(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stype
    INTEGER(C_INT), VALUE, INTENT(IN) :: dtype
    INTEGER(C_INT), VALUE, INTENT(IN) :: mtype
  END SUBROUTINE dCreate_CompRow_Matrix
END INTERFACE

PUBLIC :: dCreate_CompRow_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /*! \brief Copy matrix A into matrix B. */
! void
! dCopy_CompCol_Matrix(SuperMatrix *A, SuperMatrix *B)

INTERFACE
  SUBROUTINE dCopy_CompCol_Matrix(A, B) BIND(C, name="dCopy_CompCol_Matrix")
    IMPORT :: C_PTR, SuperMatrix
    TYPE(SuperMatrix), INTENT(INOUT) :: A, B
  END SUBROUTINE dCopy_CompCol_Matrix
END INTERFACE

PUBLIC :: dCopy_CompCol_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dCreate_Dense_Matrix(SuperMatrix *X, int m, int n, double *x, int ldx,
!                     Stype_t stype, Dtype_t dtype, Mtype_t mtype)
INTERFACE
  SUBROUTINE dCreate_Dense_Matrix(A, m, n, x, ldx, stype, dtype, mtype) &
    & BIND(C, name="dCreate_Dense_Matrix")
    IMPORT :: C_PTR, C_INT, C_DOUBLE, SuperMatrix
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    INTEGER(C_INT), VALUE, INTENT(IN) :: m
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    REAL(C_DOUBLE), INTENT(INOUT) :: x(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: ldx
    INTEGER(C_INT), VALUE, INTENT(IN) :: stype
    INTEGER(C_INT), VALUE, INTENT(IN) :: dtype
    INTEGER(C_INT), VALUE, INTENT(IN) :: mtype
  END SUBROUTINE dCreate_Dense_Matrix
END INTERFACE

PUBLIC :: dCreate_Dense_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dCopy_Dense_Matrix(int M, int N, double *X, int ldx,
!                         double *Y, int ldy)
! {
! /*! \brief Copies a two-dimensional matrix X to another matrix Y.
!  */

INTERFACE
  SUBROUTINE dCopy_Dense_Matrix(M, N, X, ldx, Y, ldy) &
  & BIND(C, name="dCopy_Dense_Matrix")
    IMPORT :: C_INT, C_DOUBLE
    INTEGER(C_INT), VALUE, INTENT(IN) :: M
    INTEGER(C_INT), VALUE, INTENT(IN) :: N
    REAL(C_DOUBLE), INTENT(IN) :: X(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: ldx
    REAL(C_DOUBLE), INTENT(INOUT) :: Y(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: ldy
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dCreate_SuperNode_Matrix(SuperMatrix *L, int m, int n, int nnz,
!                         double *nzval, int *nzval_colptr, int *rowind,
!                         int *rowind_colptr, int *col_to_sup, int *sup_to_col,
!                         Stype_t stype, Dtype_t dtype, Mtype_t mtype)

INTERFACE
  SUBROUTINE dCreate_SuperNode_Matrix(L, m, n, nnz, nzval, nzval_colptr, &
    & rowind, rowind_colptr, col_to_sup, sup_to_col, stype, dtype, mtype) &
    & BIND(C, name="dCreate_SuperNode_Matrix")
    IMPORT :: C_PTR, C_INT, C_DOUBLE, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: L
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: L
#endif
    INTEGER(C_INT), VALUE, INTENT(IN) :: m
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: nnz
    REAL(C_DOUBLE), INTENT(IN) :: nzval(*)
    INTEGER(C_INT), INTENT(IN) :: nzval_colptr(*)
    INTEGER(C_INT), INTENT(IN) :: rowind(*)
    INTEGER(C_INT), INTENT(IN) :: rowind_colptr(*)
    INTEGER(C_INT), INTENT(IN) :: col_to_sup(*)
    INTEGER(C_INT), INTENT(IN) :: sup_to_col(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: stype
    INTEGER(C_INT), VALUE, INTENT(IN) :: dtype
    INTEGER(C_INT), VALUE, INTENT(IN) :: mtype
  END SUBROUTINE dCreate_SuperNode_Matrix
END INTERFACE

PUBLIC :: dCreate_SuperNode_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dCompRow_to_CompCol(int m, int n, int nnz,
!                     double *a, int *colind, int *rowptr,
!                     double **at, int **rowind, int **colptr)
! brief Convert a row compressed storage into a column
! compressed storage.

INTERFACE
  SUBROUTINE dCompRow_to_CompCol(m, n, nnz, a, colind, rowptr, &
    & at, rowind, colptr) BIND(C, name="dCompRow_to_CompCol")
    IMPORT :: C_INT, C_DOUBLE, C_PTR
    INTEGER(C_INT), VALUE, INTENT(IN) :: m
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: nnz
    REAL(C_DOUBLE), INTENT(IN) :: a(*)
    INTEGER(C_INT), INTENT(IN) :: colind(*)
    INTEGER(C_INT), INTENT(IN) :: rowptr(*)
    TYPE(C_PTR), INTENT(INOUT) :: at
    TYPE(C_PTR), INTENT(INOUT) :: rowind
    TYPE(C_PTR), INTENT(INOUT) :: colptr
  END SUBROUTINE dCompRow_to_CompCol
END INTERFACE

PUBLIC :: dCompRow_to_CompCol

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! dPrint_CompCol_Matrix(char *what, SuperMatrix *A)

INTERFACE
  SUBROUTINE dPrint_CompCol_Matrix(what, A) BIND(C, name="dPrint_CompCol_Matrix")
    IMPORT :: C_PTR, C_CHAR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: A
#else
    TYPE(SuperMatrix), INTENT(IN) :: A
#endif
    CHARACTER(1, kind=C_CHAR), INTENT(in) :: what(*)
  END SUBROUTINE dPrint_CompCol_Matrix
END INTERFACE

PUBLIC :: dPrint_CompCol_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dPrint_SuperNode_Matrix(char *what, SuperMatrix *A)

INTERFACE
  SUBROUTINE dPrint_SuperNode_Matrix(what, A) &
    & BIND(C, name="dPrint_SuperNode_Matrix")
    IMPORT :: C_PTR, C_CHAR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: A
#else
    TYPE(SuperMatrix), INTENT(IN) :: A
#endif
    CHARACTER(1, kind=C_CHAR), INTENT(in) :: what(*)
  END SUBROUTINE dPrint_SuperNode_Matrix
END INTERFACE

PUBLIC :: dPrint_SuperNode_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dPrint_Dense_Matrix(char *what, SuperMatrix *A)

INTERFACE
  SUBROUTINE dPrint_Dense_Matrix(what, A) &
    & BIND(C, name="dPrint_Dense_Matrix")
    IMPORT :: C_PTR, C_CHAR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: A
#else
    TYPE(SuperMatrix), INTENT(IN) :: A
#endif
    CHARACTER(1, kind=C_CHAR), INTENT(in) :: what(*)
  END SUBROUTINE dPrint_Dense_Matrix
END INTERFACE

PUBLIC :: dPrint_Dense_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! brief Diagnostic print of column "jcol" in the U/L factor.
! void
! dprint_lu_col(char *msg, int jcol, int pivrow, int *xprune, GlobalLU_t *Glu)

INTERFACE
  SUBROUTINE dprint_lu_col(msg, jcol, pivrow, xprune, Glu) &
    & BIND(C, name="dprint_lu_col")
    IMPORT :: C_CHAR, C_INT, C_PTR
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: msg
    INTEGER(C_INT), VALUE, INTENT(IN) :: jcol
    INTEGER(C_INT), VALUE, INTENT(IN) :: pivrow
    INTEGER(C_INT), INTENT(IN) :: xprune(*)
    TYPE(C_PTR), INTENT(IN) :: Glu
  END SUBROUTINE dprint_lu_col
END INTERFACE

PUBLIC :: dprint_lu_col

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! brief Check whether tempv[] == 0. This should be true before and
! after calling any numeric routines, i.e., "panel_bmod" and "column_bmod".
! void dcheck_tempv(int n, double *tempv)

INTERFACE
  SUBROUTINE dcheck_tempv(n, tempv) &
    & BIND(C, name="dcheck_tempv")
    IMPORT :: C_INT, C_DOUBLE
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    REAL(C_DOUBLE), INTENT(IN) :: tempv(*)
  END SUBROUTINE dcheck_tempv
END INTERFACE

PUBLIC :: dcheck_tempv

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! dGenXtrue(int n, int nrhs, double *x, int ldx)

INTERFACE
  SUBROUTINE dGenXtrue(n, nrhs, x, ldx) &
    & BIND(C, name="dGenXtrue")
    IMPORT :: C_INT, C_DOUBLE
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: nrhs
    REAL(C_DOUBLE), INTENT(INOUT) :: x(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: ldx
  END SUBROUTINE dGenXtrue
END INTERFACE

PUBLIC :: dGenXtrue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Let rhs[i] = sum of i-th row of A, so the solution vector is all 1's
! void
! dFillRHS(trans_t trans, int nrhs, double *x, int ldx,
!          SuperMatrix *A, SuperMatrix *B)

INTERFACE
  SUBROUTINE dFillRHS(trans, nrhs, x, ldx, A, B) &
    & BIND(C, name="dFillRHS")
    IMPORT :: C_INT, C_DOUBLE, SuperMatrix
    INTEGER(C_INT), VALUE, INTENT(IN) :: trans
    INTEGER(C_INT), VALUE, INTENT(IN) :: nrhs
    REAL(C_DOUBLE), INTENT(INOUT) :: x(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: ldx
    TYPE(SuperMatrix), INTENT(IN) :: A
    TYPE(SuperMatrix), INTENT(IN) :: B
  END SUBROUTINE dFillRHS
END INTERFACE

PUBLIC :: dFillRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! ! \brief Fills a double precision array with a given value.
! void
! dfill(double *a, int alen, double dval)

INTERFACE
  SUBROUTINE dfill(a, alen, dval) &
    & BIND(C, name="dfill")
    IMPORT :: C_DOUBLE, C_INT
    REAL(C_DOUBLE), INTENT(INOUT) :: a(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: alen
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: dval
  END SUBROUTINE dfill
END INTERFACE

PUBLIC :: dfill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! brief Check the inf-norm of the error vector
! void dinf_norm_error(int nrhs, SuperMatrix *X, double *xtrue)

INTERFACE
  SUBROUTINE dinf_norm_error(nrhs, X, xtrue) &
    & BIND(C, name="dinf_norm_error")
    IMPORT :: C_INT, C_PTR, C_DOUBLE
    INTEGER(C_INT), VALUE, INTENT(IN) :: nrhs
    TYPE(C_PTR), INTENT(IN) :: X
    REAL(C_DOUBLE), INTENT(IN) :: xtrue(*)
  END SUBROUTINE dinf_norm_error
END INTERFACE

PUBLIC :: dinf_norm_error

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! brief Print performance of the code.
! void
! dPrintPerf(SuperMatrix *L, SuperMatrix *U, mem_usage_t *mem_usage,
! double rpg, double rcond, double *ferr,
! double *berr, char *equed, SuperLUStat_t *stat)

INTERFACE
  SUBROUTINE dPrintPerf(L, U, mem_usage, rpg, rcond, ferr, &
    & berr, equed, stat) &
    & BIND(C, name="dPrintPerf")
    IMPORT :: C_PTR, C_DOUBLE, C_CHAR
    TYPE(C_PTR), INTENT(IN) :: L
    TYPE(C_PTR), INTENT(IN) :: U
    TYPE(C_PTR), INTENT(IN) :: mem_usage
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: rpg
    REAL(C_DOUBLE), VALUE, INTENT(in) :: rcond
    REAL(C_DOUBLE), INTENT(IN) :: ferr(*)
    REAL(C_DOUBLE), INTENT(IN) :: berr(*)
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: equed(*)
    TYPE(C_PTR), INTENT(IN) :: stat
  END SUBROUTINE dPrintPerf
END INTERFACE

PUBLIC :: dPrintPerf

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! int
! print_double_vec(char *what, int n, double *vec)

INTERFACE
  SUBROUTINE print_double_vec(what, n, vec) &
    & BIND(C, name="print_double_vec")
    IMPORT :: C_CHAR, C_INT, C_DOUBLE
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: what(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    REAL(C_DOUBLE), INTENT(IN) :: vec(*)
  END SUBROUTINE print_double_vec
END INTERFACE

PUBLIC :: print_double_vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! extern int     dQuerySpace (SuperMatrix *, SuperMatrix *, mem_usage_t *);

INTERFACE
  SUBROUTINE dQuerySpace(A, B, mem) &
  & BIND(C, name="dQuerySpace")
    IMPORT :: SuperMatrix, mem_usage_t
    TYPE(SuperMatrix), INTENT(in) :: A, B
    TYPE(mem_usage_t), INTENT(in) :: mem
  END SUBROUTINE dQuerySpace
END INTERFACE

PUBLIC :: dQuerySpace

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SuperLU_dUtil_Methods
