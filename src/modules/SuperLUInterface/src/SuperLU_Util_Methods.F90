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

MODULE SuperLU_Util_Methods
USE ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE, C_CHAR, C_FLOAT, &
  & C_SIZE_T
USE SuperLU_Types
IMPLICIT NONE

PRIVATE

#include "./include/macros.inc"

!----------------------------------------------------------------------------
!                                                       set_default_options
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE set_default_options(options) BIND(C, &
    & NAME='set_default_options')
    IMPORT superlu_options_t
    TYPE(superlu_options_t), INTENT(INOUT) :: options
  END SUBROUTINE set_default_options
END INTERFACE

PUBLIC :: set_default_options

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Set the default values for the options argument for ILU.
! void ilu_set_default_options(superlu_options_t *options)

INTERFACE
  SUBROUTINE ilu_set_default_options(options) &
    & BIND(C, name="ilu_set_default_options")
    IMPORT :: superlu_options_t
    TYPE(superlu_options_t), INTENT(inout) :: options
  END SUBROUTINE ilu_set_default_options
END INTERFACE

PUBLIC :: ilu_set_default_options

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! brief Print the options setting.
! void print_options(superlu_options_t *options)

INTERFACE
  SUBROUTINE print_options(options) &
    & BIND(C, name="print_options")
    IMPORT :: superlu_options_t
    TYPE(superlu_options_t), INTENT(IN) :: options
  END SUBROUTINE print_options
END INTERFACE

PUBLIC :: print_options

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Print the options setting.
! void print_ilu_options(superlu_options_t *options)

INTERFACE
  SUBROUTINE print_ilu_options(options) &
    & BIND(C, name="print_ilu_options")
    IMPORT :: superlu_options_t
    TYPE(superlu_options_t), INTENT(IN) :: options
  END SUBROUTINE print_ilu_options
END INTERFACE

PUBLIC :: print_ilu_options

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Deallocate the structure pointing to the actual storage of the matrix. */
! void
! Destroy_SuperMatrix_Store(SuperMatrix *A)

INTERFACE
  SUBROUTINE Destroy_SuperMatrix_Store(A) &
    & BIND(C, name="Destroy_SuperMatrix_Store")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_SuperMatrix_Store
END INTERFACE

PUBLIC :: Destroy_SuperMatrix_Store

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Deallocate the structure pointing to the actual storage of the matrix. */
! void
! extern void    Destroy_CompCol_Matrix(SuperMatrix *);

INTERFACE
  SUBROUTINE Destroy_CompCol_Matrix(A) &
    & BIND(C, name="Destroy_CompCol_Matrix")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_CompCol_Matrix
END INTERFACE

PUBLIC :: Destroy_CompCol_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Deallocate the structure pointing to the actual storage of the matrix. */
! void
! Destroy_SuperMatrix_Store(SuperMatrix *A)

INTERFACE
  SUBROUTINE Destroy_CompRow_Matrix(A) &
    & BIND(C, name="Destroy_CompRow_Matrix")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_CompRow_Matrix
END INTERFACE

PUBLIC :: Destroy_CompRow_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Deallocate the structure pointing to the actual storage of the matrix. */
! void
! Destroy_SuperMatrix_Store(SuperMatrix *A)

INTERFACE
  SUBROUTINE Destroy_SuperNode_Matrix(A) &
    & BIND(C, name="Destroy_SuperNode_Matrix")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_SuperNode_Matrix
END INTERFACE

PUBLIC :: Destroy_SuperNode_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Deallocate the structure pointing to the actual storage of the matrix. */
! void
! Destroy_SuperMatrix_Store(SuperMatrix *A)

INTERFACE
  SUBROUTINE Destroy_CompCol_Permuted(A) &
    & BIND(C, name="Destroy_CompCol_Permuted")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_CompCol_Permuted
END INTERFACE

PUBLIC :: Destroy_CompCol_Permuted

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE Destroy_Dense_Matrix(A) &
    & BIND(C, name="Destroy_Dense_Matrix")
    IMPORT :: C_PTR, SuperMatrix
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(INOUT) :: A
#else
    TYPE(SuperMatrix), INTENT(INOUT) :: A
#endif
  END SUBROUTINE Destroy_Dense_Matrix
END INTERFACE

PUBLIC :: Destroy_Dense_Matrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Count the total number of nonzeros in factors L and U,  and in the symmetrically reduced L.
! void
! countnz(const int n, int *xprune, int *nnzL, int *nnzU, GlobalLU_t *Glu)

INTERFACE
  SUBROUTINE countnz(n, xprune, nnzL, nnzU, Glu) &
    & BIND(C, name="countnz")
    IMPORT :: C_INT, C_PTR, GlobalLU_t
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), INTENT(IN) :: xprune(*)
    INTEGER(C_INT), INTENT(INOUT) :: nnzL
    INTEGER(C_INT), INTENT(INOUT) :: nnzU
    TYPE(GlobalLU_t), INTENT(IN) :: Glu
  END SUBROUTINE countnz
END INTERFACE

PUBLIC :: countnz

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! !brief Count the total number of nonzeros in factors L and U.
! void
! ilu_countnz(const int n, int *nnzL, int *nnzU, GlobalLU_t *Glu)

INTERFACE
  SUBROUTINE ilu_countnz(n, nnzL, nnzU, Glu) &
    & BIND(C, name="ilu_countnz")
    IMPORT :: C_INT, C_PTR, GlobalLU_t
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), INTENT(INOUT) :: nnzL
    INTEGER(C_INT), INTENT(INOUT) :: nnzU
    TYPE(GlobalLU_t), INTENT(IN) :: Glu
  END SUBROUTINE ilu_countnz
END INTERFACE

PUBLIC :: ilu_countnz

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! TODO

!brief Diagnostic print of segment info after panel_dfs().
! void print_panel_seg(int n, int w, int jcol, int nseg,
!                      int *segrep, int *repfnz)

!----------------------------------------------------------------------------
!
!---------------------------------------------------------------------------

! void
! StatInit(SuperLUStat_t *stat)

INTERFACE
  SUBROUTINE StatInit(stat) &
    & BIND(C, name="StatInit")
#ifdef SUPERLU_CPTR_ONLY
    IMPORT :: C_PTR
    TYPE(C_PTR), INTENT(IN) :: stat
#else
    IMPORT :: SuperLUStat_t
    TYPE(SuperLUStat_t), INTENT(IN) :: stat
#endif
  END SUBROUTINE StatInit
END INTERFACE

PUBLIC :: StatInit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! StatPrint(SuperLUStat_t *stat)

INTERFACE
  SUBROUTINE StatPrint(stat) &
    & BIND(C, name="StatPrint")
    IMPORT :: C_PTR, SuperLUStat_t
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: stat
#else
    TYPE(SuperLUStat_t), INTENT(IN) :: stat
#endif
  END SUBROUTINE StatPrint
END INTERFACE

PUBLIC :: StatPrint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! void
! StatFree(SuperLUStat_t *stat)

INTERFACE
  SUBROUTINE StatFree(stat) &
    & BIND(C, name="StatFree")
    IMPORT :: C_PTR, SuperLUStat_t
#ifdef SUPERLU_CPTR_ONLY
    TYPE(C_PTR), INTENT(IN) :: stat
#else
    TYPE(SuperLUStat_t), INTENT(IN) :: stat
#endif
  END SUBROUTINE StatFree
END INTERFACE

PUBLIC :: StatFree

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! flops_t
! LUFactFlops(SuperLUStat_t *stat)

INTERFACE
  FUNCTION LUFactFlops(stat) RESULT(ans) &
    & BIND(C, name="LUFactFlops")
    IMPORT :: C_PTR
    TYPE(C_PTR), INTENT(IN) :: stat
    TYPE(C_PTR) :: ans
  END FUNCTION LUFactFlops
END INTERFACE

PUBLIC :: LUFactFlops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! flops_t
! LUSolveFlops(SuperLUStat_t *stat)

INTERFACE
  FUNCTION LUSolveFlops(stat) RESULT(ans) &
    & BIND(C, name="LUSolveFlops")
    IMPORT :: C_PTR
    TYPE(C_PTR), INTENT(IN) :: stat
    TYPE(C_PTR) :: ans
  END FUNCTION LUSolveFlops
END INTERFACE

PUBLIC :: LUSolveFlops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Fills an integer array with a given value.
! void ifill(int *a, int alen, int ival)

INTERFACE
  SUBROUTINE ifill(a, alen, ival) &
    & BIND(C, name="ifill")
    IMPORT :: C_INT
    INTEGER(C_INT), INTENT(INOUT) :: a(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: alen
    INTEGER(C_INT), VALUE, INTENT(IN) :: ival
  END SUBROUTINE ifill
END INTERFACE

PUBLIC :: ifill

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Get the statistics of the supernodes
! void super_stats(int nsuper, int *xsup)

INTERFACE
  SUBROUTINE super_stats(nsuper, xsup) &
    & BIND(C, name="super_stats")
    IMPORT :: C_INT
    INTEGER(C_INT), VALUE, INTENT(IN) :: nsuper
    INTEGER(C_INT), INTENT(IN) :: xsup(*)
  END SUBROUTINE super_stats
END INTERFACE

PUBLIC :: super_stats

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! float SpaSize(int n, int np, float sum_npw)

INTERFACE
  SUBROUTINE SpaSize(n, np, sum_npw) &
    & BIND(C, name="SpaSize")
    IMPORT :: C_INT, C_FLOAT
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: np
    REAL(C_FLOAT), VALUE, INTENT(IN) :: sum_npw
  END SUBROUTINE SpaSize
END INTERFACE

PUBLIC :: SpaSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! float DenseSize(int n, float sum_nw)

INTERFACE
  FUNCTION DenseSize(n, sum_nw) RESULT(ans) &
  & BIND(C, name="DenseSize")
    IMPORT :: C_INT, C_FLOAT
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    REAL(C_FLOAT), VALUE, INTENT(IN) :: sum_nw
    REAL(C_FLOAT) :: ans
  END FUNCTION DenseSize
END INTERFACE

PUBLIC :: DenseSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Check whether repfnz[] == EMPTY after reset.
! void check_repfnz(int n, int w, int jcol, int *repfnz)

INTERFACE

  SUBROUTINE check_repfnz(n, w, jcol, repfnz) &
    & BIND(C, name="check_repfnz")
    IMPORT :: C_INT
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), VALUE, INTENT(IN) :: w
    INTEGER(C_INT), VALUE, INTENT(IN) :: jcol
    INTEGER(C_INT), INTENT(IN) :: repfnz(*)
  END SUBROUTINE check_repfnz
END INTERFACE

PUBLIC :: check_repfnz

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!brief Print a summary of the testing results. */
! void
! PrintSumm(char *type, int nfail, int nrun, int nerrs)

INTERFACE
  SUBROUTINE PrintSumm(type_, nfail, nrun, nerrs) &
    & BIND(C, name="PrintSumm")
    IMPORT :: C_CHAR, C_INT
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: type_(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: nfail
    INTEGER(C_INT), VALUE, INTENT(IN) :: nrun
    INTEGER(C_INT), VALUE, INTENT(IN) :: nerrs
  END SUBROUTINE PrintSumm
END INTERFACE

PUBLIC :: PrintSumm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! int print_int_vec(char *what, int n, int *vec)

INTERFACE
  FUNCTION print_int_vec(what, n, vec) RESULT(ans) &
    & BIND(C, name="print_int_vec")
    IMPORT :: C_CHAR, C_INT
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: what(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: n
    INTEGER(C_INT), INTENT(IN) :: vec(*)
    INTEGER(C_INT) :: ans
  END FUNCTION print_int_vec
END INTERFACE

PUBLIC :: print_int_vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! int slu_PrintInt10(char *name, int len, int *x)

INTERFACE
  FUNCTION slu_PrintInt10(name, len, x) RESULT(ans) &
    & BIND(C, name="print_int_vec")
    IMPORT :: C_CHAR, C_INT
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: name(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: len
    INTEGER(C_INT), INTENT(IN) :: x(*)
    INTEGER(C_INT) :: ans
  END FUNCTION slu_PrintInt10
END INTERFACE

PUBLIC :: slu_PrintInt10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE superlu_free(addr) &
  & BIND(C, name="superlu_free")
    IMPORT :: C_PTR
    TYPE(C_PTR), INTENT(in) :: addr
  END SUBROUTINE superlu_free
END INTERFACE

PUBLIC :: superlu_free

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION superlu_malloc(size) RESULT(ans) &
    & BIND(C, name="superlu_malloc")
    IMPORT :: C_PTR, C_SIZE_T
    TYPE(C_PTR) :: ans
    INTEGER(C_SIZE_T) :: size
  END FUNCTION superlu_malloc
END INTERFACE

PUBLIC :: superlu_malloc

END MODULE SuperLU_Util_Methods
