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

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Direct solver SuperLU for [[CSRMatrix_]]
!
! [[LinSolve]] solves `A X = B` with the SuperLU routine `dgssvx`. The
! SuperLU data (a copy of the matrix in compressed column form, the
! right-hand sides, the options, the permutations, the scalings and the
! L and U factors) are kept in `A%slu`, so that later calls can factor
! a matrix with the same sparsity pattern again, or reuse the factors
! for a new right-hand side (`isFactored`). [[SuperluDeallocate]] frees
! them.
!
! The other routines are the steps of [[LinSolve]]. All of them need
! the SuperLU library (`USE_SuperLU`); without it they stop with an
! error.
!
! Changes:
!   - 2026-11-17: the factors are kept for isFactored, new argument
!     isFreeFactors (easifem/base#358); reformatted with obj_ names and
!     docblocks

MODULE CSRMatrix_Superlu
USE BaseType, ONLY: CSRMatrix_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE
PRIVATE

PUBLIC :: InitiateSuperluA
PUBLIC :: InitiateSuperluRHS
PUBLIC :: LinSolve
PUBLIC :: SuperluDeallocate

!----------------------------------------------------------------------------
!                                                    InitiateSuperluA@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy the matrix to the SuperLU matrix A
!
! Description:
!   Allocates the compressed column copy of `obj%A` in `obj%slu` and
!   creates the SuperLU matrix `obj%slu%A`. The CSR storage of row-major
!   `obj` is the CSC storage of its transpose, which is why LinSolve
!   solves with `Trans = TRANS`. Stops if it was already initiated.

INTERFACE InitiateSuperluA
  MODULE SUBROUTINE obj_InitiateSuperluA(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_InitiateSuperluA
END INTERFACE InitiateSuperluA

!----------------------------------------------------------------------------
!                                                         SetSuperluA@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy new values of the matrix to the SuperLU matrix A
!
! Description:
!   The sparsity pattern must be the one of [[InitiateSuperluA]]; only
!   the values are copied.

INTERFACE SetSuperluA
  MODULE SUBROUTINE obj_SetSuperluA(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_SetSuperluA
END INTERFACE SetSuperluA

!----------------------------------------------------------------------------
!                                                  InitiateSuperluRHS@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Allocate the right-hand sides and the solutions
!
! Description:
!   Copies `rhs` (one right-hand side) to `obj%slu%rhs` and creates the
!   SuperLU dense matrices `B` and `X`. Stops if they were already
!   initiated.

INTERFACE InitiateSuperluRHS
  MODULE SUBROUTINE obj_InitiateSuperluRHS1(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! right-hand side
  END SUBROUTINE obj_InitiateSuperluRHS1
END INTERFACE InitiateSuperluRHS

!----------------------------------------------------------------------------
!                                                  InitiateSuperluRHS@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Allocate the right-hand sides and the solutions
!
! Description:
!   Copies `rhs` (one right-hand side per column) to `obj%slu%rhs` and
!   creates the SuperLU dense matrices `B` and `X`. Stops if they were
!   already initiated.

INTERFACE InitiateSuperluRHS
  MODULE SUBROUTINE obj_InitiateSuperluRHS2(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: rhs(:, :)
    !! right-hand sides, one per column
  END SUBROUTINE obj_InitiateSuperluRHS2
END INTERFACE InitiateSuperluRHS

!----------------------------------------------------------------------------
!                                                       SetSuperluRHS@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy new right-hand sides
!
! Description:
!   The size must be the one of [[InitiateSuperluRHS]].

INTERFACE SetSuperluRHS
  MODULE SUBROUTINE obj_SetSuperluRHS1(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: rhs(:)
    !! right-hand side
  END SUBROUTINE obj_SetSuperluRHS1
END INTERFACE SetSuperluRHS

!----------------------------------------------------------------------------
!                                                       SetSuperluRHS@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy new right-hand sides
!
! Description:
!   The size must be the one of [[InitiateSuperluRHS]].

INTERFACE SetSuperluRHS
  MODULE SUBROUTINE obj_SetSuperluRHS2(obj, rhs)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: rhs(:, :)
    !! right-hand sides, one per column
  END SUBROUTINE obj_SetSuperluRHS2
END INTERFACE SetSuperluRHS

!----------------------------------------------------------------------------
!                                                         GetSuperluX@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy the solutions

INTERFACE GetSuperluX
  MODULE SUBROUTINE obj_GetSuperluX1(obj, x)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! solution
  END SUBROUTINE obj_GetSuperluX1
END INTERFACE GetSuperluX

!----------------------------------------------------------------------------
!                                                         GetSuperluX@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Copy the solutions

INTERFACE GetSuperluX
  MODULE SUBROUTINE obj_GetSuperluX2(obj, x)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! solutions, one per column
  END SUBROUTINE obj_GetSuperluX2
END INTERFACE GetSuperluX

!----------------------------------------------------------------------------
!                                              InitiateSuperluOptions@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Set the default SuperLU options
!
! Description:
!   `set_default_options` of SuperLU, then equilibration, transposed
!   solve (`Trans = TRANS`), COLAMD ordering, new factorization, double
!   precision iterative refinement, pivot growth, partial pivoting
!   (threshold 1) and condition number.

INTERFACE InitiateSuperluOptions
  MODULE SUBROUTINE obj_InitiateSuperluOptions(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_InitiateSuperluOptions
END INTERFACE InitiateSuperluOptions

!----------------------------------------------------------------------------
!                                                   SetSuperluOptions@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Set SuperLU options
!
! Only the options that are present are changed in
! `obj%slu%options`.
!
! Changes:
!   - 2026-11-18: argument names in camelCase; ILU_DropRule,
!     ILU_DropTol, ILU_FillFactor and ILU_MILU are renamed iluDropRule,
!     iluDropTol, iluFillFactor and iluMilu

INTERFACE SetSuperluOptions
  MODULE SUBROUTINE obj_SetSuperluOptions(obj, fact, equil, colPerm, trans, &
                                          iterRefine, diagPivotThresh, &
                                          symmetricMode, pivotGrowth, &
                                          conditionNumber, rowPerm, &
                                          iluDropRule, iluDropTol, &
                                          iluFillFactor, iluMilu, &
                                          printStat)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fact
    !! Fact_t%DOFACT, SamePattern, SamePattern_SameRowPerm or FACTORED
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: equil
    !! equilibrate the matrix, yes_no_t%YES or NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPerm
    !! column ordering, Colperm_t%NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD or
    !! MY_PERMC
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: trans
    !! Trans_t%NOTRANS, TRANS or CONJ
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterRefine
    !! iterative refinement, IterRefine_t%NOREFINE, SLU_SINGLE, SLU_DOUBLE or
    !! SLU_EXTRA
    REAL(DFP), OPTIONAL, INTENT(IN) :: diagPivotThresh
    !! threshold of partial pivoting, between 0 and 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: symmetricMode
    !! symmetric mode, yes_no_t%YES or NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pivotGrowth
    !! compute the reciprocal pivot growth, yes_no_t%YES or NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: conditionNumber
    !! estimate the condition number, yes_no_t%YES or NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: rowPerm
    !! row permutation, rowperm_t
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iluDropRule
    !! drop rule of ILU
    REAL(DFP), OPTIONAL, INTENT(IN) :: iluDropTol
    !! drop tolerance of ILU
    REAL(DFP), OPTIONAL, INTENT(IN) :: iluFillFactor
    !! fill factor of ILU
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iluMilu
    !! modified ILU, milu_t
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: printStat
    !! print the statistics, yes_no_t%YES or NO
  END SUBROUTINE obj_SetSuperluOptions
END INTERFACE SetSuperluOptions

!----------------------------------------------------------------------------
!                                                       SuperluDGSSVX@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Call dgssvx of SuperLU
!
! Description:
!   Solves with the options, the matrix, the right-hand sides and the
!   work arrays in `obj%slu`. The status is in `obj%slu%info`.

INTERFACE SuperluDGSSVX
  MODULE SUBROUTINE obj_SuperluDGSSVX(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_SuperluDGSSVX
END INTERFACE SuperluDGSSVX

!----------------------------------------------------------------------------
!                                          InitiateSuperluDGSSVXParam@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Allocate the work arrays of dgssvx
!
! Description:
!   The permutations `perm_c`, `perm_r`, the elimination tree `etree`,
!   the scalings `R`, `C` and the error bounds `ferr`, `berr`.

INTERFACE InitiateSuperluDGSSVXParam
  MODULE SUBROUTINE obj_InitiateSuperluDGSSVXParam(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_InitiateSuperluDGSSVXParam
END INTERFACE InitiateSuperluDGSSVXParam

!----------------------------------------------------------------------------
!                                                  SuperluDisplayStat@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Display the statistics of the last solve
!
! Description:
!   Only when `PrintStat = yes_no_t%YES`: pivot growth, condition number,
!   refinement steps and error bounds, the SuperLU statistics and the
!   memory used.

INTERFACE SuperluDisplayStat
  MODULE SUBROUTINE obj_SuperluDisplayStat(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_SuperluDisplayStat
END INTERFACE SuperluDisplayStat

!----------------------------------------------------------------------------
!                                                   SuperluDeallocate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Free all SuperLU data of a matrix
!
! Description:
!   Frees the arrays, the SuperLU matrices A, B, X, L and U, the work
!   array and the statistics, and deallocates `obj%slu`. It is called by
!   the Deallocate of [[CSRMatrix_]].

INTERFACE SuperluDeallocate
  MODULE SUBROUTINE obj_SuperluDeallocate(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! matrix, obj%slu holds the SuperLU data
  END SUBROUTINE obj_SuperluDeallocate
END INTERFACE SuperluDeallocate

!----------------------------------------------------------------------------
!                                                            LinSolve@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Solve A X = B with SuperLU, one right-hand side
!
! Description:
!   Solves `a x = b` with SuperLU (`dgssvx`).
!
!   - The first call sets up the SuperLU data of `a` and factors `a`.
!   - A later call with `isFactored = .TRUE.` uses the stored factors
!     (`Fact = FACTORED`), so only the new right-hand side is solved; new
!     values of `a` are ignored.
!   - With `isFactored = .FALSE.` (default) the matrix is factored again
!     with the same sparsity pattern (`Fact = SamePattern`).
!   - The factors are kept after the solve. With `isFreeFactors = .TRUE.`
!     they are freed at the end to save memory; the next call must then use
!     `isFactored = .FALSE.`, a call with `isFactored = .TRUE.` stops with
!     an error.
!
!   The options (colPerm to printStat) are applied at every call.
!
! Changes:
!   - 2026-11-17: the L and U factors are kept after a solve, so that
!     `isFactored = .TRUE.` works (it crashed before); new argument
!     `isFreeFactors`, easifem/base#358
!   - 2026-11-18: argument names in camelCase (x, a, b and the
!     options)
!
! Examples
!
! Full example: src/modules/CSRMatrix/examples/LinSolve_test_1.F90

INTERFACE LinSolve
  MODULE SUBROUTINE obj_LinSolve1(x, a, b, isTranspose, isFactored, colPerm, &
                                  equil, iterRefine, pivotGrowth, &
                                  diagPivotThresh, conditionNumber, &
                                  symmetricMode, printStat, info, &
                                  isFreeFactors)
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! solution
    TYPE(CSRMatrix_), INTENT(INOUT) :: a
    !! matrix, a%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: b(:)
    !! right-hand side, not changed
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! solve transpose(a) x = b instead, default false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! use the stored factors of the last factorization, default false
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPerm
    !! column ordering, Colperm_t%NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD or
    !! MY_PERMC, default COLAMD
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: equil
    !! equilibrate the matrix, yes_no_t%YES or NO, default YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterRefine
    !! iterative refinement, IterRefine_t%NOREFINE, SLU_SINGLE, SLU_DOUBLE or
    !! SLU_EXTRA, default SLU_DOUBLE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pivotGrowth
    !! compute the reciprocal pivot growth, yes_no_t%YES or NO, default NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: diagPivotThresh
    !! threshold of partial pivoting, between 0 and 1, default 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: conditionNumber
    !! estimate the condition number, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: symmetricMode
    !! symmetric mode, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: printStat
    !! print the statistics, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info of dgssvx, zero on success
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFreeFactors
    !! free the L and U factors after the solve, default false
  END SUBROUTINE obj_LinSolve1
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                            LinSolve@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Solve A X = B with SuperLU, several right-hand sides
!
! Description:
!   Solves `a x = b` with SuperLU (`dgssvx`).
!
!   - The first call sets up the SuperLU data of `a` and factors `a`.
!   - A later call with `isFactored = .TRUE.` uses the stored factors
!     (`Fact = FACTORED`), so only the new right-hand side is solved; new
!     values of `a` are ignored.
!   - With `isFactored = .FALSE.` (default) the matrix is factored again
!     with the same sparsity pattern (`Fact = SamePattern`).
!   - The factors are kept after the solve. With `isFreeFactors = .TRUE.`
!     they are freed at the end to save memory; the next call must then use
!     `isFactored = .FALSE.`, a call with `isFactored = .TRUE.` stops with
!     an error.
!
!   The options (colPerm to printStat) are applied at the first call only;
!   later calls change only Fact and Trans.
!
! Changes:
!   - 2026-11-17: the L and U factors are kept after a solve, so that
!     `isFactored = .TRUE.` works (it crashed before); new argument
!     `isFreeFactors`, easifem/base#358
!   - 2026-11-18: argument names in camelCase (x, a, b and the
!     options)
!
! Examples
!
! Full example: src/modules/CSRMatrix/examples/LinSolve_test_1.F90

INTERFACE LinSolve
  MODULE SUBROUTINE obj_LinSolve2(x, a, b, isTranspose, isFactored, colPerm, &
                                  equil, iterRefine, pivotGrowth, &
                                  diagPivotThresh, conditionNumber, &
                                  symmetricMode, printStat, info, &
                                  isFreeFactors)
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! solutions, one per column
    TYPE(CSRMatrix_), INTENT(INOUT) :: a
    !! matrix, a%slu holds the SuperLU data
    REAL(DFP), INTENT(IN) :: b(:, :)
    !! right-hand sides, one per column, not changed
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! solve transpose(a) x = b instead, default false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! use the stored factors of the last factorization, default false
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPerm
    !! column ordering, Colperm_t%NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD or
    !! MY_PERMC, default COLAMD
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: equil
    !! equilibrate the matrix, yes_no_t%YES or NO, default YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterRefine
    !! iterative refinement, IterRefine_t%NOREFINE, SLU_SINGLE, SLU_DOUBLE or
    !! SLU_EXTRA, default SLU_DOUBLE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pivotGrowth
    !! compute the reciprocal pivot growth, yes_no_t%YES or NO, default NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: diagPivotThresh
    !! threshold of partial pivoting, between 0 and 1, default 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: conditionNumber
    !! estimate the condition number, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: symmetricMode
    !! symmetric mode, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: printStat
    !! print the statistics, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info of dgssvx, zero on success
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFreeFactors
    !! free the L and U factors after the solve, default false
  END SUBROUTINE obj_LinSolve2
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                            LinSolve@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Solve A X = B with SuperLU, solution returned in B
!
! Description:
!   Solves `a x = b` with SuperLU (`dgssvx`).
!
!   - The first call sets up the SuperLU data of `a` and factors `a`.
!   - A later call with `isFactored = .TRUE.` uses the stored factors
!     (`Fact = FACTORED`), so only the new right-hand side is solved; new
!     values of `a` are ignored.
!   - With `isFactored = .FALSE.` (default) the matrix is factored again
!     with the same sparsity pattern (`Fact = SamePattern`).
!   - The factors are kept after the solve. With `isFreeFactors = .TRUE.`
!     they are freed at the end to save memory; the next call must then use
!     `isFactored = .FALSE.`, a call with `isFactored = .TRUE.` stops with
!     an error.
!
!   The options (colPerm to printStat) are applied at the first call only;
!   later calls change only Fact and Trans.
!
! Changes:
!   - 2026-11-17: the L and U factors are kept after a solve, so that
!     `isFactored = .TRUE.` works (it crashed before); new argument
!     `isFreeFactors`, easifem/base#358
!   - 2026-11-18: argument names in camelCase (a, b and the
!     options)
!
! Examples
!
! Full example: src/modules/CSRMatrix/examples/LinSolve_test_1.F90

INTERFACE LinSolve
  MODULE SUBROUTINE obj_LinSolve3(a, b, isTranspose, isFactored, colPerm, &
                                  equil, iterRefine, pivotGrowth, &
                                  diagPivotThresh, conditionNumber, &
                                  symmetricMode, printStat, info, &
                                  isFreeFactors)
    TYPE(CSRMatrix_), INTENT(INOUT) :: a
    !! matrix, a%slu holds the SuperLU data
    REAL(DFP), INTENT(INOUT) :: b(:)
    !! right-hand side on entry, solution on return
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! solve transpose(a) x = b instead, default false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! use the stored factors of the last factorization, default false
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPerm
    !! column ordering, Colperm_t%NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD or
    !! MY_PERMC, default COLAMD
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: equil
    !! equilibrate the matrix, yes_no_t%YES or NO, default YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterRefine
    !! iterative refinement, IterRefine_t%NOREFINE, SLU_SINGLE, SLU_DOUBLE or
    !! SLU_EXTRA, default SLU_DOUBLE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pivotGrowth
    !! compute the reciprocal pivot growth, yes_no_t%YES or NO, default NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: diagPivotThresh
    !! threshold of partial pivoting, between 0 and 1, default 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: conditionNumber
    !! estimate the condition number, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: symmetricMode
    !! symmetric mode, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: printStat
    !! print the statistics, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info of dgssvx, zero on success
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFreeFactors
    !! free the L and U factors after the solve, default false
  END SUBROUTINE obj_LinSolve3
END INTERFACE LinSolve

!----------------------------------------------------------------------------
!                                                            LinSolve@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Solve A X = B with SuperLU, solutions returned in B
!
! Description:
!   Solves `a x = b` with SuperLU (`dgssvx`).
!
!   - The first call sets up the SuperLU data of `a` and factors `a`.
!   - A later call with `isFactored = .TRUE.` uses the stored factors
!     (`Fact = FACTORED`), so only the new right-hand side is solved; new
!     values of `a` are ignored.
!   - With `isFactored = .FALSE.` (default) the matrix is factored again
!     with the same sparsity pattern (`Fact = SamePattern`).
!   - The factors are kept after the solve. With `isFreeFactors = .TRUE.`
!     they are freed at the end to save memory; the next call must then use
!     `isFactored = .FALSE.`, a call with `isFactored = .TRUE.` stops with
!     an error.
!
!   The options (colPerm to printStat) are applied at the first call only;
!   later calls change only Fact and Trans.
!
! Changes:
!   - 2026-11-17: the L and U factors are kept after a solve, so that
!     `isFactored = .TRUE.` works (it crashed before); new argument
!     `isFreeFactors`, easifem/base#358
!   - 2026-11-18: argument names in camelCase (a, b and the
!     options)
!
! Examples
!
! Full example: src/modules/CSRMatrix/examples/LinSolve_test_1.F90

INTERFACE LinSolve
  MODULE SUBROUTINE obj_LinSolve4(a, b, isTranspose, isFactored, colPerm, &
                                  equil, iterRefine, pivotGrowth, &
                                  diagPivotThresh, conditionNumber, &
                                  symmetricMode, printStat, info, &
                                  isFreeFactors)
    TYPE(CSRMatrix_), INTENT(INOUT) :: a
    !! matrix, a%slu holds the SuperLU data
    REAL(DFP), INTENT(INOUT) :: b(:, :)
    !! right-hand sides on entry, solutions on return, one per column
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTranspose
    !! solve transpose(a) x = b instead, default false
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFactored
    !! use the stored factors of the last factorization, default false
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: colPerm
    !! column ordering, Colperm_t%NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD or
    !! MY_PERMC, default COLAMD
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: equil
    !! equilibrate the matrix, yes_no_t%YES or NO, default YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: iterRefine
    !! iterative refinement, IterRefine_t%NOREFINE, SLU_SINGLE, SLU_DOUBLE or
    !! SLU_EXTRA, default SLU_DOUBLE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: pivotGrowth
    !! compute the reciprocal pivot growth, yes_no_t%YES or NO, default NO
    REAL(DFP), OPTIONAL, INTENT(IN) :: diagPivotThresh
    !! threshold of partial pivoting, between 0 and 1, default 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: conditionNumber
    !! estimate the condition number, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: symmetricMode
    !! symmetric mode, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: printStat
    !! print the statistics, yes_no_t%YES or NO, default NO
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: info
    !! info of dgssvx, zero on success
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isFreeFactors
    !! free the L and U factors after the solve, default false
  END SUBROUTINE obj_LinSolve4
END INTERFACE LinSolve

END MODULE CSRMatrix_Superlu
