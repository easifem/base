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

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Direct solver SuperLU for [[CSRMatrix_]]
!
! The SuperLU data of a matrix live in `obj%slu` (a pointer to
! `SuperLU_`, allocated by the first [[LinSolve]]). All routines need
! the SuperLU library (`USE_SuperLU`); without it they stop with an
! error.
!
! Changes:
!   - 2026-11-17: the factors are kept for isFactored, new argument
!     isFreeFactors (easifem/base#358); reformatted: obj_ names,
!     trailing continuation lines, SuperluError and SuperluNotLinked
!     instead of repeated ErrorMsg blocks, USE with ONLY

SUBMODULE(CSRMatrix_Superlu) Methods
USE BaseMethod, ONLY: Copy
USE BaseMethod, ONLY: Display
USE BaseMethod, ONLY: ErrorMsg
USE BaseMethod, ONLY: GetNNZ
USE BaseMethod, ONLY: Input
USE BaseMethod, ONLY: Reallocate
USE BaseMethod, ONLY: SIZE
USE GlobalData, ONLY: stderr
#ifdef USE_SuperLU
USE SuperLUInterface, ONLY: colperm_t
USE SuperLUInterface, ONLY: dCreate_CompCol_Matrix
USE SuperLUInterface, ONLY: dCreate_Dense_Matrix
USE SuperLUInterface, ONLY: Destroy_CompCol_Matrix
USE SuperLUInterface, ONLY: Destroy_SuperMatrix_Store
USE SuperLUInterface, ONLY: Destroy_SuperNode_Matrix
USE SuperLUInterface, ONLY: dgssvx
USE SuperLUInterface, ONLY: dtype_t
USE SuperLUInterface, ONLY: fact_t
USE SuperLUInterface, ONLY: IterRefine_t
USE SuperLUInterface, ONLY: mtype_t
USE SuperLUInterface, ONLY: set_default_options
USE SuperLUInterface, ONLY: StatFree
USE SuperLUInterface, ONLY: StatInit
USE SuperLUInterface, ONLY: StatPrint
USE SuperLUInterface, ONLY: stype_t
USE SuperLUInterface, ONLY: Superlu_Free
USE SuperLUInterface, ONLY: trans_t
USE SuperLUInterface, ONLY: yes_no_t
#endif
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               SuperluError
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Write an error message and stop
!
! Replaces the repeated `CALL ErrorMsg(...)` and `STOP` blocks of this
! submodule; the message, routine and line are the same as before.

SUBROUTINE SuperluError(msg, routine, line)
  CHARACTER(*), INTENT(IN) :: msg
  !! error message
  CHARACTER(*), INTENT(IN) :: routine
  !! name of the calling routine
  INTEGER(I4B), INTENT(IN) :: line
  !! line number of the call

  CALL ErrorMsg(msg=msg, file=__FILE__, routine=routine, line=line, &
                unitno=stderr)
  STOP
END SUBROUTINE SuperluError

!----------------------------------------------------------------------------
!                                                           SuperluNotLinked
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Stop because easifemBase is built without SuperLU

SUBROUTINE SuperluNotLinked(routine, line)
  CHARACTER(*), INTENT(IN) :: routine
  !! name of the calling routine
  INTEGER(I4B), INTENT(IN) :: line
  !! line number of the call

  CALL SuperluError(msg="This routine requires Superlu library, and it "// &
                    "seems this library is not linked with the "// &
                    "easifemBase", routine=routine, line=line)
END SUBROUTINE SuperluNotLinked

!----------------------------------------------------------------------------
!                                                        CheckErrorCSRMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-26
! summary: Stop if the matrix is not ready for SuperLU
!
! The values `obj%A` must be allocated, and the sparsity `obj%csr` must
! be initiated and locked.

SUBROUTINE CheckErrorCSRMatrix(obj, lineNo, routine)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  !! matrix
  INTEGER(I4B), INTENT(IN) :: lineNo
  !! line number of the call
  CHARACTER(*), INTENT(IN) :: routine
  !! name of the calling routine
  IF (.NOT. ALLOCATED(obj%A)) THEN
    CALL SuperluError(msg="CSRMatrix_::obj%A is not allocated", &
                      routine=routine, line=lineNo)
  END IF

  IF (.NOT. obj%csr%isInitiated) THEN
    CALL SuperluError(msg="CSRMatrix_::obj%csr is not initiated", &
                      routine=routine, line=lineNo)
  END IF

  IF (.NOT. obj%csr%isSparsityLock) THEN
    CALL SuperluError(msg="CSRMatrix_::obj%csr%isSparsityLock is not True", &
                      routine=routine, line=lineNo)
  END IF

END SUBROUTINE CheckErrorCSRMatrix

!----------------------------------------------------------------------------
!                                                          DestroySuperluLU
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Free the L and U factors before a new factorization
!
! LinSolve keeps L and U after a solve, so that the next call with
! isFactored = .TRUE. can use them (Fact = FACTORED). Before the matrix is
! factored again (Fact = SamePattern) the old factors are freed here,
! otherwise dgssvx allocates new ones and the old ones leak. When the
! user supplies the work array (lwork /= 0), L and U live in it and are
! not freed. Changes: easifem/base#358.

#ifdef USE_SuperLU
SUBROUTINE DestroySuperluLU(obj)
  TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  !! matrix with SuperLU data, obj%slu is associated

  IF (obj%slu%lwork .NE. 0) RETURN

  IF (obj%slu%isLInitiated) THEN
    CALL Destroy_SuperNode_Matrix(obj%slu%L)
    obj%slu%isLInitiated = .FALSE.
  END IF

  IF (obj%slu%isUInitiated) THEN
    CALL Destroy_CompCol_Matrix(obj%slu%U)
    obj%slu%isUInitiated = .FALSE.
  END IF
END SUBROUTINE DestroySuperluLU

!----------------------------------------------------------------------------
!                                                       CheckSuperluFactors
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-17
! summary: Stop if the factors are not available for isFactored = .TRUE.
!
! The factors are missing when a previous call used isFreeFactors =
! .TRUE.; reading them would crash in dgssvx (easifem/base#358).

SUBROUTINE CheckSuperluFactors(obj, routine)
  TYPE(CSRMatrix_), INTENT(IN) :: obj
  !! matrix with SuperLU data, obj%slu is associated
  CHARACTER(*), INTENT(IN) :: routine
  !! name of the calling routine

  IF (obj%slu%isLInitiated .AND. obj%slu%isUInitiated) RETURN

  CALL SuperluError(msg="isFactored = .TRUE. but the L and U factors are "// &
                    "not available (freed by isFreeFactors = .TRUE.); "// &
                    "call LinSolve with isFactored = .FALSE.", &
                    routine=routine, line=__LINE__)
END SUBROUTINE CheckSuperluFactors
#endif

!----------------------------------------------------------------------------
!                                                            InitiateSuperluA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSuperluA
#ifdef USE_SuperLU
INTEGER(I4B) :: nnz, m, n, ii

! check if csr is initiated
! check if csr sparsity is locked
! check if A is allocated
CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_InitiateSuperluA()")
IF (ALLOCATED(obj%slu%nzval) &
    .OR. ALLOCATED(obj%slu%ia) &
    .OR. ALLOCATED(obj%slu%ja)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu nzval, ia, ja are already "// &
                    "allocated", routine="obj_InitiateSuperluA()", &
                    line=__LINE__)
END IF

IF (obj%slu%isAInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%A is already Initiated", &
                    routine="obj_InitiateSuperluA()", line=__LINE__)
END IF

obj%slu%isAInitiated = .TRUE.

nnz = SIZE(obj%A)
m = SIZE(obj, 1)
n = SIZE(obj, 2)

CALL Reallocate(obj%slu%nzval, nnz)
CALL Reallocate(obj%slu%ia, m + 1)
CALL Reallocate(obj%slu%ja, nnz)

CALL Copy(x=obj%A, y=obj%slu%nzval)

DO CONCURRENT(ii=1:m + 1)
  obj%slu%ia(ii) = obj%csr%ia(ii) - 1
END DO

DO CONCURRENT(ii=1:nnz)
  obj%slu%ja(ii) = obj%csr%ja(ii) - 1
END DO

CALL dCreate_CompCol_Matrix(A=obj%slu%A, m=m, n=n, nnz=nnz, &
                            nzval=obj%slu%nzval, rowind=obj%slu%ja, &
                            colptr=obj%slu%ia, stype=stype_t%SLU_NC, &
                            dtype=dtype_t%SLU_D, mtype=mtype_t%SLU_GE)

#else
CALL SuperluNotLinked(routine="obj_InitiateSuperluA()", line=__LINE__)
#endif

END PROCEDURE obj_InitiateSuperluA

!----------------------------------------------------------------------------
!                                                                 SetSuperluA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSuperluA
#ifdef USE_SuperLU

INTEGER(I4B) :: nnz, m, n
! check if csr is initiated
! check if csr sparsity is locked
! check if A is allocated
CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_SetSuperluA()")
IF (.NOT. ALLOCATED(obj%slu%nzval) &
    .OR. .NOT. ALLOCATED(obj%slu%ia) &
    .OR. .NOT. ALLOCATED(obj%slu%ja)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu nzval, ia, ja are not allocated", &
                    routine="obj_SetSuperluA()", line=__LINE__)
END IF

IF (.NOT. obj%slu%isAInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%A is not Initiated", &
                    routine="obj_SetSuperluA()", line=__LINE__)
END IF

nnz = SIZE(obj%A)
m = SIZE(obj, 1)
n = SIZE(obj, 2)

IF (SIZE(obj%slu%nzval) .NE. nnz &
    .OR. SIZE(obj%slu%ia) .NE. m + 1 &
    .OR. SIZE(obj%slu%ja) .NE. nnz) THEN
  CALL SuperluError(msg="Superlu_::obj%slu nzval, ia, ja are allocated "// &
                    "but there is some issue with size and shape", &
                    routine="obj_SetSuperluA()", line=__LINE__)
END IF

CALL Copy(x=obj%A, y=obj%slu%nzval)

#else
CALL SuperluNotLinked(routine="obj_SetSuperluA()", line=__LINE__)
#endif

END PROCEDURE obj_SetSuperluA

!----------------------------------------------------------------------------
!                                                          InitiateSuperluRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSuperluRHS1
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii

IF (obj%slu%isBInitiated .OR. obj%slu%isXInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%B or obj%slu%x is already "// &
                    "Initiated", routine="obj_InitiateSuperluRHS1()", &
                    line=__LINE__)
END IF

obj%slu%isBInitiated = .TRUE.
obj%slu%isXInitiated = .TRUE.

IF (ALLOCATED(obj%slu%rhs) .OR. ALLOCATED(obj%slu%sol)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs or obj%slu%sol is already "// &
                    "Allocated", routine="obj_InitiateSuperluRHS1()", &
                    line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_InitiateSuperluRHS1()")

nrhs = 1
m = SIZE(rhs, 1)

CALL Reallocate(obj%slu%rhs, m, nrhs)
CALL Reallocate(obj%slu%sol, m, nrhs)

DO CONCURRENT(ii=1:m)
  obj%slu%rhs(ii, 1) = rhs(ii)
END DO

CALL dCreate_Dense_Matrix(A=obj%slu%B, m=m, n=nrhs, x=obj%slu%rhs, ldx=m, &
                          stype=stype_t%SLU_DN, dtype=dtype_t%SLU_D, &
                          mtype=mtype_t%SLU_GE)

CALL dCreate_Dense_Matrix(A=obj%slu%X, m=m, n=nrhs, x=obj%slu%sol, ldx=m, &
                          stype=stype_t%SLU_DN, dtype=dtype_t%SLU_D, &
                          mtype=mtype_t%SLU_GE)

#else
CALL SuperluNotLinked(routine="obj_InitiateSuperluRHS1()", line=__LINE__)
#endif
END PROCEDURE obj_InitiateSuperluRHS1

!----------------------------------------------------------------------------
!                                                          InitiateSuperluRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSuperluRHS2
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii, jj

IF (obj%slu%isBInitiated .OR. obj%slu%isXInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%B is already Initiated", &
                    routine="obj_InitiateSuperluRHS2()", line=__LINE__)
END IF

obj%slu%isBInitiated = .TRUE.
obj%slu%isXInitiated = .TRUE.

IF (ALLOCATED(obj%slu%rhs) .OR. ALLOCATED(obj%slu%sol)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs obj%slu%sol is already "// &
                    "Allocated", routine="obj_InitiateSuperluRHS2()", &
                    line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_InitiateSuperluRHS2()")

nrhs = SIZE(rhs, 2)
m = SIZE(rhs, 1)

CALL Reallocate(obj%slu%rhs, m, nrhs)
CALL Reallocate(obj%slu%sol, m, nrhs)

DO CONCURRENT(ii=1:m, jj=1:nrhs)
  obj%slu%rhs(ii, jj) = rhs(ii, jj)
END DO

CALL dCreate_Dense_Matrix(A=obj%slu%B, m=m, n=nrhs, x=obj%slu%rhs, ldx=m, &
                          stype=stype_t%SLU_DN, dtype=dtype_t%SLU_D, &
                          mtype=mtype_t%SLU_GE)

CALL dCreate_Dense_Matrix(A=obj%slu%X, m=m, n=nrhs, x=obj%slu%sol, ldx=m, &
                          stype=stype_t%SLU_DN, dtype=dtype_t%SLU_D, &
                          mtype=mtype_t%SLU_GE)

#else
CALL SuperluNotLinked(routine="obj_InitiateSuperluRHS2()", line=__LINE__)
#endif

END PROCEDURE obj_InitiateSuperluRHS2

!----------------------------------------------------------------------------
!                                                               SetSuperluRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSuperluRHS1
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii

IF (.NOT. obj%slu%isBInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%B is not Initiated", &
                    routine="obj_SetSuperluRHS1()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%rhs)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs is not Allocated", &
                    routine="obj_SetSuperluRHS1()", line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_SetSuperluRHS1()")

nrhs = 1
m = SIZE(rhs, 1)

IF (SIZE(obj%slu%rhs, 1) .NE. m .OR. SIZE(obj%slu%rhs, 2) .NE. nrhs) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs Allocated but shape does "// &
                    "not match", routine="obj_SetSuperluRHS1()", &
                    line=__LINE__)
END IF

DO CONCURRENT(ii=1:m)
  obj%slu%rhs(ii, 1) = rhs(ii)
END DO

#else
CALL SuperluNotLinked(routine="obj_SetSuperluRHS1()", line=__LINE__)
#endif
END PROCEDURE obj_SetSuperluRHS1

!----------------------------------------------------------------------------
!                                                               SetSuperluRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSuperluRHS2
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii, jj

IF (.NOT. obj%slu%isBInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%B is not Initiated", &
                    routine="obj_SetSuperluRHS2()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%rhs)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs is not Allocated", &
                    routine="obj_SetSuperluRHS2()", line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_SetSuperluRHS2()")

nrhs = SIZE(rhs, 2)
m = SIZE(rhs, 1)

IF (SIZE(obj%slu%rhs, 1) .NE. m .OR. SIZE(obj%slu%rhs, 2) .NE. nrhs) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs Allocated but shape does "// &
                    "not match", routine="obj_SetSuperluRHS2()", &
                    line=__LINE__)
END IF

DO CONCURRENT(ii=1:m, jj=1:nrhs)
  obj%slu%rhs(ii, jj) = rhs(ii, jj)
END DO

#else
CALL SuperluNotLinked(routine="obj_SetSuperluRHS2()", line=__LINE__)
#endif

END PROCEDURE obj_SetSuperluRHS2

!----------------------------------------------------------------------------
!                                                                 GetSuperluX
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSuperluX1
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii

IF (.NOT. obj%slu%isXInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%X is NOT Initiated", &
                    routine="obj_GetSuperluX1()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%sol)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%sol is not Allocated", &
                    routine="obj_GetSuperluX1()", line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_GetSuperluX1()")

nrhs = 1
m = SIZE(x, 1)

IF (SIZE(obj%slu%sol, 1) .NE. m .OR. SIZE(obj%slu%sol, 2) .NE. nrhs) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%rhs Allocated but shape does "// &
                    "not match", routine="obj_GetSuperluX1()", line=__LINE__)
END IF

DO CONCURRENT(ii=1:m)
  x(ii) = obj%slu%sol(ii, 1)
END DO

#else
CALL SuperluNotLinked(routine="obj_GetSuperluX1()", line=__LINE__)
#endif

END PROCEDURE obj_GetSuperluX1

!----------------------------------------------------------------------------
!                                                                 GetSuperluX
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSuperluX2
#ifdef USE_SuperLU
INTEGER(I4B) :: nrhs, m, ii, jj

IF (.NOT. obj%slu%isXInitiated) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%X is not Initiated", &
                    routine="obj_GetSuperluX2()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%sol)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%sol is not Allocated", &
                    routine="obj_GetSuperluX2()", line=__LINE__)
END IF

CALL CheckErrorCSRMatrix(obj=obj, lineNo=__LINE__, &
                         routine="obj_GetSuperluX2()")

m = SIZE(x, 1)
nrhs = SIZE(x, 2)

IF (SIZE(obj%slu%sol, 1) .NE. m .OR. SIZE(obj%slu%sol, 2) .NE. nrhs) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%sol Allocated but shape does "// &
                    "not match", routine="obj_GetSuperluX2()", line=__LINE__)
END IF

DO CONCURRENT(ii=1:m, jj=1:nrhs)
  x(ii, jj) = obj%slu%sol(ii, jj)
END DO

#else
CALL SuperluNotLinked(routine="obj_GetSuperluX2()", line=__LINE__)
#endif

END PROCEDURE obj_GetSuperluX2

!----------------------------------------------------------------------------
!                                                      InitiateSuperluOptions
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSuperluOptions
#ifdef USE_SuperLU
CALL set_default_options(obj%slu%options)
obj%slu%options%Equil = yes_no_t%YES
obj%slu%options%Trans = Trans_t%TRANS
obj%slu%options%ColPerm = colperm_t%COLAMD
obj%slu%options%Fact = Fact_t%DOFACT
obj%slu%options%IterRefine = IterRefine_t%SLU_DOUBLE
obj%slu%options%PivotGrowth = yes_no_t%YES
obj%slu%options%DiagPivotThresh = 1.0
obj%slu%options%ConditionNumber = yes_no_t%YES
#else
CALL SuperluNotLinked(routine="obj_InitiateSuperluOptions()", line=__LINE__)
#endif
END PROCEDURE obj_InitiateSuperluOptions

!----------------------------------------------------------------------------
!                                                           SetSuperluOptions
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSuperluOptions
#ifdef USE_SuperLU

IF (PRESENT(Fact)) THEN
  obj%slu%options%Fact = Fact
END IF

IF (PRESENT(Equil)) THEN
  obj%slu%options%Equil = Equil
END IF

IF (PRESENT(ColPerm)) THEN
  obj%slu%options%ColPerm = ColPerm
END IF

IF (PRESENT(Trans)) THEN
  obj%slu%options%Trans = Trans
END IF

IF (PRESENT(IterRefine)) THEN
  obj%slu%options%IterRefine = IterRefine
END IF

IF (PRESENT(DiagPivotThresh)) THEN
  obj%slu%options%DiagPivotThresh = DiagPivotThresh
END IF

IF (PRESENT(SymmetricMode)) THEN
  obj%slu%options%SymmetricMode = SymmetricMode
END IF

IF (PRESENT(PivotGrowth)) THEN
  obj%slu%options%PivotGrowth = PivotGrowth
END IF

IF (PRESENT(ConditionNumber)) THEN
  obj%slu%options%ConditionNumber = ConditionNumber
END IF

IF (PRESENT(RowPerm)) THEN
  obj%slu%options%RowPerm = RowPerm
END IF

IF (PRESENT(ILU_DropRule)) THEN
  obj%slu%options%ILU_DropRule = ILU_DropRule
END IF

IF (PRESENT(ILU_DropTol)) THEN
  obj%slu%options%ILU_DropTol = ILU_DropTol
END IF

IF (PRESENT(ILU_FillFactor)) THEN
  obj%slu%options%ILU_FillFactor = ILU_FillFactor
END IF

IF (PRESENT(ILU_MILU)) THEN
  obj%slu%options%ILU_MILU = ILU_MILU
END IF

IF (PRESENT(PrintStat)) THEN
  obj%slu%options%PrintStat = PrintStat
END IF

#else

CALL SuperluNotLinked(routine="obj_SetSuperluOptions()", line=__LINE__)

#endif

END PROCEDURE obj_SetSuperluOptions

!----------------------------------------------------------------------------
!                                                               SuperluDGSSVX
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SuperluDGSSVX
#ifdef USE_SuperLU

IF (.NOT. ALLOCATED(obj%slu%perm_c)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%perm_c is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%perm_r)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%perm_r is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%etree)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%etree is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%R)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%R is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%C)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%C is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%ferr)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%ferr is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

IF (.NOT. ALLOCATED(obj%slu%berr)) THEN
  CALL SuperluError(msg="Superlu_::obj%slu%berr is not allocated", &
                    routine="obj_SuperluDGSSVX()", line=__LINE__)
END IF

CALL dgssvx(options=obj%slu%options, A=obj%slu%A, perm_c=obj%slu%perm_c, &
            perm_r=obj%slu%perm_r, etree=obj%slu%etree, equed=obj%slu%equed, &
            R=obj%slu%R, C=obj%slu%C, L=obj%slu%L, U=obj%slu%U, &
            Work=obj%slu%Work, B=obj%slu%B, X=obj%slu%X, &
            recip_pivot_growth=obj%slu%recip_pivot_growth, &
            rcond=obj%slu%rcond, ferr=obj%slu%ferr, berr=obj%slu%berr, &
            Glu=obj%slu%Glu, mem_usage=obj%slu%mem_usage, STAT=obj%slu%STAT, &
            lwork=obj%slu%lwork, info=obj%slu%info)

obj%slu%isLInitiated = .TRUE.
obj%slu%isUInitiated = .TRUE.
obj%slu%isGluInitiated = .TRUE.

#else

CALL SuperluNotLinked(routine="obj_SuperluDGSSVX()", line=__LINE__)

#endif
END PROCEDURE obj_SuperluDGSSVX

!----------------------------------------------------------------------------
!                                                  InitiateSuperluDGSSVXParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateSuperluDGSSVXParam
#ifdef USE_SuperLU
INTEGER(I4B) :: m, n, nnz, nrhs

m = SIZE(obj, 1)
n = SIZE(obj, 2)
nnz = GetNNZ(obj)
nrhs = SIZE(obj%slu%rhs, 2)

CALL Reallocate(obj%slu%perm_c, n)
CALL Reallocate(obj%slu%perm_r, m)
CALL Reallocate(obj%slu%etree, n)
CALL Reallocate(obj%slu%R, m)
CALL Reallocate(obj%slu%C, n)
CALL Reallocate(obj%slu%ferr, nrhs)
CALL Reallocate(obj%slu%berr, nrhs)

#else

CALL SuperluNotLinked(routine="obj_InitiateSuperluDGSSVXParam()", &
                      line=__LINE__)

#endif

END PROCEDURE obj_InitiateSuperluDGSSVXParam

!----------------------------------------------------------------------------
!                                                          SuperluDisplayStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SuperluDisplayStat
#ifdef USE_SuperLU
INTEGER(I4B) :: ii, nrhs

IF (obj%slu%options%PrintStat .EQ. yes_no_t%YES) THEN
  IF (obj%slu%options%PivotGrowth .EQ. yes_no_t%YES) THEN
    CALL Display(obj%slu%recip_pivot_growth, "recip_pivot_growth=")
  END IF

  IF (obj%slu%options%ConditionNumber .EQ. yes_no_t%YES) THEN
    CALL Display(obj%slu%rcond, "rcond=")
  END IF

  IF (obj%slu%options%IterRefine .NE. IterRefine_t%NOREFINE) THEN
    CALL Display("rhs, Steps, Ferr, Berr")
    nrhs = SIZE(obj%slu%rhs, 2)
    DO ii = 1, nrhs
      CALL Display([REAL(ii, kind=DFP), &
                    REAL(obj%slu%STAT%RefineSteps, kind=DFP), &
                    obj%slu%Ferr(ii), obj%slu%Berr(ii)], "", orient="row")
    END DO
  END IF
  CALL StatPrint(obj%slu%STAT)
  CALL Display(obj%slu%mem_usage%total_needed / 1.0E+6, &
               "total size needed = ")
END IF

#else

CALL SuperluNotLinked(routine="obj_SuperluDisplayStat()", line=__LINE__)

#endif
END PROCEDURE obj_SuperluDisplayStat

!----------------------------------------------------------------------------
!                                                           SuperluDeallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SuperluDeallocate
#ifdef USE_SuperLU
CHARACTER(LEN=*), PARAMETER :: myName = "SuperluDeallocate()"
IF (ASSOCIATED(obj%slu)) THEN
  IF (ALLOCATED(obj%slu%rhs)) DEALLOCATE (obj%slu%rhs)
  IF (ALLOCATED(obj%slu%sol)) DEALLOCATE (obj%slu%sol)
  IF (ALLOCATED(obj%slu%etree)) DEALLOCATE (obj%slu%etree)
  IF (ALLOCATED(obj%slu%perm_r)) DEALLOCATE (obj%slu%perm_r)
  IF (ALLOCATED(obj%slu%perm_c)) DEALLOCATE (obj%slu%perm_c)
  IF (ALLOCATED(obj%slu%R)) DEALLOCATE (obj%slu%R)
  IF (ALLOCATED(obj%slu%C)) DEALLOCATE (obj%slu%C)
  IF (ALLOCATED(obj%slu%ferr)) DEALLOCATE (obj%slu%ferr)
  IF (ALLOCATED(obj%slu%berr)) DEALLOCATE (obj%slu%berr)
  IF (ALLOCATED(obj%slu%ia)) DEALLOCATE (obj%slu%ia)
  IF (ALLOCATED(obj%slu%ja)) DEALLOCATE (obj%slu%ja)
  IF (ALLOCATED(obj%slu%nzval)) DEALLOCATE (obj%slu%nzval)
  IF (obj%slu%isAInitiated) THEN
    CALL Destroy_SuperMatrix_Store(obj%slu%A)
  END IF
  IF (obj%slu%isBInitiated) THEN
    CALL Destroy_SuperMatrix_Store(obj%slu%B)
  END IF
  IF (obj%slu%isXInitiated) THEN
    CALL Destroy_SuperMatrix_Store(obj%slu%X)
  END IF
  IF (obj%slu%isLInitiated) THEN
    CALL Destroy_SuperNode_Matrix(obj%slu%L)
  END IF
  IF (obj%slu%isUInitiated) THEN
    CALL Destroy_CompCol_Matrix(obj%slu%U)
  END IF
  IF (obj%slu%lwork .NE. 0) THEN
    CALL Superlu_Free(obj%slu%work)
  END IF
  IF (obj%slu%isStatInitiated) THEN
    CALL StatFree(obj%slu%STAT)
  END IF
  obj%slu%lwork = 0
  obj%slu%info = 0
  obj%slu%recip_pivot_growth = 0.0_DFP
  obj%slu%rcond = 0.0_DFP
  obj%slu%isAInitiated = .FALSE.
  obj%slu%isBInitiated = .FALSE.
  obj%slu%isXInitiated = .FALSE.
  obj%slu%isLInitiated = .FALSE.
  obj%slu%isUInitiated = .FALSE.
  obj%slu%isGluInitiated = .FALSE.
  obj%slu%isStatInitiated = .FALSE.
  DEALLOCATE (obj%slu)
  obj%slu => NULL()
END IF
#else
CALL SuperluNotLinked(routine="obj_SuperluDeallocate()", line=__LINE__)
#endif
END PROCEDURE obj_SuperluDeallocate

!----------------------------------------------------------------------------
!                                                                    LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinSolve1
#ifdef USE_SuperLU
LOGICAL(LGT) :: isFactored0, isTranspose0, isFreeFactors0
INTEGER(I4B) :: Trans0
REAL(DFP) :: DiagPivotThresh0
INTEGER(I4B) :: ColPerm0
INTEGER(I4B) :: IterRefine0
INTEGER(I4B) :: PivotGrowth0
INTEGER(I4B) :: ConditionNumber0
INTEGER(I4B) :: Equil0
INTEGER(I4B) :: SymmetricMode0
INTEGER(I4B) :: PrintStat0

CALL CheckErrorCSRMatrix(obj=A, lineNo=__LINE__, routine="obj_LinSolve1()")

isTranspose0 = input(option=isTranspose, default=.FALSE.)

IF (isTranspose0) THEN
  Trans0 = Trans_t%NOTRANS
ELSE
  Trans0 = Trans_t%TRANS
END IF

Equil0 = input(option=Equil, default=yes_no_t%YES)
ColPerm0 = input(option=ColPerm, default=colperm_t%COLAMD)
IterRefine0 = input(option=IterRefine, default=IterRefine_t%SLU_DOUBLE)
DiagPivotThresh0 = input(option=DiagPivotThresh, default=1.0_DFP)
PivotGrowth0 = input(option=PivotGrowth, default=yes_no_t%NO)
ConditionNumber0 = input(option=ConditionNumber, default=yes_no_t%NO)
SymmetricMode0 = input(option=SymmetricMode, default=yes_no_t%NO)
PrintStat0 = input(option=PrintStat, default=yes_no_t%NO)

! first call: set up A, B, X, the options and the work arrays

IF (.NOT. ASSOCIATED(A%slu)) THEN
  ALLOCATE (A%slu)
END IF

IF (.NOT. A%slu%isAInitiated) THEN
  CALL InitiateSuperluA(obj=A)
  CALL InitiateSuperluRHS(obj=A, rhs=B)
  CALL InitiateSuperluOptions(obj=A)
  CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                         ColPerm=ColPerm0, Fact=Fact_t%DOFACT, &
                         IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                         DiagPivotThresh=DiagPivotThresh0, &
                         SymmetricMode=SymmetricMode0, PrintStat=PrintStat0, &
                         ConditionNumber=ConditionNumber0)
  CALL InitiateSuperluDGSSVXParam(obj=A)
  CALL StatInit(A%slu%STAT)
  A%slu%isStatInitiated = .TRUE.

ELSE
  isFactored0 = input(option=isFactored, default=.FALSE.)
  IF (isFactored0) THEN
    ! use the stored factors (Fact = FACTORED)
    CALL CheckSuperluFactors(obj=A, routine="obj_LinSolve1()")
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                           ColPerm=ColPerm0, Fact=Fact_t%FACTORED, &
                           IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                           DiagPivotThresh=DiagPivotThresh0, &
                           SymmetricMode=SymmetricMode0, &
                           PrintStat=PrintStat0, &
                           ConditionNumber=ConditionNumber0)
  ELSE
    ! factor again (Fact = SamePattern); the sparsity is the same, so the
    ! work arrays of InitiateSuperluDGSSVXParam are kept
    CALL DestroySuperluLU(obj=A)
    CALL SetSuperluA(obj=A)
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                           ColPerm=ColPerm0, Fact=Fact_t%SamePattern, &
                           IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                           DiagPivotThresh=DiagPivotThresh0, &
                           SymmetricMode=SymmetricMode0, &
                           PrintStat=PrintStat0, &
                           ConditionNumber=ConditionNumber0)
  END IF

END IF

CALL SuperluDGSSVX(obj=A)
CALL Copy(x=A%slu%sol(:, 1), y=x)
IF (PRESENT(info)) info = A%slu%info
CALL SuperluDisplayStat(obj=A)

isFreeFactors0 = input(option=isFreeFactors, default=.FALSE.)
IF (isFreeFactors0) CALL DestroySuperluLU(obj=A)

! L and U are kept for the next call with isFactored (easifem/base#358)

#else

CALL SuperluNotLinked(routine="obj_LinSolve1()", line=__LINE__)

#endif
END PROCEDURE obj_LinSolve1

!----------------------------------------------------------------------------
!                                                                    LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinSolve2
#ifdef USE_SuperLU
LOGICAL(LGT) :: isFactored0, isTranspose0, isFreeFactors0
INTEGER(I4B) :: Trans0
REAL(DFP) :: DiagPivotThresh0
INTEGER(I4B) :: ColPerm0
INTEGER(I4B) :: IterRefine0
INTEGER(I4B) :: PivotGrowth0
INTEGER(I4B) :: ConditionNumber0
INTEGER(I4B) :: Equil0
INTEGER(I4B) :: SymmetricMode0
INTEGER(I4B) :: PrintStat0
INTEGER(I4B) :: ii, nrhs

CALL CheckErrorCSRMatrix(obj=A, lineNo=__LINE__, routine="obj_LinSolve2()")

isTranspose0 = input(option=isTranspose, default=.FALSE.)

IF (isTranspose0) THEN
  Trans0 = Trans_t%NOTRANS
ELSE
  Trans0 = Trans_t%TRANS
END IF

Equil0 = input(option=Equil, default=yes_no_t%YES)
ColPerm0 = input(option=ColPerm, default=colperm_t%COLAMD)
IterRefine0 = input(option=IterRefine, default=IterRefine_t%SLU_DOUBLE)
DiagPivotThresh0 = input(option=DiagPivotThresh, default=1.0_DFP)
PivotGrowth0 = input(option=PivotGrowth, default=yes_no_t%NO)
ConditionNumber0 = input(option=ConditionNumber, default=yes_no_t%NO)
SymmetricMode0 = input(option=SymmetricMode, default=yes_no_t%NO)
PrintStat0 = input(option=PrintStat, default=yes_no_t%NO)

! first call: set up A, B, X, the options and the work arrays

IF (.NOT. ASSOCIATED(A%slu)) THEN
  ALLOCATE (A%slu)
END IF

IF (.NOT. A%slu%isAInitiated) THEN
  CALL InitiateSuperluA(obj=A)
  CALL InitiateSuperluRHS(obj=A, rhs=B)
  CALL InitiateSuperluOptions(obj=A)
  CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                         ColPerm=ColPerm0, Fact=Fact_t%DOFACT, &
                         IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                         DiagPivotThresh=DiagPivotThresh0, &
                         SymmetricMode=SymmetricMode0, PrintStat=PrintStat0, &
                         ConditionNumber=ConditionNumber0)
  CALL InitiateSuperluDGSSVXParam(obj=A)
  CALL StatInit(A%slu%STAT)
  A%slu%isStatInitiated = .TRUE.
ELSE
  isFactored0 = input(option=isFactored, default=.FALSE.)
  IF (isFactored0) THEN
    ! use the stored factors (Fact = FACTORED)
    CALL CheckSuperluFactors(obj=A, routine="obj_LinSolve2()")
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%FACTORED, Trans=Trans0)
  ELSE
    ! factor again (Fact = SamePattern); the sparsity is the same, so the
    ! work arrays of InitiateSuperluDGSSVXParam are kept
    CALL DestroySuperluLU(obj=A)
    CALL SetSuperluA(obj=A)
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%SamePattern, Trans=Trans0)
  END IF

END IF

CALL SuperluDGSSVX(obj=A)
nrhs = SIZE(x, 2)
DO ii = 1, nrhs
  CALL Copy(x=A%slu%sol(:, ii), y=x(:, ii))
END DO
IF (PRESENT(info)) info = A%slu%info
CALL SuperluDisplayStat(obj=A)

isFreeFactors0 = input(option=isFreeFactors, default=.FALSE.)
IF (isFreeFactors0) CALL DestroySuperluLU(obj=A)

! L and U are kept for the next call with isFactored (easifem/base#358)

#else

CALL SuperluNotLinked(routine="obj_LinSolve2()", line=__LINE__)

#endif
END PROCEDURE obj_LinSolve2

!----------------------------------------------------------------------------
!                                                                    LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinSolve3
#ifdef USE_SuperLU
LOGICAL(LGT) :: isFactored0, isTranspose0, isFreeFactors0
INTEGER(I4B) :: Trans0
REAL(DFP) :: DiagPivotThresh0
INTEGER(I4B) :: ColPerm0
INTEGER(I4B) :: IterRefine0
INTEGER(I4B) :: PivotGrowth0
INTEGER(I4B) :: ConditionNumber0
INTEGER(I4B) :: Equil0
INTEGER(I4B) :: SymmetricMode0
INTEGER(I4B) :: PrintStat0

CALL CheckErrorCSRMatrix(obj=A, lineNo=__LINE__, routine="obj_LinSolve3()")

isTranspose0 = input(option=isTranspose, default=.FALSE.)

IF (isTranspose0) THEN
  Trans0 = Trans_t%NOTRANS
ELSE
  Trans0 = Trans_t%TRANS
END IF

Equil0 = input(option=Equil, default=yes_no_t%YES)
ColPerm0 = input(option=ColPerm, default=colperm_t%COLAMD)
IterRefine0 = input(option=IterRefine, default=IterRefine_t%SLU_DOUBLE)
DiagPivotThresh0 = input(option=DiagPivotThresh, default=1.0_DFP)
PivotGrowth0 = input(option=PivotGrowth, default=yes_no_t%NO)
ConditionNumber0 = input(option=ConditionNumber, default=yes_no_t%NO)
SymmetricMode0 = input(option=SymmetricMode, default=yes_no_t%NO)
PrintStat0 = input(option=PrintStat, default=yes_no_t%NO)

! first call: set up A, B, X, the options and the work arrays

IF (.NOT. ASSOCIATED(A%slu)) THEN
  ALLOCATE (A%slu)
END IF

IF (.NOT. A%slu%isAInitiated) THEN
  CALL InitiateSuperluA(obj=A)
  CALL InitiateSuperluRHS(obj=A, rhs=B)
  CALL InitiateSuperluOptions(obj=A)
  CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                         ColPerm=ColPerm0, Fact=Fact_t%DOFACT, &
                         IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                         DiagPivotThresh=DiagPivotThresh0, &
                         SymmetricMode=SymmetricMode0, PrintStat=PrintStat0, &
                         ConditionNumber=ConditionNumber0)
  CALL InitiateSuperluDGSSVXParam(obj=A)
  CALL StatInit(A%slu%STAT)
  A%slu%isStatInitiated = .TRUE.
ELSE
  isFactored0 = input(option=isFactored, default=.FALSE.)
  IF (isFactored0) THEN
    ! use the stored factors (Fact = FACTORED)
    CALL CheckSuperluFactors(obj=A, routine="obj_LinSolve3()")
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%FACTORED, Trans=Trans0)
  ELSE
    ! factor again (Fact = SamePattern); the sparsity is the same, so the
    ! work arrays of InitiateSuperluDGSSVXParam are kept
    CALL DestroySuperluLU(obj=A)
    CALL SetSuperluA(obj=A)
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%SamePattern, Trans=Trans0)
  END IF

END IF

CALL SuperluDGSSVX(obj=A)
CALL Copy(x=A%slu%sol(:, 1), y=B)
IF (PRESENT(info)) info = A%slu%info
CALL SuperluDisplayStat(obj=A)

isFreeFactors0 = input(option=isFreeFactors, default=.FALSE.)
IF (isFreeFactors0) CALL DestroySuperluLU(obj=A)
! L and U are kept for the next call with isFactored (easifem/base#358)
#else

CALL SuperluNotLinked(routine="obj_LinSolve3()", line=__LINE__)

#endif
END PROCEDURE obj_LinSolve3

!----------------------------------------------------------------------------
!                                                                    LinSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LinSolve4
#ifdef USE_SuperLU
LOGICAL(LGT) :: isFactored0, isTranspose0, isFreeFactors0
INTEGER(I4B) :: Trans0
REAL(DFP) :: DiagPivotThresh0
INTEGER(I4B) :: ColPerm0
INTEGER(I4B) :: IterRefine0
INTEGER(I4B) :: PivotGrowth0
INTEGER(I4B) :: ConditionNumber0
INTEGER(I4B) :: Equil0
INTEGER(I4B) :: SymmetricMode0
INTEGER(I4B) :: PrintStat0
INTEGER(I4B) :: ii, nrhs

CALL CheckErrorCSRMatrix(obj=A, lineNo=__LINE__, routine="obj_LinSolve4()")

isTranspose0 = input(option=isTranspose, default=.FALSE.)

IF (isTranspose0) THEN
  Trans0 = Trans_t%NOTRANS
ELSE
  Trans0 = Trans_t%TRANS
END IF

Equil0 = input(option=Equil, default=yes_no_t%YES)
ColPerm0 = input(option=ColPerm, default=colperm_t%COLAMD)
IterRefine0 = input(option=IterRefine, default=IterRefine_t%SLU_DOUBLE)
DiagPivotThresh0 = input(option=DiagPivotThresh, default=1.0_DFP)
PivotGrowth0 = input(option=PivotGrowth, default=yes_no_t%NO)
ConditionNumber0 = input(option=ConditionNumber, default=yes_no_t%NO)
SymmetricMode0 = input(option=SymmetricMode, default=yes_no_t%NO)
PrintStat0 = input(option=PrintStat, default=yes_no_t%NO)

! first call: set up A, B, X, the options and the work arrays

IF (.NOT. ASSOCIATED(A%slu)) THEN
  ALLOCATE (A%slu)
END IF

IF (.NOT. A%slu%isAInitiated) THEN
  CALL InitiateSuperluA(obj=A)
  CALL InitiateSuperluRHS(obj=A, rhs=B)
  CALL InitiateSuperluOptions(obj=A)
  CALL SetSuperluOptions(obj=A, Equil=Equil0, Trans=Trans0, &
                         ColPerm=ColPerm0, Fact=Fact_t%DOFACT, &
                         IterRefine=IterRefine0, PivotGrowth=PivotGrowth0, &
                         DiagPivotThresh=DiagPivotThresh0, &
                         SymmetricMode=SymmetricMode0, PrintStat=PrintStat0, &
                         ConditionNumber=ConditionNumber0)
  CALL InitiateSuperluDGSSVXParam(obj=A)
  CALL StatInit(A%slu%STAT)
  A%slu%isStatInitiated = .TRUE.

ELSE
  isFactored0 = input(option=isFactored, default=.FALSE.)
  IF (isFactored0) THEN
    ! use the stored factors (Fact = FACTORED)
    CALL CheckSuperluFactors(obj=A, routine="obj_LinSolve4()")
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%FACTORED, Trans=Trans0)
  ELSE
    ! factor again (Fact = SamePattern); the sparsity is the same, so the
    ! work arrays of InitiateSuperluDGSSVXParam are kept
    CALL DestroySuperluLU(obj=A)
    CALL SetSuperluA(obj=A)
    CALL SetSuperluRHS(obj=A, rhs=B)
    CALL SetSuperluOptions(obj=A, Fact=Fact_t%SamePattern, Trans=Trans0)
  END IF

END IF

CALL SuperluDGSSVX(obj=A)
nrhs = SIZE(B, 2)
DO ii = 1, nrhs
  CALL Copy(x=A%slu%sol(:, ii), y=B(:, ii))
END DO
IF (PRESENT(info)) info = A%slu%info
CALL SuperluDisplayStat(obj=A)

isFreeFactors0 = input(option=isFreeFactors, default=.FALSE.)
IF (isFreeFactors0) CALL DestroySuperluLU(obj=A)
! L and U are kept for the next call with isFactored (easifem/base#358)
#else

CALL SuperluNotLinked(routine="obj_LinSolve4()", line=__LINE__)

#endif
END PROCEDURE obj_LinSolve4

END SUBMODULE Methods
