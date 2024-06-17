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

SUBMODULE(CSRMatrix_UnaryMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Scal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Scal
CALL SCAL(X=obj%A, A=a)
END PROCEDURE obj_Scal

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert1
INTEGER(I4B) :: i, j, nrow
nrow = SIZE(IA) - 1
CALL Reallocate(mat, nrow, nrow)
DO i = 1, nrow
  DO j = IA(i), IA(i + 1) - 1
    mat(i, JA(j)) = A(j)
  END DO
END DO
END PROCEDURE obj_Convert1

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert2
INTEGER(I4B) :: i, j, nrow, ncol
!!
nrow = SIZE(obj=From, dims=1)
ncol = SIZE(obj=From, dims=2)
!!
CALL Reallocate(To, nrow, ncol)
!!
DO i = 1, nrow
  DO j = From%csr%IA(i), From%csr%IA(i + 1) - 1
    To(i, From%csr%JA(j)) = From%A(j)
  END DO
END DO
!!
END PROCEDURE obj_Convert2

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert3
CALL Convert(From=From, To=To%val)
! CALL Convert(A=From%A, IA=From%csr%IA, JA=From%csr%JA, &
!   & mat=To%val)
CALL setTotalDimension(To, 2_I4B)
END PROCEDURE obj_Convert3

!----------------------------------------------------------------------------
!                                                                    ColSORT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ColumnSORT
CALL CSORT(obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, &
  & INPUT(Option=isValues, Default=.TRUE.))
obj%csr%isSorted = .TRUE.
END PROCEDURE obj_ColumnSORT

!----------------------------------------------------------------------------
!                                                           RemoveDuplicates
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_RemoveDuplicates
INTEGER(I4B), ALLOCATABLE :: iwk(:), UT(:)
CALL Reallocate(UT, obj%csr%nrow, iwk, obj%csr%nrow + 1)
CALL CLNCSR(1, 1, obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, UT, iwk)
!> Some entries are removed so fix sparsity
obj%csr%isSparsityLock = .FALSE.
CALL setSparsity(obj)
DEALLOCATE (iwk, UT)
END PROCEDURE obj_RemoveDuplicates

!----------------------------------------------------------------------------
!                                                                      Clean
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Clean
INTEGER(I4B), ALLOCATABLE :: iwk(:), UT(:)
INTEGER(I4B) :: value2

IF (INPUT(option=isValues, default=.TRUE.)) THEN
  value2 = 1
ELSE
  value2 = 0
END IF
CALL Reallocate(UT, obj%csr%nrow, iwk, obj%csr%nrow + 1)
CALL CLNCSR(INPUT(option=ExtraOption, default=1), value2, obj%csr%nrow, &
  & obj%A, obj%csr%JA, obj%csr%IA, UT, iwk)
!> Some entries are removed so fix sparsity
obj%csr%isSparsityLock = .FALSE.
CALL setSparsity(obj)
DEALLOCATE (iwk, UT)
END PROCEDURE obj_Clean

!----------------------------------------------------------------------------
!                                                                      Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
To = From
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
INTERFACE
  FUNCTION GETELM(I, J, A, JA, IA, IADD, SORTED)
    INTEGER :: I, J, IA(*), JA(*), IADD
    LOGICAL :: SORTED
    DOUBLE PRECISION :: GETELM, A(*)
  END FUNCTION GETELM
END INTERFACE
INTEGER(I4B) :: iadd0
Ans = GETELM(I, J, obj%A, obj%csr%JA, obj%csr%IA, iadd0, obj%csr%isSorted)
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                    Filter
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DropEntry
INTEGER(I4B) :: ierr, nnz
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)
REAL(DFP), ALLOCATABLE :: A(:)
ALLOCATE (IA(objIn%csr%nrow + 1), JA(objIn%csr%nnz), &
  & A(objIn%csr%nnz))
CALL FILTER(objIn%csr%nrow, INPUT(option=option, default=1), &
  & droptol, objIn%A, objIn%csr%JA, objIn%csr%IA, A, JA, IA,&
  & objIn%csr%nnz, ierr)
nnz = IA(objIn%csr%nrow + 1) - 1
CALL Initiate(obj=objOut, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
DEALLOCATE (IA, JA, A)
END PROCEDURE obj_DropEntry

!----------------------------------------------------------------------------
!                                                                 Transpose
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Transpose
INTEGER(I4B), ALLOCATABLE :: iwk(:)
INTEGER(I4B) :: ierr
TYPE(DOF_) :: dofobj
CALL Reallocate(iwk, obj%csr%nnz)
CALL TRANSP(obj%csr%nrow,obj%csr%ncol,obj%A,obj%csr%JA,obj%csr%IA,iwk,ierr)
IF (ierr .NE. 0) THEN
  CALL ErrorMSG( &
    & msg="Error occured during transposing!", &
    & file="CSRMatrix_Method@UnaryMethods.F90", &
    & routine="obj_Transpose()", &
    & line=__LINE__, &
    & unitno=stderr)
  STOP
END IF
CALL ColumnSORT(obj)
dofobj = obj%csr%idof
obj%csr%jdof = obj%csr%idof
obj%csr%idof = dofobj
CALL DEALLOCATE (dofobj)
DEALLOCATE (iwk)
END PROCEDURE obj_Transpose

!----------------------------------------------------------------------------
!                                                                 getDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getDiagonal1
CALL getDiagonal(obj=obj%csr, A=obj%A, diag=diag, idiag=idiag, &
  & offset=offset)
END PROCEDURE obj_getDiagonal1

!----------------------------------------------------------------------------
!                                                                 getDiagonal
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getDiagonal2
CALL getDiagonal(obj=obj%csr, A=obj%A, diag=diag, offset=offset)
END PROCEDURE obj_getDiagonal2

!----------------------------------------------------------------------------
!                                                          getLowerTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getLowerTriangle
REAL(DFP), ALLOCATABLE :: A(:)
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)
INTEGER(I4B) :: nnz, nrow
nrow = obj%csr%nrow; nnz = obj%csr%nnz
ALLOCATE (A(nnz), JA(nnz), IA(nrow + 1))
CALL GETL(obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, A, JA, IA)
nnz = IA(nrow + 1) - 1
CALL Initiate(obj=L, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
DEALLOCATE (A, IA, JA)
END PROCEDURE obj_getLowerTriangle

!----------------------------------------------------------------------------
!                                                          getUpperTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getUpperTriangle
REAL(DFP), ALLOCATABLE :: A(:)
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)
INTEGER(I4B) :: nnz, nrow
nrow = obj%csr%nrow; nnz = obj%csr%nnz
ALLOCATE (A(nnz), JA(nnz), IA(nrow + 1))
CALL GETU(obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, A, JA, IA)
nnz = IA(nrow + 1) - 1
CALL Initiate(obj=U, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
DEALLOCATE (A, IA, JA)
END PROCEDURE obj_getUpperTriangle

!----------------------------------------------------------------------------
!                                                                PermuteRow
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PermuteRow
INTEGER(I4B) :: nrow, job
nrow = SIZE(obj, 1); job = 1
IF (PRESENT(isValues)) THEN
  IF (.NOT. isValues) job = 0
END IF
CALL initiate(ans, obj, .TRUE.)
CALL RPERM(nrow, obj%A, obj%csr%JA, obj%csr%IA, ans%A, ans%csr%JA, &
  & ans%csr%IA, PERM, job)
END PROCEDURE obj_PermuteRow

!----------------------------------------------------------------------------
!                                                            PermuteColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PermuteColumn
INTEGER(I4B) :: nrow, job
nrow = SIZE(obj, 1); job = 1
IF (PRESENT(isValues)) THEN
  IF (.NOT. isValues) job = 0
END IF
CALL initiate(ans, obj, .TRUE.)
CALL CPERM(nrow, obj%A, obj%csr%JA, obj%csr%IA, ans%A, ans%csr%JA, &
  & ans%csr%IA, PERM, job)
END PROCEDURE obj_PermuteColumn

!----------------------------------------------------------------------------
!                                                                   Permute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Permute
INTEGER(I4B) :: nrow, job
LOGICAL(LGT) :: isSymPERM
!
nrow = SIZE(obj, 1)
CALL initiate(ans, obj, .TRUE.)
!
IF (PRESENT(symPERM)) THEN
  isSymPERM = symPERM
ELSE
  isSymPERM = .FALSE.
END IF
!
IF (PRESENT(rowPERM) .AND. PRESENT(colPERM)) THEN
  job = 3
  IF (PRESENT(isValues)) THEN
    IF (.NOT. isValues) job = 4
  END IF
  CALL DPERM(nrow, obj%A, obj%csr%JA, obj%csr%IA, ans%A, &
    & ans%csr%JA, ans%csr%IA, rowPERM, colPERM, job)
  RETURN
END IF
!
IF (PRESENT(rowPERM)) THEN
  IF (isSymPERM) THEN
    job = 1
    IF (PRESENT(isValues)) THEN
      IF (.NOT. isValues) job = 2
    END IF
    CALL DPERM(nrow, obj%A, obj%csr%JA, obj%csr%IA, ans%A, &
      & ans%csr%JA, ans%csr%IA, rowPERM, rowPERM, job)
    RETURN
  ELSE
    ans = PermuteRow(obj=obj, PERM=rowPERM, isValues=isValues)
    RETURN
  END IF
END IF
!
IF (PRESENT(colPERM)) THEN
  IF (isSymPERM) THEN
    job = 1
    IF (PRESENT(isValues)) THEN
      IF (.NOT. isValues) job = 2
    END IF
    CALL DPERM(nrow, obj%A, obj%csr%JA, obj%csr%IA, ans%A, &
      & ans%csr%JA, ans%csr%IA, colPERM, colPERM, job)
    RETURN
  ELSE
    ans = PermuteColumn(obj=obj, PERM=colPERM, isValues=isValues)
    RETURN
  END IF
END IF
END PROCEDURE obj_Permute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymU1(obj, symobj, A, symA)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  TYPE(CSRSparsity_), INTENT(INOUT) :: symobj
  REAL(DFP), INTENT(IN) :: A(:)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: symA(:)
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzU, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP), ALLOCATABLE :: A_csr(:), A_csc(:)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzU = nnz_parts(1)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzU, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  CALL Reallocate(A_csc, nnzU)
  CALL Reallocate(A_csr, nnzU)
  !
  indx = 0
  !
  DO ii = 1, nrow
    !
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    !
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .LT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
        A_csr(indx) = A(rindx)
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
      END IF
    END DO
    !
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzU)
  CALL Reallocate(A_csc, nnzU)
  !
  CALL csrcsc( &
    & nrow, &
    & 1, &
    & 1, &
    & A_csr, &
    & JA_csr, &
    & IA_csr, &
    & A_csc, &
    & JA_csc, &
    & IA_csc)
  !
  symobj%nnz = nnzU * 2 + nnzD
  symobj%ncol = ncol
  symobj%nrow = nrow
  symobj%isSorted = obj%isSorted
  symobj%isInitiated = obj%isInitiated
  symobj%isSparsityLock = obj%isSparsityLock
  symobj%isDiagStored = .TRUE.
  symobj%idof = obj%idof
  symobj%jdof = obj%jdof
  !
  CALL Reallocate(symobj%IA, nrow + 1, symobj%idiag, nrow)
  CALL Reallocate(symobj%JA, symobj%nnz)
  CALL Reallocate(symA, symobj%nnz)
  !
  indx = 0
  !
  DO ii = 1, symobj%nrow
    ar = IA_csr(ii + 1) - IA_csr(ii)
    al = IA_csc(ii + 1) - IA_csr(ii)
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    symobj%IA(ii) = indx + 1
    symobj%IA(ii + 1) = symobj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csc(rindx)
      symA(indx) = A_csc(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      symobj%JA(indx) = ii !!obj%JA(idiag(ii))
      symobj%idiag(ii) = indx
      symA(indx) = A(idiag(ii))
    END IF
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csr(rindx)
      symA(indx) = A_csr(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag, A_csr, A_csc)
  !
END SUBROUTINE obj_GetSymU1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymU2(obj, A)
  TYPE(CSRSparsity_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzU, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP), ALLOCATABLE :: A_csr(:), A_csc(:), A_diag(:)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzU = nnz_parts(1)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzU, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  CALL Reallocate(A_csc, nnzU)
  CALL Reallocate(A_csr, nnzU)
  CALL Reallocate(A_diag, nrow)
  !
  indx = 0
  !
  DO ii = 1, nrow
    !
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    !
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .LT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
        A_csr(indx) = A(rindx)
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
        A_diag(ii) = A(rindx)
      END IF
    END DO
    !
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzU)
  CALL Reallocate(A_csc, nnzU)
  !
  CALL csrcsc( &
    & nrow, &
    & 1, &
    & 1, &
    & A_csr, &
    & JA_csr, &
    & IA_csr, &
    & A_csc, &
    & JA_csc, &
    & IA_csc)
  !
  obj%nnz = nnz_parts(1) * 2 + nnz_parts(3)
  obj%isDiagStored = .TRUE.
  !
  CALL Reallocate(obj%IA, nrow + 1, obj%idiag, nrow)
  CALL Reallocate(obj%JA, obj%nnz)
  CALL Reallocate(A, obj%nnz)
  !
  indx = 0
  !
  DO ii = 1, obj%nrow
    ar = IA_csr(ii + 1) - IA_csr(ii)
    al = IA_csc(ii + 1) - IA_csr(ii)
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    obj%IA(ii) = indx + 1
    obj%IA(ii + 1) = obj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      obj%JA(indx) = JA_csc(rindx)
      A(indx) = A_csc(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      obj%JA(indx) = ii !!obj%JA(idiag(ii))
      obj%idiag(ii) = indx
      A(indx) = A_diag(ii)
    END IF
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      obj%JA(indx) = JA_csr(rindx)
      A(indx) = A_csr(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag, A_csr, &
    & A_csc, A_diag)
  !
END SUBROUTINE obj_GetSymU2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymL1(obj, symobj, A, symA)
  TYPE(CSRSparsity_), INTENT(IN) :: obj
  TYPE(CSRSparsity_), INTENT(INOUT) :: symobj
  REAL(DFP), INTENT(IN) :: A(:)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: symA(:)
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzL, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP), ALLOCATABLE :: A_csr(:), A_csc(:), A_diag(:)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzL = nnz_parts(2)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzL, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  CALL Reallocate(A_csr, nnzL)
  CALL Reallocate(A_diag, nrow)
  !
  indx = 0
  !
  DO ii = 1, nrow
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .GT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
        A_csr(indx) = A(rindx)
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
        A_diag(ii) = A(rindx)
      END IF
    END DO
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzL)
  CALL Reallocate(A_csc, nnzL)
  !
  CALL csrcsc( &
    & nrow, &
    & 1, &
    & 1, &
    & A_csr, &
    & JA_csr, &
    & IA_csr, &
    & A_csc, &
    & JA_csc, &
    & IA_csc)
  !
  symobj%nnz = nnzL * 2 + nnzD
  symobj%ncol = ncol
  symobj%nrow = nrow
  symobj%isSorted = obj%isSorted
  symobj%isInitiated = obj%isInitiated
  symobj%isSparsityLock = obj%isSparsityLock
  symobj%isDiagStored = .TRUE.
  symobj%idof = obj%idof
  symobj%jdof = obj%jdof
  !
  CALL Reallocate(symobj%IA, nrow + 1, symobj%idiag, nrow)
  CALL Reallocate(symobj%JA, symobj%nnz)
  CALL Reallocate(symA, symobj%nnz)
  !
  indx = 0
  !
  DO ii = 1, symobj%nrow
    al = IA_csr(ii + 1) - IA_csr(ii)
    ar = IA_csc(ii + 1) - IA_csc(ii)
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    symobj%IA(ii) = indx + 1
    symobj%IA(ii + 1) = symobj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csr(rindx)
      symA(indx) = A_csr(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      symobj%JA(indx) = ii !!obj%JA(idiag(ii))
      symobj%idiag(ii) = indx
      symA(indx) = A_diag(ii)
    END IF
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      symobj%JA(indx) = JA_csc(rindx)
      symA(indx) = A_csc(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag, A_csr, A_csc, A_diag)
  !
END SUBROUTINE obj_GetSymL1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_GetSymL2(obj, A)
  TYPE(CSRSparsity_), INTENT(INOUT) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: A(:)
  !
  INTEGER(I4B) :: nnz_parts(3), ii, jj, rindx, indx, nrow, nnzL, ncol, &
    & nnzD, al, ar, ad
  INTEGER(I4B), ALLOCATABLE :: IA_csr(:), IA_csc(:), JA_csr(:), &
    & JA_csc(:), idiag(:)
  REAL(DFP), ALLOCATABLE :: A_csr(:), A_csc(:), A_diag(:)
  !
  nnz_parts = GetNNZ(obj, [""])
  nrow = obj%nrow
  ncol = obj%ncol
  nnzL = nnz_parts(2)
  nnzD = nnz_parts(3)
  !
  CALL Reallocate(JA_csr, nnzL, IA_csr, nrow + 1)
  CALL Reallocate(idiag, nrow)
  CALL Reallocate(A_csr, nnzL)
  CALL Reallocate(A_Diag, nrow)
  !
  indx = 0
  !
  DO ii = 1, nrow
    IA_csr(ii) = indx + 1
    IA_csr(ii + 1) = IA_csr(ii)
    DO rindx = obj%IA(ii), obj%IA(ii + 1) - 1
      jj = obj%JA(rindx)
      IF (ii .GT. jj) THEN
        indx = indx + 1
        IA_csr(ii + 1) = IA_csr(ii + 1) + 1
        JA_csr(indx) = jj
        A_csr(indx) = A(rindx)
      ELSE IF (ii .EQ. jj) THEN
        idiag(ii) = rindx
        A_diag(ii) = A(rindx)
      END IF
    END DO
  END DO
  !
  CALL Reallocate(IA_csc, ncol + 1, JA_csc, nnzL)
  CALL Reallocate(A_csc, nnzL)
  !
  CALL csrcsc( &
    & nrow, &
    & 1, &
    & 1, &
    & A_csr, &
    & JA_csr, &
    & IA_csr, &
    & A_csc, &
    & JA_csc, &
    & IA_csc)
  !
  obj%nnz = nnzL * 2 + nnzD
  obj%ncol = ncol
  obj%nrow = nrow
  obj%isSorted = obj%isSorted
  obj%isInitiated = obj%isInitiated
  obj%isSparsityLock = obj%isSparsityLock
  obj%isDiagStored = .TRUE.
  obj%idof = obj%idof
  obj%jdof = obj%jdof
  !
  CALL Reallocate(obj%IA, nrow + 1, obj%idiag, nrow)
  CALL Reallocate(obj%JA, obj%nnz)
  CALL Reallocate(A, obj%nnz)
  !
  indx = 0
  !
  DO ii = 1, obj%nrow
    al = IA_csr(ii + 1) - IA_csr(ii)
    ar = IA_csc(ii + 1) - IA_csc(ii)
    !
    IF (idiag(ii) .NE. 0) THEN
      ad = 1
    ELSE
      ad = 0
    END IF
    !
    obj%IA(ii) = indx + 1
    obj%IA(ii + 1) = obj%IA(ii) + ar + al + ad
    !
    DO rindx = IA_csr(ii), IA_csr(ii + 1) - 1
      indx = indx + 1
      obj%JA(indx) = JA_csr(rindx)
      A(indx) = A_csr(rindx)
    END DO
    !
    IF (idiag(ii) .NE. 0) THEN
      indx = indx + 1
      obj%JA(indx) = ii
      obj%idiag(ii) = indx
      A(indx) = A_diag(ii)
    END IF
    !
    DO rindx = IA_csc(ii), IA_csc(ii + 1) - 1
      indx = indx + 1
      obj%JA(indx) = JA_csc(rindx)
      A(indx) = A_csc(rindx)
    END DO
    !
  END DO
  !
  ! Clean up
  !
  DEALLOCATE (IA_csr, IA_csc, JA_csr, JA_csc, idiag, A_csr, A_csc, A_diag)
  !
END SUBROUTINE obj_GetSymL2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSym1
INTEGER(I4B) :: ii, jj, nrow, rindx
REAL(DFP) :: VALUE
!
! Matrix should be square
!
symObj%csrOwnership = obj%csrOwnership
symObj%tDimension = obj%tDimension
symObj%matrixProp = "SYM"

IF (ALLOCATED(obj%A)) THEN
  SELECT CASE (from)
  CASE ("U", "u")
    CALL obj_GetSymU1(obj=obj%csr, symobj=symobj%csr, A=obj%A, &
      & symA=symobj%A)
  CASE ("L", "l")
    CALL obj_GetSymL1(obj=obj%csr, symobj=symobj%csr, A=obj%A, &
      & symA=symobj%A)
  CASE DEFAULT
    CALL Errormsg(&
     & msg="No match found for given from = "//from, &
     & file=__FILE__, &
     & routine="obj_GetSym1()", &
     & line=__LINE__, &
     & unitno=stderr)
    STOP
  END SELECT
END IF

END PROCEDURE obj_GetSym1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSym2
INTEGER(I4B) :: ii, jj, nrow, rindx
REAL(DFP) :: VALUE
!
! Matrix should be square
!
obj%matrixProp = "SYM"

IF (ALLOCATED(obj%A)) THEN
  SELECT CASE (from)
  CASE ("U", "u")
    CALL obj_GetSymU2(obj=obj%csr, A=obj%A)
  CASE ("L", "l")
    CALL obj_GetSymL2(obj=obj%csr, A=obj%A)
  CASE DEFAULT
    CALL Errormsg(&
     & msg="No match found for given from = "//from, &
     & file=__FILE__, &
     & routine="obj_GetSym2()", &
     & line=__LINE__, &
     & unitno=stderr)
    STOP
  END SELECT
END IF

END PROCEDURE obj_GetSym2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
