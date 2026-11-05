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
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE(CSRSparsity_Method) SetMethods
USE BaseType, ONLY: TypeDOFOpt
USE DOF_Method, ONLY: OPERATOR(.tdof.)
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetIndex
USE ErrorHandling, ONLY: Warningmsg
USE ErrorHandling, ONLY: Errormsg
USE GlobalData, ONLY: stderr
USE IntegerUtility, ONLY: RemoveDuplicates_
USE IntVector_Method, ONLY: IntVectorInitiate => Initiate
USE IntVector_Method, ONLY: IntVectorSize => Size
USE IntVector_Method, ONLY: IntVectorRemoveDuplicates => RemoveDuplicates
USE IntVector_Method, ONLY: IntVectorAppend => Append
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity1()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity1()", &
    __LINE__, stderr)
  STOP
END IF
#endif

IF (.NOT. ALLOCATED(obj%row)) ALLOCATE (obj%row(obj%nrow))

IF (SIZE(col) .GT. 0) THEN
  ! No sorting here: SetSparsity_final sorts and removes duplicates
  colIndx = GetIndex(obj=obj%jdof, nodeNum=col)
  ! Size of colIndx is size(col)*.tdof. obj%jdof
  rowIndx = GetIndex(obj=obj%idof, nodeNum=row)
  ! Size of rowIndx is .tdof. obj%idof
  obj%nnz = obj%nnz + SIZE(col) * (.tdof. obj%jdof) * SIZE(rowIndx)

  DO ii = 1, SIZE(rowIndx)
    CALL IntVectorAppend(obj%row(rowIndx(ii)), colIndx)
  END DO

END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
INTEGER(I4B) :: ii, jj, kk, tColIndx, tRowIndx
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity2()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity2()", &
    __LINE__, stderr)
  STOP
END IF
#endif

IF (.NOT. ALLOCATED(obj%row)) ALLOCATE (obj%row(obj%nrow))

IF (SIZE(col) .GT. 0) THEN

#ifdef DEBUG_VER
  IF (SIZE(row) .NE. SIZE(col)) THEN
    CALL Errormsg( &
      "size of row not same as the size of col!", &
      "CSRSparsity_Method@SetMethods.F90", &
      "obj_SetSparsity2()", &
      __LINE__, stderr)
    STOP
  END IF
#endif

  ! largest column list, to allocate the buffers only once
  jj = 0
  DO ii = 1, SIZE(col)
    kk = IntVectorSize(col(ii))
    jj = MAX(jj, kk)
  END DO

  CALL Reallocate(colIndx, jj * (.tdof. obj%jdof))
  CALL Reallocate(rowIndx, (.tdof. obj%idof))

  DO ii = 1, SIZE(row)
    CALL GetIndex_(obj=obj%jdof, nodeNum=col(ii)%val(1:col(ii)%tsize), &
                   ans=colIndx, tsize=tColIndx)
    CALL GetIndex_(obj=obj%idof, nodeNum=row(ii), ans=rowIndx, tsize=tRowIndx)

    obj%nnz = obj%nnz + tColIndx * tRowIndx

    DO jj = 1, tRowIndx
      CALL IntVectorAppend(obj%row(rowIndx(jj)), colIndx(1:tColIndx))
    END DO

  END DO

END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                                SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%idof%StorageFMT .EQ. TypeDOFOpt%storageFormatNodes) THEN
  CALL Errormsg( &
    "This subroutine works for storage format FMT_DOF, only", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%jdof%StorageFMT .EQ. TypeDOFOpt%storageFormatNodes) THEN
  CALL Errormsg( &
    "This subroutine works for storage format FMT_DOF, only", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF
#endif

IF (.NOT. ALLOCATED(obj%row)) ALLOCATE (obj%row(obj%nrow))

IF (SIZE(col) .GT. 0) THEN
  colIndx = GetIndex(obj=obj%jdof, nodeNum=col, ivar=jvar)
  rowIndx = GetIndex(obj=obj%idof, nodeNum=row, ivar=ivar)
  obj%nnz = obj%nnz + SIZE(col) * (obj%jdof .tdof. jvar) * SIZE(rowIndx)
  DO ii = 1, SIZE(rowIndx)
    CALL IntVectorAppend(obj%row(rowIndx(ii)), colIndx)
  END DO
END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
INTEGER(I4B) :: ii, jj, kk, tColIndx, tRowIndx
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity4()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity4()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%idof%StorageFMT .EQ. TypeDOFOpt%storageFormatNodes) THEN
  CALL Errormsg( &
    "This subroutine works for storage format FMT_DOF, only", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity4()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%jdof%StorageFMT .EQ. TypeDOFOpt%storageFormatNodes) THEN
  CALL Errormsg( &
    "This subroutine works for storage format FMT_DOF, only", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity4()", &
    __LINE__, stderr)
  STOP
END IF

IF (SIZE(row) .NE. SIZE(col)) THEN
  CALL Errormsg( &
    "size of row not same as the size of col!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity4()", &
    __LINE__, stderr)
  STOP
END IF
#endif

IF (.NOT. ALLOCATED(obj%row)) ALLOCATE (obj%row(obj%nrow))

IF (SIZE(col) .GT. 0) THEN

  ! largest column list, to allocate the buffers only once
  jj = 0
  DO ii = 1, SIZE(col)
    kk = IntVectorSize(col(ii))
    jj = MAX(jj, kk)
  END DO

  CALL Reallocate(colIndx, jj * (obj%jdof .tdof. jvar))
  CALL Reallocate(rowIndx, (obj%idof .tdof. ivar))

  ! No sorting here: SetSparsity_final sorts and removes duplicates
  DO ii = 1, SIZE(row)
    CALL GetIndex_(obj=obj%jdof, nodeNum=col(ii)%val(1:col(ii)%tsize), &
                   ivar=jvar, ans=colIndx, tsize=tColIndx)
    CALL GetIndex_(obj=obj%idof, nodeNum=row(ii), ivar=ivar, &
                   ans=rowIndx, tsize=tRowIndx)

    obj%nnz = obj%nnz + tColIndx * tRowIndx

    DO jj = 1, tRowIndx
      CALL IntVectorAppend(obj%row(rowIndx(jj)), colIndx(1:tColIndx))
    END DO
  END DO

END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                                 setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity5
INTEGER(I4B), ALLOCATABLE :: row(:), cnt(:)
TYPE(IntVector_), ALLOCATABLE :: col(:)
INTEGER(I4B) :: ii, jj, nrow, ncol

nrow = SIZE(graph, 1)
ncol = SIZE(graph, 2)

! graph is column major: loop over columns outside, rows inside
CALL Reallocate(cnt, nrow)
DO jj = 1, ncol
  DO ii = 1, nrow
    IF (graph(ii, jj) .NE. 0) cnt(ii) = cnt(ii) + 1
  END DO
END DO

CALL Reallocate(row, nrow)
ALLOCATE (col(nrow))
DO ii = 1, nrow
  row(ii) = ii
  CALL IntVectorInitiate(col(ii), cnt(ii))
END DO

! cnt is reused as the fill position of each row
cnt = 0
DO jj = 1, ncol
  DO ii = 1, nrow
    IF (graph(ii, jj) .NE. 0) THEN
      cnt(ii) = cnt(ii) + 1
      col(ii)%val(cnt(ii)) = jj
    END IF
  END DO
END DO

CALL SetSparsity(obj=obj, row=row, col=col)

IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
IF (ALLOCATED(cnt)) DEALLOCATE (cnt)
END PROCEDURE obj_SetSparsity5

!----------------------------------------------------------------------------
!                                                                 setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity6
INTEGER(I4B), ALLOCATABLE :: row(:), cnt(:)
TYPE(IntVector_), ALLOCATABLE :: col(:)
INTEGER(I4B) :: ii, jj, nrow, ncol

nrow = SIZE(graph, 1)
ncol = SIZE(graph, 2)

! graph is column major: loop over columns outside, rows inside
CALL Reallocate(cnt, nrow)
DO jj = 1, ncol
  DO ii = 1, nrow
    IF (graph(ii, jj)) cnt(ii) = cnt(ii) + 1
  END DO
END DO

CALL Reallocate(row, nrow)
ALLOCATE (col(nrow))
DO ii = 1, nrow
  row(ii) = ii
  CALL IntVectorInitiate(col(ii), cnt(ii))
END DO

! cnt is reused as the fill position of each row
cnt = 0
DO jj = 1, ncol
  DO ii = 1, nrow
    IF (graph(ii, jj)) THEN
      cnt(ii) = cnt(ii) + 1
      col(ii)%val(cnt(ii)) = jj
    END IF
  END DO
END DO

CALL SetSparsity(obj=obj, row=row, col=col)

IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
IF (ALLOCATED(cnt)) DEALLOCATE (cnt)
END PROCEDURE obj_SetSparsity6

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity_final
INTEGER(I4B) :: i, j, k
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL Errormsg( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity_final()", &
    __LINE__, stderr)
  STOP
END IF
#endif

#ifdef DEBUG_VER
IF (obj%isSparsityLock) THEN
  CALL Warningmsg( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity_final()", &
    __LINE__, stderr)
  RETURN
END IF
#endif

IF (obj%isSparsityLock) THEN
  RETURN
END IF

obj%isSparsityLock = .TRUE.

! Remove duplicate entries in obj%row( irow )%col
IF (ALLOCATED(obj%row)) THEN
  k = 0
  DO i = 1, obj%nrow
    CALL IntVectorRemoveDuplicates(obj%row(i))
    k = k + IntVectorSize(obj%row(i))
  END DO

  ! update nnz: number of non zeros
  obj%nnz = k

  ! allocate obj%JA and obj%A
  CALL Reallocate(obj%JA, obj%nnz)

  ! convert data into IA, JA
  obj%IA(1) = 1

  DO i = 1, obj%nrow
    k = IntVectorSize(obj%row(i))
    obj%IA(i + 1) = obj%IA(i) + k
    IF (k .NE. 0) THEN
      obj%JA(obj%IA(i):obj%IA(i + 1) - 1) = obj%row(i)%Val
    END IF
  END DO

  DEALLOCATE (obj%row)

END IF

j = SIZE(obj%JA)
IF (j .GT. obj%nnz) THEN
  intvec = obj%JA(1:obj%nnz)
  CALL Reallocate(obj%JA, obj%nnz)
  obj%JA = intvec
  CALL Reallocate(intvec, obj%ncol)
END IF
END PROCEDURE obj_SetSparsity_final

!----------------------------------------------------------------------------
!                                                                   SetIA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIA
obj%IA(irow) = VALUE
END PROCEDURE obj_SetIA

!----------------------------------------------------------------------------
!                                                                  SetJA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetJA
obj%JA(indx) = VALUE
END PROCEDURE obj_SetJA

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
