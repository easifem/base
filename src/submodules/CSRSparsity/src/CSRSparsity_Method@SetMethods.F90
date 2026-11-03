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
USE BaseMethod
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
  CALL ErrorMSG( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity1()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL ErrorMSG( &
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
    CALL APPEND(obj%row(rowIndx(ii)), colIndx)
  END DO

END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
! INTEGER(I4B) :: i
! DO i = 1, SIZE(row)
!   CALL SetSparsity(obj, row(i), col(i)%val)
! END DO

INTEGER(I4B) :: ii, jj, kk, tColIndx, tRowIndx
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL ErrorMSG( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity2()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL ErrorMSG( &
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
    CALL ErrorMSG( &
      "size of row not same as the size of col!", &
      "CSRSparsity_Method@SetMethods.F90", &
      "obj_SetSparsity2()", &
      __LINE__, stderr)
    STOP
  END IF
#endif

  tColIndx = 0
  jj = 0
  DO ii = 1, SIZE(col)
    kk = SIZE(col(ii))
    tColIndx = tColIndx + kk
    jj = MAX(jj, kk)
  END DO

  CALL Reallocate(colIndx, jj * (.tdof. obj%jdof))
  CALL Reallocate(rowIndx, (.tdof. obj%idof))

  obj%nnz = obj%nnz + tColIndx * SIZE(rowIndx) * SIZE(row)

  DO ii = 1, SIZE(row)
    CALL GetIndex_(obj=obj%jdof, nodeNum=col(ii)%val, ans=colIndx, &
                   tsize=tColIndx)
    CALL GetIndex_(obj=obj%idof, nodeNum=row(ii), ans=rowIndx, tsize=tRowIndx)

    DO jj = 1, tRowIndx
      CALL APPEND(obj%row(rowIndx(jj)), colIndx(1:tColIndx))
    END DO

  END DO

END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity3
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: colIndx(:), rowIndx(:)
!
#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL ErrorMSG( &
    "Instance of CSRSparsity is not initiated!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%isSparsityLock) THEN
  CALL ErrorMSG( &
    "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%idof%StorageFMT .EQ. NODES_FMT) THEN
  CALL ErrorMSG( &
    "This subroutine works for storage format FMT_DOF, only", &
    "CSRSparsity_Method@SetMethods.F90", &
    "obj_SetSparsity3()", &
    __LINE__, stderr)
  STOP
END IF

IF (obj%jdof%StorageFMT .EQ. NODES_FMT) THEN
  CALL ErrorMSG( &
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
    CALL APPEND(obj%row(rowIndx(ii)), colIndx)
  END DO
END IF

IF (ALLOCATED(colIndx)) DEALLOCATE (colIndx)
IF (ALLOCATED(rowIndx)) DEALLOCATE (rowIndx)
END PROCEDURE obj_SetSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity4
INTEGER(I4B) :: i
DO i = 1, SIZE(row)
  CALL SetSparsity(obj, row(i), col(i)%val, ivar, jvar)
END DO
END PROCEDURE obj_SetSparsity4

!----------------------------------------------------------------------------
!                                                                 setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity5
INTEGER(I4B), ALLOCATABLE :: row(:), graphT(:, :)
TYPE(IntVector_), ALLOCATABLE :: col(:)
INTEGER(I4B) :: ii, jj, kk, nn, nrow, ncol
!
nrow = SIZE(graph, 1)
ncol = SIZE(graph, 2)
graphT = TRANSPOSE(graph)
!
CALL Reallocate(row, nrow)
ALLOCATE (col(nrow))
!
DO ii = 1, nrow
  row(ii) = ii
  nn = COUNT(graphT(:, ii) .NE. 0)
  CALL ALLOCATE (col(ii), nn)
  kk = 0
  DO jj = 1, ncol
    IF (graphT(jj, ii) .NE. 0) THEN
      kk = kk + 1
      CALL Set(col(ii), indx=kk, VALUE=jj)
    END IF
  END DO
END DO
!
CALL setSparsity(obj=obj, row=row, col=col)
!
IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
IF (ALLOCATED(graphT)) DEALLOCATE (graphT)
!
END PROCEDURE obj_SetSparsity5

!----------------------------------------------------------------------------
!                                                                 setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity6
INTEGER(I4B), ALLOCATABLE :: row(:)
LOGICAL(LGT), ALLOCATABLE :: graphT(:, :)
TYPE(IntVector_), ALLOCATABLE :: col(:)
INTEGER(I4B) :: ii, jj, kk, nn, nrow, ncol
!
nrow = SIZE(graph, 1)
ncol = SIZE(graph, 2)
graphT = TRANSPOSE(graph)
!
CALL Reallocate(row, nrow)
ALLOCATE (col(nrow))
!
DO ii = 1, nrow
  row(ii) = ii
  nn = COUNT(graphT(:, ii))
  CALL ALLOCATE (col(ii), nn)
  kk = 0
  DO jj = 1, ncol
    IF (graphT(jj, ii)) THEN
      kk = kk + 1
      CALL Set(col(ii), indx=kk, VALUE=jj)
    END IF
  END DO
END DO
!
CALL setSparsity(obj=obj, row=row, col=col)
!
IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
IF (ALLOCATED(graphT)) DEALLOCATE (graphT)
!
END PROCEDURE obj_SetSparsity6

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity_final
INTEGER(I4B) :: i, j, k
INTEGER(I4B), ALLOCATABLE :: intvec(:)
!
IF (.NOT. obj%isInitiated) THEN
  CALL ErrorMSG( &
    & "Instance of CSRSparsity is not initiated!", &
    & "CSRSparsity_Method@SetMethods.F90", &
    & "obj_SetSparsity_final()", &
    & __LINE__, stderr)
  STOP
END IF
!
IF (obj%isSparsityLock) THEN
  CALL WarningMSG( &
    & "Instance of CSRSparsity is locked for setting sparsity pattern!", &
    & "CSRSparsity_Method@SetMethods.F90", &
    & "obj_SetSparsity_final()", &
    & __LINE__, stderr)
  RETURN
ELSE
  obj%isSparsityLock = .TRUE.
END IF
!
! Remove duplicate entries in obj%row( irow )%col
!
IF (ALLOCATED(obj%row)) THEN
  k = 0
  DO i = 1, obj%nrow
    CALL RemoveDuplicates(obj%row(i))
    k = k + SIZE(obj%row(i))
  END DO
  !
  ! update nnz: number of non zeros
  !
  obj%nnz = k
  !
  ! allocate obj%JA and obj%A
  !
  CALL Reallocate(obj%JA, obj%nnz)
  !
  ! convert data into IA, JA
  !
  obj%IA(1) = 1
  !
  DO i = 1, obj%nrow
    ! obj%rowSize( i ) = SIZE( obj%row( i ) )
    k = SIZE(obj%row(i))
    obj%IA(i + 1) = obj%IA(i) + k
    IF (k .NE. 0) &
      & obj%JA(obj%IA(i):obj%IA(i + 1) - 1) = obj%row(i)%Val
  END DO
  !
  DEALLOCATE (obj%row)
  !
END IF
!
j = SIZE(obj%JA)
!
IF (j .GT. obj%nnz) THEN
  !
  intvec = obj%JA(1:obj%nnz)
  CALL Reallocate(obj%JA, obj%nnz)
  obj%JA = intvec
  CALL Reallocate(intvec, obj%ncol)
  !
END IF
!
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
