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

!! authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_GetMethods) Methods
USE CSRSparsity_Method
USE ConvertUtility
USE InputUtility
USE BaseType, ONLY: DOF_
USE DOF_GetMethods
USE CSRMatrix_GetMethods
USE CSRMatrix_SetMethods
USE ErrorHandling
USE GlobalData, ONLY: DofToNodes, NodesToDOF, FMT_NODES, FMT_DOF, stderr
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetIA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIA
ans = GetIA(obj%csr, irow)
END PROCEDURE obj_GetIA

!----------------------------------------------------------------------------
!                                                                     GetJA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetJA
ans = GetJA(obj%csr, indx)
END PROCEDURE obj_GetJA

!----------------------------------------------------------------------------
!                                                           GetSingleValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingleValue
ans = obj%A(indx)
END PROCEDURE obj_GetSingleValue

!----------------------------------------------------------------------------
!                                                           GetSeveralValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSeveralValue
INTEGER(I4B) :: ii, tsize
tsize = SIZE(indx)
DO ii = 1, tsize; ans(ii) = obj%A(indx(ii)); END DO
END PROCEDURE obj_GetSeveralValue

!----------------------------------------------------------------------------
!                                                             GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStorageFMT
IF (i .EQ. 1) THEN
  ans = obj%csr%idof%storageFMT
ELSE
  ans = obj%csr%jdof%storageFMT
END IF
END PROCEDURE obj_GetStorageFMT

!----------------------------------------------------------------------------
!                                                            GetMatrixProp
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMatrixProp
ans = TRIM(obj%matrixProp)
END PROCEDURE obj_GetMatrixProp

!----------------------------------------------------------------------------
!                                                              GetDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDOFPointer
IF (i .EQ. 1) THEN
  ans => obj%csr%idof
ELSE
  ans => obj%csr%jdof
END IF
END PROCEDURE obj_GetDOFPointer

!----------------------------------------------------------------------------
!                                                                 isSquare
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isSquare
IF (obj%csr%nrow .EQ. obj%csr%ncol) THEN
  ans = .TRUE.
ELSE
  ans = .FALSE.
END IF
END PROCEDURE obj_isSquare

!----------------------------------------------------------------------------
!                                                               isRectangle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isRectangle
IF (obj%csr%nrow .EQ. obj%csr%ncol) THEN
  ans = .FALSE.
ELSE
  ans = .TRUE.
END IF
END PROCEDURE obj_isRectangle

!----------------------------------------------------------------------------
!                                                               GetColNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColNumber
ans = GetColNumber(obj%csr, indx)
END PROCEDURE obj_GetColNumber

!----------------------------------------------------------------------------
!                                                               GetColIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColIndex
ans = GetColIndex(obj%csr, irow)
END PROCEDURE obj_GetColIndex

!----------------------------------------------------------------------------
!                                                              startColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_startColumn
ans = obj%csr.startColumn.irow
END PROCEDURE obj_startColumn

!----------------------------------------------------------------------------
!                                                              endColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_endColumn
ans = obj%csr.endColumn.irow
END PROCEDURE obj_endColumn

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get0
! Internal variables
INTEGER(I4B), ALLOCATABLE :: indx(:)
INTEGER(I4B) :: ii, jj

nrow = .tdof. (obj%csr%idof)
nrow = nrow * SIZE(nodenum)

ncol = .tdof. (obj%csr%jdof)
ncol = ncol * SIZE(nodenum)

ALLOCATE (indx(nrow + ncol))

CALL GetIndex_(obj=obj%csr%idof, nodeNum=nodenum, &
               ans=indx(1:), tsize=ii)

CALL GetIndex_(obj=obj%csr%jdof, nodeNum=nodenum, &
               ans=indx(nrow + 1:), tsize=ii)

! row = GetIndex(obj=obj%csr%idof, nodeNum=nodenum)
! col = GetIndex(obj=obj%csr%jdof, nodeNum=nodenum)

VALUE(1:nrow, 1:ncol) = 0.0_DFP

DO ii = 1, nrow
  DO jj = 1, ncol
    CALL GetValue(obj=obj, VALUE=VALUE(ii, jj), irow=indx(ii), &
                  icolumn=indx(nrow + jj))
  END DO
END DO

DEALLOCATE (indx)

END PROCEDURE obj_Get0

!----------------------------------------------------------------------------
!                                                                 GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
REAL(DFP) :: m2(SIZE(VALUE, 1), SIZE(VALUE, 2))
INTEGER(I4B) :: tdof, nns, myfmt

CALL GetValue(obj=obj, nodenum=nodenum, VALUE=m2, nrow=nrow, ncol=ncol)

myfmt = GetStorageFMT(obj, 1)

IF (myfmt .EQ. storageFMT) THEN
  VALUE(1:nrow, 1:ncol) = m2(1:nrow, 1:ncol)
  RETURN
END IF

tdof = .tdof. (obj%csr%idof)
nns = SIZE(nodenum)

SELECT CASE (storageFMT)

CASE (FMT_NODES)

  CALL ConvertSafe(From=m2(1:nrow, 1:ncol), To=VALUE(1:nrow, 1:ncol), &
                   Conversion=DOFToNodes, nns=nns, tDOF=tdof)

CASE (FMT_DOF)

  CALL ConvertSafe(From=m2(1:nrow, 1:ncol), To=VALUE(1:nrow, 1:ncol), &
                   Conversion=NodesToDOF, nns=nns, tDOF=tdof)

END SELECT

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: j

! VALUE = 0.0_DFP
DO j = obj%csr%IA(irow), obj%csr%IA(irow + 1) - 1
  IF (obj%csr%JA(j) .EQ. icolumn) THEN
    VALUE = obj%A(j)
    EXIT
  END IF
END DO

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                               GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10
INTEGER(I4B) :: ii, jj

! VALUE = 0.0_DFP
nrow = SIZE(irow)
ncol = SIZE(icolumn)
DO jj = 1, ncol
  DO ii = 1, nrow
    CALL GetValue(obj=obj, VALUE=VALUE(ii, jj), irow=irow(ii), &
                  icolumn=icolumn(jj))
  END DO
END DO

END PROCEDURE obj_Get10

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
INTEGER(I4B) :: irow, icolumn
irow = GetNodeLoc(obj=obj%csr%idof, nodenum=iNodeNum, idof=iDOF)
icolumn = GetNodeLoc(obj=obj%csr%jdof, nodenum=jNodeNum, idof=jDOF)
CALL GetValue(obj=obj, irow=irow, icolumn=icolumn, VALUE=VALUE)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj

row = GetIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = GetIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)

nrow = SIZE(row)
ncol = SIZE(col)

DO ii = 1, nrow
  DO jj = 1, ncol
    CALL GetValue(obj=obj, VALUE=VALUE(ii, jj), irow=row(ii), &
                  icolumn=col(jj))
  END DO
END DO

IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
INTEGER(I4B) :: irow, icolumn
irow = GetNodeLoc(obj=obj%csr%idof, nodenum=iNodeNum, ivar=ivar, idof=iDOF)
icolumn = GetNodeLoc(obj=obj%csr%jdof, nodenum=jNodeNum, ivar=jvar, idof=jDOF)
CALL GetValue(obj=obj, irow=irow, icolumn=icolumn, VALUE=VALUE)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj

row = GetIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = GetIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)

nrow = SIZE(row)
ncol = SIZE(col)

DO ii = 1, nrow
  DO jj = 1, ncol
    CALL GetValue(obj=obj, VALUE=VALUE(ii, jj), irow=row(ii), &
                  icolumn=col(jj))
  END DO
END DO

IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
INTEGER(I4B) :: irow, icolumn

irow = GetNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=iNodeNum, &
  & ivar=ivar, &
  & spacecompo=ispacecompo, &
  & timecompo=itimecompo)

icolumn = GetNodeLoc( &
    & obj=obj%csr%jdof, &
    & nodenum=jNodeNum, &
    & ivar=jvar, &
    & spacecompo=jspacecompo, &
    & timecompo=jtimecompo)

CALL GetValue(obj=obj, irow=irow, icolumn=icolumn, VALUE=VALUE)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                   GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
INTEGER(I4B) :: irow(SIZE(iNodeNum)), icolumn(SIZE(jNodeNum))

irow = GetNodeLoc(obj=obj%csr%idof, nodenum=iNodeNum, ivar=ivar, &
                  spacecompo=ispacecompo, timecompo=itimecompo)

icolumn = GetNodeLoc(obj=obj%csr%jdof, nodenum=jNodeNum, ivar=jvar, &
                     spacecompo=jspacecompo, timecompo=jtimecompo)

CALL GetValue(obj=obj, irow=irow, icolumn=icolumn, VALUE=VALUE, &
              nrow=nrow, ncol=ncol)
END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                             GetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
CHARACTER(*), PARAMETER :: myName = "CSR2CSR_Get_Master()"
CHARACTER(*), PARAMETER :: filename = __FILE__
INTEGER(I4B) :: myindx(6, 2), idof1, jdof1, idof2, jdof2,  &
  & row1, row2, col1, col2, ierr0
CLASS(DOF_), POINTER :: dof_obj
LOGICAL(LGT) :: problem

! 1 ivar
! 2 ispacecompo
! 3 itimecompo
! 4 jvar
! 5 jspacecompo
! 6 jtimecompo

IF (PRESENT(ierr)) ierr = 0

myindx(1, 1) = Input(default=1, option=ivar1)
myindx(2, 1) = Input(default=1, option=ispacecompo1)
myindx(3, 1) = Input(default=1, option=itimecompo1)
myindx(4, 1) = Input(default=1, option=jvar1)
myindx(5, 1) = Input(default=1, option=jspacecompo1)
myindx(6, 1) = Input(default=1, option=jtimecompo1)

myindx(1, 2) = Input(default=1, option=ivar2)
myindx(2, 2) = Input(default=1, option=ispacecompo2)
myindx(3, 2) = Input(default=1, option=itimecompo2)
myindx(4, 2) = Input(default=1, option=jvar2)
myindx(5, 2) = Input(default=1, option=jspacecompo2)
myindx(6, 2) = Input(default=1, option=jtimecompo2)

NULLIFY (dof_obj)

dof_obj => GetDOFPointer(obj1, 1)
problem = .NOT. ASSOCIATED(dof_obj)
IF (problem) THEN
  CALL ErrorMSG( &
    & "Cannot get idof pointer from obj1",  &
    & filename, &
    & myName, &
    & __LINE__, stderr)
  ierr0 = -1
  IF (PRESENT(ierr)) ierr = ierr0
  RETURN
END IF
idof1 = GetIDOF(obj=dof_obj,  &
  & ivar=myindx(1, 1),  &
  & spacecompo=myindx(2, 1),  &
  & timecompo=myindx(3, 1))
row1 = dof_obj.tNodes.idof1

dof_obj => GetDOFPointer(obj1, 2)
problem = .NOT. ASSOCIATED(dof_obj)
IF (problem) THEN
  CALL ErrorMSG( &
    & "Cannot get jdof pointer from obj1",  &
    & filename, &
    & myName, &
    & __LINE__, stderr)
  ierr0 = -2
  IF (PRESENT(ierr)) ierr = ierr0
  RETURN
END IF
jdof1 = GetIDOF(obj=dof_obj,  &
  & ivar=myindx(4, 1),  &
  & spacecompo=myindx(5, 1),  &
  & timecompo=myindx(6, 1))
col1 = dof_obj.tNodes.jdof1

dof_obj => GetDOFPointer(obj2, 1)
problem = .NOT. ASSOCIATED(dof_obj)
IF (problem) THEN
  CALL ErrorMSG( &
    & "Cannot get idof pointer from obj2",  &
    & filename, &
    & myName, &
    & __LINE__, stderr)
  ierr0 = -3
  IF (PRESENT(ierr)) ierr = ierr0
  RETURN
END IF
idof2 = GetIDOF(obj=dof_obj,  &
  & ivar=myindx(1, 2),  &
  & spacecompo=myindx(2, 2),  &
  & timecompo=myindx(3, 2))
row2 = dof_obj.tNodes.idof2

dof_obj => GetDOFPointer(obj2, 2)
problem = .NOT. ASSOCIATED(dof_obj)
IF (problem) THEN
  CALL ErrorMSG( &
    & "Cannot get jdof pointer from obj2",  &
    & filename, &
    & myName, &
    & __LINE__, stderr)
  ierr0 = -4
  IF (PRESENT(ierr)) ierr = ierr0
  RETURN
END IF
jdof2 = GetIDOF(obj=dof_obj,  &
  & ivar=myindx(4, 2),  &
  & spacecompo=myindx(5, 2),  &
  & timecompo=myindx(6, 2))
col2 = dof_obj.tNodes.jdof2

NULLIFY (dof_obj)

problem = (row1 .NE. row2) .OR. (col1 .NE. col2)
IF (problem) THEN
  CALL ErrorMSG( &
    & "Some error occured in sizes.", &
    & filename, &
    & myName, &
    & __LINE__, stderr)
  ierr0 = -5
  IF (PRESENT(ierr)) ierr = ierr0
  RETURN
END IF

CALL CSR2CSR_Get_Master(obj1=obj1, obj2=obj2, idof1=idof1, idof2=idof2,  &
& jdof1=jdof1, jdof2=jdof2, tNodes1=row1, tNodes2=col1)

END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                            CSR2CSRGetValue
!----------------------------------------------------------------------------

MODULE PROCEDURE CSR2CSR_Get_Master
INTEGER(I4B) :: ii, jj
REAL(DFP) :: VALUE
DO jj = 1, tNodes2
  DO ii = 1, tNodes1
    CALL GetValue(obj=obj1,  &
      & idof=idof1,  &
      & jdof=jdof1,  &
      & iNodeNum=ii,  &
      & jNodeNum=jj, &
      & VALUE=VALUE)

    CALL Set(obj=obj2,  &
      & idof=idof2,  &
      & jdof=jdof2,  &
      & iNodeNum=ii,  &
      & jNodeNum=jj, &
      & VALUE=VALUE)
  END DO
END DO

END PROCEDURE CSR2CSR_Get_Master

END SUBMODULE Methods
