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
! date:         22 March 2021
! summary: It contains method for setting values in [[CSRMatrix_]]

SUBMODULE(CSRMatrix_AddMethods) Methods
USE GlobalData, ONLY: FMT_NODES, FMT_DOF, NodesToDOF, DofToNodes
USE DOF_Method, ONLY: GetIndex, GetNodeLoc, OPERATOR(.tdof.)
USE ConvertUtility, ONLY: Convert
USE CSRSparsity_Method, ONLY: CSR_SetIA => SetIA, CSR_SetJA => SetJA
USE InputUtility, ONLY: Input
USE F95_BLAS, ONLY: Scal, Copy
USE ReallocateUtility, ONLY: Reallocate

USE CSRMatrix_Method, ONLY: OPERATOR(.StorageFMT.), &
                            CSRMatrix_GetColIndex => GetColIndex, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_GetNNZ => GetNNZ, &
                            CSRMatrixAPLSB, &
                            CSRMatrixAPLSBSorted

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = __FILE__
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            AddContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE AddMaster1
! Internal variables
INTEGER(I4B) :: ii, jj, kk, trow, tcol

trow = SIZE(row)
tcol = SIZE(col)

DO ii = 1, trow
  DO kk = 1, tcol
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * VALUE(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO

END PROCEDURE AddMaster1

!----------------------------------------------------------------------------
!                                                                 AddMaster
!----------------------------------------------------------------------------

MODULE PROCEDURE AddMaster2
! Internal variables
INTEGER(I4B) :: ii, jj, kk, trow, tcol

trow = SIZE(row)
tcol = SIZE(col)

DO ii = 1, trow
  DO kk = 1, tcol
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * VALUE
        EXIT
      END IF
    END DO
  END DO
END DO

END PROCEDURE AddMaster2

!----------------------------------------------------------------------------
!                                                            AddContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add0
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetIndex(obj=obj%csr%idof, nodeNum=nodenum)
col = GetIndex(obj=obj%csr%jdof, nodeNum=nodenum)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
END PROCEDURE obj_Add0

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
REAL(DFP), ALLOCATABLE :: m2(:, :)
INTEGER(I4B) :: tdof

tdof = .tdof.obj%csr%idof
SELECT CASE (storageFMT)
CASE (FMT_NODES)
  IF ((obj.StorageFMT.1) .EQ. FMT_NODES) THEN
    m2 = VALUE
  ELSE
    CALL Convert(From=VALUE, To=m2, Conversion=NodesToDOF, &
                 nns=SIZE(nodenum), tDOF=tdof)
  END IF

CASE (FMT_DOF)
  IF ((obj.StorageFMT.1) .EQ. FMT_DOF) THEN
    m2 = VALUE
  ELSE
    CALL Convert(From=VALUE, To=m2, Conversion=DofToNodes, &
                 nns=SIZE(nodenum), tDOF=tdof)
  END IF
END SELECT

CALL Add(obj=obj, nodenum=nodenum, VALUE=m2, scale=scale)
IF (ALLOCATED(m2)) DEALLOCATE (m2)

END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
obj%A = obj%A + scale * VALUE
END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add3
INTEGER(I4B) :: j

DO j = obj%csr%IA(irow), obj%csr%IA(irow + 1) - 1
  IF (obj%csr%JA(j) .EQ. icolumn) THEN
    obj%A(j) = obj%A(j) + scale * VALUE
    EXIT
  END IF
END DO
END PROCEDURE obj_Add3

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add4

CALL Add( &
  obj=obj, &
  irow=GetNodeLoc(obj=obj%csr%idof, nodenum=inodenum, idof=idof), &
  icolumn=GetNodeLoc(obj=obj%csr%jdof, nodenum=jnodenum, idof=jdof), &
  VALUE=VALUE, scale=scale)

END PROCEDURE obj_Add4

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add5
REAL(DFP), ALLOCATABLE :: m2(:, :)
INTEGER(I4B) :: tdof1, tdof2

tdof1 = .tdof.obj%csr%idof
tdof2 = .tdof.obj%csr%jdof

ALLOCATE (m2(tdof1 * SIZE(nodenum), tdof2 * SIZE(nodenum)))

m2 = VALUE

CALL Add(obj=obj, nodenum=nodenum, VALUE=m2, scale=scale)
DEALLOCATE (m2)
END PROCEDURE obj_Add5

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add6
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = GetIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

DEALLOCATE (row, col)
END PROCEDURE obj_Add6

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add7
CALL Add( &
  obj=obj, irow=getNodeLoc(obj=obj%csr%idof, nodenum=iNodeNum, ivar=ivar, &
                           idof=iDOF), &
  icolumn=getNodeLoc(obj=obj%csr%jdof, nodenum=jNodeNum, ivar=jvar, &
                     idof=jDOF), &
  VALUE=VALUE, scale=scale)
END PROCEDURE obj_Add7

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add8
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = GetIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

DEALLOCATE (row, col)
END PROCEDURE obj_Add8

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add9
CALL Add( &
  obj=obj, &
  irow=GetNodeLoc(obj=obj%csr%idof, nodenum=iNodeNum, ivar=ivar, &
                  spacecompo=ispacecompo, timecompo=itimecompo), &
  icolumn=GetNodeLoc(obj=obj%csr%jdof, nodenum=jNodeNum, ivar=jvar, &
                     spacecompo=jspacecompo, timecompo=jtimecompo), &
  VALUE=VALUE, scale=scale)
END PROCEDURE obj_Add9

!----------------------------------------------------------------------------
!                                                                     Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add10
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
!
row = GetIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = GetIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)
CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

DEALLOCATE (row, col)
END PROCEDURE obj_Add10

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add11
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)
CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

DEALLOCATE (row, col)
END PROCEDURE obj_Add11

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add12
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
                 spacecompo=ispacecompo, timecompo=itimecompo)

col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
                 spacecompo=jspacecompo, timecompo=jtimecompo)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)

DEALLOCATE (row, col)
END PROCEDURE obj_Add12

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add13
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
                 spacecompo=ispacecompo, timecompo=itimecompo)

col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
                 spacecompo=jspacecompo, timecompo=jtimecompo)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)
DEALLOCATE (row, col)
END PROCEDURE obj_Add13

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add14
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)

row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
                 spacecompo=ispacecompo, timecompo=itimecompo)

col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
                 spacecompo=jspacecompo, timecompo=jtimecompo)

CALL AddMaster(obj=obj, row=row, col=col, VALUE=VALUE, scale=scale)
DEALLOCATE (row, col)
END PROCEDURE obj_Add14

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add15
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Add15()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: sameStructure0, isSorted0
INTEGER(I4B) :: nrow, ncol, nzmax, ierr

sameStructure0 = Input(default=.FALSE., option=isSameStructure)

IF (sameStructure0) THEN
  obj%A = obj%A + scale * VALUE%A
  RETURN
END IF

isSorted0 = Input(default=.FALSE., option=isSorted)

nrow = CSRMatrix_SIZE(obj, 1)
ncol = CSRMatrix_SIZE(obj, 2)
nzmax = CSRMatrix_GetNNZ(obj)

IF (isSorted0) THEN
  CALL CSRMatrixAPLSBSorted( &
    nrow=nrow, ncol=ncol, a=obj%A, ja=obj%csr%ja, ia=obj%csr%ia, &
    s=scale, b=VALUE%a, jb=VALUE%csr%ja, ib=VALUE%csr%ia, c=obj%A, &
    jc=obj%csr%ja, ic=obj%csr%ia, nzmax=nzmax, ierr=ierr)

ELSE
  CALL CSRMatrixAPLSB( &
    nrow=nrow, ncol=ncol, a=obj%A, ja=obj%csr%ja, ia=obj%csr%ia, &
    s=scale, b=VALUE%a, jb=VALUE%csr%ja, ib=VALUE%csr%ia, &
    c=obj%A, jc=obj%csr%ja, ic=obj%csr%ia, nzmax=nzmax, &
    ierr=ierr)
END IF

#ifdef DEBUG_VER
isok = ierr .NE. 0
CALL AssertError1(isok, myName, modName, __LINE__, &
                  "Some error occured while calling CSRMarixAPLSB.")
#endif
END PROCEDURE obj_Add15

!----------------------------------------------------------------------------
!                                                               AddToSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AddToSTMatrix1
REAL(DFP) :: scale0
INTEGER(I4B) :: icol
INTEGER(I4B) :: irow_rhs, trow_rhs, icol_rhs, colIndex_rhs(2), &
                tcol_rhs
INTEGER(I4B) :: irow_lhs, icol_lhs, colIndex_lhs(2), &
                offAdd_row_lhs, offAdd_col_lhs

scale0 = Input(default=1.0_DFP, option=scale)

trow_rhs = CSRMatrix_Size(obj=VALUE, dims=1)
offAdd_row_lhs = (itimecompo - 1) * trow_rhs

! start row loop
DO irow_rhs = 1, trow_rhs
  ! Get the starting and ending data index for irow in value
  colIndex_rhs = CSRMatrix_GetColIndex(obj=VALUE, irow=irow_rhs)
  tcol_rhs = colIndex_rhs(2) - colIndex_rhs(1) + 1

  ! Calculate the column offAdd for lhs
  offAdd_col_lhs = (jtimecompo - 1) * tcol_rhs

  irow_lhs = offAdd_row_lhs + irow_rhs
  colIndex_lhs = CSRMatrix_GetColIndex(obj=obj, irow=irow_lhs)

  DO icol = 1, tcol_rhs
    icol_rhs = colIndex_rhs(1) + icol - 1
    icol_lhs = colIndex_lhs(1) + offAdd_col_lhs + icol - 1

    obj%A(icol_lhs) = obj%A(icol_lhs) + scale0 * VALUE%A(icol_rhs)
  END DO
END DO
END PROCEDURE obj_AddToSTMatrix1

!----------------------------------------------------------------------------
!                                                             Include Errror
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
