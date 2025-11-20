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

SUBMODULE(CSRMatrix_SetMethods) Methods
USE GlobalData, ONLY: FMT_NODES, FMT_DOF, NodesToDOF, DofToNodes
USE DOF_Method, ONLY: GetIndex, GetNodeLoc, OPERATOR(.tdof.)
USE ConvertUtility, ONLY: Convert
USE CSRSparsity_Method, ONLY: CSR_SetIA => SetIA, CSR_SetJA => SetJA
USE InputUtility, ONLY: Input
USE F95_BLAS, ONLY: Scal, Copy
USE ReallocateUtility, ONLY: Reallocate

USE CSRMatrix_GetMethods, ONLY: OPERATOR(.StorageFMT.), &
                                CSRMatrix_GetColIndex => GetColIndex
USE CSRMatrix_ConstructorMethods, ONLY: CSRMatrix_Size => Size

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           SetSingleValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingleValue
obj%A(indx) = VALUE
END PROCEDURE obj_SetSingleValue

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set0
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk

row = GetIndex(obj=obj%csr%idof, nodeNum=nodenum)
col = GetIndex(obj=obj%csr%jdof, nodeNum=nodenum)
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO
IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
END PROCEDURE obj_set0

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set1
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
CALL Set(obj=obj, nodenum=nodenum, VALUE=m2)
IF (ALLOCATED(m2)) DEALLOCATE (m2)
END PROCEDURE obj_set1

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set2
obj%A = VALUE
END PROCEDURE obj_set2

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set3
INTEGER(I4B) :: i, j
!
DO j = obj%csr%IA(irow), obj%csr%IA(irow + 1) - 1
  IF (obj%csr%JA(j) .EQ. icolumn) obj%A(j) = VALUE
END DO
!
END PROCEDURE obj_set3

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set4
CALL set(obj=obj,  &
  & irow=GetNodeLoc( &
          & obj=obj%csr%idof, &
          & nodenum=iNodeNum, idof=iDOF), &
  & icolumn=GetNodeLoc( &
              & obj=obj%csr%jdof, &
              & nodenum=jNodeNum, idof=jDOF), &
  & VALUE=VALUE)
END PROCEDURE obj_set4

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set5
REAL(DFP), ALLOCATABLE :: m2(:, :)
INTEGER(I4B) :: tdof1, tdof2
!
tdof1 = .tdof. (obj%csr%idof)
tdof2 = .tdof. (obj%csr%jdof)
!
CALL Reallocate(m2, tdof1 * SIZE(nodenum), tdof2 * SIZE(nodenum))
m2 = VALUE
CALL Set(obj=obj, nodenum=nodenum, VALUE=m2)
!
DEALLOCATE (m2)
END PROCEDURE obj_set5

!----------------------------------------------------------------------------
!                                                                     set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set6
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO
!
IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
!
END PROCEDURE obj_set6

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set7
CALL set(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=iNodeNum, &
  & ivar=ivar, &
  & idof=iDOF),&
  & icolumn=GetNodeLoc( &
  & obj=obj%csr%jdof, &
  & nodenum=jNodeNum, &
  & ivar=jvar, &
  & idof=jDOF), &
  & VALUE=VALUE)
END PROCEDURE obj_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set8
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO
!
DEALLOCATE (row, col)
!
END PROCEDURE obj_set8

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set9
CALL set(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=iNodeNum, &
  & ivar=ivar, &
  & spacecompo=ispacecompo, &
  & timecompo=itimecompo),&
  & icolumn=GetNodeLoc( &
  & obj=obj%csr%jdof, &
  & nodenum=jNodeNum, &
  & ivar=jvar, &
  & spacecompo=jspacecompo, &
  & timecompo=jtimecompo), &
  & VALUE=VALUE)
END PROCEDURE obj_set9

!----------------------------------------------------------------------------
!                                                                     set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set10
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE
        EXIT
      END IF
    END DO
  END DO
END DO
!
DEALLOCATE (row, col)
!
END PROCEDURE obj_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set11
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE
        EXIT
      END IF
    END DO
  END DO
END DO
!
DEALLOCATE (row, col)
!
END PROCEDURE obj_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set12
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE
        EXIT
      END IF
    END DO
  END DO
END DO
!
DEALLOCATE (row, col)
!
END PROCEDURE obj_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set13
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!
row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE
        EXIT
      END IF
    END DO
  END DO
END DO
!
DEALLOCATE (row, col)
!
END PROCEDURE obj_set13

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set14
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk

row = GetNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
col = GetNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = VALUE
        EXIT
      END IF
    END DO
  END DO
END DO
DEALLOCATE (row, col)
END PROCEDURE obj_set14

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set15
CALL Copy(Y=obj%A, X=VALUE%A)
IF (PRESENT(scale)) THEN
  CALL Scal(X=obj%A, A=scale)
END IF
END PROCEDURE obj_set15

!----------------------------------------------------------------------------
!                                                                     SetIA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIA
CALL CSR_SetIA(obj=obj%csr, irow=irow, VALUE=VALUE)
END PROCEDURE obj_SetIA

!----------------------------------------------------------------------------
!                                                                     SetJA
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetJA
CALL CSR_SetJA(obj=obj%csr, indx=indx, VALUE=VALUE)
END PROCEDURE obj_SetJA

!----------------------------------------------------------------------------
!                                                               SetToSTMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetToSTMatrix1
REAL(DFP) :: scale0
INTEGER(I4B) :: icol
INTEGER(I4B) :: irow_rhs, trow_rhs, icol_rhs, colIndex_rhs(2), &
                tcol_rhs
INTEGER(I4B) :: irow_lhs, icol_lhs, colIndex_lhs(2), &
                offset_row_lhs, offset_col_lhs

scale0 = Input(default=1.0_DFP, option=scale)

trow_rhs = CSRMatrix_Size(obj=VALUE, dims=1)
offset_row_lhs = (itimecompo - 1) * trow_rhs

! start row loop
DO irow_rhs = 1, trow_rhs
  ! Get the starting and ending data index for irow in value
  colIndex_rhs = CSRMatrix_GetColIndex(obj=VALUE, irow=irow_rhs)
  tcol_rhs = colIndex_rhs(2) - colIndex_rhs(1) + 1

  ! Calculate the column offset for lhs
  offset_col_lhs = (jtimecompo - 1) * tcol_rhs

  irow_lhs = offset_row_lhs + irow_rhs
  colIndex_lhs = CSRMatrix_GetColIndex(obj=obj, irow=irow_lhs)

  DO icol = 1, tcol_rhs
    icol_rhs = colIndex_rhs(1) + icol - 1
    icol_lhs = colIndex_lhs(1) + offset_col_lhs + icol - 1

    obj%A(icol_lhs) = scale0 * VALUE%A(icol_rhs)
  END DO
END DO

END PROCEDURE obj_SetToSTMatrix1

END SUBMODULE Methods
