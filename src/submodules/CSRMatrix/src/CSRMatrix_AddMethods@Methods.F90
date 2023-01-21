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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add0
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
!! main
!!
row = getIndex(obj=obj%csr%idof, nodeNum=nodenum)
col = getIndex(obj=obj%csr%jdof, nodeNum=nodenum)
  !!
DO ii = 1, SIZE(row)
  !!
  DO kk = 1, SIZE(col)
    !!
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      !!
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value(ii, kk)
        EXIT
      END IF
      !!
    END DO
    !!
  END DO
  !!
END DO
IF (ALLOCATED(row)) DEALLOCATE (row)
IF (ALLOCATED(col)) DEALLOCATE (col)
END PROCEDURE csrMat_add0

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add1
REAL(DFP), ALLOCATABLE :: m2(:, :)
INTEGER(I4B) :: tdof
!!
!! main
!!
tdof = .tdof.obj%csr%idof
!!
SELECT CASE (storageFMT)
!!
!!
!!
CASE (FMT_NODES)
  !!
  IF ((obj.StorageFMT.1) .EQ. FMT_NODES) THEN
    m2 = value
  ELSE
    CALL Convert(From=value, To=m2, Conversion=NodesToDOF, &
      & nns=SIZE(nodenum), tDOF=tdof)
  END IF
!!
!!
!!
!!
CASE (FMT_DOF)
  !!
  IF ((obj.StorageFMT.1) .EQ. FMT_DOF) THEN
    m2 = value
  ELSE
    CALL Convert(From=value, To=m2, Conversion=DofToNodes, &
      & nns=SIZE(nodenum), tDOF=tdof)
  END IF
  !!
END SELECT
!!
CALL Add(obj=obj, nodenum=nodenum, value=m2, scale=scale)
!!
IF (ALLOCATED(m2)) DEALLOCATE (m2)
!!
END PROCEDURE csrMat_add1

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add2
obj%A = obj%A + scale * value
END PROCEDURE csrMat_add2

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add3
INTEGER(I4B) :: i, j
DO j = obj%csr%IA(iRow), obj%csr%IA(iRow + 1) - 1
  IF (obj%csr%JA(j) .EQ. iColumn) &
    & obj%A(j) = obj%A(j) + scale * value
END DO
END PROCEDURE csrMat_add3

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add4
  !!
CALL add(obj=obj, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodenum=inodenum, idof=idof), &
  & icolumn=getNodeLoc(obj=obj%csr%jdof, nodenum=jnodenum, idof=jdof), &
  & value=value, scale=scale)
  !!
END PROCEDURE csrMat_add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add5
REAL(DFP), ALLOCATABLE :: m2(:, :)
INTEGER(I4B) :: tdof1, tdof2
!!
tdof1 = .tdof.obj%csr%idof
tdof2 = .tdof.obj%csr%jdof
!!
ALLOCATE (m2(tdof1 * SIZE(nodenum), tdof2 * SIZE(nodenum)))
!!
m2 = value
!!
CALL Add(obj=obj, nodenum=nodenum, value=m2, scale=scale)
!!
DEALLOCATE (m2)
END PROCEDURE csrMat_add5

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add6
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
!!
END PROCEDURE csrMat_add6

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add7
CALL Add(obj=obj, &
  & irow=getNodeLoc( &
          & obj=obj%csr%idof, &
          & nodenum=iNodeNum, &
          & ivar=ivar, &
          & idof=iDOF),&
  & icolumn=getNodeLoc( &
              & obj=obj%csr%jdof, &
              & nodenum=jNodeNum, &
              & ivar=jvar, &
              & idof=jDOF), &
  & value=value, &
  & scale=scale)
  !!
END PROCEDURE csrMat_add7

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add8
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
  !!
  !! main
  !!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)
  !!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value(ii, kk)
        EXIT
      END IF
    END DO
  END DO
END DO
  !!
DEALLOCATE (row, col)
  !!
END PROCEDURE csrMat_add8

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add9
CALL Add( &
  & obj=obj, &
  & irow=getNodeLoc( &
          & obj=obj%csr%idof, &
          & nodenum=iNodeNum, &
          & ivar=ivar, &
          & spacecompo=ispacecompo, &
          & timecompo=itimecompo),&
  & icolumn=getNodeLoc( &
              & obj=obj%csr%jdof, &
              & nodenum=jNodeNum, &
              & ivar=jvar, &
              & spacecompo=jspacecompo, &
              & timecompo=jtimecompo), &
  & value=value, &
  & scale=scale)
END PROCEDURE csrMat_add9

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add10
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getIndex(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar)
col = getIndex(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
END PROCEDURE csrMat_add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add11
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, idof=idof)
col = getNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, idof=jdof)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
!!
END PROCEDURE csrMat_add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add12
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
!!
col = getNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
END PROCEDURE csrMat_add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add13
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
!!
col = getNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
!!
END PROCEDURE csrMat_add13

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add14
! Internal variables
INTEGER(I4B), ALLOCATABLE :: row(:), col(:)
INTEGER(I4B) :: ii, jj, kk
!!
row = getNodeLoc(obj=obj%csr%idof, nodeNum=iNodeNum, ivar=ivar, &
  & spacecompo=ispacecompo, timecompo=itimecompo)
!!
col = getNodeLoc(obj=obj%csr%jdof, nodeNum=jNodeNum, ivar=jvar, &
  & spacecompo=jspacecompo, timecompo=jtimecompo)
!!
DO ii = 1, SIZE(row)
  DO kk = 1, SIZE(col)
    DO jj = obj%csr%IA(row(ii)), obj%csr%IA(row(ii) + 1) - 1
      IF (obj%csr%JA(jj) .EQ. col(kk)) THEN
        obj%A(jj) = obj%A(jj) + scale * value
        EXIT
      END IF
    END DO
  END DO
END DO
!!
DEALLOCATE (row, col)
!!
END PROCEDURE csrMat_add14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
