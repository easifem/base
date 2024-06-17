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

SUBMODULE(CSRMatrix_GetColMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn1
INTEGER(I4B) :: i, j
REAL(DFP) :: alpha
  !!
#ifdef DEBUG_VER
  !!
IF (SIZE(value) .LT. obj%csr%nrow .OR. iColumn .GT. SIZE(obj, 2)) THEN
  CALL ErrorMSG( &
  & Msg="SIZE of column vector should be same as number of &
  & rows in sparse matrix", &
  & File="CSRMatrix_Method@getMethod.F90", &
  & Routine="csrMat_getColumn1", Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
#endif
  !!
IF (PRESENT(addContribution)) THEN
    !!
  alpha = INPUT(default=1.0_DFP, option=scale)
    !!
  DO i = 1, obj%csr%nrow
      !!
    DO j = obj%csr%IA(i), obj%csr%IA(i + 1) - 1
        !!
      IF (obj%csr%JA(j) .EQ. iColumn) THEN
        value(i) = value(i) + alpha * obj%A(j)
        EXIT
      END IF
        !!
    END DO
      !!
  END DO
    !!
ELSE
    !!
  DO i = 1, obj%csr%nrow
      !!
    value(i) = 0.0_DFP
      !!
    DO j = obj%csr%IA(i), obj%csr%IA(i + 1) - 1
      IF (obj%csr%JA(j) .EQ. iColumn) THEN
        value(i) = obj%A(j)
        EXIT
      END IF
    END DO
      !!
  END DO
    !!
END IF
  !!
END PROCEDURE csrMat_getColumn1

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn1b
INTEGER(I4B) :: i, j, k
REAL(DFP) :: alpha
  !!
  !!
  !!
IF (PRESENT(addContribution)) THEN
    !!
  alpha = INPUT(default=1.0_DFP, option=scale)
    !!
  DO i = 1, obj%csr%nrow
    DO k = 1, size(iColumn)
      DO j = obj%csr%IA(i), obj%csr%IA(i + 1) - 1
        IF (obj%csr%JA(j) .EQ. iColumn(k)) THEN
          value(i) = value(i) + alpha * obj%A(j)
          EXIT
        END IF
      END DO
    END DO
  END DO
    !!
ELSE
    !!
  DO i = 1, obj%csr%nrow
    value(i) = 0.0_DFP
    DO k = 1, size(iColumn)
      DO j = obj%csr%IA(i), obj%csr%IA(i + 1) - 1
        IF (obj%csr%JA(j) .EQ. iColumn(k)) THEN
          value(i) = obj%A(j)
          EXIT
        END IF
      END DO
    END DO
  END DO
    !!
END IF
  !!
END PROCEDURE csrMat_getColumn1b

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn2
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc(obj=obj%csr%jdof, idof=idof, nodenum=nodenum), &
  & value=value, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_getColumn2

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn3
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc(obj=obj%csr%jdof,  &
    & ivar=ivar, idof=idof, nodenum=nodenum), &
  & value=value, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_getColumn3

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn4
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc(obj=obj%csr%jdof,  &
  & ivar=ivar, spacecompo=spacecompo, &
  & timecompo=timecompo, nodenum=nodenum), &
  & value=value, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_getColumn4

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn5
  !!
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc( &
  & obj=obj%csr%jdof,  &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getColumn5

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn6
  !!
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc( &
  & obj=obj%csr%jdof,  &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getColumn6

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn7
  !!
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc( &
  & obj=obj%csr%jdof,  &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getColumn7

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn8
  !!
CALL GetColumn(obj=obj, &
  & iColumn=getNodeLoc( &
  & obj=obj%csr%jdof,  &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getColumn8

END SUBMODULE Methods
