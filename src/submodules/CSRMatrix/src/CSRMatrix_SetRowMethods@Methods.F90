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

SUBMODULE(CSRMatrix_SetRowMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow1
  !!
#ifdef DEBUG_VER
  !!
IF (SIZE(value) .LT. obj%csr%ncol .OR. irow .GT. SIZE(obj, 1)) THEN
  CALL ErrorMSG( &
    & Msg="SIZE of value vector should be same as number of col &
    & in sparse matrix or irow is out of bound", &
    & File="CSRMatrix_Method@setMethod.F90", &
    & Routine="csrMat_setRow1", Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
#endif
  !!
obj%A(obj%csr%IA(irow):obj%csr%IA(irow + 1) - 1) = value( &
  & obj%csr%JA(obj%csr%IA(irow):obj%csr%IA(irow + 1) - 1))
  !!
END PROCEDURE csrMat_setRow1

!----------------------------------------------------------------------------
!                                                                     setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow1b
INTEGER(I4B) :: ii
  !!
DO ii = 1, size(irow)
    !!
  obj%A(obj%csr%IA(irow(ii)):obj%csr%IA(irow(ii) + 1) - 1)  &
    & = value(obj%csr%JA(obj%csr%IA(irow(ii)) &
    & :obj%csr%IA(irow(ii) + 1) - 1))
    !!
END DO
  !!
END PROCEDURE csrMat_setRow1b

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow2
CALL csrMat_setRow1( &
  & obj=obj, &
  & irow=getNodeLoc( &
          & obj=obj%csr%idof, &
          & nodenum=nodenum, &
          & idof=idof), &
  & value=value)
END PROCEDURE csrMat_setRow2

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow3
obj%A(obj%csr%IA(irow):obj%csr%IA(irow + 1) - 1) = value
END PROCEDURE csrMat_setRow3

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow3b
INTEGER(I4B) :: ii
  !!
DO ii = 1, size(irow)
  obj%A(obj%csr%IA(irow(ii)):obj%csr%IA(irow(ii) + 1) - 1) = value
END DO
  !!
END PROCEDURE csrMat_setRow3b

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow4
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodenum=nodenum, idof=idof),&
  & value=value)
  !!
END PROCEDURE csrMat_setRow4

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow5
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodenum=nodenum, ivar=ivar, &
    & idof=idof), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow5

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow6
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodenum=nodenum, ivar=ivar, &
    & idof=idof), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow6

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow7
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow7

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow8
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow8

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow9
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow9

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow10
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow10

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow11
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow11

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow12
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow12

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow13
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow13

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow14
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow14

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow15
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow15

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow16
  !!
CALL SetRow(obj=obj, &
  & irow=getNodeLoc( &
  & obj=obj%csr%idof, &
  & nodenum=nodenum, &
  & ivar=ivar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value)
  !!
END PROCEDURE csrMat_setRow16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
