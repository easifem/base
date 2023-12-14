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

! authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_GetRowMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow1
INTEGER(I4B) :: a, b
REAL(DFP) :: alpha

#ifdef DEBUG_VER
IF (SIZE(VALUE) .LT. obj%csr%ncol .OR. irow .GT. SIZE(obj, 1)) THEN
  CALL ErrorMSG(  &
  & Msg="SIZE of row vector should be same as number of col &
  & in sparse matrix or irow is out of bound", &
  & File="CSRMatrix_Method@GetMethod.F90", &
  & Routine="csrMat_GetRow1", Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
#endif

a = obj%csr%IA(irow)
b = obj%csr%IA(irow + 1) - 1

IF (PRESENT(addContribution)) THEN
  alpha = INPUT(Default=1.0_DFP, Option=scale)
  VALUE(obj%csr%JA(a:b)) = VALUE(obj%csr%JA(a:b)) + alpha * obj%A(a:b)
ELSE
  VALUE = 0.0_DFP
  VALUE(obj%csr%JA(a:b)) = obj%A(a:b)
END IF

END PROCEDURE csrMat_GetRow1

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow1b
INTEGER(I4B) :: a, b, ii
REAL(DFP) :: alpha

IF (PRESENT(addContribution)) THEN
  alpha = INPUT(Default=1.0_DFP, Option=scale)
  DO ii = 1, SIZE(irow)
    a = obj%csr%IA(irow(ii))
    b = obj%csr%IA(irow(ii) + 1) - 1
    VALUE(obj%csr%JA(a:b)) = VALUE(obj%csr%JA(a:b)) + alpha * obj%A(a:b)
  END DO
ELSE
  VALUE = 0.0_DFP
  DO ii = 1, SIZE(irow)
    a = obj%csr%IA(irow(ii))
    b = obj%csr%IA(irow(ii) + 1) - 1
    VALUE(obj%csr%JA(a:b)) = obj%A(a:b)
  END DO
END IF
END PROCEDURE csrMat_GetRow1b

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow2
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & idof=idof, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE csrMat_GetRow2

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow3
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & idof=idof, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE csrMat_GetRow3

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow4
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & spacecompo=spacecompo,&
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_GetRow4

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow5
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & spacecompo=spacecompo,&
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_GetRow5

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow6
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & spacecompo=spacecompo,&
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_GetRow6

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow7
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & spacecompo=spacecompo,&
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_GetRow7

!----------------------------------------------------------------------------
!                                                                   GetRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_GetRow8
CALL GetRow(obj=obj, &
  & irow=GetNodeLoc( &
  & obj=obj%csr%idof, &
  & ivar=ivar, &
  & spacecompo=spacecompo,&
  & timecompo=timecompo, &
  & nodenum=nodenum), &
  & VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE csrMat_GetRow8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
