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

SUBMODULE(CSRMatrix_SetBlockColMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn1
INTEGER(I4B) :: ii, jj, c(3), row_start, row_end
CLASS(DOF_), POINTER :: idofobj
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (icolumn .GT. SIZE(obj, 2)) THEN
  CALL ErrorMSG(  &
    & Msg="icolumn is out of Bound", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
idofobj => GetDOFPointer(obj, 1)
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (ivar .GT. (.tNames.dofobj)) THEN
  CALL ErrorMSG(  &
    & Msg="ivar is out of Bound", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF ((obj.StorageFMT.1) .NE. FMT_DOF) THEN
  CALL ErrorMSG(  &
    & Msg="For this rotuine storage format should FMT_DOF", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
  !! start, end, stride of idof
  !!
c = getNodeLoc(obj=idofobj, idof=(idofobj.DOFStartIndex.ivar))
row_start = c(1) ! start
c = getNodeLoc(idofobj, (idofobj.DOFEndIndex.ivar))
row_end = c(2) ! end
  !!
DO ii = row_start, row_end
  DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
    IF (obj%csr%JA(jj) .EQ. icolumn) &
      & obj%A(jj) = value
  END DO
END DO
  !!
idofobj => NULL()
  !!
END PROCEDURE csrMat_setBlockColumn1

!----------------------------------------------------------------------------
!                                                              setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn2
INTEGER(I4B) :: ii, c(3), row_start, row_end, jj
CLASS(DOF_), POINTER :: idofobj
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (SIZE(value) .LT. obj%csr%ncol) THEN
  CALL ErrorMSG(  &
    & Msg="SIZE of row vector should be less than the number of col &
    & in sparse matrix", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn2",  &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF (icolumn .GT. SIZE(obj, 2)) THEN
  CALL ErrorMSG(  &
    & Msg="icolumn is out of Bound", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
idofobj => GetDOFPointer(obj, 1)
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (ivar .GT. (.tNames.idofobj)) THEN
  CALL ErrorMSG(  &
    & Msg="jVar is out of Bound", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF ((obj.StorageFMT.1) .NE. FMT_DOF) THEN
  CALL ErrorMSG(  &
    & Msg="For this rotuine storage format should FMT_DOF", &
    & File="CSRMatrix_Method@SetBlockColMethods.F90", &
    & Routine="csrMat_setBlockColumn2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
  !! start, end, stride
  !!
c = getNodeLoc(obj=idofobj, idof=(idofobj.DOFStartIndex.ivar))
row_start = c(1) ! start
c = getNodeLoc(obj=idofobj, idof=(idofobj.DOFEndIndex.ivar))
row_end = c(2) ! end
  !!
DO ii = row_start, row_end
  DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
    IF (obj%csr%JA(jj) .EQ. icolumn) &
      & obj%A(jj) = value(ii)
  END DO
END DO
  !!
idofobj => NULL()
  !!
END PROCEDURE csrMat_setBlockColumn2

!----------------------------------------------------------------------------
!                                                               setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn3
CALL SetBlockColumn( &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(obj=obj%csr%jdof, nodeNum=nodenum, &
  & ivar=jvar, idof=idof), &
  & value=value)
END PROCEDURE csrMat_setBlockColumn3

!----------------------------------------------------------------------------
!                                                               setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn4
CALL SetBlockColumn( &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(obj=obj%csr%jdof, nodeNum=nodenum, &
  & ivar=jvar, idof=idof), &
  & value=value)
END PROCEDURE csrMat_setBlockColumn4

!----------------------------------------------------------------------------
!                                                               setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn5
CALL SetBlockColumn( &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getIndex( &
    & obj=obj%csr%jdof, &
    & nodeNum=nodenum, &
    & ivar=jvar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
  & value=value)
END PROCEDURE csrMat_setBlockColumn5

!----------------------------------------------------------------------------
!                                                               setBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockColumn6
CALL SetBlockColumn( &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getIndex( &
    & obj=obj%csr%jdof, &
    & nodeNum=nodenum, &
    & ivar=jvar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
  & value=value)
END PROCEDURE csrMat_setBlockColumn6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
