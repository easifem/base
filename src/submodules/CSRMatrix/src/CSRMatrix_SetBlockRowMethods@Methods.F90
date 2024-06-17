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

SUBMODULE(CSRMatrix_SetBlockRowMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow1
INTEGER(I4B) :: jj, c(3), col_start, col_end
CLASS(DOF_), POINTER :: jdofobj
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (irow .GT. SIZE(obj, 1)) THEN
  CALL ErrorMSG(  &
    & Msg="irow is out of Bound", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
jdofobj => GetDOFPointer(obj, 2)
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (jvar .GT. (.tNames.jdofobj)) THEN
  CALL ErrorMSG(  &
    & Msg="jVar is out of Bound", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF ((obj.StorageFMT.2) .NE. FMT_DOF) THEN
  CALL ErrorMSG(  &
    & Msg="For this rotuine storage format should FMT_DOF", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
!!
!! start, end, stride of idof
!!
c = getNodeLoc(obj=jdofobj, idof=(jdofobj.DOFStartIndex.jvar))
col_start = c(1) ! start
c = getNodeLoc(obj=jdofobj, idof=(jdofobj.DOFEndIndex.jvar))
col_end = c(2) ! end
!!
DO jj = obj%csr%IA(irow), obj%csr%IA(irow + 1) - 1
  IF ((jj .GE. col_start) .AND. jj .LE. col_end) &
    & obj%A(jj) = value
END DO
  !!
jdofobj => NULL()
  !!
END PROCEDURE csrMat_setBlockRow1

!----------------------------------------------------------------------------
!                                                              setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow2
INTEGER(I4B) :: jj, c(3), col_start, col_end
CLASS(DOF_), POINTER :: jdofobj
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (SIZE(value) .LT. obj%csr%ncol) THEN
  CALL ErrorMSG(  &
    & Msg="SIZE of row vector should be less than the number of col &
    & in sparse matrix", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow2",  &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF (irow .GT. SIZE(obj, 1)) THEN
  CALL ErrorMSG(  &
    & Msg="irow is out of Bound", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
jdofobj => GetDOFPointer(obj, 2)
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
IF (jvar .GT. (.tNames.jdofobj)) THEN
  CALL ErrorMSG(  &
    & Msg="jVar is out of Bound", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF ((obj.StorageFMT.2) .NE. FMT_DOF) THEN
  CALL ErrorMSG(  &
    & Msg="For this rotuine storage format should FMT_DOF", &
    & File="CSRMatrix_Method@SetBlockRowMethods.F90", &
    & Routine="csrMat_setBlockRow2", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
  !! start, end, stride of idof
  !!
c = getNodeLoc(obj=jdofobj, idof=(jdofobj.DOFStartIndex.jvar))
col_start = c(1) ! start
c = getNodeLoc(obj=jdofobj, idof=(jdofobj.DOFEndIndex.jvar))
col_end = c(2) ! end
  !!
DO jj = obj%csr%IA(irow), obj%csr%IA(irow + 1) - 1
  IF ((jj .GE. col_start) .AND. jj .LE. col_end) &
    & obj%A(jj) = value(obj%csr%JA(jj))
END DO
!!
jdofobj => NULL()
!!
END PROCEDURE csrMat_setBlockRow2

!----------------------------------------------------------------------------
!                                                               setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow3
CALL SetBlockRow( &
  & obj=obj, &
  & jvar=jvar, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodeNum=nodenum, &
  & ivar=ivar, idof=idof), &
  & value=value)
END PROCEDURE csrMat_setBlockRow3

!----------------------------------------------------------------------------
!                                                               setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow4
CALL SetBlockRow( &
  & obj=obj, &
  & jvar=jvar, &
  & irow=getNodeLoc(obj=obj%csr%idof, nodeNum=nodenum, &
  & ivar=ivar, idof=idof), &
  & value=value)
END PROCEDURE csrMat_setBlockRow4

!----------------------------------------------------------------------------
!                                                               setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow5
CALL SetBlockRow( &
  & obj=obj, &
  & jvar=jvar, &
  & irow=getIndex( &
    & obj=obj%csr%idof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
  & value=value)
END PROCEDURE csrMat_setBlockRow5

!----------------------------------------------------------------------------
!                                                               setBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setBlockRow6
CALL SetBlockRow( &
  & obj=obj, &
  & jvar=jvar, &
  & irow=getIndex( &
    & obj=obj%csr%idof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
  & value=value)
END PROCEDURE csrMat_setBlockRow6

END SUBMODULE Methods
