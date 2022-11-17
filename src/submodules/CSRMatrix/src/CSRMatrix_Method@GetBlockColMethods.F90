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

SUBMODULE(CSRMatrix_Method) GetBlockColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn1
INTEGER(I4B) :: jj, ii, c(3), row_start, row_end
REAL(DFP) :: alpha
  !!
#ifdef DEBUG_VER
  !!
  !!check
  !!
IF (SIZE(value) .LT. obj%csr%nrow) THEN
  CALL ErrorMSG(  &
    & Msg="SIZE of column vector is less than the number of row &
    & in sparse matrix", &
    & File="CSRMatrix_Method@getMethod.F90", &
    & Routine="csrMat_getBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF (icolumn .GT. SIZE(obj, 2)) THEN
  CALL ErrorMSG(  &
    & Msg="icolumn is out of Bound", &
    & File="CSRMatrix_Method@getMethod.F90", &
    & Routine="csrMat_getBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF (ivar .GT. (.tNames.obj%csr%idof)) THEN
  CALL ErrorMSG(  &
    & Msg="ivar is out of Bound", &
    & File="CSRMatrix_Method@getMethod.F90", &
    & Routine="csrMat_getBlockColumn1", &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
  !! check
  !!
IF ((.StorageFMT.obj) .NE. FMT_DOF) THEN
  CALL ErrorMSG(  &
    & Msg="For this rotuine storage format should FMT_DOF", &
    & File="CSRMatrix_Method@getMethod.F90", &
    & Routine="csrMat_getBlockColumn1",  &
    & Line=__LINE__, UnitNo=stdout)
  RETURN
END IF
  !!
#endif
  !!
  !! start, end, stride
  !!
c = getNodeLoc(obj=obj%csr%idof, idof=(obj%csr%idof.DOFStartIndex.ivar))
row_start = c(1) ! start
c = getNodeLoc(obj=obj%csr%idof, idof=(obj%csr%idof.DOFEndIndex.ivar))
row_end = c(2) ! end
  !!
IF (PRESENT(addContribution)) THEN
    !!
    !!
    !!
  alpha = INPUT(Default=1.0_DFP, Option=scale)
    !!
  DO ii = row_start, row_end
    value(ii) = 0.0_DFP
    DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
      IF (obj%csr%JA(jj) .EQ. icolumn) THEN
        value(ii) = value(ii) + alpha * obj%A(jj)
        EXIT
      END IF
    END DO
  END DO
    !!
    !!
    !!
ELSE
    !!
  DO ii = row_start, row_end
    value(ii) = 0.0_DFP
    DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
      IF (obj%csr%JA(jj) .EQ. icolumn) THEN
        value(ii) = obj%A(jj)
        EXIT
      END IF
    END DO
  END DO
    !!
END IF
  !!
END PROCEDURE csrMat_getBlockColumn1

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn1b
INTEGER(I4B) :: ii, jj, kk, c(3), row_start, row_end
REAL(DFP) :: alpha
  !!
  !! start, end, stride
  !!
c = getNodeLoc(obj=obj%csr%idof, idof=(obj%csr%idof.DOFStartIndex.ivar))
row_start = c(1) ! start
c = getNodeLoc(obj=obj%csr%idof, idof=(obj%csr%idof.DOFEndIndex.ivar))
row_end = c(2) ! end
  !!
IF (PRESENT(addContribution)) THEN
    !!
  alpha = INPUT(Default=1.0_DFP, Option=scale)
    !!
  DO ii = row_start, row_end
    value(ii) = 0.0_DFP
    DO kk = 1, size(icolumn)
      DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
        IF (obj%csr%JA(jj) .EQ. icolumn(kk)) THEN
          value(ii) = value(ii) + alpha * obj%A(jj)
          EXIT
        END IF
      END DO
    END DO
  END DO
    !!
ELSE
    !!
  DO ii = row_start, row_end
    value(ii) = 0.0_DFP
    DO kk = 1, size(icolumn)
      DO jj = obj%csr%IA(ii), obj%csr%IA(ii + 1) - 1
        IF (obj%csr%JA(jj) .EQ. icolumn(kk)) THEN
          value(ii) = obj%A(jj)
          EXIT
        END IF
      END DO
    END DO
  END DO
    !!
END IF
  !!
END PROCEDURE csrMat_getBlockColumn1b

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn2
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc( &
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & idof=idof), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn2

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn3
  !!
CALL getBlockColumn( &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & idof=idof), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn3

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn4
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn4

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn5
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn5

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn6
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn6

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn7
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn7

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn8
  !!
CALL getBlockColumn(  &
  & obj=obj, &
  & ivar=ivar, &
  & icolumn=getNodeLoc(&
  & obj=obj%csr%jdof, &
  & nodeNum=nodenum, &
  & ivar=jvar, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & value=value, &
  & scale=scale, &
  & addContribution=addContribution)
  !!
END PROCEDURE csrMat_getBlockColumn8

END SUBMODULE GetBlockColMethods
