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

SUBMODULE(CSRMatrix_Method) GetBlockRowMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow1
  INTEGER( I4B ) :: jj, c(3), col_start, col_end
  REAL( DFP ) :: alpha
  !!
#ifdef DEBUG_VER
  !!
  !! check
  !!
  IF( SIZE( value ) .LT. obj%csr%ncol ) THEN
    CALL ErrorMSG(  &
      & Msg="SIZE of row vector should be less &
      & than the number of col &
      & in sparse matrix", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !!
  !! check
  !!
  IF( irow .GT. SIZE(obj, 1) ) THEN
    CALL ErrorMSG(  &
      & Msg="irow is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !!
  !! check
  !!
  IF( jvar .GT. (.tNames. obj%csr%dof) ) THEN
    CALL ErrorMSG(  &
      & Msg="jVar is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", &
      & Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !!
  !! check
  !!
  IF( (.StorageFMT. obj ) .NE. FMT_DOF ) THEN
    CALL ErrorMSG(  &
      & Msg="For this rotuine storage format should FMT_DOF", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", &
      & Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !!
#endif
  !!
  !! start, end, stride of idof
  !!
  c = getNodeLoc( obj=obj%csr%dof, idof=(obj%csr%dof .DOFStartIndex. jvar) )
  col_start = c( 1 ) ! start
  c = getNodeLoc( obj%csr%dof, (obj%csr%dof .DOFEndIndex. jvar) )
  col_end = c( 2 ) ! end
  !!
  !!
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    alpha = INPUT( Default=1.0_DFP, Option=scale )
    !!
    DO jj = obj%csr%IA( irow ), obj%csr%IA( irow+1 ) - 1
      IF( (jj .GE. col_start) .AND. (jj .LE. col_end) ) &
        & value(obj%csr%JA(jj)) = value(obj%csr%JA(jj)) + alpha * obj%A(jj)
    END DO
  !!
  !!
  !!
  ELSE
    !!
    value = 0.0_DFP
    !!
    DO jj = obj%csr%IA( irow ), obj%csr%IA( irow+1 ) - 1
      IF( (jj .GE. col_start) .AND. jj .LE. col_end ) &
        & value(obj%csr%JA(jj)) = obj%A(jj)
    END DO
    !!
  END IF
  !!
END PROCEDURE csrMat_getBlockRow1

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow1b
  INTEGER( I4B ) :: ii, jj, c(3), col_start, col_end
  REAL( DFP ) :: alpha
  !!
  !! start, end, stride of idof
  !!
  c = getNodeLoc( obj=obj%csr%dof, idof=(obj%csr%dof .DOFStartIndex. jvar) )
  col_start = c( 1 ) ! start
  c = getNodeLoc( obj%csr%dof, (obj%csr%dof .DOFEndIndex. jvar) )
  col_end = c( 2 ) ! end
  !!
  !!
  !!
  IF( PRESENT( addContribution ) ) THEN
    !!
    alpha = INPUT( Default=1.0_DFP, Option=scale )
    !!
    DO ii = 1, size(irow)
      DO jj = obj%csr%IA( irow(ii) ), obj%csr%IA( irow(ii)+1 ) - 1
        IF( (jj .GE. col_start) .AND. (jj .LE. col_end) ) &
          & value(obj%csr%JA(jj)) = value(obj%csr%JA(jj)) + alpha * obj%A(jj)
      END DO
    END DO
  !!
  !!
  !!
  ELSE
    !!
    value = 0.0_DFP
    !!
    DO ii = 1, size(irow)
      DO jj = obj%csr%IA( irow(ii) ), obj%csr%IA( irow(ii)+1 ) - 1
        IF( (jj .GE. col_start) .AND. jj .LE. col_end ) &
          & value(obj%csr%JA(jj)) = obj%A(jj)
      END DO
    END DO
    !!
  END IF
  !!
END PROCEDURE csrMat_getBlockRow1b

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow2
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & idof=idof ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow2

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow3
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc(&
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & idof=idof ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow3

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow4
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow4

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow5
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow5

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow6
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow6

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow7
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow7

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow8
  CALL GetBlockRow( &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodeNum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value, &
    & scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow8

END SUBMODULE GetBlockRowMethods