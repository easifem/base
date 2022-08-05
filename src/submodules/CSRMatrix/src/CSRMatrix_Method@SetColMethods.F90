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
! date: 	22 March 2021
! summary: It contains method for setting values in [[CSRMatrix_]]

SUBMODULE(CSRMatrix_Method) SetColMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn1
  !!
  INTEGER( I4B ) :: i, j
  !!
#ifdef DEBUG_VER
  !!
  IF( SIZE( value ) .LT. obj%csr%nrow .OR. icolumn .GT. SIZE(obj, 2) ) THEN
    CALL ErrorMSG( Msg="SIZE of column vector should be same as number of &
      & rows in sparse matrix", &
      & File = "CSRMatrix_Method@setMethod.F90", &
      & Routine = "csrMat_setColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !!
#endif
  !!
  DO i = 1, obj%csr%nrow
    DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
      IF( obj%csr%JA(j) .EQ. icolumn ) THEN
        obj%A( j ) = value( i )
        EXIT
      END IF
    END DO
  END DO
  !!
END PROCEDURE csrMat_setColumn1

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn1b
  !!
  INTEGER( I4B ) :: i, j, k
  !!
  DO i = 1, obj%csr%nrow
    DO k = 1, SIZE(icolumn)
      DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
        IF( obj%csr%JA(j) .EQ. icolumn(k) ) THEN
          obj%A( j ) = value( i )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
END PROCEDURE csrMat_setColumn1b

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn2
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & idof=idof ), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn2

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn3
  !!
  INTEGER( I4B ) :: i, j
  !!
  DO i = 1, obj%csr%nrow
    DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
      IF( obj%csr%JA(j) .EQ. icolumn ) obj%A( j ) = value
    END DO
  END DO
  !!
END PROCEDURE csrMat_setColumn3

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn3b
  !!
  INTEGER( I4B ) :: i, j, k
  !!
  DO i = 1, obj%csr%nrow
    DO k = 1, SIZE(icolumn)
      DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
        IF( obj%csr%JA(j) .EQ. icolumn(k) ) THEN
          obj%A( j ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
END PROCEDURE csrMat_setColumn3b

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn4
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & idof=idof ),&
    & value=value )
  !!
END PROCEDURE csrMat_setColumn4

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn5
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, idof=idof), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn5

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn6
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & idof=idof), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn6

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn7
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo ), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn7

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn8
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn8

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn9
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn9

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn10
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn10

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn11
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn11

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn12
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn12

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn13
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn13

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn14
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn14

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn15
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn15

!----------------------------------------------------------------------------
!                                                                    setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn16
  !!
  CALL SetColumn( obj=obj, &
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=nodenum, &
    & ivar=ivar, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo), &
    & value=value )
  !!
END PROCEDURE csrMat_setColumn16

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetColMethods
