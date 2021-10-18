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

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE(CSRMatrix_Method) getMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getStorageFMT
  ans = obj%csr%dof%storageFMT
END PROCEDURE csrMat_getStorageFMT

!----------------------------------------------------------------------------
!                                                            getMatrixProp
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getMatrixProp
  ans = TRIM( obj%matrixProp )
END PROCEDURE csrMat_getMatrixProp

!----------------------------------------------------------------------------
!                                                              getDOFPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getDOFPointer
  ans => obj%csr%dof
END PROCEDURE csrMat_getDOFPointer

!----------------------------------------------------------------------------
!                                                                 isSquare
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_isSquare
  IF( obj%csr%nrow .EQ. obj%csr%ncol ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE csrMat_isSquare

!----------------------------------------------------------------------------
!                                                               isRectangle
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_isRectangle
  IF( obj%csr%nrow .EQ. obj%csr%ncol ) THEN
    ans = .FALSE.
  ELSE
    ans = .TRUE.
  END IF
END PROCEDURE csrMat_isRectangle

!----------------------------------------------------------------------------
!                                                                   getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getRow1
  INTEGER( I4B ) :: a, b
  REAL( DFP ) :: alpha
  IF( SIZE( val ) .LT. obj%csr%ncol .OR. iRow .GT. SIZE(obj, 1) ) THEN
    CALL ErrorMSG(  &
    & Msg="SIZE of row vector should be same as number of col &
    & in sparse matrix or iRow is out of bound", &
    & File = "CSRMatrix_Method@getMethod.F90", &
    & Routine = "csrMat_getRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  a = obj%csr%IA( iRow )
  b = obj%csr%IA( iRow+1 ) - 1
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( Default=1.0_DFP, Option=scale )
    val(obj%csr%JA(a:b)) = val(obj%csr%JA(a:b)) + alpha * obj%A( a:b )
  ELSE
    val = 0.0_DFP
    val(obj%csr%JA(a:b)) = obj%A( a:b )
  END IF
END PROCEDURE csrMat_getRow1

!----------------------------------------------------------------------------
!                                                                   getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getRow2
  CALL csrMat_getRow1( obj=obj, &
    & irow=getNodeLoc( obj=obj%csr%dof, idof=idof, inode=inode), &
    & val=val, scale=scale, addContribution=addContribution )
END PROCEDURE csrMat_getRow2

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow1
  INTEGER( I4B ) :: jj, c(3), col_start, col_end
  REAL( DFP ) :: alpha
  CLASS( DOF_ ), POINTER :: dofobj
  !>check
  IF( SIZE( val ) .LT. obj%csr%ncol ) THEN
    CALL ErrorMSG(  &
      & Msg="SIZE of row vector should be less than the number of col &
      & in sparse matrix", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  IF( iRow .GT. SIZE(obj, 1) ) THEN
    CALL ErrorMSG(  &
      & Msg="iRow is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  dofobj => getDOFPointer( obj )
  IF( jvar .GT. (.tNames. dofobj) ) THEN
    CALL ErrorMSG(  &
      & Msg="jVar is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  IF( (.StorageFMT. obj ) .NE. FMT_DOF ) THEN
    CALL ErrorMSG(  &
      & Msg="For this rotuine storage format should FMT_DOF", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  ! start, end, stride
  c = getNodeLoc( dofobj, (dofobj .DOFStartIndex. jvar) )
  col_start = c( 1 ) ! start
  c = getNodeLoc( dofobj, (dofobj .DOFEndIndex. jvar) )
  col_end = c( 2 ) ! end
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( Default=1.0_DFP, Option=scale )
    DO jj = obj%csr%IA( iRow ), obj%csr%IA( iRow+1 ) - 1
      IF( jj .GE. col_start .AND. jj .LE. col_end ) &
        & val(obj%csr%JA(jj)) = val(obj%csr%JA(jj)) + alpha * obj%A(jj)
    END DO
  ELSE
    val = 0.0_DFP
    DO jj = obj%csr%IA( iRow ), obj%csr%IA( iRow+1 ) - 1
      IF( jj .GE. col_start .AND. jj .LE. col_end ) &
        & val(obj%csr%JA(jj)) = obj%A(jj)
    END DO
  END IF
END PROCEDURE csrMat_getBlockRow1

!----------------------------------------------------------------------------
!                                                               getBlockRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockRow2
  CALL csrMat_getBlockRow1(  &
    & obj=obj, &
    & jvar=jvar, &
    & irow=getIndex(obj=obj%csr%dof, idof=idof, nodeNum=inode, ivar=ivar), &
    & val=val, scale=scale, addContribution=addContribution )
END PROCEDURE csrMat_getBlockRow2

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn1
  INTEGER( I4B ) :: i, j
  REAL( DFP ) :: alpha
  IF( SIZE( val ) .LT. obj%csr%nrow .OR. iColumn .GT. SIZE(obj, 2) ) THEN
    CALL ErrorMSG( &
    & Msg="SIZE of column vector should be same as number of &
    & rows in sparse matrix", &
    & File = "CSRMatrix_Method@getMethod.F90", &
    & Routine = "csrMat_getColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( default=1.0_DFP, option=scale )
    DO i = 1, obj%csr%nrow
      DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
        IF( obj%csr%JA(j) .EQ. iColumn ) val( i ) = val( i )+alpha*obj%A( j )
      END DO
    END DO
  ELSE
    DO i = 1, obj%csr%nrow
      val( i ) = 0.0_DFP
      DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
        IF( obj%csr%JA(j) .EQ. iColumn ) val( i ) = obj%A( j )
      END DO
    END DO
  END IF
END PROCEDURE csrMat_getColumn1

!----------------------------------------------------------------------------
!                                                                   getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn2
  CALL csrMat_getColumn1( obj=obj, &
    & iColumn=getNodeLoc( obj=obj%csr%dof, idof=idof, inode=inode), &
    & val=val, scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getColumn2

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn1
  INTEGER( I4B ) :: jj, ii, c(3), row_start, row_end
  REAL( DFP ) :: alpha
  CLASS( DOF_ ), POINTER :: dofobj
  !>check
  IF( SIZE( val ) .LT. obj%csr%nrow ) THEN
    CALL ErrorMSG(  &
      & Msg="SIZE of column vector is less than the number of row &
      & in sparse matrix", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  IF( iColumn .GT. SIZE(obj, 2) ) THEN
    CALL ErrorMSG(  &
      & Msg="iColumn is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  dofobj => getDOFPointer( obj )
  IF( ivar .GT. (.tNames. dofobj) ) THEN
    CALL ErrorMSG(  &
      & Msg="ivar is out of Bound", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  !>check
  IF( (.StorageFMT. obj ) .NE. FMT_DOF ) THEN
    CALL ErrorMSG(  &
      & Msg="For this rotuine storage format should FMT_DOF", &
      & File = "CSRMatrix_Method@getMethod.F90", &
      & Routine = "csrMat_getBlockColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  ! start, end, stride
  c = getNodeLoc( dofobj, (dofobj .DOFStartIndex. ivar) )
  row_start = c( 1 ) ! start
  c = getNodeLoc( dofobj, (dofobj .DOFEndIndex. ivar) )
  row_end = c( 2 ) ! end
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( Default=1.0_DFP, Option=scale )
    DO ii = row_start, row_end
      val( ii ) = 0.0_DFP
      DO jj = obj%csr%IA( ii ), obj%csr%IA( ii+1 ) - 1
        IF( obj%csr%JA(jj) .EQ. iColumn ) val(ii) = val(ii)+alpha*obj%A(jj)
      END DO
    END DO
  ELSE
    DO ii = row_start, row_end
      val( ii ) = 0.0_DFP
      DO jj = obj%csr%IA( ii ), obj%csr%IA( ii+1 ) - 1
        IF( obj%csr%JA(jj) .EQ. iColumn ) val( ii ) = obj%A( jj )
      END DO
    END DO
  END IF
END PROCEDURE csrMat_getBlockColumn1

!----------------------------------------------------------------------------
!                                                            getBlockColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getBlockColumn2
  CALL csrMat_getBlockColumn1(  &
    & obj=obj, &
    & ivar=ivar, &
    & iColumn=getIndex(obj=obj%csr%dof, idof=idof, &
    & nodeNum=inode, ivar=jvar), &
    & val=val, scale=scale, addContribution=addContribution )
END PROCEDURE csrMat_getBlockColumn2

END SUBMODULE getMethods