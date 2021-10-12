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
    CALL ErrorMSG( Msg="SIZE of row vector should be same as number of col &
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
  INTEGER( I4B ) :: irow, tdof, tnodes
  tdof = .tdof. obj%csr%dof
  IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
    irow = (inode-1)*tdof + idof
  ELSE
    tnodes = obj%csr%dof .tNodes. idof
    irow = (idof-1)*tnodes + inode
  END IF
  CALL csrMat_getRow1( obj=obj, irow=irow, val=val, scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getRow2

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getColumn1
  INTEGER( I4B ) :: i, j
  REAL( DFP ) :: alpha
  IF( SIZE( val ) .LT. obj%csr%nrow .OR. iColumn .GT. SIZE(obj, 2) ) THEN
    CALL ErrorMSG( Msg="SIZE of column vector should be same as number of rows in sparse matrix", &
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
  INTEGER( I4B ) :: iColumn, tdof, tnodes
  tdof = .tdof. obj%csr%dof
  IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
    iColumn = (inode-1)*tdof + idof
  ELSE
    tnodes = obj%csr%dof .tNodes. idof
    iColumn = (idof-1)*tnodes + inode
  END IF
  CALL csrMat_getColumn1( obj=obj, iColumn=iColumn, val=val, scale=scale, &
    & addContribution=addContribution )
END PROCEDURE csrMat_getColumn2

END SUBMODULE getMethods