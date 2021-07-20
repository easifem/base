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
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE( CSRSparsity_Method ) setMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_setSparsity1
  INTEGER( I4B ) :: n, a, b, m, tdof
  INTEGER( I4B ), ALLOCATABLE :: n2ntemp( : )

  IF( .NOT. obj%isInitiated ) THEN
    CALL ErrorMSG( &
      & "Instance of CSRSparsity is not initiated!", &
      & "CSRSparsity_Method@setMethods.f90", &
      & "csr_setSparsity1()", &
      & __LINE__, stderr )
    STOP
  END IF

  IF( obj%isSparsityLock ) THEN
    CALL ErrorMSG( &
      & "Instance of CSRSparsity is locked for setting sparsity pattern!", &
      & "CSRSparsity_Method@setMethods.f90", &
      & "csr_setSparsity1()", &
      & __LINE__, stderr )
    STOP
  END IF

  IF( .NOT. ALLOCATED( obj%row ) ) &
    & ALLOCATE( obj%row( obj%nrow ) )

  tdof = .tdof. obj%dof
  n = SIZE( Col )

  IF( tdof .EQ. 1 ) THEN
    obj%nnz = obj%nnz + n
    n2ntemp = SORT( Col )
    CALL APPEND( obj%Row( Row ), n2ntemp )
  ELSE
    ALLOCATE( n2ntemp( n * tdof ) )
    IF( obj%dof%StorageFMT .EQ. NODES_FMT ) THEN
      DO a = 1, n
        DO b = 1, tdof
          n2ntemp( ( a - 1 ) * tdof + b ) = ( Col( a ) - 1 ) * tdof + b
        END DO
      END DO
      n = n * tdof
      n2ntemp = SORT( n2ntemp )
      DO b = 1, tdof
        obj%nnz = obj%nnz + n
        CALL APPEND( obj%Row( ( Row - 1 ) * tdof + b ), n2ntemp )
      END DO
      DEALLOCATE( n2ntemp )
    ELSE
      DO b = 1, tdof
        m = obj%dof%valmap( b+1 ) - obj%dof%valmap( b )
        DO a = 1, n
          n2ntemp( ( b - 1 ) * n + a ) = ( b - 1 ) * m + Col( a )
        END DO
      END DO
      n = n * tdof
      n2ntemp = SORT( n2ntemp )
      DO b = 1, tdof
        m = obj%dof%valmap( b+1 ) - obj%dof%valmap( b )
        obj%nnz = obj%nnz + n
        CALL APPEND( obj%Row( ( b - 1 )*m + Row ), n2ntemp )
      END DO
      DEALLOCATE( n2ntemp )
    END IF
  END IF
END PROCEDURE csr_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_setSparsity2
  INTEGER( I4B ) :: i
  DO i = 1, SIZE( Row )
    CALL setSparsity( obj, Row( i ), Col( i )%Val )
  END DO
END PROCEDURE csr_setSparsity2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_setSparsity3
  INTEGER( I4B ) :: i, j, k

  IF( .NOT. obj%isInitiated ) THEN
    CALL ErrorMSG( &
      & "Instance of CSRSparsity is not initiated!", &
      & "CSRSparsity_Method@setMethods.f90", &
      & "csr_setSparsity3()", &
      & __LINE__, stderr )
    STOP
  END IF

  IF( obj%isSparsityLock ) THEN
    CALL WarningMSG( &
      & "Instance of CSRSparsity is locked for setting sparsity pattern!", &
      & "CSRSparsity_Method@setMethods.f90", &
      & "csr_setSparsity1()", &
      & __LINE__, stderr )
    RETURN
  ELSE
    obj%isSparsityLock = .TRUE.
  END IF

  !<--- Remove duplicate entries in obj%Row( irow )%Col
  IF( ALLOCATED( obj%Row ) ) THEN
    k = 0
    DO i = 1, obj%nrow
      CALL RemoveDuplicates( obj%Row( i ) )
      k = k + SIZE( obj%Row( i ) )
    END DO
    !<--- update nnz: number of non zeros
    obj%nnz = k
    !<--- allocate obj%JA and obj%A
    CALL Reallocate( obj%JA, obj%nnz )
    !<--- convert data into IA, JA
    obj%IA( 1 ) = 1
    DO i = 1, obj%nrow
      ! obj%RowSize( i ) = SIZE( obj%Row( i ) )
      k = SIZE( obj%Row( i ) )
      obj%IA( i + 1 ) = obj%IA( i ) + k
      IF( k .NE. 0 ) &
        & obj%JA( obj%IA( i ) : obj%IA( i + 1 ) - 1 ) = obj%Row( i )%Val
    END DO
    DEALLOCATE( obj%Row )
  END IF

  j = SIZE( obj%JA )
  IF( j .GT. obj%nnz ) THEN
    obj%ColSize = obj%JA( 1 : obj%nnz )
    CALL Reallocate( obj%JA, obj%nnz )
    obj%JA = obj%ColSize
    CALL Reallocate( obj%ColSize, obj%ncol )
  END IF

  DO i = 1, obj%nrow
    obj%RowSize( i ) = obj%IA( i + 1 ) - obj%IA( i )
    IF( obj%RowSize( i ) .NE. 0 ) THEN
      DO j = obj%IA( i ), obj%IA( i + 1 ) - 1
        obj%ColSize( obj%JA( j ) ) = &
        & obj%ColSize( obj%JA( j ) ) + 1
        IF( obj%JA( j ) .EQ. i ) obj%DiagIndx( i ) =j
      END DO
    END IF
  END DO

END PROCEDURE csr_setSparsity3

END SUBMODULE setMethods