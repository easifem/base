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
! date: 	22 March 2021
! summary: 	This submodule contains method for setting values in [[CSRMatrix_]]

SUBMODULE( CSRMatrix_Method ) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity1
  CALL setSparsity( obj=obj%csr, row=row, col=col )
END PROCEDURE csrMat_setSparsity1

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity2
  CALL setSparsity( obj=obj%csr, row=row, col=col )
END PROCEDURE csrMat_setSparsity2

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity3
  REAL( DFP ), ALLOCATABLE :: tempA( : )
  INTEGER( I4B ) :: m

  IF( ALLOCATED( obj%A ) ) THEN
    m = SIZE(obj%A)
    IF( m .NE. obj%csr%nnz ) THEN
      tempA = obj%A
      CALL Reallocate( obj%A, obj%csr%nnz )
      IF( SIZE(obj%A) .LT. SIZE( tempA ) ) THEN
        obj%A( 1:SIZE( tempA ) ) = tempA( : )
      ELSE
        obj%A( 1:obj%csr%nnz ) =  tempA( 1:obj%csr%nnz )
      END IF
      DEALLOCATE( tempA )
    END IF
  ELSE
    CALL Reallocate( obj%A, obj%csr%nnz )
  END IF

  IF( .NOT. obj%csr%isSparsityLock ) CALL setSparsity( obj%csr )
  !> Sort entries according to their column index
  CALL CSORT( obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, &
    & .TRUE. )
  obj%csr%isSorted = .TRUE.
  obj%csr%isSparsityLock = .FALSE.
  CALL setSparsity( obj%csr )
END PROCEDURE csrMat_setSparsity3

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in [[CSRMatrix_]]
!
!### Introduction
! This subroutine sets the value in [[CSRMatrix_]]
! - Shape( val ) = [SIZE(nptrs)*tdof, SIZE(nptrs)*tdof]
! - Usually `val` denotes the element matrix
! - Symbolic we are performing following task `obj(nptrs, nptrs)=val`

PURE SUBROUTINE setValueInternally( obj, nptrs, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val( :, : )
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: i, j, k, n, tdof, m

  n = SIZE( nptrs ); tdof = .tdof. obj%csr%dof
  ALLOCATE( row( tdof*n ) )
  IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
    DO i = 1, n
      DO j = 1, tdof
        row(( i-1 )*tdof + j ) = (nptrs( i ) - 1)*tdof + j
      END DO
    END DO
  ELSE
    DO j = 1, tdof
      m = obj%csr%dof%valmap( j + 1 ) - obj%csr%dof%valmap( j )
      DO i = 1, n
        row( ( j-1 )*n + i ) = ( j-1 )*m + nptrs( i )
      END DO
    END DO
  END IF
  !
  DO i =1, SIZE( row )
    DO k = 1, SIZE( row )
      DO j = obj%csr%IA( row( i ) ), obj%csr%IA( row( i ) + 1 ) - 1
        IF( obj%csr%JA( j ) .EQ. row( k ) ) THEN
          obj%A( j ) = val( i, k )
          EXIT
        END IF
      END DO
    END DO
  END DO
  DEALLOCATE( row )
END SUBROUTINE setValueInternally

!----------------------------------------------------------------------------
!                                                                 setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set1
  REAL( DFP ), ALLOCATABLE :: Mat( :, : )
  INTEGER( I4B ) :: tdof

  tdof = .tdof. obj%csr%dof
  SELECT CASE( storageFMT )
  CASE( NODES_FMT )
    IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
      Mat = val
    ELSE
      CALL Convert( From = val, To = Mat, Conversion = NodesToDOF, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  CASE( DOF_FMT )
    IF( obj%csr%dof%storageFMT .EQ. DOF_FMT ) THEN
      Mat=val
    ELSE
      CALL Convert( From = val, To = Mat, Conversion = DofToNodes, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  END SELECT
  CALL setValueInternally( obj, nptrs, Mat )
  IF( ALLOCATED( Mat ) ) DEALLOCATE( Mat )
END PROCEDURE csrMat_set1

!----------------------------------------------------------------------------
!                                                                 setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set2
  obj%A( : ) = val
END PROCEDURE csrMat_set2

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

PURE SUBROUTINE addContributionInternally( obj, nptrs, val, Scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val( :, : )
  REAL( DFP ), INTENT( IN ) :: Scale
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: i, j, k, n, tdof, m
  !
  n = SIZE( nptrs ); tdof = .tdof. obj%csr%dof
  ALLOCATE( row( tdof*n ) )
  !
  IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
    DO i = 1, n
      DO j = 1, tdof
        row( ( i - 1 ) * tdof + j ) = ( nptrs( i ) - 1 ) * tdof + j
      END DO
    END DO
  ELSE
    DO j = 1, tdof
      m = obj%csr%dof%valmap( j + 1 ) - obj%csr%dof%valmap( j )
      DO i = 1, n
        row( ( j - 1 ) * n + i ) = ( j - 1 ) * m + nptrs( i )
      END DO
    END DO
  END IF
  !
  DO i =1, SIZE( row )
    DO k = 1, SIZE( row )
      DO j = obj%csr%IA( row( i ) ), obj%csr%IA( row( i ) + 1 ) - 1
        IF( obj%csr%JA( j ) .EQ. row( k ) ) THEN
          obj%A( j ) = obj%A( j ) + Scale * val( i, k )
          EXIT
        END IF
      END DO
    END DO
  END DO
  DEALLOCATE( row )
END SUBROUTINE addContributionInternally

!----------------------------------------------------------------------------
!                                                           addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Add
  REAL( DFP ), ALLOCATABLE :: mat( :, : )
  INTEGER( I4B ) :: tdof
  !
  tdof = .tdof. obj%csr%dof
  SELECT CASE( storageFMT )
  CASE( NODES_FMT )
    IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
      mat = val
    ELSE
      CALL Convert( From = val, To = mat, Conversion = NodesToDOF, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  CASE( DOF_FMT )
    IF( obj%csr%dof%storageFMT .EQ. DOF_FMT ) THEN
      mat=val
    ELSE
      CALL Convert( From = val, To = mat, Conversion = DofToNodes, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  CASE DEFAULT
    mat = val
  END SELECT
  CALL addContributionInternally( obj, nptrs, mat, Scale )
  IF( ALLOCATED( mat ) ) DEALLOCATE( mat )
END PROCEDURE csrMat_Add

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethod
