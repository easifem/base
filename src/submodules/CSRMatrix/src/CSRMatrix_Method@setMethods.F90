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
! summary: It contains method for setting values in [[CSRMatrix_]]

SUBMODULE(CSRMatrix_Method) setMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                setSparsity
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
  CALL setSparsity( obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar )
END PROCEDURE csrMat_setSparsity3

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity4
  CALL setSparsity( obj=obj%csr, row=row, col=col, ivar=ivar, jvar=jvar )
END PROCEDURE csrMat_setSparsity4

!----------------------------------------------------------------------------
!                                                                setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setSparsity_final
  REAL( DFP ), ALLOCATABLE :: tempA( : )
  INTEGER( I4B ) :: m
  !
  IF( .NOT. obj%csr%isSparsityLock ) CALL setSparsity( obj%csr )
  IF( ALLOCATED( obj%A ) ) THEN
    m = SIZE(obj%A)
    IF( m .EQ. 0 ) THEN
      CALL Reallocate( obj%A, obj%csr%nnz )
    ELSE IF ( m .NE. obj%csr%nnz ) THEN
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
  !> Sort entries according to their column index
  CALL CSORT( obj%csr%nrow, obj%A, obj%csr%JA, obj%csr%IA, .TRUE. )
  obj%csr%isSorted = .TRUE.
  obj%csr%isSparsityLock = .FALSE.
  CALL setSparsity( obj%csr )
END PROCEDURE csrMat_setSparsity_final

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in [[CSRMatrix_]]
!
!# Introduction
! This subroutine sets the value in [[CSRMatrix_]]
! - Shape( val ) = [SIZE(nptrs)*tdof, SIZE(nptrs)*tdof]
! - Usually `val` denotes the element matrix
! - Symbolic we are performing following task `obj(nptrs, nptrs)=val`

PURE SUBROUTINE setInternally( obj, nptrs, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
  REAL( DFP ), INTENT( IN ) :: val( :, : )
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: ii, jj, kk
  !> main
  row = getIndex( obj=obj%csr%dof, nodeNum=Nptrs )
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( row )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. row( kk ) ) THEN
          obj%A( jj ) = val( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  IF( ALLOCATED( row ) ) DEALLOCATE( row )
END SUBROUTINE setInternally

! PURE SUBROUTINE setInternally( obj, nptrs, val )
!   TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
!   INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
!   REAL( DFP ), INTENT( IN ) :: val( :, : )
!   ! Internal variables
!   INTEGER( I4B ), ALLOCATABLE :: row( : )
!   INTEGER( I4B ) :: i, j, k, n, tdof, m
!   !> main
!   n = SIZE( nptrs ); tdof = .tdof. obj%csr%dof
!   ALLOCATE( row( tdof*n ) )
!   IF( (.StorageFMT. obj) .EQ. FMT_NODES ) THEN
!     DO i = 1, n
!       DO j = 1, tdof
!         row(( i-1 )*tdof + j ) = (nptrs( i ) - 1)*tdof + j
!       END DO
!     END DO
!   ELSE
!     DO j = 1, tdof
!       m = obj%csr%dof%valmap( j + 1 ) - obj%csr%dof%valmap( j )
!       DO i = 1, n
!         row( ( j-1 )*n + i ) = ( j-1 )*m + nptrs( i )
!       END DO
!     END DO
!   END IF
!   !
!   DO i =1, SIZE( row )
!     DO k = 1, SIZE( row )
!       DO j = obj%csr%IA( row( i ) ), obj%csr%IA( row( i ) + 1 ) - 1
!         IF( obj%csr%JA( j ) .EQ. row( k ) ) THEN
!           obj%A( j ) = val( i, k )
!           EXIT
!         END IF
!       END DO
!     END DO
!   END DO
!   DEALLOCATE( row )
! END SUBROUTINE setInternally

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set1
  REAL( DFP ), ALLOCATABLE :: Mat( :, : )
  INTEGER( I4B ) :: tdof
  !> main
  tdof = .tdof. obj%csr%dof
  SELECT CASE( storageFMT )
  CASE( FMT_NODES )
    IF( (.StorageFMT. obj) .EQ. FMT_NODES ) THEN
      Mat = val
    ELSE
      CALL Convert( From = val, To = Mat, Conversion = NodesToDOF, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  CASE( FMT_DOF )
    IF( (.StorageFMT. obj) .EQ. FMT_DOF ) THEN
      Mat=val
    ELSE
      CALL Convert( From = val, To = Mat, Conversion = DofToNodes, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  END SELECT
  CALL setInternally( obj, nptrs, Mat )
  IF( ALLOCATED( Mat ) ) DEALLOCATE( Mat )
END PROCEDURE csrMat_set1

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set2
  obj%A( : ) = val
END PROCEDURE csrMat_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set3
  INTEGER( I4B ) :: i,j
  DO j = obj%csr%IA( iRow ), obj%csr%IA( iRow+1 ) - 1
    IF( obj%csr%JA(j) .EQ. iColumn ) obj%A( j ) = val
  END DO
END PROCEDURE csrMat_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set4
  INTEGER( I4B ) :: irow, icolumn
  irow=getNodeLoc(obj=obj%csr%dof, inode=rowNodeNum, idof=rowDOF)
  icolumn=getNodeLoc(obj=obj%csr%dof, inode=colNodeNum, idof=colDOF)
  CALL set(obj=obj, irow=irow, icolumn=icolumn, val=val)
END PROCEDURE csrMat_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set5
  REAL( DFP ), ALLOCATABLE :: mat( :, : )
  INTEGER( I4B ) :: tdof
  tdof = .tdof. (obj%csr%dof)
  CALL Reallocate( mat, tdof, tdof )
  mat=val
  CALL setInternally( obj, nptrs, mat )
  IF( ALLOCATED( mat ) ) DEALLOCATE( mat )
END PROCEDURE csrMat_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: This subroutine set the value in [[CSRMatrix_]]
!
!# Introduction
! This subroutine sets the value in [[CSRMatrix_]]
! - Shape( val ) = [SIZE(nptrs)*tdof, SIZE(nptrs)*tdof]
! - Usually `val` denotes the element matrix
! - Symbolic we are performing following task `obj(nptrs, nptrs)=val`

PURE SUBROUTINE setBlockInternally( obj, iNptrs, jNptrs, ivar, jvar, val )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iNptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: jNptrs( : )
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: jvar
  REAL( DFP ), INTENT( IN ) :: val( :, : )
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !> main
  row = getIndex( obj=obj%csr%dof, nodeNum=iNptrs, ivar=ivar )
  col = getIndex( obj=obj%csr%dof, nodeNum=jNptrs, ivar=jvar )
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = val( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  IF(ALLOCATED(row) ) DEALLOCATE( row )
  IF(ALLOCATED(col) ) DEALLOCATE( col )
END SUBROUTINE setBlockInternally

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set6
  CALL setBlockInternally(obj=obj, iNptrs=iNptrs, jNptrs=jNptrs, &
    & ivar=ivar, jvar=jvar, val=val  )
END PROCEDURE csrMat_set6

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 Marach 2021
! summary: This subroutine add contribution

PURE SUBROUTINE addContributionInternally( obj, nptrs, val, Scale )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
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
  IF( obj%csr%dof%storageFMT .EQ. FMT_NODES ) THEN
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
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add1
  REAL( DFP ), ALLOCATABLE :: mat( :, : )
  INTEGER( I4B ) :: tdof
  !
  tdof = .tdof. obj%csr%dof
  SELECT CASE( storageFMT )
  CASE( FMT_NODES )
    IF( obj%csr%dof%storageFMT .EQ. FMT_NODES ) THEN
      mat = val
    ELSE
      CALL Convert( From = val, To = mat, Conversion = NodesToDOF, &
        & nns = SIZE( nptrs ), tDOF = tdof )
    END IF
  CASE( FMT_DOF )
    IF( obj%csr%dof%storageFMT .EQ. FMT_DOF ) THEN
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
END PROCEDURE csrMat_add1

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add2
  obj%A( : ) = obj%A( : ) + scale * val
END PROCEDURE csrMat_add2

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add3
  INTEGER( I4B ) :: i,j
  DO j = obj%csr%IA( iRow ), obj%csr%IA( iRow+1 ) - 1
    IF( obj%csr%JA(j) .EQ. iColumn ) obj%A( j ) = obj%A( j ) + scale*val
  END DO
END PROCEDURE csrMat_add3

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add4
  INTEGER( I4B ) :: irow, icolumn
  irow=getNodeLoc(obj=obj%csr%dof, inode=rowNodeNum, idof=rowDOF)
  icolumn=getNodeLoc(obj=obj%csr%dof, inode=colNodeNum, idof=colDOF)
  CALL add(obj=obj, irow=irow, icolumn=icolumn, val=val, scale=scale)
END PROCEDURE csrMat_add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add5
  REAL( DFP ), ALLOCATABLE :: mat( :, : )
  INTEGER( I4B ) :: tdof
  tdof = .tdof. obj%csr%dof
  ALLOCATE( mat( tdof*SIZE(nptrs), tdof*SIZE(nptrs) ) )
  mat=val
  CALL addContributionInternally( obj=obj, nptrs=nptrs, val=mat, scale=scale )
  DEALLOCATE( mat )
END PROCEDURE csrMat_add5

!----------------------------------------------------------------------------
!                                                                     setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow1
  INTEGER( I4B ) :: a, b
  IF( SIZE( val ) .LT. obj%csr%ncol .OR. iRow .GT. SIZE(obj, 1) ) THEN
    CALL ErrorMSG( Msg="SIZE of val vector should be same as number of col &
    & in sparse matrix or iRow is out of bound", &
    & File = "CSRMatrix_Method@setMethod.F90", &
    & Routine = "csrMat_setRow1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  a = obj%csr%IA( iRow )
  b = obj%csr%IA( iRow+1 ) - 1
  obj%A( a:b ) = val(obj%csr%JA(a:b))
END PROCEDURE csrMat_setRow1

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow2
  INTEGER( I4B ) :: irow
  irow = getNodeLoc(obj=obj%csr%dof, inode=inode, idof=idof)
  CALL csrMat_setRow1( obj=obj, irow=irow, val=val )
END PROCEDURE csrMat_setRow2

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow3
  INTEGER( I4B ) :: a, b
  a = obj%csr%IA( iRow )
  b = obj%csr%IA( iRow+1 ) - 1
  obj%A( a:b ) = val
END PROCEDURE csrMat_setRow3

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setRow4
  INTEGER( I4B ) :: irow
  irow = getNodeLoc(obj=obj%csr%dof, inode=inode, idof=idof)
  CALL csrMat_setRow3( obj=obj, irow=irow, val=val )
END PROCEDURE csrMat_setRow4

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn1
  INTEGER( I4B ) :: i, j
  IF( SIZE( val ) .LT. obj%csr%nrow .OR. iColumn .GT. SIZE(obj, 2) ) THEN
    CALL ErrorMSG( Msg="SIZE of column vector should be same as number of &
      & rows in sparse matrix", &
      & File = "CSRMatrix_Method@setMethod.F90", &
      & Routine = "csrMat_setColumn1", Line= __LINE__ , UnitNo=stdout )
    RETURN
  END IF
  DO i = 1, obj%csr%nrow
    DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
      IF( obj%csr%JA(j) .EQ. iColumn ) obj%A( j ) = val( i )
    END DO
  END DO
END PROCEDURE csrMat_setColumn1

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn2
  INTEGER( I4B ) :: icolumn
  icolumn = getNodeLoc(obj=obj%csr%dof, inode=inode, idof=idof)
  CALL csrMat_setColumn1( obj=obj, icolumn=icolumn, val=val )
END PROCEDURE csrMat_setColumn2

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn3
  INTEGER( I4B ) :: i, j
  DO i = 1, obj%csr%nrow
    DO j = obj%csr%IA( i ), obj%csr%IA( i+1 ) - 1
      IF( obj%csr%JA(j) .EQ. iColumn ) obj%A( j ) = val
    END DO
  END DO
END PROCEDURE csrMat_setColumn3

!----------------------------------------------------------------------------
!                                                                  setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setColumn4
  INTEGER( I4B ) :: icolumn
  icolumn = getNodeLoc(obj=obj%csr%dof, inode=inode, idof=idof)
  CALL csrMat_setColumn3( obj=obj, icolumn=icolumn, val=val )
END PROCEDURE csrMat_setColumn4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE setMethods
