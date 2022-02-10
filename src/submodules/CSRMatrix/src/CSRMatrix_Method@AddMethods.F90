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

SUBMODULE(CSRMatrix_Method) AddMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add0
  !! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: i, j, k, n, tdof, m
  !!
  !!
  !!
  n = SIZE( nodenum ); tdof = .tdof. obj%csr%dof
  ALLOCATE( row( tdof*n ) )
  !
  IF( obj%csr%dof%storageFMT .EQ. FMT_NODES ) THEN
    DO i = 1, n
      DO j = 1, tdof
        row( ( i - 1 ) * tdof + j ) = ( nodenum( i ) - 1 ) * tdof + j
      END DO
    END DO
  ELSE
    DO j = 1, tdof
      m = obj%csr%dof%valmap( j + 1 ) - obj%csr%dof%valmap( j )
      DO i = 1, n
        row( ( j - 1 ) * n + i ) = ( j - 1 ) * m + nodenum( i )
      END DO
    END DO
  END IF
  !
  DO i =1, SIZE( row )
    DO k = 1, SIZE( row )
      DO j = obj%csr%IA( row( i ) ), obj%csr%IA( row( i ) + 1 ) - 1
        IF( obj%csr%JA( j ) .EQ. row( k ) ) THEN
          obj%A( j ) = obj%A( j ) + scale * value( i, k )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row )
  !!
END PROCEDURE csrMat_add0

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add1
  REAL( DFP ), ALLOCATABLE :: mat( :, : )
  INTEGER( I4B ) :: tdof
  !!
  !!
  !!
  !!
  SELECT CASE( storageFMT )
  !!
  !!
  !!
  !!
  CASE( FMT_NODES )
    !!
    IF( obj%csr%dof%storageFMT .EQ. FMT_NODES ) THEN
      CALL Add( obj=obj, nodenum=nodenum, value=value, &
        & scale=scale )
    ELSE
      tdof = .tdof. obj%csr%dof
      CALL Convert( From = value, To = mat, Conversion = NodesToDOF, &
        & nns = SIZE( nodenum ), tDOF = tdof )
      CALL Add( obj=obj, nodenum=nodenum, value=mat, &
        & scale=scale )
      DEALLOCATE( mat )
    END IF
  !!
  !!
  !!
  !!
  CASE( FMT_DOF )
    !!
    IF( obj%csr%dof%storageFMT .EQ. FMT_DOF ) THEN
      CALL Add( obj=obj, nodenum=nodenum, value=value, &
        & scale=scale )
    ELSE
      tdof = .tdof. obj%csr%dof
      CALL Convert( From = value, To = mat, Conversion = DofToNodes, &
        & nns = SIZE( nodenum ), tDOF = tdof )
      CALL Add( obj=obj, nodenum=nodenum, value=mat, &
        & scale=scale )
      DEALLOCATE( mat )
    END IF
    !!
  END SELECT
  !!
END PROCEDURE csrMat_add1

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add2
  obj%A = obj%A + scale * value
END PROCEDURE csrMat_add2

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add3
  INTEGER( I4B ) :: i,j
  DO j = obj%csr%IA( iRow ), obj%csr%IA( iRow+1 ) - 1
    IF( obj%csr%JA(j) .EQ. iColumn ) &
      & obj%A( j ) = obj%A( j ) + scale*value
  END DO
END PROCEDURE csrMat_add3

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add4
  !!
  CALL add(obj=obj, &
    & irow=getNodeLoc(obj=obj%csr%dof, nodenum=inodenum, idof=idof), &
    & icolumn=getNodeLoc(obj=obj%csr%dof, nodenum=jnodenum, idof=jdof), &
    & value=value, scale=scale)
  !!
END PROCEDURE csrMat_add4

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add5
  REAL( DFP ), ALLOCATABLE :: m2( :, : )
  INTEGER( I4B ) :: tdof
  !!
  !!
  !!
  !!
  tdof = .tdof. obj%csr%dof
  ALLOCATE( m2( tdof*SIZE(nodenum), tdof*SIZE(nodenum) ) )
  m2=value
  CALL Add( obj=obj, nodenum=nodenum, value=m2, scale=scale )
  !!
  DEALLOCATE( m2 )
  !!
END PROCEDURE csrMat_add5

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add6
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getIndex( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar )
  col = getIndex( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add6

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add7
  CALL Add(obj=obj, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=iNodeNum, &
    & ivar=ivar, &
    & idof=iDOF),&
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=jNodeNum, &
    & ivar=jvar, &
    & idof=jDOF ), &
    & value=value, &
    & scale=scale)
  !!
END PROCEDURE csrMat_add7

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add8
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getIndex( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar, idof=idof )
  col = getIndex( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar, idof=jdof )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add8

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add9
  CALL Add(obj=obj, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=iNodeNum, &
    & ivar=ivar, &
    & spacecompo=ispacecompo, &
    & timecompo=itimecompo),&
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=jNodeNum, &
    & ivar=jvar, &
    & spacecompo=jspacecompo, &
    & timecompo=jtimecompo), &
    & value=value, &
    & scale=scale)
  !!
END PROCEDURE csrMat_add9

!----------------------------------------------------------------------------
!                                                                     add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add10
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getIndex( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar )
  col = getIndex( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add10

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add11
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getNodeLoc( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar, idof=idof )
  col = getNodeLoc( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar, idof=jdof )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add11

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add12
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getNodeLoc( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar, &
    & spacecompo=ispacecompo, timecompo=itimecompo )
  col = getNodeLoc( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar, &
    & spacecompo=jspacecompo, timecompo=jtimecompo )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add12

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add13
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getNodeLoc( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar, &
    & spacecompo=ispacecompo, timecompo=itimecompo )
  col = getNodeLoc( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar, &
    & spacecompo=jspacecompo, timecompo=jtimecompo )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add13

!----------------------------------------------------------------------------
!                                                                       add
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_add14
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : ), col( : )
  INTEGER( I4B ) :: ii,jj,kk
  !!
  !! main
  !!
  row = getNodeLoc( obj=obj%csr%dof, nodeNum=iNodeNum, ivar=ivar, &
    & spacecompo=ispacecompo, timecompo=itimecompo )
  col = getNodeLoc( obj=obj%csr%dof, nodeNum=jNodeNum, ivar=jvar, &
    & spacecompo=jspacecompo, timecompo=jtimecompo )
  !!
  DO ii =1, SIZE( row )
    DO kk = 1, SIZE( col )
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        IF( obj%csr%JA( jj ) .EQ. col( kk ) ) THEN
          obj%A( jj ) = obj%A( jj ) + scale * value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_add14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods
