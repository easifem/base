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

SUBMODULE(CSRMatrix_Method) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set0
  ! Internal variables
  INTEGER( I4B ), ALLOCATABLE :: row( : )
  INTEGER( I4B ) :: ii, jj, kk
  !!
  !! main
  !!
  row = getIndex( obj=obj%csr%dof, nodeNum=nodenum )
  !!
  DO ii =1, SIZE( row )
    !!
    DO kk = 1, SIZE( row )
      !!
      DO jj = obj%csr%IA( row( ii ) ), obj%csr%IA( row( ii ) + 1 ) - 1
        !!
        IF( obj%csr%JA( jj ) .EQ. row( kk ) ) THEN
          obj%A( jj ) = value( ii, kk )
          EXIT
        END IF
        !!
      END DO
      !!
    END DO
    !!
  END DO
  DEALLOCATE( row )
END PROCEDURE csrMat_set0

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set1
  REAL( DFP ), ALLOCATABLE :: m2( :, : )
  INTEGER( I4B ) :: tdof
  !!
  !! main
  !!
  tdof = .tdof. obj%csr%dof
  !!
  SELECT CASE( storageFMT )
  !!
  !!
  !!
  !!
  CASE( FMT_NODES )
    !!
    IF( (.StorageFMT. obj) .EQ. FMT_NODES ) THEN
      m2 = value
    ELSE
      CALL Convert( From = value, To = m2, Conversion = NodesToDOF, &
        & nns = SIZE( nodenum ), tDOF = tdof )
    END IF
  !!
  !!
  !!
  !!
  CASE( FMT_DOF )
    !!
    IF( (.StorageFMT. obj) .EQ. FMT_DOF ) THEN
      m2=value
    ELSE
      CALL Convert( From = value, To = m2, Conversion = DofToNodes, &
        & nns = SIZE( nodenum ), tDOF = tdof )
    END IF
    !!
  END SELECT
  !!
  CALL Set( obj=obj, nodenum=nodenum, value=m2 )
  !!
  IF( ALLOCATED( m2 ) ) DEALLOCATE( m2 )
  !!
END PROCEDURE csrMat_set1

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set2
  obj%A = value
END PROCEDURE csrMat_set2

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set3
  INTEGER( I4B ) :: i,j
  !!
  DO j = obj%csr%IA( irow ), obj%csr%IA( irow+1 ) - 1
    IF( obj%csr%JA(j) .EQ. icolumn ) obj%A( j ) = value
  END DO
  !!
END PROCEDURE csrMat_set3

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set4
  !!
  CALL set( obj=obj,  &
    & irow=getNodeLoc(obj=obj%csr%dof, nodenum=iNodeNum, idof=iDOF), &
    & icolumn=getNodeLoc(obj=obj%csr%dof, nodenum=jNodeNum, idof=jDOF), &
    & value=value)
  !!
END PROCEDURE csrMat_set4

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set5
  REAL( DFP ), ALLOCATABLE :: m2( :, : )
  INTEGER( I4B ) :: tdof
  !!
  !! main
  !!
  tdof = .tdof. (obj%csr%dof)
  CALL Reallocate( m2, tdof * size(nodenum), tdof * size(nodenum) )
  m2=value
  CALL Set( obj=obj, nodenum=nodenum, value=m2 )
  !!
  DEALLOCATE( m2 )
END PROCEDURE csrMat_set5

!----------------------------------------------------------------------------
!                                                                     set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set6
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
          obj%A( jj ) = value( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set6

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set7
  CALL set(obj=obj, &
    & irow=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=iNodeNum, &
    & ivar=ivar, &
    & idof=iDOF),&
    & icolumn=getNodeLoc( &
    & obj=obj%csr%dof, &
    & nodenum=jNodeNum, &
    & ivar=jvar, &
    & idof=jDOF), &
    & value=value)
  !!
END PROCEDURE csrMat_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set8
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
          obj%A( jj ) = value( ii, kk )
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set8

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set9
  !!
  CALL set(obj=obj, &
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
    & value=value)
  !!
END PROCEDURE csrMat_set9

!----------------------------------------------------------------------------
!                                                                     set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set10
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
          obj%A( jj ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set11
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
          obj%A( jj ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set12
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
          obj%A( jj ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set13
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
          obj%A( jj ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set13

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_set14
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
          obj%A( jj ) = value
          EXIT
        END IF
      END DO
    END DO
  END DO
  !!
  DEALLOCATE( row, col )
  !!
END PROCEDURE csrMat_set14

END SUBMODULE SetMethods
