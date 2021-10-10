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

SUBMODULE(DOF_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate1
  INTEGER( I4B ) :: n, i, k, j
  !> main
  obj%StorageFMT = StorageFMT; n = SIZE( Names )
  CALL reallocate( obj%Map, n + 1, 6 )
  ASSOCIATE( Map => obj%Map )
    !
    !<- Names in ascii code
    Map( 1:n, 1 ) = ICHAR( Names( 1:n ) )
    Map( 1 + n, 1 ) = 0
    !
    !<- Space components; -1 if scalar component like pressure
    Map( 1:n, 2 ) = SpaceCompo
    Map( 1 + n, 2 ) = 0
    !
    !<- Time component; 1 if time invariant
    Map( 1:n, 3 ) = TimeCompo
    Map( 1 + n, 3 ) = 0
    !
    !<- tDOF for each physical name
    DO i = 1, n
      IF( SpaceCompo( i ) .LT. 0 ) THEN
        Map( i, 4 ) = TimeCompo( i )
      ELSE
        Map( i, 4 ) = TimeCompo( i ) * SpaceCompo( i )
      END IF
    END DO
    Map( n + 1, 4 ) = SUM( Map( 1:n, 4 ) )
    !
    !<- Here we set Indx
    Map( 1, 5 ) = 1
    DO i = 2, n + 1
      Map( i, 5 ) = Map( i - 1, 5 ) + Map( i - 1, 4 )
    END DO
    !
    !<- tNodes
    Map( 1:n, 6 ) = tNodes
    Map( n+1, 6 ) = SUM( Map( 1:n, 6 ) * Map( 1:n, 4 ) )
    !
    !<- ValMap( tDOF + 1, 2 )
    CALL Reallocate( obj%ValMap, Map( n + 1, 4 ) + 1 )
    obj%ValMap( 1 ) = 1; k = 1
    DO i = 1, n
      DO j = 1, Map( i, 4 )
        k = k + 1
        obj%ValMap( k ) = obj%ValMap( k-1 ) + Map( i, 6 )
      END DO
    END DO
  END ASSOCIATE
END PROCEDURE dof_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate2
  CALL Reallocate( Val, .tNodes. obj )
END PROCEDURE dof_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate3
  CALL Initiate( obj=val, tSize= (.tNodes. obj))
END PROCEDURE dof_initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate4
  INTEGER( I4B ) :: i
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : )
  ! main
  ASSOCIATE( Map => obj%Map )
    ALLOCATE( tNodes( .tDOF. obj ) )
    DO i = 1, SIZE( Map, 1 ) - 1
      tNodes( Map( i, 5 ) : Map( i + 1, 5 ) - 1 ) = Map( i, 6 )
    END DO
    CALL Initiate( Val, tNodes )
    DEALLOCATE( tNodes )
  END ASSOCIATE
END PROCEDURE dof_initiate4

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate5
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. obj
  CALL Reallocate( Val1, tNodes, Val2, tNodes )
END PROCEDURE dof_initiate5

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_initiate6
  obj1%StorageFMT = obj2%StorageFMT
  IF( ALLOCATED( obj2%valmap ) ) obj1%valmap = obj2%valmap
  IF( ALLOCATED( obj2%map ) ) obj1%map = obj2%map
END PROCEDURE dof_initiate6

!----------------------------------------------------------------------------
!                                                                        DOF
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_Constructor1
  CALL Initiate( obj = obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, StorageFMT = StorageFMT)
END PROCEDURE dof_Constructor1

!----------------------------------------------------------------------------
!                                                                 DOF_Pointer
!----------------------------------------------------------------------------
MODULE PROCEDURE dof_Constructor_1
  ALLOCATE( obj )
  CALL Initiate( obj = obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, &
    & StorageFMT = StorageFMT)
END PROCEDURE dof_Constructor_1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE dof_DeallocateData
  IF( ALLOCATED( obj%Map ) ) DEALLOCATE( obj%Map )
  IF( ALLOCATED( obj%ValMap ) ) DEALLOCATE( obj%ValMap )
END PROCEDURE dof_DeallocateData

END SUBMODULE ConstructorMethods