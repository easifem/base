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

SUBMODULE(DOF_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_st_dof
  INTEGER( I4B ) :: n, i, k, j

  Obj % StorageFMT = StorageFMT; n = SIZE( Names )

  CALL reallocate( Obj % Map, n + 1, 6 )
  ASSOCIATE( Map => Obj % Map )
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
    CALL Reallocate( Obj % ValMap, Map( n + 1, 4 ) + 1 )
    Obj % ValMap( 1 ) = 1; k = 1
    DO i = 1, n
      DO j = 1, Map( i, 4 )
        k = k + 1
        Obj % ValMap( k ) = Obj % ValMap( k-1 ) + Map( i, 6 )
      END DO
    END DO
  END ASSOCIATE
END PROCEDURE initiate_st_dof

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_val
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val, tNodes )
END PROCEDURE initiate_val

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_2val
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val1, tNodes, Val2, tNodes )
END PROCEDURE initiate_2val

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_realvector_scalar
  INTEGER( I4B ) :: tNodes
  tNodes = .tNodes. Obj
  CALL Reallocate( Val % Val, tNodes )
END PROCEDURE initiate_realvector_scalar

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_realvector_vector
  INTEGER( I4B ) :: tDOF, i, n
  INTEGER( I4B ), ALLOCATABLE :: tNodes( : )
  !
  ASSOCIATE( Map => Obj % Map )
    tDOF = .tDOF. Obj
    ALLOCATE( tNodes( tDOF ) )
    n = SIZE( Map, 1 )
    DO i = 1, n-1
      tNodes( Map( i, 5 ) : Map( i + 1, 5 ) - 1 ) = Map( i, 6 )
    END DO
    CALL Initiate( Val, tNodes )
    DEALLOCATE( tNodes )
  END ASSOCIATE
END PROCEDURE initiate_realvector_vector

!----------------------------------------------------------------------------
!                                                                        DOF
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Initiate( Obj = Obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, StorageFMT = StorageFMT)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                 DOF_Pointer
!----------------------------------------------------------------------------
MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL Initiate( Obj = Obj, Names = Names, tNodes = tNodes, &
    & SpaceCompo = SpaceCompo, TimeCompo = TimeCompo, &
    & StorageFMT = StorageFMT)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Map ) ) DEALLOCATE( Obj % Map )
  IF( ALLOCATED( Obj % ValMap ) ) DEALLOCATE( Obj % ValMap )
END PROCEDURE deallocate_data


END SUBMODULE Constructor