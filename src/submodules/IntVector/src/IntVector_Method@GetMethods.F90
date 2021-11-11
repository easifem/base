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
! date: 	25 Feb 2021
! summary: This submodule implements get methods of [[IntVector_]]

SUBMODULE ( IntVector_Method) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_1
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = IntVector( obj%Val )
  END IF
END PROCEDURE intVec_get_1

MODULE PROCEDURE intVec_get_2
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = IntVector( obj%Val( Indx ) )
  END IF
END PROCEDURE intVec_get_2

MODULE PROCEDURE intVec_get_3
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = IntVector( obj%Val( iStart:iEnd:Stride ) )
  END IF
END PROCEDURE intVec_get_3

MODULE PROCEDURE intVec_get_4
  Val = IntVector( get( obj, TypeInt ) )
END PROCEDURE intVec_get_4

MODULE PROCEDURE intVec_get_5
  Val = IntVector( get( obj, Indx, TypeInt ) )
END PROCEDURE intVec_get_5

MODULE PROCEDURE intVec_get_6
  Val = IntVector( get( obj, iStart, iEnd, Stride, &
    & TypeInt ) )
END PROCEDURE intVec_get_6

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_7
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = obj%Val
  END IF
END PROCEDURE intVec_get_7

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_8
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = obj%Val( Indx )
  END IF
END PROCEDURE intVec_get_8

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_9
  IF( ALLOCATED( obj%Val ) ) THEN
    Val = obj%Val( iStart:iEnd:Stride )
  END IF
END PROCEDURE intVec_get_9

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_10
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( obj( i )%Val )
  END DO

  ALLOCATE( Val( tNodes ) )

  tNodes = 0; r1 = 0; r2 = 0

  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( obj( i )%Val )
    Val( r1 : r2 ) = obj( i )%Val
  END DO

END PROCEDURE intVec_get_10

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_11
  INTEGER( I4B ) :: N, i, M

  N = SIZE( obj )
  M = SIZE( Indx )

  ALLOCATE( Val( N * M ) )

  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( Indx )
  END DO

END PROCEDURE intVec_get_11

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_12
  INTEGER( I4B ) :: N, i, M

  N = SIZE( obj )
  M = 1 + ( iEnd - iStart ) / Stride

  ALLOCATE( Val( M * N ) )

  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = obj( i )%Val( iStart:iEnd:Stride )
  END DO

END PROCEDURE intVec_get_12

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_get_13
  val = obj%val( indx )
END PROCEDURE intVec_get_13

!----------------------------------------------------------------------------
!                                                                 Assignment
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVec_assign_a
  Val = get( obj, datatype=TYPEInt )
END PROCEDURE IntVec_assign_a

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getPointer_1
  Val => obj
END PROCEDURE intVec_getPointer_1

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getPointer_2
  Val => obj%Val
END PROCEDURE intVec_getPointer_2

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_int
  IF( ALLOCATED( From%Val ) ) THEN
    To = From%Val
  END IF
END PROCEDURE obj_convert_int

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex1
  Ans = MINLOC( ABS( obj%Val - Value ), 1 )
END PROCEDURE intVec_getIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex2
  ! Ans = MINLOC( ABS( obj%Val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  !
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0

  DO i = 1, SIZE( obj%Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. obj%Val( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE intVec_getIndex2

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex3
  Ans = MINLOC( ABS( obj - Value ), 1 )
END PROCEDURE intVec_getIndex3

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_getIndex4
  ! Ans = MINLOC( ABS( obj%Val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  !
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0

  DO i = 1, SIZE( obj )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. obj( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE intVec_getIndex4

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isPresent1
  INTEGER( I4B ) :: i
  Ans = .FALSE.
  DO i = 1, SIZE( obj%Val )
    IF( obj%Val( i ) .EQ. Value ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE intVec_isPresent1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_isPresent2
  INTEGER( I4B ) :: i, m, j
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  m = SIZE( Value )
  ALLOCATE( Ans( m ), Search( m ) )
  Search = .TRUE.
  Ans = .FALSE.

  DO i = 1, SIZE( obj%Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. obj%Val( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = .TRUE.
        END IF
      END IF
    END DO
  END DO

END PROCEDURE intVec_isPresent2

END SUBMODULE getMethod
