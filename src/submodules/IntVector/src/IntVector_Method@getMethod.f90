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

SUBMODULE ( IntVector_Method ) setMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValues_self
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = IntVector( Obj % Val )
  END IF
END PROCEDURE f_getValues_self

MODULE PROCEDURE f_getSectionValues_self
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = IntVector( Obj % Val( Indx ) )
  END IF
END PROCEDURE f_getSectionValues_self

MODULE PROCEDURE f_getValuesFromTriplet_self
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = IntVector( Obj % Val( iStart:iEnd:Stride ) )
  END IF
END PROCEDURE f_getValuesFromTriplet_self

MODULE PROCEDURE f_getARRAYValues_self
  Val = IntVector( ArrayValues( Obj, TypeInt ) )
END PROCEDURE f_getARRAYValues_self

MODULE PROCEDURE f_getARRAYSectionValues_self
  Val = IntVector( ArrayValues( Obj, Indx, TypeInt ) )
END PROCEDURE f_getARRAYSectionValues_self

MODULE PROCEDURE f_getARRAYValuesFromTriplet_self
  Val = IntVector( ArrayValues( Obj, iStart, iEnd, Stride, &
    & TypeInt ) )
END PROCEDURE f_getARRAYValuesFromTriplet_self

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValues_Int
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val
  END IF
END PROCEDURE f_getValues_Int

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getSectionValues_Int
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val( Indx )
  END IF
END PROCEDURE f_getSectionValues_Int

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValuesFromTriplet_Int
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val( iStart:iEnd:Stride )
  END IF
END PROCEDURE f_getValuesFromTriplet_Int

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValues_Int
  INTEGER( I4B ) :: N, i, tNodes, r1, r2
  N = SIZE( Obj )
  tNodes = 0
  DO i = 1, N
    tNodes = tNodes + SIZE( Obj( i ) % Val )
  END DO

  ALLOCATE( Val( tNodes ) )

  tNodes = 0; r1 = 0; r2 = 0

  DO i = 1, N
    r1 = r2 + 1; r2 = r2 + SIZE( Obj( i ) % Val )
    Val( r1 : r2 ) = Obj( i ) % Val
  END DO

END PROCEDURE f_getARRAYValues_Int

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYSectionValues_Int
  INTEGER( I4B ) :: N, i, M

  N = SIZE( Obj )
  M = SIZE( Indx )

  ALLOCATE( Val( N * M ) )

  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = Obj( i ) % Val( Indx )
  END DO

END PROCEDURE f_getARRAYSectionValues_Int

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValuesFromTriplet_Int
  INTEGER( I4B ) :: N, i, M

  N = SIZE( Obj )
  M = 1 + ( iEnd - iStart ) / Stride

  ALLOCATE( Val( M * N ) )

  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = Obj( i ) % Val( iStart:iEnd:Stride )
  END DO

END PROCEDURE f_getARRAYValuesFromTriplet_Int

!----------------------------------------------------------------------------
!                                                               ArrayPointers
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getPointer_self
  Val => Obj
END PROCEDURE f_getPointer_self

!----------------------------------------------------------------------------
!                                                               ArrayPointers
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getPointer_Int
  Val => Obj % Val
END PROCEDURE f_getPointer_Int

!----------------------------------------------------------------------------
!                                                                 Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_convert_int
  IF( ALLOCATED( From % Val ) ) THEN
    To = From % Val
  END IF
END PROCEDURE obj_convert_int

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE IndexOf_1
  Ans = MINLOC( ABS( Obj % Val - Value ), 1 )
END PROCEDURE IndexOf_1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE IndexOf_2
  ! Ans = MINLOC( ABS( Obj % Val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  !
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0

  DO i = 1, SIZE( Obj % Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. Obj % Val( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE IndexOf_2

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE IndexOf_3
  Ans = MINLOC( ABS( Obj - Value ), 1 )
END PROCEDURE IndexOf_3

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE IndexOf_4
  ! Ans = MINLOC( ABS( Obj % Val - Value ), 1 )
  INTEGER( I4B ) :: i, j, m
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  !
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0

  DO i = 1, SIZE( Obj )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. Obj( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE IndexOf_4

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE isPresent_1
  INTEGER( I4B ) :: i
  Ans = .FALSE.
  DO i = 1, SIZE( Obj % Val )
    IF( Obj % Val( i ) .EQ. Value ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE isPresent_1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE isPresent_2
  INTEGER( I4B ) :: i, m, j
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )
  m = SIZE( Value )
  ALLOCATE( Ans( m ), Search( m ) )
  Search = .TRUE.
  Ans = .FALSE.

  DO i = 1, SIZE( Obj % Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( Value( j ) .EQ. Obj % Val( i ) ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = .TRUE.
        END IF
      END IF
    END DO
  END DO

END PROCEDURE isPresent_2

END SUBMODULE getMethod
