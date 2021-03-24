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
! summary: 	This submodule contains get methods of [[RealVector_]]

SUBMODULE ( RealVector_Method ) getMethod
USE BaseMethod
IMPLICIT NONE
CONTAINS

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

MODULE PROCEDURE f_getValues_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    CALL Reallocate( Val, SIZE( Obj ) )
    CALL COPY( Y=Val, X=Obj%Val )
  END IF
END PROCEDURE f_getValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getSectionValues_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    CALL Reallocate( Val, SIZE(Indx ) )
    CALL COPY( Y=Val, X=Obj%Val(Indx) )
  END IF
END PROCEDURE f_getSectionValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValuesFromTriplet_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj%Val ( iStart:iEnd:Stride )
  END IF
END PROCEDURE f_getValuesFromTriplet_Real

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
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValues_Real
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
END PROCEDURE f_getARRAYValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYSectionValues_Real
  INTEGER( I4B ) :: N, i, M
  N = SIZE( Obj )
  M = SIZE( Indx )
  ALLOCATE( Val( M * N ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = Obj( i ) % Val( Indx )
  END DO
END PROCEDURE f_getARRAYSectionValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValuesFromTriplet_Real
  INTEGER( I4B ) :: N, i, M
  N = SIZE( Obj )
  M = 1 + ( iEnd - iStart ) / Stride
  ALLOCATE( Val( M * N ) )
  DO i = 1, N
    Val( ( i - 1 ) * M + 1 : i * M ) = Obj( i ) % Val( iStart:iEnd:Stride )
  END DO
END PROCEDURE f_getARRAYValuesFromTriplet_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getSectionValues_self
  IF( ALLOCATED( Obj % Val ) ) THEN
    ! CALL COPY( Y=Val, X=Obj%Val( Indx ) )
    STOP
  END IF
END PROCEDURE f_getSectionValues_self

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValuesFromTriplet_self
  IF( ALLOCATED( Obj % Val ) ) THEN
    ! CALL COPY( Y=Val, X=Obj%Val( iStart:iEnd:Stride ) )
    STOP
  END IF
END PROCEDURE f_getValuesFromTriplet_self

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValues_self
  ! CALL COPY( Y=Val, X=ArrayValues( Obj, TypeReal64 ) )
  STOP
END PROCEDURE f_getARRAYValues_self

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYSectionValues_self
  ! CALL COPY( Y=Val, X=ArrayValues( Obj, Indx, TypeReal64 ) )
  STOP
END PROCEDURE f_getARRAYSectionValues_self

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getARRAYValuesFromTriplet_self
  ! CALL COPY( Y=Val, X=ArrayValues( Obj, iStart, iEnd, Stride, TypeReal64 ) )
  STOP
END PROCEDURE f_getARRAYValuesFromTriplet_self

!----------------------------------------------------------------------------
!                                                               ArrayPointers
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getPointer_Real64
  Val => Obj % Val
END PROCEDURE f_getPointer_Real64

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
  REAL( DFP ) :: tol0

  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  m = SIZE( Value )
  ALLOCATE( Search( m ), Ans( m ) )
  Search = .TRUE.
  Ans = 0
  DO i = 1, SIZE( Obj % Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS(Value(j) - Obj % Val(i)) .LE. tol0  ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = i
        END IF
      END IF
    END DO
  END DO
END PROCEDURE IndexOf_2

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE isPresent_1
  INTEGER( I4B ) :: i
  REAL( DFP ) :: tol0
  Ans = .FALSE.
  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  DO i = 1, SIZE( Obj % Val )
    IF( ABS( Obj%Val(i) - Value ) .LE. tol0 ) THEN
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
  REAL( DFP ) :: tol0
  LOGICAL( LGT ), ALLOCATABLE :: Search( : )

  tol0 = INPUT( default = REAL(1.0E-10, DFP), option = tol )
  m = SIZE( Value )
  ALLOCATE( Ans( m ), Search( m ) )
  Search = .TRUE.
  Ans = .FALSE.
  DO i = 1, SIZE( Obj % Val )
    DO j = 1, m
      IF( Search( j ) ) THEN
        IF( ABS( Value(j) - Obj%Val(i) ) .LE. tol0 ) THEN
          Search( j ) = .FALSE.
          Ans( j ) = .TRUE.
        END IF
      END IF
    END DO
  END DO
END PROCEDURE isPresent_2

END SUBMODULE getMethod
