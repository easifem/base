
SUBMODULE ( RealVector_Method ) GetMethod

USE BaseMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

  MODULE PROCEDURE f_getValues_self
    IF( ALLOCATED( Obj % Val ) ) THEN
      Val = RealVector( Obj % Val )
    END IF
  END PROCEDURE f_getValues_self

  MODULE PROCEDURE f_getSectionValues_self
    IF( ALLOCATED( Obj % Val ) ) THEN
      Val = RealVector( Obj % Val( Indx ) )
    END IF
  END PROCEDURE f_getSectionValues_self

  MODULE PROCEDURE f_getValuesFromTriplet_self
    IF( ALLOCATED( Obj % Val ) ) THEN
      Val = RealVector( Obj % Val( iStart:iEnd:Stride ) )
    END IF
  END PROCEDURE f_getValuesFromTriplet_self

  MODULE PROCEDURE f_getARRAYValues_self
    Val = RealVector( ArrayValues( Obj, TypeReal64 ) )
  END PROCEDURE f_getARRAYValues_self

  MODULE PROCEDURE f_getARRAYSectionValues_self
    Val = RealVector( ArrayValues( Obj, Indx, TypeReal64 ) )
  END PROCEDURE f_getARRAYSectionValues_self

  MODULE PROCEDURE f_getARRAYValuesFromTriplet_self
    Val = RealVector( ArrayValues( Obj, iStart, iEnd, Stride, &
      & TypeReal64 ) )
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
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValues_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val
  END IF
END PROCEDURE f_getValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getSectionValues_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val( Indx )
  END IF
END PROCEDURE f_getSectionValues_Real

!----------------------------------------------------------------------------
!                                                                 ArrayValues
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getValuesFromTriplet_Real
  IF( ALLOCATED( Obj % Val ) ) THEN
    Val = Obj % Val( iStart:iEnd:Stride )
  END IF
END PROCEDURE f_getValuesFromTriplet_Real

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
!                                                               ArrayPointers
!----------------------------------------------------------------------------

MODULE PROCEDURE f_getPointer_self
  Val => Obj
END PROCEDURE f_getPointer_self

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

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_1
  CALL Append( Obj % Val, Value )
END PROCEDURE Append_1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_2
  CALL Append( Obj % Val, Value )
END PROCEDURE Append_2

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_3
  CALL Append( Obj % Val, AnotherObj % Val )
END PROCEDURE Append_3

END SUBMODULE GetMethod
