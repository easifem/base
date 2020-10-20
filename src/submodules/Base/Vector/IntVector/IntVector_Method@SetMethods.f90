SUBMODULE( IntVector_Method ) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE setValue_1
  IF( ALLOCATED( Obj % Val ) ) THEN
    IF( SIZE( Value ) .EQ. 1 ) THEN
      Obj % Val( Indx ) = Value( 1 )
    ELSE
      Obj % Val( Indx ) = Value
    END IF
  END IF
END PROCEDURE setValue_1

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE remove_duplicates
  ! Define internal variables
  INTEGER( I4B ) :: i,k, j, N
  INTEGER( I4B ), ALLOCATABLE :: Res( : )

  IF( ALLOCATED( Obj % Val )  ) THEN

    N = SIZE ( Obj % Val )
    ALLOCATE( Res( N ) )
    Res = 0
    Res( 1 ) = Obj % Val ( 1 )
    k = 1

    DO i = 2, N
      IF( .NOT. ANY( Res .EQ. Obj % Val( i ) ) ) THEN
        k = k + 1
        Res( k ) = Obj % Val( i )
      END IF
    END DO

    Obj % Val = Res( 1 : k )
    DEALLOCATE( Res )

  END IF

END PROCEDURE remove_duplicates

!----------------------------------------------------------------------------
!                                                             RemoveDuplicate
!----------------------------------------------------------------------------

MODULE PROCEDURE remove_dupl_intvec
  ! Define internal variables
  INTEGER( I4B ) :: i,k, j, N
  INTEGER( I4B ), ALLOCATABLE :: Res( : )

  IF( ALLOCATED( Obj )  ) THEN

    N = SIZE ( Obj )
    ALLOCATE( Res( N ) )
    Res = 0
    Res( 1 ) = Obj ( 1 )
    k = 1

    DO i = 2, N
      IF( .NOT. ANY( Res .EQ. Obj( i ) ) ) THEN
        k = k + 1
        Res( k ) = Obj( i )
      END IF
    END DO

    Obj = Res( 1 : k )
    DEALLOCATE( Res )

  END IF

END PROCEDURE remove_dupl_intvec

!----------------------------------------------------------------------------
!                                                                     Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE repeat_int
  INTEGER( I4B ) :: n, i
  n = SIZE( Val )
  Ans( 1 : n ) = Val
  DO i = 1, rtimes-1
    Ans( i * n + 1: ( i + 1 ) * n ) = Val
  END DO
END PROCEDURE repeat_int

!----------------------------------------------------------------------------
!                                                                     Repeat
!----------------------------------------------------------------------------

MODULE PROCEDURE repeat_obj
  Ans = Repeat( Obj % Val, rtimes )
END PROCEDURE repeat_obj

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

END SUBMODULE SetMethods
