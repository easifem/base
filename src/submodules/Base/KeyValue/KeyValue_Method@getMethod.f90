SUBMODULE( KeyValue_Method ) getMethod
  !! This submodule includes implementation of method to set values in
  !! [[keyvalue_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     getKey
!----------------------------------------------------------------------------

MODULE PROCEDURE getKey1
  Key = TRIM( Obj % Key % Raw )
END PROCEDURE getKey1

!----------------------------------------------------------------------------
!                                                                     getKey
!----------------------------------------------------------------------------

MODULE PROCEDURE getKey2
  Key = Obj % Key
END PROCEDURE getKey2

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue1
  Value = Obj % Value( 1, 1 )
END PROCEDURE getValue1

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue2
  Value = Obj % Value( :, 1 )
END PROCEDURE getValue2

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue3
  Value = Obj % Value
END PROCEDURE getValue3

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue4
  Value = INT( Obj % Value( 1, 1 ) )
END PROCEDURE getValue4

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue5
  Value = INT( Obj % Value( :, 1 ) )
END PROCEDURE getValue5

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue6
  Value = INT( Obj % Value )
END PROCEDURE getValue6

!----------------------------------------------------------------------------
!                                                                     INDEX
!----------------------------------------------------------------------------

MODULE PROCEDURE Index1
  INTEGER( I4B ) :: I
  Ans = 0
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = I
      EXIT
    END IF
  END DO
END PROCEDURE Index1

!----------------------------------------------------------------------------
!                                                                     INDEX
!----------------------------------------------------------------------------

MODULE PROCEDURE Index2
  INTEGER( I4B ) :: I
  Ans = 0
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = I
      EXIT
    END IF
  END DO
END PROCEDURE Index2

!----------------------------------------------------------------------------
!                                                                   Present
!----------------------------------------------------------------------------

MODULE PROCEDURE Present1
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Present1

!----------------------------------------------------------------------------
!                                                                   Present
!----------------------------------------------------------------------------

MODULE PROCEDURE Present2
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Present2

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

MODULE PROCEDURE Contains1
  INTEGER( I4B ) :: I

  Ans = .FALSE.
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO

END PROCEDURE Contains1

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

MODULE PROCEDURE Contains2
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( Obj )
    IF( Obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Contains2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE getMethod
