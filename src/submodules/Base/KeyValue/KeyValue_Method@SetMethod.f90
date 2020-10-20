SUBMODULE( KeyValue_Method ) SetMethod
  !! This submodule includes implementation of method to set values in
  !! [[keyvalue_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setKey1
  Obj % Key = Key
END PROCEDURE setKey1

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setKey2
  Obj % Key = Key
END PROCEDURE setKey2

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue1
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue1

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue2
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue2

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue3
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue3

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue4
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue4

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue5
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue5

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue6
  CALL Initiate( Obj, Obj % Key, Value )
END PROCEDURE SetValue6

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE keyvalue_append
  INTEGER( I4B ) :: I, Indx, tSize
  LOGICAL( LGT ) :: isPresent

  IF( .NOT. ALLOCATED( Obj ) ) THEN
    ALLOCATE( Obj( 1 ) )
    Obj( 1 ) = KeyValObj
  ELSE
    tSize = SIZE( Obj )
    DO  I = 1, tSize
      isPresent = Obj( I ) .EQ. KeyValObj % Key
      IF( isPresent ) THEN
        Indx = I
        EXIT
      END IF
    END DO

    IF( isPresent ) THEN

      Obj( Indx ) = KeyValObj

    ELSE

      BLOCK
        TYPE( keyvalue_ ) :: DummyObj( tSize )

        DO I = 1, tSize
          DummyObj( I ) = Obj( I )
        END DO

        DEALLOCATE( Obj )
        ALLOCATE( Obj( tSize + 1 ) )

        DO I = 1, tSize
          Obj( I ) = DummyObj( I )
        END DO

        Obj( tSize + 1 ) = KeyValObj

      END BLOCK
    END IF
  END IF

END PROCEDURE keyvalue_append

END SUBMODULE SetMethod