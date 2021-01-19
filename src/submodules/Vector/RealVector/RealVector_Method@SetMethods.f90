SUBMODULE( RealVector_Method ) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE setValue_1
  IF( ALLOCATED( Obj % Val ) ) THEN
    IF( SIZE( Value) .EQ. 1 ) THEN
      Obj % Val( Indx ) = Value( 1 )
    ELSE
      Obj % Val( Indx ) = Value
    END IF
  END IF
END PROCEDURE setValue_1

END SUBMODULE SetMethods