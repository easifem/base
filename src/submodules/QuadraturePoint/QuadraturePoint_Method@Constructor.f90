SUBMODULE( QuadraturePoint_Method ) Constructor
USE BaseMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  Obj % Points = Points
  Obj % tXi = SIZE( Points, 1 ) - 1
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_txi
  Obj % tXi = tXi
  CALL Reallocate( Obj % Points, tXi + 1, tPoints )
END PROCEDURE initiate_obj_txi

!----------------------------------------------------------------------------
!                                                            QuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  Obj % Points = Points
  Obj % tXi = SIZE( Points, 1 ) - 1
END PROCEDURE Constructor1

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  Obj % Points = Points
  Obj % tXi = SIZE( Points, 1 ) - 1
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( Obj % Points ) ) DEALLOCATE( Obj % Points )
  Obj % tXi = -1
END PROCEDURE Deallocate_Data

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE size_obj
  IF( Dims .EQ. 1 ) THEN
    Ans = SIZE( Obj % Points, 1 )
  ELSE IF( Dims .EQ. 2 ) THEN
    Ans = SIZE( Obj % Points, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE size_obj

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj

  INTEGER( I4B ) :: I, j

  IF( .NOT. ALLOCATED( Obj % Points ) ) RETURN

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  CALL Blanklines( UnitNo = I )
  CALL Display( msg, UnitNo = I )
  CALL Blanklines( UnitNo = I )

  WRITE( I, "(A)" ) "          Weights    |      Points       "
  WRITE( I, "(A)" ) "-----------------------------------------"
  DO j = 1, SIZE( Obj % Points, 2 )
    WRITE( I,"( 2X, G15.8, 2X" // TRIM( INT2STR( Obj % tXi ) ) // "G15.8 )")&
      & Obj % Points( Obj % tXi + 1, j ), &
      & Obj % Points( 1 : Obj % tXi, j )
  END DO
  WRITE( I, "(A)" ) "-----------------------------------------"

END PROCEDURE display_obj

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE getQP1
  Point = 0.0_DFP
  Point( 1 : Obj % tXi ) = Obj % Points( 1 : Obj % tXi, Num )
  Weight = Obj % Points( Obj % tXi + 1, Num )
END PROCEDURE getQP1

!----------------------------------------------------------------------------
!                                                         getQuadraturePoint
!----------------------------------------------------------------------------

MODULE PROCEDURE getQP2
  INTEGER( I4B ) :: n

  n = SIZE( Obj % Points, 2 ) !#column
  IF( ALLOCATED( Point ) ) DEALLOCATE( Point )
  ALLOCATE( Point( 3, n ) )
  Point = 0.0_DFP
  Point( 1 : Obj % tXi, 1:n ) = Obj % Points( 1 : Obj % tXi, 1:n )
  Weight = Obj % Points( Obj % tXi + 1, 1:n )
END PROCEDURE getQP2

END SUBMODULE Constructor
