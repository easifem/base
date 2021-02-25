SUBMODULE ( IntVector_Method ) Constructor

USE BaseMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  IF( ALLOCATED( Obj % Val ) ) THEN
    Ans(1) = SIZE( Obj % Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  IF( ALLOCATED( Obj % Val ) ) THEN
    Ans = SIZE( Obj % Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  IF( ALLOCATED( Obj % Val ) ) THEN
    IF( SIZE( Obj % Val ) .NE. Dims ) THEN
      DEALLOCATE( Obj % Val )
      ALLOCATE( Obj % Val( Dims ) )
      Obj % Val = 0
    ELSE
      Obj % Val = 0
    END IF
  ELSE
    ALLOCATE( Obj % Val( Dims ) )
    Obj % Val = 0
  END IF
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
END PROCEDURE deallocate_data

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE IntVectorDisplay

  INTEGER( I4B ) :: I, j
  
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  
  WRITE( I, "(A)" ) TRIM( msg )
  DO j = 1, SIZE( Obj )
    CALL Display( Obj( j ) % Val, "", I )
  END DO

END PROCEDURE IntVectorDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE IntscalarDisplay

  IF( PRESENT( UnitNo ) ) THEN
    CALL Display( Obj % Val, msg )
  ELSE
    CALL Display( Obj % Val, msg, UnitNo )
  END IF

END PROCEDURE IntscalarDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE IntIntrinsicDisplay

  INTEGER( I4B ) :: I
  CHARACTER( LEN = 20 ) :: Fmt1, Fmt2

  Fmt1 = FInt32

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF

  Fmt2 = "( "//"1"//Fmt1( 2 : )
  WRITE( I, "(A)" ) TRIM( msg )
  WRITE( I, Fmt2 ) Val

END PROCEDURE IntIntrinsicDisplay

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  CALL AllocateData( Obj, tSize )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_vector
  INTEGER( I4B ) :: n, i

  n = SIZE( tSize )

  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF

  DO i = 1, n
    CALL AllocateData( Obj( i ), tSize( i ) )
  END DO

END PROCEDURE initiate_obj_vector

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj_ab
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
  ALLOCATE( Obj % Val( a:b ) )
  Obj % Val = 0
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE initiate_obj_ab

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL AllocateData( Obj, tSize )
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                                     Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_Int
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ConstructorInt
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt

!----------------------------------------------------------------------------
!                                                                      Vector
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_Real
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Real

!----------------------------------------------------------------------------
!                                                              Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE ConstructorReal
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorReal

END SUBMODULE Constructor