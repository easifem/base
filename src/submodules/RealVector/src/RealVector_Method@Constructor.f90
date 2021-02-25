SUBMODULE ( RealVector_Method ) Constructor
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
  IF( ALLOCATED( Obj%Val ) ) THEN
    Ans = SIZE( Obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  CALL Reallocate( Obj%Val, Dims )
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Val ) ) DEALLOCATE( Obj % Val )
END PROCEDURE deallocate_data

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
  Obj % Val = 0.0_DFP
  CALL setTotalDimension( Obj, 1_I4B )
END PROCEDURE initiate_obj_ab

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE Random_Number_obj
  CALL Initiate( Obj=Obj, tSize=tSize )
  CALL RANDOM_NUMBER( Obj%Val )
END PROCEDURE Random_Number_obj

!----------------------------------------------------------------------------
!                                                             Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE Random_Number_obj_vec
  INTEGER( I4B ) :: ii, n

  n = SIZE( tSize )
  IF( ALLOCATED( Obj ) ) THEN
    IF( SIZE( Obj ) .NE. n ) THEN
      DEALLOCATE( Obj )
      ALLOCATE( Obj( n ) )
    END IF
  ELSE
    ALLOCATE( Obj( n ) )
  END IF

  DO ii = 1, n
    CALL Initiate( Obj=Obj(ii), tSize=tSize(ii) )
    CALL RANDOM_NUMBER( Obj(ii)%Val )
  END DO

END PROCEDURE Random_Number_obj_vec

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
!                                                    Vector and Vector_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_Int8
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int8

MODULE PROCEDURE ConstructorInt8
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt8

MODULE PROCEDURE Constructor_Int16
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int16

MODULE PROCEDURE ConstructorInt16
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt16

MODULE PROCEDURE Constructor_Int32
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int32

MODULE PROCEDURE ConstructorInt32
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt32

MODULE PROCEDURE Constructor_Int64
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Int64

MODULE PROCEDURE ConstructorInt64
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorInt64

MODULE PROCEDURE Constructor_Real32
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Real32

MODULE PROCEDURE ConstructorReal32
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorReal32

MODULE PROCEDURE Constructor_Real64
  ALLOCATE( Obj )
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE Constructor_Real64

MODULE PROCEDURE ConstructorReal64
  Obj % Val = Val
  CALL setTotalDimension( Obj , 1_I4B )
END PROCEDURE ConstructorReal64

END SUBMODULE Constructor