MODULE Random_Method
  !! Module to handle random numbers in easifem

USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE initRandom( Obj )
  CLASS( Random_ ), INTENT( INOUT) :: Obj
END SUBROUTINE initRandom
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initRandom
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                 getRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getRandom( Obj, distribution ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: distribution
  REAL( DFP ) :: Ans
END FUNCTION getRandom
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandom
END INTERFACE RandomValue

PUBLIC :: RandomValue

!----------------------------------------------------------------------------
!                                                                 SaveRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE SaveRandom( Obj )
  CLASS( Random_ ), INTENT( INOUT) :: Obj
END SUBROUTINE SaveRandom
END INTERFACE

PUBLIC :: SaveRandom

!----------------------------------------------------------------------------
!                                                             UniformRandom
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION uniformRandom( Obj, From, To ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: From, To
  REAL( DFP ) :: Ans
END FUNCTION uniformRandom
END INTERFACE

PUBLIC :: uniformRandom

INTERFACE RandomValue
  MODULE PROCEDURE uniformRandom
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                             RandomInteger
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getRandomInteger( Obj, From, To ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) ::  Obj
  INTEGER( I4B ), INTENT( IN ) ::  From, To
  INTEGER( I4B ) :: Ans
END FUNCTION getRandomInteger
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE getRandomInteger
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!                                                                RandomValue
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION select_random_int_from_vec( Obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ) :: Ans
END FUNCTION select_random_int_from_vec
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_int_from_array( Obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Val( :, : )
  INTEGER( I4B ) :: Ans
END FUNCTION select_random_int_from_array
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_real_from_vec( Obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION select_random_real_from_vec
END INTERFACE

INTERFACE
MODULE FUNCTION select_random_real_from_array( Obj, Val ) RESULT( Ans )
  CLASS( Random_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( :, : )
  REAL( DFP ) :: Ans
END FUNCTION select_random_real_from_array
END INTERFACE

INTERFACE RandomValue
  MODULE PROCEDURE select_random_int_from_vec, select_random_int_from_array,&
    & select_random_real_from_vec, select_random_real_from_array
END INTERFACE RandomValue

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


END MODULE Random_Method