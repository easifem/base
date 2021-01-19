SUBMODULE( RealMatrix_Method ) MatrixMultiplication
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE MatMul_1
  Ans % Val = MATMUL( Obj1 % Val, Obj2 % Val )
  CALL SetTotalDimension( Ans, 2_I4B )
END PROCEDURE MatMul_1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE MatMul_2
  Ans = MATMUL( Obj % Val, Vec )
END PROCEDURE MatMul_2

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE MatMul_3
  Ans = RealVector( MATMUL( Obj % Val, Vec % Val ) )
END PROCEDURE MatMul_3

END SUBMODULE MatrixMultiplication