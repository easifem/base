MODULE RealMatrix_Method
  !! This module contains methods for [[RealMatrix_]]
USE GlobalData
USE BaseType

IMPLICIT NONE

PRIVATE

#include "./constructor.inc"
#include "./lapack.inc"
#include "./getvalue.inc"
#include "./setvalue.inc"

!----------------------------------------------------------------------------
!                                                MatMul@MatrixMultiplication
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION MatMul_1( Obj1, Obj2 ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj1, Obj2
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION MatMul_1
END INTERFACE

INTERFACE
MODULE PURE FUNCTION MatMul_2( Obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  REAL( DFP ), ALLOCATABLE :: Ans( : )
END FUNCTION MatMul_2
END INTERFACE

INTERFACE
MODULE PURE FUNCTION MatMul_3( Obj, Vec ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: Vec
  TYPE( RealVector_ ) :: Ans
END FUNCTION MatMul_3
END INTERFACE

INTERFACE Matmul
  MODULE PROCEDURE MatMul_1, MatMul_2, MatMul_3
END INTERFACE Matmul

PUBLIC :: Matmul

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

#include "./contains.part"
END MODULE RealMatrix_Method