MODULE AbstractMatrix_Method
USE GlobalData
USE BaseType, ONLY: AbstractMatrix_
IMPLICIT NONE

PRIVATE

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

PUBLIC :: Size

CONTAINS

  PURE FUNCTION get_shape( Obj ) RESULT( Ans )
    TYPE( AbstractMatrix_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ) :: Ans( 2 )
    Ans = 0_I4B
  END FUNCTION get_shape

!<--------------------------------------------------------------------->|
!
!<--------------------------------------------------------------------->|

  PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
    TYPE( AbstractMatrix_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
    INTEGER( I4B ) :: Ans
    Ans = 0
  END FUNCTION get_size

END MODULE AbstractMatrix_Method