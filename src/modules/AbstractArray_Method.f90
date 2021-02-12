MODULE AbstractArray_Method
  USE GlobalData
  USE BaseType, ONLY:AbstractArray_

  IMPLICIT NONE

  PRIVATE

  INTERFACE Size
    MODULE PROCEDURE get_size
  END INTERFACE Size

  PUBLIC :: Size

  INTERFACE Shape
    MODULE PROCEDURE get_shape
  END INTERFACE Shape

  PUBLIC :: Shape

  INTERFACE TotalDimension
    MODULE PROCEDURE get_tdimension
  END INTERFACE TotalDimension

  PUBLIC :: TotalDimension

  INTERFACE setTotalDimension
    MODULE PROCEDURE set_tdimension
  END INTERFACE setTotalDimension

  PUBLIC :: setTotalDimension

  CONTAINS

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
    TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
    INTEGER( I4B ) :: Ans
    Ans = 0
  END FUNCTION get_size

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_shape( Obj ) RESULT( Ans )
    TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), ALLOCATABLE :: Ans( : )
    Ans = [0]
  END FUNCTION get_shape

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_tdimension( Obj ) RESULT( Ans )
    CLASS( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ) :: Ans
    Ans = Obj % tDimension
  END FUNCTION get_tdimension

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE SUBROUTINE set_tdimension( Obj, tDimension )
    CLASS( AbstractArray_ ), INTENT( INOUT ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: tDimension
    Obj % tDimension = tDimension
  END SUBROUTINE set_tdimension

END MODULE AbstractArray_Method