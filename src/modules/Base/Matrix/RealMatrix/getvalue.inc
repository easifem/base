
!----------------------------------------------------------------------------
!                                                      ArrayValues@getValues
!----------------------------------------------------------------------------

INTERFACE
!! Returns the values of [[RealMatrix_]] obj in 2D array

!> authors: Dr. Vikas Sharma
!
! This function returns the value stored in `Obj % Val` in a 2D fortran array
!
!### Usage
!
! ```fortran
!	Val = ArrayValues( Obj, 1.0_dfp )
! ```

MODULE PURE FUNCTION f_getValues_Real( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValues_Real
END INTERFACE

INTERFACE
!! Returns the values of [[RealMatrix_]] obj in 2D array

!> authors: Dr. Vikas Sharma
!
! This function returns a section of `Obj % Val` in a 2D fortran array. This
! is equivalent to `Val = Obj % Val(RIndx, CIndx)`
!
!### Usage
!
! ```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: Obj
! call initiate( Obj, [4,4] )
! call random_number( obj ); r=[1,2]; c=[2,3]
! Val = ArrayValues( Obj, R, C, 1.0_dfp )
! ```
!
! The above call will return `Obj%Val[1:2, 2:3]`

MODULE PURE FUNCTION f_getSectionValues_Real( Obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getSectionValues_Real
END INTERFACE

INTERFACE
!! Returns the values of [[RealMatrix_]] obj in 2D array

!> authors: Dr. Vikas Sharma
!
! This function returns a section of `Obj % Val` in a 2D fortran array. This
! is equivalent to `Val = Obj % Val(is:ie:s, is:ie:s)`
!
!### Usage
!
! ```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: Obj
! call initiate( Obj, [4,4] )
! call random_number( obj )
! Val = ArrayValues( Obj, 1, 2, 1, 1.0_dfp )
! ```
!
! The above call will return `Obj%Val[1:2:1, 1:2:1]`

MODULE PURE FUNCTION f_getValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValuesFromTriplet_Real
END INTERFACE

INTERFACE
!! Returns [[RealMatrix_]] obj from [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function is essentially copy method `Ans=Obj`

MODULE PURE FUNCTION f_getValues_self( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValues_self
END INTERFACE

INTERFACE
!! Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function is essentially copy method `Ans=Obj(RIndx, CIndx)`

MODULE PURE FUNCTION f_getSectionValues_Self( Obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getSectionValues_Self
END INTERFACE

INTERFACE
!! Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function is essentially copy method `Ans=Obj(is:ie, is:ie)`

MODULE PURE FUNCTION f_getValuesFromTriplet_self( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValuesFromTriplet_self
END INTERFACE

INTERFACE
!! Returns values in 2D fortran array from [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function combines all [[realmatrix_]] value of `Obj` and
! returns a 2D fortrn array

MODULE PURE FUNCTION f_getValues_1( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( :, : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION f_getValues_1

!! Returns [[realmatrix_]] object from a 2D array of [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function combines all [[realmatrix_]] value of `Obj` and
! returns a [[realmatrix_]] object

MODULE PURE FUNCTION f_getValues_2( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: Obj( :, : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION f_getValues_2
END INTERFACE

!> Generic function to get values form [[realmatrix_]]
INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_Real, f_getSectionValues_Real, &
    & f_getValuesFromTriplet_Real, f_getValues_self, f_getSectionValues_Self,&
    & f_getValuesFromTriplet_self, f_getValues_1, f_getValues_2
END INTERFACE ArrayValues

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                             Copy@getValues
!----------------------------------------------------------------------------

INTERFACE
!! Copy from [[realmatrix_]] to 2D fortran array

!> authors: Dr. Vikas Sharma
!
! This subroutine copy the contents of [[realmatrix_]] object into a 2D
! fortran array

MODULE PURE SUBROUTINE Copy_Obj_to_Val( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
END SUBROUTINE Copy_Obj_to_Val
END INTERFACE

INTERFACE
!! Copy from [[realmatrix_]] to another [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine copy the contents of [[realmatrix_]] object to another
! [[realmatrix_]] object

MODULE PURE SUBROUTINE Copy_Obj_to_Obj( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  TYPE( RealMatrix_ ), INTENT( INOUT) :: To
END SUBROUTINE Copy_Obj_to_Obj
END INTERFACE

INTERFACE
!! Copy from 2D fortran array to [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine copy the contents of a 2D
! fortran array to [[realmatrix_]] object

MODULE PURE SUBROUTINE Copy_Val_to_Obj( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, : )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
END SUBROUTINE Copy_Val_to_Obj

END INTERFACE

!> Generic method to copy data to and from [[realmatrix_]]
INTERFACE COPY
  MODULE PROCEDURE Copy_Obj_to_Val, Copy_Obj_to_Obj, Copy_Val_to_Obj
END INTERFACE COPY

PUBLIC :: COPY

!> Generic method to convert data to and from [[realmatrix_]]
INTERFACE CONVERT
  MODULE PROCEDURE Copy_Obj_to_Val, Copy_Obj_to_Obj, Copy_Val_to_Obj
END INTERFACE CONVERT

PUBLIC :: CONVERT

!----------------------------------------------------------------------------
!                                                     ArrayPointer@getValues
!----------------------------------------------------------------------------

INTERFACE
!! Get pointer to the values stored inside [[realmatrix_]]

!> authors: Dr. Vikas Sharma
!
! This function returns the pointer to the values stored inside the
! [[realmatrix_]]

MODULE FUNCTION f_getPointer_Real( Obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ), TARGET :: Obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), POINTER :: Ans( :, : )
END FUNCTION f_getPointer_Real
END INTERFACE

!> Generic function to get pointer to the values in [[realmatrix_]]
INTERFACE ArrayPointer
  MODULE PROCEDURE f_getPointer_Real
END INTERFACE ArrayPointer

PUBLIC :: ArrayPointer
