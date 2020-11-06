MODULE RealVector_Method
USE GlobalData
USE IO
USE BaseType, ONLY : RealVector_, File_

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION get_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                            SIZE@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE get_size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                    AllocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Allocate_Data( Obj, Dims )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims
END SUBROUTINE Allocate_Data
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE Allocate_Data
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                  DeAllocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine allocate the memory for [[RealVector_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine allocates the memeory for [[RealVector_]]
!
! ```fortran
! CALL Initiate(Obj, 5)
!```

MODULE PURE SUBROUTINE initiate_obj( Obj, tSize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END SUBROUTINE initiate_obj

END INTERFACE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine allocate the memory for a vector of type [[RealVector_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine allocate the memory for a vector of type [[RealVector_]]
! The size of `Obj` would be same as the size of `tSize`

MODULE PURE SUBROUTINE initiate_obj_vector( Obj, tSize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: tSize( : )
END SUBROUTINE initiate_obj_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine allocate the memory for an instance of [[RealVector_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine allocate the memory for an instance of [[RealVector_]].
! User can specify the lowerbounds and upper bounds

MODULE PURE SUBROUTINE initiate_obj_ab( Obj, a, b )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: a, b
END SUBROUTINE initiate_obj_ab
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj, initiate_obj_vector, initiate_obj_ab
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE RANDOM_NUMBER_Obj( Obj, tsize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tsize
END SUBROUTINE RANDOM_NUMBER_Obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Random_number_obj_vec( Obj, tsize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT) :: Obj(:)
  INTEGER( I4B ), INTENT( IN ) :: tsize( : )
END SUBROUTINE Random_number_obj_vec
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE RANDOM_NUMBER_Obj, Random_number_obj_vec
END INTERFACE RANDOM_NUMBER

PUBLIC :: RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                          Vector@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor1( tSize ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor1
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE Constructor1
END INTERFACE RealVector

PUBLIC :: RealVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( tSize ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor_1
END INTERFACE


INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE RealVector_Pointer

PUBLIC :: RealVector_Pointer

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_Int8( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER(Int8), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Int8

MODULE PURE FUNCTION ConstructorInt8( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER(Int8), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorInt8
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorInt8
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Int8
END INTERFACE RealVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Int16( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER(Int16), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Int16

MODULE PURE FUNCTION ConstructorInt16( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER(Int16), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorInt16
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorInt16
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Int16
END INTERFACE RealVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Int32( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER(Int32), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Int32

MODULE PURE FUNCTION ConstructorInt32( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER(Int32), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorInt32
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorInt32
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Int32
END INTERFACE RealVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Int64( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER(Int64), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Int64

MODULE PURE FUNCTION ConstructorInt64( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER(Int64), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorInt64
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorInt64
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Int64
END INTERFACE RealVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Real32( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  REAL(Real32), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Real32

MODULE PURE FUNCTION ConstructorReal32( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  REAL(Real32), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorReal32
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorReal32
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Real32
END INTERFACE RealVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Real64( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  REAL(Real64), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Real64

MODULE PURE FUNCTION ConstructorReal64( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  REAL(Real64), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorReal64
END INTERFACE

INTERFACE RealVector
  MODULE PROCEDURE ConstructorReal64
END INTERFACE RealVector

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_Real64
END INTERFACE RealVector_Pointer

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getValues_self( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getValues_self

MODULE PURE FUNCTION f_getSectionValues_self( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getSectionValues_self

MODULE PURE FUNCTION f_getValuesFromTriplet_self( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getValuesFromTriplet_self

MODULE PURE FUNCTION f_getARRAYValues_self( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYValues_self

MODULE PURE FUNCTION f_getARRAYSectionValues_self( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYSectionValues_self

MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_self( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYValuesFromTriplet_self
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_self, &
    & f_getSectionValues_self, &
    & f_getValuesFromTriplet_self, &
    & f_getARRAYValues_self, &
    & f_getARRAYSectionValues_self, &
    & f_getARRAYValuesFromTriplet_self
END INTERFACE

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValues_Int

MODULE PURE FUNCTION f_getSectionValues_Int( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getSectionValues_Int

MODULE PURE FUNCTION f_getValuesFromTriplet_Int( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValuesFromTriplet_Int

MODULE PURE FUNCTION f_getARRAYValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValues_Int

MODULE PURE FUNCTION f_getARRAYSectionValues_Int( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYSectionValues_Int

MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_Int( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValuesFromTriplet_Int
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_Int, &
    & f_getSectionValues_Int, &
    & f_getValuesFromTriplet_Int, &
    & f_getARRAYValues_Int, &
    & f_getARRAYValuesFromTriplet_Int
END INTERFACE ArrayValues

INTERFACE
MODULE PURE FUNCTION f_getValues_Real( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL(DFP), INTENT( IN ) :: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getValues_Real

MODULE PURE FUNCTION f_getSectionValues_Real( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getSectionValues_Real

MODULE PURE FUNCTION f_getValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getValuesFromTriplet_Real

MODULE PURE FUNCTION f_getARRAYValues_Real( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL(DFP), INTENT( IN ) :: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValues_Real

MODULE PURE FUNCTION f_getARRAYSectionValues_Real( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL(DFP), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYSectionValues_Real

MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValuesFromTriplet_Real
END INTERFACE

INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_Real, &
    & f_getSectionValues_Real, &
    & f_getValuesFromTriplet_Real, &
    & f_getARRAYValues_Real, &
    & f_getARRAYSectionValues_Real, &
    & f_getARRAYValuesFromTriplet_Real
END INTERFACE ArrayValues

!----------------------------------------------------------------------------
!                                                    ArrayPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION f_getPointer_self( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ), POINTER :: Val
END FUNCTION f_getPointer_self

MODULE FUNCTION f_getPointer_Real64( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: Obj
  REAL(Real64), INTENT( IN ) :: DataType
  REAL(Real64), POINTER :: Val( : )
END FUNCTION f_getPointer_Real64
END INTERFACE

INTERFACE ArrayPointer
  MODULE PROCEDURE f_getPointer_self, f_getPointer_Real64
END INTERFACE

PUBLIC :: ArrayPointer

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION IndexOf_1( Obj, Value ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
  INTEGER( I4B ) :: Ans
END FUNCTION IndexOf_1

MODULE PURE FUNCTION IndexOf_2( Obj, Value ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION IndexOf_2
END INTERFACE

INTERFACE IndexOF
  MODULE PROCEDURE IndexOf_1, IndexOf_2
END INTERFACE

PUBLIC :: IndexOf

INTERFACE LOC
  MODULE PROCEDURE IndexOf_1, IndexOf_2
END INTERFACE LOC

PUBLIC :: LOC

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isPresent_1( Obj, Value ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
  LOGICAL( LGT ) :: Ans
END FUNCTION isPresent_1

MODULE PURE FUNCTION isPresent_2( Obj, Value ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION isPresent_2
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE isPresent_1, isPresent_2
END INTERFACE isPresent

PUBLIC :: isPresent

INTERFACE OPERATOR( .EQ. )
  MODULE PROCEDURE isPresent_1, isPresent_2
END INTERFACE

PUBLIC :: OPERATOR( .EQ. )

!----------------------------------------------------------------------------
!                                                          Append@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Append_1( Obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE Append_1

MODULE PURE SUBROUTINE Append_2( Obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE Append_2

MODULE PURE SUBROUTINE Append_3( Obj, AnotherObj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  CLASS( RealVector_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE Append_3
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE Append_1, Append_2, Append_3
END INTERFACE Append

PUBLIC :: Append

!<----------------------------------------------------------------------------
!                                                                   NRM2@BLAS1
!<----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION NRM2scalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2scalar

MODULE PURE FUNCTION NRM2vector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2vector
END INTERFACE

INTERFACE NORM2
  MODULE PROCEDURE NRM2scalar, NRM2vector
END INTERFACE NORM2

PUBLIC :: NORM2

! <---------------------------------------------------------------------------
!                                                               NORM2SQR@BLAS1
! <---------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION NRM2SQRscalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRscalar

MODULE PURE FUNCTION NRM2SQRvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRvector

MODULE PURE FUNCTION NRM2SQRintrinsic( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRintrinsic
END INTERFACE

INTERFACE NORM2SQR
  MODULE PROCEDURE NRM2SQRscalar, NRM2SQRvector, NRM2SQRintrinsic
END INTERFACE NORM2SQR

PUBLIC :: NORM2SQR

!<----------------------------------------------------------------------------
!                                                                   DOT@BLAS1
!<----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intrinsicDOTintrinsic( Val1, Val2 ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val1( : ), Val2( : )
  REAL( DFP ) :: Ans
END FUNCTION intrinsicDOTintrinsic

MODULE PURE FUNCTION scalarDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTscalar

MODULE PURE FUNCTION vectorDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTvector

MODULE PURE FUNCTION vectorDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTscalar

MODULE PURE FUNCTION scalarDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTvector

MODULE PURE FUNCTION intrinsicDOTscalar( Val, Obj ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION intrinsicDOTscalar

MODULE PURE FUNCTION scalarDOTintrinsic( Obj, Val ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTintrinsic
END INTERFACE

INTERFACE DOT
  MODULE PROCEDURE scalarDOTscalar, vectorDOTvector, vectorDOTscalar, &
    & scalarDOTvector, intrinsicDOTintrinsic, &
    & scalarDOTintrinsic
END INTERFACE DOT

PUBLIC :: DOT

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ASUMIntrinsic( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION ASUMIntrinsic

MODULE PURE FUNCTION ASUMScalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION ASUMScalar

MODULE PURE FUNCTION ASUMvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION ASUMvector
END INTERFACE

INTERFACE ASUM
  MODULE PROCEDURE ASUMScalar, ASUMvector, ASUMIntrinsic
END INTERFACE ASUM

PUBLIC :: ASUM

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicCOPYintrinsic( Val1, Val2 )
  REAL( DFP ), INTENT( IN ) :: Val2( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val1( : )
END SUBROUTINE intrinsicCOPYintrinsic

MODULE PURE SUBROUTINE scalarCOPYscalar( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2
END SUBROUTINE scalarCOPYscalar

MODULE PURE SUBROUTINE vectorCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Obj1( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE vectorCOPYvector

MODULE PURE SUBROUTINE scalarCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE scalarCOPYvector

MODULE PURE SUBROUTINE scalarCOPYintrinsic( Obj, Val )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE scalarCOPYintrinsic

MODULE PURE SUBROUTINE intrinsicCOPYscalar( Val, Obj )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
END SUBROUTINE intrinsicCOPYscalar
END INTERFACE

INTERFACE COPY
  MODULE PROCEDURE intrinsicCOPYintrinsic, scalarCOPYscalar, &
    & vectorCOPYvector, scalarCOPYvector, scalarCOPYintrinsic
END INTERFACE COPY

PUBLIC :: COPY

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYintrinsic( Val1, Val2 )
  REAL( DFP ), INTENT( IN ) :: Val2( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val1( : )
END SUBROUTINE intrinsicSHALLOWCOPYintrinsic

MODULE PURE SUBROUTINE scalarSHALLOWCOPYscalar( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2
END SUBROUTINE scalarSHALLOWCOPYscalar

MODULE PURE SUBROUTINE vectorSHALLOWCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Obj1( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE vectorSHALLOWCOPYvector

MODULE PURE SUBROUTINE scalarSHALLOWCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE scalarSHALLOWCOPYvector

MODULE PURE SUBROUTINE scalarSHALLOWCOPYintrinsic( Obj, Val )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE scalarSHALLOWCOPYintrinsic

MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYscalar( Val, Obj )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
END SUBROUTINE intrinsicSHALLOWCOPYscalar
END INTERFACE

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE intrinsicSHALLOWCOPYintrinsic, scalarSHALLOWCOPYscalar, &
    & vectorSHALLOWCOPYvector, scalarSHALLOWCOPYvector, &
    & scalarSHALLOWCOPYintrinsic
END INTERFACE SHALLOWCOPY

PUBLIC :: SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                                 SWAP@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSWAPintrinsic( Val1, Val2 )
  REAL( DFP ), INTENT( INOUT ) :: Val1( : ), Val2( : )
END SUBROUTINE intrinsicSWAPintrinsic

MODULE PURE SUBROUTINE scalarSWAPscalar( Obj1, Obj2 )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj1, Obj2
END SUBROUTINE scalarSWAPscalar

MODULE PURE SUBROUTINE vectorSWAPvector( Obj1, Obj2 )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj1( : ), Obj2( : )
END SUBROUTINE vectorSWAPvector

MODULE PURE SUBROUTINE scalarSWAPintrinsic( Obj, Val )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: Val( : )
END SUBROUTINE scalarSWAPintrinsic

MODULE PURE SUBROUTINE intrinsicSWAPscalar( Val, Obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: Val( : )
END SUBROUTINE intrinsicSWAPscalar
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE scalarSWAPscalar, &
    & vectorSWAPvector, scalarSWAPintrinsic
END INTERFACE SWAP

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!                                                                SCALE@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE SCALintrinsic( alpha, Val )
  REAL( DFP ), INTENT( INOUT ) :: Val( : )
  REAL( DFP ), INTENT( IN ) :: alpha
END SUBROUTINE SCALintrinsic

MODULE PURE SUBROUTINE SCALscalar( alpha, Obj )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: alpha
END SUBROUTINE SCALscalar

MODULE PURE SUBROUTINE SCALvector( alpha, Obj )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: Obj( : )
  REAL( DFP ), INTENT( IN ) :: alpha
END SUBROUTINE SCALvector
END INTERFACE

INTERFACE SCALE
  MODULE PROCEDURE SCALintrinsic, SCALscalar, SCALvector
END INTERFACE SCALE

PUBLIC :: SCALE

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarAXPYscalar( Obj1, alpha, Obj2 )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: alpha
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2
END SUBROUTINE scalarAXPYscalar

MODULE PURE SUBROUTINE vectorAXPYvector( Obj1, alpha, Obj2 )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj1( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE vectorAXPYvector

MODULE PURE SUBROUTINE scalarAXPYintrinsic( Obj, alpha, Val )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE scalarAXPYintrinsic

MODULE PURE SUBROUTINE intrinsicAXPYintrinsic( Val1, alpha, Val2 )
  REAL( DFP ), INTENT( INOUT ) :: Val1( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: Val2( : )
END SUBROUTINE intrinsicAXPYintrinsic
END INTERFACE

INTERFACE AXPY
  MODULE PROCEDURE scalarAXPYscalar, vectorAXPYvector, &
    & scalarAXPYintrinsic, intrinsicAXPYintrinsic
END INTERFACE AXPY

PUBLIC :: AXPY

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setValue_1( Obj, Indx, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE setValue_1
END INTERFACE

INTERFACE SetValue
  MODULE PROCEDURE setValue_1
END INTERFACE

PUBLIC :: SetValue

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE RealVectorDisplay ( Obj, msg, UnitNo, path, filename, &
  & extension )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
END SUBROUTINE RealVectorDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE RealscalarDisplay ( Obj, msg, UnitNo, path, filename, &
  & extension )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
END SUBROUTINE RealscalarDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Display_Vector_Real( vec, msg, unitNo, path, filename, &
  & extension )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: vec( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
END SUBROUTINE Display_Vector_Real
END INTERFACE



INTERFACE Display
  MODULE PROCEDURE RealVectorDisplay, RealscalarDisplay, Display_Vector_Real
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_a( a, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ReadLine@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_ab( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_ab
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ReadLine@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abc( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abc
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ReadLine@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcd( a, b, c, d, buffer, fileName, unitNo )
  real(DFP), intent(out) :: a, b, c, d
    !! Number
  character(len=*), intent(in) :: fileName
    !! File name
  integer(I4B), intent(in) :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcd
END INTERFACE
INTERFACE

!----------------------------------------------------------------------------
!                                                                 ReadLine@IO
!----------------------------------------------------------------------------

MODULE SUBROUTINE readline_abcde( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d, e
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  character(len=*), intent(in), optional :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcde
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_a, readline_ab, readline_abc, readline_abcd, &
    & readline_abcde
END INTERFACE ReadLine

PUBLIC :: ReadLine

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_av( a, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_av
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_avbv( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : ), b( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_avbv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_avbvcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a( : ), b( : ), c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_avbvcv
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_av, readline_avbv, readline_avbvcv
END INTERFACE ReadLine

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abv( a, b, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abvcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b( : ), c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abvcv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcv( a, b, c, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcvdv( a, b, c, d, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c( : ), d( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcvdv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdv( a, b, c, d, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdv
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdvev( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d( : ), e( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdvev
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Readline@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE readline_abcdev( a, b, c, d, e, buffer, fileName, unitNo )
  REAL(DFP), INTENT(OUT) :: a, b, c, d, e( : )
    !! Number
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fileName
    !! File name
  INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
    !! File id number to read from
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: buffer
    !! Character string to read from instead of the line in the file
END SUBROUTINE readline_abcdev
END INTERFACE

INTERFACE ReadLine
  MODULE PROCEDURE readline_abv, readline_abvcv, readline_abcv, &
    & readline_abcvdv, readline_abcdv, readline_abcdvev, readline_abcdev
END INTERFACE ReadLine

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

#include "./contains.part"
END MODULE RealVector_Method