! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This module contains methods of [[RealVector_]] data type.
!
!###Introduction
!
! This module contains methods of [[RealVector_]] data type. This module only contains the definition of the interfaces of these methods. The actual implementation is given inside the submodules. This modules has following submodules:
!
!@todo Documentation, testing, usage

MODULE RealVector_Method
USE GlobalData, ONLY: DFP, I4B, Int8, Int16, Int32, Int64, Real32, Real64, LGT
USE BaseType, ONLY : RealVector_

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
!                                                           SIZE@Constructor
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
!                                                   AllocateData@Constructor
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
!                                                 DeAllocateData@Constructor
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
!                                                       Initiate@Constructor
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
!                                                       Initiate@Constructor
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
!                                                       Initiate@Constructor
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
!                                                         Vector@Constructor
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

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine performs dot_product of two fortran vectors
!
!@note
! 	This subroutine calls BLAS function `DDOT` `SDOT`.
!@endnote
!@todo
! 	usage, documentation
!@endtodo

INTERFACE
MODULE PURE FUNCTION intrinsicDOTintrinsic( Val1, Val2 ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val1( : ), Val2( : )
  REAL( DFP ) :: Ans
END FUNCTION intrinsicDOTintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine returns dot product of two [[RealVector_]]
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION scalarDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routines returns the dot product of vector of [[RealVector_]] data type.
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION vectorDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a vector of [[RealVector_]] and scalar of [[RealVector_]]
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION vectorDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a scalar of [[RealVector_]] and vector of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION scalarDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a fortran vector and a scalar instance of [[RealVector_]]
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION intrinsicDOTscalar( Val, Obj ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION intrinsicDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a fortran array and scalar instance of [[RealVector_]]
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION scalarDOTintrinsic( Obj, Val ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This generic routine computes dot product

INTERFACE DOT
  MODULE PROCEDURE scalarDOTscalar, vectorDOTvector, vectorDOTscalar, &
    & scalarDOTvector, intrinsicDOTintrinsic, &
    & scalarDOTintrinsic
END INTERFACE DOT

PUBLIC :: DOT

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes Euclidean norm of [[RealVector_]]
!
!### Introduction
!
! L2 norm of a vector is give by
!
! $$\left| \left| \bf{V} \right|  \right|  =\sqrt{\bf{V} \cdot \bf{V} }$$
!
!@note
! 	This subroutine uses DOT function.
!@endnote
!
!### Usage
!
!```fortran
!	s = NORM2(Obj)
!```

INTERFACE
MODULE PURE FUNCTION NRM2scalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes the L2 norm of [[RealVector_]]
!
!### Introduction
!
! This routine computes L2 norm of a vector of [[RealVector_]].
!
!@note
! 	This function employs DOT function.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2vector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes norm2
!
!@note
! 	This function internally calls DOT function
!@endnote

INTERFACE NORM2
  MODULE PROCEDURE NRM2scalar, NRM2vector
END INTERFACE NORM2

PUBLIC :: NORM2

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of [[RealVector_]]
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2SQRscalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of [[RealVector_]]
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2SQRvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of fortran vector
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! 	usage
!@endtodo
INTERFACE
MODULE PURE FUNCTION NRM2SQRintrinsic( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This is a generic function which computers square of L2 norm of a vector.
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! 	usage
!@endtodoe

INTERFACE NORM2SQR
  MODULE PROCEDURE NRM2SQRscalar, NRM2SQRvector, NRM2SQRintrinsic
END INTERFACE NORM2SQR

PUBLIC :: NORM2SQR

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector.
!
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION ASUMIntrinsic( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION ASUMIntrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION ASUMScalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION ASUMScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION ASUMvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION ASUMvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE ASUM
  MODULE PROCEDURE ASUMScalar, ASUMvector, ASUMIntrinsic
END INTERFACE ASUM

PUBLIC :: ASUM

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy one vector into another, i.e. `Val1=Val2
!
!@note
! 	This subroutine calls BLAS function (D,S)COPY
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE intrinsicCOPYintrinsic( Val1, Val2 )
  REAL( DFP ), INTENT( IN ) :: Val2( : )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Val1( : )
END SUBROUTINE intrinsicCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy one vector into another, i.e. `Val1=Val2
!
!@note
! 	This subroutine calls BLAS function (D,S)COPY
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYscalar( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2
END SUBROUTINE scalarCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy one vector into another, i.e. `Val1=Val2
!
!@note
! 	This subroutine internally calls COPY routine
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE vectorCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Obj1( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE vectorCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy one fortran vector into another, i.e. `Val1=Val2`
!
!@note
! 	This subroutine calls BLAS function (D,S)COPY
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYvector( Obj1, Obj2 )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Obj1
  CLASS( RealVector_ ), INTENT( IN ) :: Obj2( : )
END SUBROUTINE scalarCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy fortran vector into [[RealVector_]], i.e. `Obj=Val`
!
!@note
! 	This subroutine internally calls COPY
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYintrinsic( Obj, Val )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE scalarCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy one [[RealVector_]] into fortran vector, i.e. `Val=Obj`
!
!@note
! 	This subroutine internally calls COPY subroutine. Also `Val` is allocatable.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
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
MODULE SUBROUTINE Display_Vector_Real( vec, msg, path, filename, &
  & extension )
  ! Define intent of dummy variables
  REAL( DFP ), INTENT( IN ) :: vec( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: extension
END SUBROUTINE Display_Vector_Real
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE RealVectorDisplay, RealscalarDisplay, Display_Vector_Real
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

END MODULE RealVector_Method