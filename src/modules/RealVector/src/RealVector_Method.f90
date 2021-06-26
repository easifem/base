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
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This function returns the shape of [[RealVector_]]

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

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This function returns the size of [[RealVector_]]

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
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!### Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE
MODULE PURE FUNCTION RealVec_getTotalDimension( obj ) RESULT( Ans )
  TYPE( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION RealVec_getTotalDimension
END INTERFACE

INTERFACE getTotalDimension
  MODULE PROCEDURE RealVec_getTotalDimension
END INTERFACE getTotalDimension

PUBLIC :: getTotalDimension

!----------------------------------------------------------------------------
!                                             setTotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array
!
!### Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
MODULE PURE SUBROUTINE RealVec_setTotalDimension( Obj, tDimension )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE RealVec_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE RealVec_setTotalDimension
END INTERFACE setTotalDimension

PUBLIC :: setTotalDimension

!----------------------------------------------------------------------------
!                                                   AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 Feb 2021
! summary: 	This subroutine allocates memory for [[RealVector_]]
!
!
!### Usage
!
!```fortran
! type( _obj_ ) :: obj
! call display("test1")
! call equalline()
! call allocateData(obj, 10)
! call display( obj, "test1=")
! call dashline()
!```

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

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine deallocates the data in [[RealVector_]]

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

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine allocates the memory for [[RealVector_]]
!
!### Introduction This subroutine allocates the memeory for [[RealVector_]]
!
!@note
! 	This subroutine is an alias for [[Allocate_Data]]
!@endnote
!
! ```fortran
! CALL Initiate(Obj, 5)
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( Obj, tSize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END SUBROUTINE initiate_obj

END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for a vector of type [[RealVector_]]
!
!### Introduction
! This subroutine allocate the memory for a vector of type [[RealVector_]]
!@note
! The size of `Obj` would be same as the size of `tSize`
!@endnote
!
!
!### Usage
!
!```fortran
! type(_obj_), allocatable :: obj( : )
! call display("test2")
! call equalline()
! call initiate(obj, [10,10,5,5])
! call display( obj, "initiate Obj(:)=")
! call dashline()
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_vector( Obj, tSize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: tSize( : )
END SUBROUTINE initiate_obj_vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for an instance of [[RealVector_]]
!
!### Introduction
! This subroutine allocate the memory for an instance of [[RealVector_]].
! User can specify the lowerbounds and upper bounds.
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! call display("test3")
! call equalline()
! call initiate(obj, 2, 10)
! obj%val(2) = 1.0_DFP
! call display( obj, "initiate Obj(a:b)=")
! call dashline()
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_ab( Obj, a, b )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: a, b
END SUBROUTINE initiate_obj_ab
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	Generic subroutine to initiate [[RealVector_]]

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj, initiate_obj_vector, initiate_obj_ab
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!### Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of [[RealVector_]]
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! call display("test4:: initiate_obj_ab")
! call equalline()
! call random_number(obj=obj, tsize=5)
! call display( obj, "obj =")
! call dashline()
!```

INTERFACE
MODULE SUBROUTINE random_number_obj( Obj, tsize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tsize
END SUBROUTINE random_number_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!### Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of [[RealVector_]]
!
!@note
! 	Here argument `Obj` is a vector of [[RealVector_]] data-types.
!@endnote
!
!### Usage
!
!```fortran
! type(_obj_), allocatable :: obj( : )
! call display("test4:: random_number_obj_vec")
! call equalline()
! call random_number(obj=obj, tsize=[4,5,6])
! call display( obj, "obj =")
! call dashline()
!```

INTERFACE
MODULE SUBROUTINE random_number_obj_vec( Obj, tsize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT) :: Obj(:)
  INTEGER( I4B ), INTENT( IN ) :: tsize( : )
END SUBROUTINE random_number_obj_vec
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE random_number_obj, random_number_obj_vec
END INTERFACE RANDOM_NUMBER

PUBLIC :: RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!### Introduction
!
! This function returns an instance of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! call display("test6:: RealVector()")
! call equalline()
! obj = RealVector( tsize = 6 )
! call display( obj, "obj =")
! call dashline()
!```

INTERFACE
MODULE PURE FUNCTION Constructor1( tSize ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     RealVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!### Introduction
! This function returns an instance of [[RealVector_]] by copying the contents of a fortran integer vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote
!
!### Usage
!
!```fortran
! type(_obj_) :: obj1, obj2, obj3
! call display("test6:: RealVector()")
! call equalline()
! obj1 = RealVector( tsize = 6 )
! call display( obj1, "obj1 =")
! obj2 = RealVector( [1,2,3,4,5] )
! call display( obj2, "obj2 = ")
! obj3 = RealVector( [1._dfp, 2.0_dfp, 3.0_dfp] )
! call display( obj3, "obj3 = ")
! call dashline()
!```

INTERFACE
MODULE PURE FUNCTION Constructor2( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!### Introduction
! This function returns an instance of [[RealVector_]] by copying the contents of a fortran real vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote
!
!### Usage
!
!```fortran
! type(_obj_) :: obj1, obj2, obj3
! call display("test6:: RealVector()")
! call equalline()
! obj1 = RealVector( tsize = 6 )
! call display( obj1, "obj1 =")
! obj2 = RealVector( [1,2,3,4,5] )
! call display( obj2, "obj2 = ")
! obj3 = RealVector( [1._dfp, 2.0_dfp, 3.0_dfp] )
! call display( obj3, "obj3 = ")
! call dashline()
!```

INTERFACE
MODULE PURE FUNCTION Constructor3( Val ) RESULT( Obj )
  TYPE( RealVector_ ) :: Obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

INTERFACE RealVector
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE RealVector

PUBLIC :: RealVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returnt the pointer to a newly created instance of [[RealVector_]]
!
!### Introduction
! This function returnt the pointer to a newly created instance of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! type(_obj_), pointer :: obj_ptr
! call display("test6:: RealVector()")
! call equalline()
! obj = RealVector( tsize = 6 )
! obj_ptr => RealVector_Pointer( tsize=5 )
! call display( obj, "obj =")
! call dashline()
!```


INTERFACE
MODULE PURE FUNCTION Constructor_1( tSize ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!### Introduction
! This function returns a pointer to an newly created instance of [[RealVector_]] by copying the contents of a fortran integer vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote
!
!### Usage
!
!```fortran
! type(_obj_), pointer :: obj1, obj2, obj3
! call display("test7:: RealVector_Pointer()")
! call equalline()
! obj1 => RealVector_Pointer( tsize = 6 )
! call display( obj1, "obj1 =")
! obj2 => RealVector_Pointer( [1,2,3,4,5] )
! call display( obj2, "obj2 = ")
! obj3 => RealVector_Pointer( [1._dfp, 2.0_dfp, 3.0_dfp] )
! call display( obj3, "obj3 = ")
! call dashline()
!```

INTERFACE
MODULE PURE FUNCTION Constructor_2( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!### Introduction
! This function returns a pointer to an newly created instance of [[RealVector_]] by copying the contents of a fortran real vector.
!
!@note
! 	This routine internally calls [[RealVector_Method:COPY]] routine.
!@endnote
!
!### Usage
!
!```fortran
! type(_obj_), pointer :: obj1, obj2, obj3
! call display("test7:: RealVector_Pointer()")
! call equalline()
! obj1 => RealVector_Pointer( tsize = 6 )
! call display( obj1, "obj1 =")
! obj2 => RealVector_Pointer( [1,2,3,4,5] )
! call display( obj2, "obj2 = ")
! obj3 => RealVector_Pointer( [1._dfp, 2.0_dfp, 3.0_dfp] )
! call display( obj3, "obj3 = ")
! call dashline()
!```

INTERFACE
MODULE PURE FUNCTION Constructor_3( Val ) RESULT( Obj )
  CLASS( RealVector_ ), POINTER :: Obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_3
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE RealVector_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3
END INTERFACE RealVector_Pointer

PUBLIC :: RealVector_Pointer


!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYintrinsic( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE intrinsicSHALLOWCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSHALLOWCOPYscalar( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE scalarSHALLOWCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE vectorSHALLOWCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE vectorSHALLOWCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSHALLOWCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarSHALLOWCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE scalarSHALLOWCOPYintrinsic( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarSHALLOWCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYscalar( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE intrinsicSHALLOWCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE intrinsicSHALLOWCOPYintrinsic, scalarSHALLOWCOPYscalar, &
    & vectorSHALLOWCOPYvector, scalarSHALLOWCOPYvector, &
    & scalarSHALLOWCOPYintrinsic
END INTERFACE SHALLOWCOPY

PUBLIC :: SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of Integer from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! integer( i4b ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! integer( i4b ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getSectionValues_Int( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getSectionValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! integer( i4b ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_Int( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValuesFromTriplet_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! real( dfp ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getValues_Real( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL(DFP), INTENT( IN ) :: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! real( dfp ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getSectionValues_Real( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getSectionValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! real( dfp ), allocatable :: nptrs( : )
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getValuesFromTriplet_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!### Introduction
!
! This function returns an instance of [[RealVector_]] by using selective from `Obj`
!
!### Usage
!
!```fortran
! type(_obj_) :: obj, nptrs
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = obj
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], TypeRealVector)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getSectionValues_self( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getSectionValues_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!### Introduction
! This function returns the instance of [[RealVector_]] using istart, iend, stride values
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj, nptrs
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = obj
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], TypeRealVector)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_self( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getValuesFromTriplet_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: This function returns the vector of integer from [[RealVector_]]
!
!### Usage
!
!```fortran
! subroutine test11
! type(_obj_) :: obj( 3 )
! integer( I4B ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]
!
!### Usage
!
!```fortran
! subroutine test11
! type(_obj_) :: obj( 3 )
! integer( I4B ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYSectionValues_Int( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYSectionValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an integer vector from [[RealVector_]]
!
!### Usage
!
!```fortran
! subroutine test11
! type(_obj_) :: obj( 3 )
! integer( I4B ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_Int( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValuesFromTriplet_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 )
! real( dfp ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValues_Real( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL(DFP), INTENT( IN ) :: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 )
! real( dfp ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYSectionValues_Real( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL(DFP), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYSectionValues_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 )
! real( dfp ), allocatable :: nptrs( : )
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_Real( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL(DFP), INTENT( IN ):: DataType
  REAL(DFP), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValuesFromTriplet_Real
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!### Introduction
! This function returns an scalar instance of [[RealVector_]] by combining different entries of a vector of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValues_self( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYValues_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!### Introduction
! This function returns the instance of [[RealVector_]] from the vector of [[RealVector_]].
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

MODULE PURE FUNCTION f_getARRAYSectionValues_self( Obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYSectionValues_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!### Introduction
! This function returns the instance of [[RealVector_]] from the vector of [[RealVector_]].
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = ArrayValues(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = ArrayValues(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_self( Obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION f_getARRAYValuesFromTriplet_self
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE ArrayValues
  MODULE PROCEDURE &
    & f_getSectionValues_self, &
    & f_getValuesFromTriplet_self, &
    & f_getARRAYValues_self, &
    & f_getARRAYSectionValues_self, &
    & f_getARRAYValuesFromTriplet_self, &
    & f_getValues_Int, &
    & f_getSectionValues_Int, &
    & f_getValuesFromTriplet_Int, &
    & f_getARRAYValues_Int, &
    & f_getARRAYSectionValues_Int, &
    & f_getARRAYValuesFromTriplet_Int, &
    & f_getValues_Real, &
    & f_getSectionValues_Real, &
    & f_getValuesFromTriplet_Real, &
    & f_getARRAYValues_Real, &
    & f_getARRAYSectionValues_Real, &
    & f_getARRAYValuesFromTriplet_Real
END INTERFACE

PUBLIC :: ArrayValues

!----------------------------------------------------------------------------
!                                                    ArrayPointers@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to vector of real numbers stored inside [[RealVector_]]
!
!### Introduction
! This function returns the pointer to vector of real numbers stored inside [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! real(dfp), pointer :: ptr( : ) => null()
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! ptr => ArrayValuesPointer(obj, 1.0_DFP)
! call display( ptr, "ptr =" )
!```

INTERFACE
MODULE FUNCTION f_getPointer_Real( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: Obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), POINTER :: Val( : )
END FUNCTION f_getPointer_Real
END INTERFACE

INTERFACE ArrayValuesPointer
  MODULE PROCEDURE f_getPointer_Real
END INTERFACE

PUBLIC :: ArrayValuesPointer

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: This function finds the location of Value inside the [[RealVector_]]
!
!### Introduction
!
! This function finds the location of `Value` inside the instance of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( LOC(Obj=Obj, Value=6.0_DFP ), "LOC =" )
! call display( LOC(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION IndexOf_1( Obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ) :: Ans
END FUNCTION IndexOf_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the location of values inside the [[RealVector_]]
!
!### Introduction
!
! This function returns the nearest location of values inside the [[RealVector_]]
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( LOC(Obj=Obj, Value=6.0_DFP ), "LOC =" )
! call display( LOC(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION IndexOf_2( Obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION IndexOf_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This generic function returns the location of value in [[RealVector_]]
!
INTERFACE LOC
  MODULE PROCEDURE IndexOf_1, IndexOf_2
END INTERFACE LOC

PUBLIC :: LOC

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns true if Value is present inside [[RealVector_]]
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( isPresent(Obj=Obj, Value=6.0_DFP ), "LOC =" )
! ! call display( isPresent(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION isPresent_1( Obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION isPresent_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns true if Value is present inside [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( isPresent(Obj=Obj, Value=6.0_DFP ), "LOC =" )
! ! call display( isPresent(Obj=Obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION isPresent_2( Obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION isPresent_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

INTERFACE isPresent
  MODULE PROCEDURE isPresent_1, isPresent_2
END INTERFACE isPresent

PUBLIC :: isPresent

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This subroutine appends Value to [[RealVector_]]

!
!### Usage
!
!```fortran
! type(_obj_) :: obj, obj1
! obj = RealVector([1,2,3,4])
! call display(obj, "obj=")
! call append(obj, 5.0_DFP)
! call display(obj, "obj=")
! call append(obj, [6.0_DFP, 7.0_DFP])
! call display(obj, "obj=")
! call append(obj1, obj)
! call display(obj1, "obj=")
!```

INTERFACE
MODULE PURE SUBROUTINE Append_1( Obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE Append_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This subroutine appends Value to [[RealVector_]]

!
!### Usage
!
!```fortran
! type(_obj_) :: obj, obj1
! obj = RealVector([1,2,3,4])
! call display(obj, "obj=")
! call append(obj, 5.0_DFP)
! call display(obj, "obj=")
! call append(obj, [6.0_DFP, 7.0_DFP])
! call display(obj, "obj=")
! call append(obj1, obj)
! call display(obj1, "obj=")
!```

INTERFACE
MODULE PURE SUBROUTINE Append_2( Obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE Append_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This subroutine appends Value to [[RealVector_]]

!
!### Usage
!
!```fortran
! type(_obj_) :: obj, obj1
! obj = RealVector([1,2,3,4])
! call display(obj, "obj=")
! call append(obj, 5.0_DFP)
! call display(obj, "obj=")
! call append(obj, [6.0_DFP, 7.0_DFP])
! call display(obj, "obj=")
! call append(obj1, obj)
! call display(obj1, "obj=")
!```

INTERFACE
MODULE PURE SUBROUTINE Append_3( Obj, AnotherObj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  CLASS( RealVector_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE Append_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------
INTERFACE Append
  MODULE PROCEDURE Append_1, Append_2, Append_3
END INTERFACE Append

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE realVec_setValue1( Obj, Indx, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_setValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values to given scalar

INTERFACE
MODULE SUBROUTINE realVec_setValue2( Obj,  Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE realVec_setValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values by given vector

INTERFACE
MODULE SUBROUTINE realVec_setValue3( Obj,  Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_setValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a scalar

INTERFACE
MODULE SUBROUTINE realVec_setValue4( Obj,  istart, iend, stride, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE realVec_setValue4
END INTERFACE

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a vector

INTERFACE
MODULE SUBROUTINE realVec_setValue5( Obj,  istart, iend, stride, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_setValue5
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_setValue1, realVec_setValue2, realVec_setValue3, &
    & realVec_setValue4, realVec_setValue5
END INTERFACE Set

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Obj_Scalar_Display( Obj, msg, UnitNo )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Obj_Scalar_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Obj_Vector_Display ( Obj, msg, UnitNo )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE Obj_Vector_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------
INTERFACE Display
  MODULE PROCEDURE Obj_Scalar_Display, Obj_Vector_Display
END INTERFACE Display

PUBLIC :: Display

END MODULE RealVector_Method