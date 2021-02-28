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
  INTEGER(Int32), INTENT( IN ) :: Val( : )
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
  REAL(Real64), INTENT( IN ) :: Val( : )
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
  INTEGER(Int32), INTENT( IN ) :: Val( : )
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
  REAL(Real64), INTENT( IN ) :: Val( : )
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
MODULE FUNCTION f_getPointer_Real64( Obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: Obj
  REAL( Real64 ), INTENT( IN ) :: DataType
  REAL( Real64 ), POINTER :: Val( : )
END FUNCTION f_getPointer_Real64
END INTERFACE

INTERFACE ArrayValuesPointer
  MODULE PROCEDURE f_getPointer_Real64
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
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine returns dot product of two [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1, obj2
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( obj2, 100 )
! CALL Display( DOT(obj1, obj2), "dot 1=" )
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
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
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
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
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
!
!### Usage
!
!```fortran
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!```

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
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
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
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
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
    & scalarDOTvector, scalarDOTintrinsic
END INTERFACE DOT

PUBLIC :: DOT

INTERFACE OPERATOR(.DOT.)
  MODULE PROCEDURE scalarDOTscalar, vectorDOTvector, vectorDOTscalar, &
    & scalarDOTvector, scalarDOTintrinsic, intrinsicDOTscalar
END INTERFACE OPERATOR(.DOT.)

PUBLIC :: OPERATOR(.DOT.)

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
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
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
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
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
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
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
! type(_obj_) :: obj, obj1( 2 )
! obj = RealVector( [1,2,3] )
! call display( NORM2( obj ), "norm2 = " )
! call display( NORM2SQR( obj ), "norm2sqr = " )
! obj1(1) = RealVector( [1,2,3] )
! obj1(2) = RealVector( [1,2,3] )
! call display( NORM2( obj1 ), "norm2 = " )
! call display( NORM2SQR( obj1 ), "norm2sqr = " )
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
! type(_obj_) :: obj, obj1( 2 )
! obj = RealVector( [1,2,3] )
! call display( NORM2( obj ), "norm2 = " )
! call display( NORM2SQR( obj ), "norm2sqr = " )
! obj1(1) = RealVector( [1,2,3] )
! obj1(2) = RealVector( [1,2,3] )
! call display( NORM2( obj1 ), "norm2 = " )
! call display( NORM2SQR( obj1 ), "norm2sqr = " )
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
  MODULE PROCEDURE ASUMScalar, ASUMvector
END INTERFACE ASUM

PUBLIC :: ASUM

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
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copies one [[RealVector_]] object into another object, i.e. `Obj1=Obj2`. See figure given below:
!
!
! <img src=|media|/scalar_copy_scalar.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYscalar( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
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
! This subroutine copy a vector of [[RealVector_]] into another vector, i.e. `Obj1=Obj2` see the figure below:
!
! <img src=|media|/vector_copy_vector.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine. Also note that `Obj1` and `Obj2` are vectors of [[RealVector_]] data type.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE vectorCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
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
! This subroutine copies a vector of [[RealVector_]] into a scalar instance of [[RealVector_]]. See Figure below:
!
! <img src=|media|/scalar_copy_vector.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
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
! This subroutine copy a fortran vector into [[RealVector_]] obj, i.e. `Obj=Val`
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYintrinsic( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
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
! This subroutine copy an instance of [[RealVector_]] in another fortran vector, i.e. `Val=Obj`
!
!@note
! 	This subroutine internally calls [[intrinsicCOPYintrinsic]]. Also `Val` is allocatable.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE intrinsicCOPYscalar( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE intrinsicCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	26 Feb 2021
! summary: 	This is generic subroutine for copying.
!
!### Introduction
! See
! * [[intrinsicCOPYintrinsic]]
! * [[scalarCOPYscalar]]
! * [[vectorCOPYvector]]
! * [[scalarCOPYvector]]
! * [[scalarCOPYintrinsic]]

INTERFACE COPY
  MODULE PROCEDURE scalarCOPYscalar, &
    & vectorCOPYvector, scalarCOPYvector, scalarCOPYintrinsic
END INTERFACE COPY

PUBLIC :: COPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE scalarCOPYscalar, &
    & vectorCOPYvector, scalarCOPYvector, scalarCOPYintrinsic
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                 SWAP@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSWAPscalar( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
END SUBROUTINE scalarSWAPscalar

MODULE PURE SUBROUTINE vectorSWAPvector( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y( : )
END SUBROUTINE vectorSWAPvector

MODULE PURE SUBROUTINE scalarSWAPintrinsic( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X
  REAL( DFP ), INTENT( INOUT ) :: Y( : )
END SUBROUTINE scalarSWAPintrinsic
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
MODULE PURE SUBROUTINE SCALscalar( X, A )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: X
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE SCALscalar

MODULE PURE SUBROUTINE SCALvector( X, A )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: X( : )
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE SCALvector
END INTERFACE

INTERFACE SCAL
  MODULE PROCEDURE SCALscalar, SCALvector
END INTERFACE SCAL

PUBLIC :: SCAL

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE

! Y = Y + A*X
MODULE PURE SUBROUTINE scalarAXPYscalar( X, Y, A )
  CLASS( RealVector_ ), INTENT( IN ) :: X
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE scalarAXPYscalar
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE vectorAXPYvector( X, Y, A )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE vectorAXPYvector
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE scalarAXPYintrinsic( X, Y, A )
  REAL( DFP ), INTENT( IN ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE scalarAXPYintrinsic
END INTERFACE

INTERFACE AXPY
  MODULE PROCEDURE scalarAXPYscalar, vectorAXPYvector, &
    & scalarAXPYintrinsic
END INTERFACE AXPY

PUBLIC :: AXPY

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
MODULE SUBROUTINE setValue_1( Obj, Indx, Value )
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

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

END MODULE RealVector_Method