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
MODULE PURE FUNCTION realVec_shape( obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION realVec_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE realVec_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This function returns the size of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_size( obj, Dims ) RESULT( Ans )
  TYPE( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION realVec_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE realVec_size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!# Introduction
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
!# Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
MODULE PURE SUBROUTINE RealVec_setTotalDimension( obj, tDimension )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE realVec_AllocateData( obj, Dims )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims
END SUBROUTINE realVec_AllocateData
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE realVec_AllocateData
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 DeAllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine deallocates the data in [[RealVector_]]

INTERFACE
MODULE PURE SUBROUTINE realVec_DeallocateData( obj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE realVec_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE realVec_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	This subroutine allocates the memory for [[RealVector_]]
!
!# Introduction This subroutine allocates the memeory for [[RealVector_]]
!
!@note
! This subroutine is an alias for [[Allocate_Data]]
!@endnote
!
!```fortran
! CALL Initiate(obj, 5)
!```

INTERFACE
MODULE PURE SUBROUTINE realVec_initiate1( obj, tSize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END SUBROUTINE realVec_initiate1

END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for a vector of type [[RealVector_]]
!
!# Introduction
! This subroutine allocate the memory for a vector of type [[RealVector_]]
!@note
! The size of `obj` would be same as the size of `tSize`
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
! call display( obj, "initiate obj(:)=")
! call dashline()
!```

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate2( obj, tSize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: tSize( : )
END SUBROUTINE realVec_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: This subroutine allocate the memory for an instance of [[RealVector_]]
!
!# Introduction
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
! call display( obj, "initiate obj(a:b)=")
! call dashline()
!```

INTERFACE
MODULE PURE SUBROUTINE realVec_Initiate3( obj, a, b )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: a, b
END SUBROUTINE realVec_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	27 Feb 2021
! summary: 	Generic subroutine to initiate [[RealVector_]]

INTERFACE Initiate
  MODULE PROCEDURE realVec_initiate1, realVec_initiate2, realVec_initiate3
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!# Introduction
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
MODULE SUBROUTINE realVec_Random_Number1( obj, tsize )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tsize
END SUBROUTINE realVec_Random_Number1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Random_number@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This routine computes radom_number
!
!# Introduction
!
! This routine calls `RANDOM_NUMBER` to generate a random instnance of [[RealVector_]]
!
!@note
! 	Here argument `obj` is a vector of [[RealVector_]] data-types.
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
MODULE SUBROUTINE realVec_Random_Number2( obj, tsize )
  TYPE( RealVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj(:)
  INTEGER( I4B ), INTENT( IN ) :: tsize( : )
END SUBROUTINE realVec_Random_Number2
END INTERFACE

INTERFACE RANDOM_NUMBER
  MODULE PROCEDURE realVec_Random_Number1, realVec_Random_Number2
END INTERFACE RANDOM_NUMBER

PUBLIC :: RANDOM_NUMBER

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor1( tSize ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION realVec_Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     RealVector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor2( Val ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor3( Val ) RESULT( obj )
  TYPE( RealVector_ ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    RealVector@Constructor
!----------------------------------------------------------------------------

INTERFACE RealVector
  MODULE PROCEDURE realVec_Constructor1, realVec_Constructor2, realVec_Constructor3
END INTERFACE RealVector

PUBLIC :: RealVector

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returnt the pointer to a newly created instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor_1( tSize ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION realVec_Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor_2( Val ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to an instance of [[RealVector_]]
!
!# Introduction
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
MODULE PURE FUNCTION realVec_Constructor_3( Val ) RESULT( obj )
  CLASS( RealVector_ ), POINTER :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END FUNCTION realVec_Constructor_3
END INTERFACE

!----------------------------------------------------------------------------
!                                             RealVector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE RealVector_Pointer
  MODULE PROCEDURE realVec_Constructor_1, realVec_Constructor_2, realVec_Constructor_3
END INTERFACE RealVector_Pointer

PUBLIC :: RealVector_Pointer

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY1( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY2( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY3( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY3
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY4( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY4
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY5( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY5
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY6( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE realVec_SHALLOWCOPY6
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 |June 2021
! summary: Copy only the structure for Y = X

INTERFACE
MODULE PURE SUBROUTINE realVec_SHALLOWCOPY7( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE realVec_SHALLOWCOPY7
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SHALLOWCOPY@Constructor
!----------------------------------------------------------------------------

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE realVec_SHALLOWCOPY1, realVec_SHALLOWCOPY2, &
    & realVec_SHALLOWCOPY3, realVec_SHALLOWCOPY4, realVec_SHALLOWCOPY5, &
    & realVec_SHALLOWCOPY6, realVec_SHALLOWCOPY7
END INTERFACE SHALLOWCOPY

PUBLIC :: SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign1( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign2( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign3( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  REAL( DFP ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign4( lhs, rhs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign4
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign5( lhs, rhs )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign5
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign6( lhs, rhs )
  CLASS( RealVector_ ), INTENT( INOUT ) :: lhs
  INTEGER( I4B ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign6
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign7( lhs, rhs )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs
END SUBROUTINE realVec_assign7
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Assign@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE realVec_assign8( lhs, rhs )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: lhs( : )
  CLASS( RealVector_ ), INTENT( IN ) :: rhs( : )
END SUBROUTINE realVec_assign8
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE realVec_assign1, realVec_assign2, realVec_assign3, &
    & realVec_assign4, realVec_assign5, realVec_assign6, realVec_assign7, &
    & realVec_assign8
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get1( obj, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get2( obj, Indx, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get3( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get4( obj, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get5( obj, Indx, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL( DFP ), INTENT( IN ):: DataType
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get6( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL( DFP ), INTENT( IN ):: DataType
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                     get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION realVec_get7( obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION realVec_get8( obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1_I4B )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1_I4B)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1_I4B)
! call display( nptrs, "nptrs =")
! end
!```

INTERFACE
MODULE PURE FUNCTION realVec_get9( obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get10( obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                     get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get11( obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( DFP ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get11
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
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
! nptrs = get(obj, 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], 1.0_DFP )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, 1.0_DFP )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get12( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL( DFP ), INTENT( IN ):: DataType
  REAL( DFP ), ALLOCATABLE :: Val( : )
END FUNCTION realVec_get12
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns an scalar instance of [[RealVector_]] by combining different entries of a vector of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = get(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get13( obj, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION realVec_get13
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of [[RealVector_]].
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = get(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

MODULE PURE FUNCTION realVec_get14( obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION realVec_get14
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of [[RealVector_]].
!
!### Usage
!
!```fortran
! type(_obj_) :: obj( 3 ), nptrs
! obj(1) = RealVector([1,2,3,4,5,6,7,8,9,10])
! obj(2) = RealVector([11,12,13,14,15,16,17,18,19,20])
! obj(3) = RealVector([21,22,23,24,25])
! nptrs = get(obj, TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], TypeRealVector )
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, TypeRealVector )
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get15( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: Val
END FUNCTION realVec_get15
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
!
! This function returns an instance of [[RealVector_]] by using selective from `obj`
!
!### Usage
!
!```fortran
! type(_obj_) :: obj, nptrs
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! nptrs = obj
! call display( nptrs, "nptrs =")
! nptrs = get(obj, [2,3,4], TypeRealVector)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, TypeRealVector)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get16( obj, Indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( RealVector_ ) :: Val
END FUNCTION realVec_get16
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
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
! nptrs = get(obj, [2,3,4], TypeRealVector)
! call display( nptrs, "nptrs =")
! nptrs = get(obj, 1,10, 2, TypeRealVector)
! call display( nptrs, "nptrs =")
!```

INTERFACE
MODULE PURE FUNCTION realVec_get17( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealVector_ ) :: Val
END FUNCTION realVec_get17
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realVec_get18( obj, indx, DataType ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  REAL( DFP ), INTENT( IN ):: DataType
  REAL( DFP ) :: val
END FUNCTION realVec_get18
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE get
  MODULE PROCEDURE &
    & realVec_get1, realVec_get2, realVec_get3, realVec_get4, &
    & realVec_get5, realVec_get6, realVec_get7, realVec_get8, &
    & realVec_get9, realVec_get10, realVec_get11, realVec_get12, &
    & realVec_get13, realVec_get14, realVec_get15, realVec_get16,&
    & realVec_get17, realVec_get18
END INTERFACE get

PUBLIC :: get

!----------------------------------------------------------------------------
!                                                    getPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to vector of real numbers stored inside [[RealVector_]]
!
!# Introduction
! This function returns the pointer to vector of real numbers stored inside [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! REAL( DFP ), pointer :: ptr( : ) => null()
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! ptr => getPointer(obj)
! call display( ptr, "ptr =" )
!```

INTERFACE
MODULE FUNCTION realVec_getPointer1( obj ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: obj
  REAL( DFP ), POINTER :: Val( : )
END FUNCTION realVec_getPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the pointer to vector of real numbers stored inside [[RealVector_]]
!
!# Introduction
! This function returns the pointer to vector of real numbers stored inside [[RealVector_]] for a given degree of freedom
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! REAL( DFP ), pointer :: ptr( : ) => null()
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! ptr => getPointer(obj)
! call display( ptr, "ptr =" )
!```

INTERFACE
MODULE FUNCTION realVec_getPointer2( obj, dofobj, dofno ) RESULT( Val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: dofno
  REAL( DFP ), POINTER :: Val( : )
END FUNCTION realVec_getPointer2
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE realVec_getPointer1, realVec_getPointer2
END INTERFACE

PUBLIC :: getPointer

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: This function finds the location of Value inside the [[RealVector_]]
!
!# Introduction
!
! This function finds the location of `Value` inside the instance of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( LOC(obj=obj, Value=6.0_DFP ), "LOC =" )
! call display( LOC(obj=obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION realVec_getIndex1( obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ) :: Ans
END FUNCTION realVec_getIndex1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the location of values inside the [[RealVector_]]
!
!# Introduction
!
! This function returns the nearest location of values inside the [[RealVector_]]
!
!
!### Usage
!
!```fortran
! type(_obj_) :: obj
! obj = RealVector([1,2,3,4,5,6,7,8,9,10])
! call display( LOC(obj=obj, Value=6.0_DFP ), "LOC =" )
! call display( LOC(obj=obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION realVec_getIndex2( obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION realVec_getIndex2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This generic function returns the location of value in [[RealVector_]]
!
INTERFACE getIndex
  MODULE PROCEDURE realVec_getIndex1, realVec_getIndex2
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                       isPresent@getMethod
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
! call display( isPresent(obj=obj, Value=6.0_DFP ), "LOC =" )
! ! call display( isPresent(obj=obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION realVec_isPresent1( obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION realVec_isPresent1
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
! call display( isPresent(obj=obj, Value=6.0_DFP ), "LOC =" )
! ! call display( isPresent(obj=obj, Value=[6.0_DFP, 5.0_DFP] ), "LOC =" )
!```

INTERFACE
MODULE PURE FUNCTION realVec_isPresent2( obj, Value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION realVec_isPresent2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

INTERFACE isPresent
  MODULE PROCEDURE realVec_isPresent1, realVec_isPresent2
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
MODULE PURE SUBROUTINE realVec_Append1( obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE realVec_Append1
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
MODULE PURE SUBROUTINE realVec_Append2( obj, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_Append2
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
MODULE PURE SUBROUTINE realVec_Append3( obj, Anotherobj )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  CLASS( RealVector_ ), INTENT( IN ) :: Anotherobj
END SUBROUTINE realVec_Append3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Append@setMethod
!----------------------------------------------------------------------------
INTERFACE Append
  MODULE PROCEDURE realVec_Append1, realVec_Append2, realVec_Append3
END INTERFACE Append

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE realVec_set1( obj, Indx, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values to given scalar

INTERFACE
MODULE SUBROUTINE realVec_set2( obj,  Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE realVec_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values by given vector

INTERFACE
MODULE SUBROUTINE realVec_set3( obj,  Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a scalar

INTERFACE
MODULE SUBROUTINE realVec_set4( obj,  istart, iend, stride, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE realVec_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a vector

INTERFACE
MODULE SUBROUTINE realVec_set5( obj,  istart, iend, stride, Value )
  CLASS( RealVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE realVec_set5
END INTERFACE

INTERFACE Set
  MODULE PROCEDURE realVec_set1, realVec_set2, realVec_set3, &
    & realVec_set4, realVec_set5
END INTERFACE Set

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE realVec_display1( obj, msg, UnitNo )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE realVec_display1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE realVec_display2 ( obj, msg, UnitNo )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE realVec_display2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------
INTERFACE Display
  MODULE PROCEDURE realVec_display1, realVec_display2
END INTERFACE Display

PUBLIC :: Display

END MODULE RealVector_Method