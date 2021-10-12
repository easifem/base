
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
! summary: 	This module contains methods of [[IntVector_]] data type.
!
!###Introduction
!
! This module contains methods of [[IntVector_]] data type.
! This module only contains the definition of the interfaces of these
! methods. The actual implementation is given inside the submodules. This
! modules has following submodules:
!

MODULE IntVector_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                         Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns shape of the vector

INTERFACE
MODULE PURE FUNCTION intVec_shape( obj ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION intVec_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE intVec_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns size of the vector

INTERFACE
MODULE PURE FUNCTION intVec_Size( obj, Dims ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION intVec_Size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE intVec_Size
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
MODULE PURE FUNCTION IntVec_getTotalDimension( obj ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION IntVec_getTotalDimension
END INTERFACE

INTERFACE getTotalDimension
  MODULE PROCEDURE IntVec_getTotalDimension
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
MODULE PURE SUBROUTINE IntVec_setTotalDimension( obj, tDimension )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE IntVec_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE IntVec_setTotalDimension
END INTERFACE setTotalDimension

PUBLIC :: setTotalDimension

!----------------------------------------------------------------------------
!                                                   AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary: 	Allocate memory for the vector

INTERFACE
MODULE PURE SUBROUTINE intVec_AllocateData( obj, Dims )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Dims
END SUBROUTINE intVec_AllocateData
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE intVec_AllocateData
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 DeAllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Deallocate memory occupied by IntVector

INTERFACE
MODULE PURE SUBROUTINE intVec_DeallocateData( obj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE intVec_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE intVec_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE
MODULE SUBROUTINE intVec_Display1 ( obj, msg, UnitNo, orient )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: orient
END SUBROUTINE intVec_Display1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE
MODULE SUBROUTINE intVec_Display2 ( obj, msg, UnitNo, orient )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: orient
END SUBROUTINE intVec_Display2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

INTERFACE Display
  MODULE PROCEDURE intVec_Display1, intVec_Display2
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine initiates the [[IntVector_]]
!
!
!### Usage
!
!```fortran
! type(IntVector_) :: obj
! call display( "test1" )
! call EqualLine()
! call initiate(obj=obj, tSize=10)
! call display( obj, msg = "test1")
! call DashLine()
!```

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate1( obj, tSize )
    CLASS( IntVector_ ), INTENT( INOUT ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: tSize
  END SUBROUTINE intVec_initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: 	This routine initiates the vector of [[IntVector_]]
!
!
!### Usage
!
!```fortran
! type(IntVector_), allocatable :: obj( : )
! integer( I4B ) :: tsize( 4 )
! tsize = [5,5,10,10]
! call display( "test2" )
! call EqualLine()
! call initiate(obj=obj, tSize=tsize)
! call display( obj, msg = "test2")
! call DashLine()
!```

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate2( obj, tSize )
    TYPE( IntVector_ ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
    INTEGER( I4B ), INTENT( IN ) :: tSize( : )
  END SUBROUTINE intVec_initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate3( obj, a, b )
    CLASS( IntVector_ ), INTENT( INOUT ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: a, b
  END SUBROUTINE intVec_initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate4( obj, val )
    CLASS( IntVector_ ), INTENT( INOUT ) :: obj
    INTEGER( I4B ), INTENT( IN ) :: val( : )
  END SUBROUTINE intVec_initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE intVec_initiate5( obj, val )
    CLASS( IntVector_ ), INTENT( INOUT ) :: obj
    REAL( DFP ), INTENT( IN ) :: val( : )
  END SUBROUTINE intVec_initiate5
END INTERFACE


INTERFACE Initiate
  MODULE PROCEDURE intVec_initiate1, intVec_initiate2, intVec_initiate3, &
    intVec_initiate4, intVec_initiate5
END INTERFACE Initiate

PUBLIC :: Initiate

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE intVec_initiate4, intVec_initiate5
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                  isAllocated@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_isAllocated( obj ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION intVec_isAllocated
END INTERFACE

INTERFACE isAllocated
  MODULE PROCEDURE intVec_isAllocated
END INTERFACE isAllocated

PUBLIC :: isAllocated

INTERFACE isInitiated
  MODULE PROCEDURE intVec_isAllocated
END INTERFACE isInitiated

PUBLIC :: isInitiated

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor1( tSize ) RESULT( obj )
  TYPE( IntVector_ ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION intVec_Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor2( Val ) RESULT( obj )
  TYPE( IntVector_ ) :: obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION intVec_Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor3( Val ) RESULT( obj )
  TYPE( IntVector_ ) :: obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION intVec_Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IntVector@Constructor
!----------------------------------------------------------------------------

INTERFACE IntVector
  MODULE PROCEDURE intVec_Constructor1, &
    & intVec_Constructor2, intVec_Constructor3
END INTERFACE IntVector

PUBLIC :: IntVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor_1( tSize ) RESULT( obj )
  CLASS( IntVector_ ), POINTER :: obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION intVec_Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor_2( Val ) RESULT( obj )
CLASS( IntVector_ ), POINTER :: obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION intVec_Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_Constructor_3( Val ) RESULT( obj )
  CLASS( IntVector_ ), POINTER :: obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION intVec_Constructor_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE IntVector_Pointer
  MODULE PROCEDURE intVec_Constructor_1, intVec_Constructor_2, intVec_Constructor_3
END INTERFACE IntVector_Pointer

PUBLIC :: IntVector_Pointer

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_1( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_2( obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_3( obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_4( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_5( obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_6( obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
END FUNCTION intVec_get_6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_7( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_8( obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_9( obj, iStart, iEnd, Stride,&
  & DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_10( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_10
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_11( obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_11
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_12( obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION intVec_get_12
END INTERFACE

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_get_13( obj, indx, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B) :: val
END FUNCTION intVec_get_13
END INTERFACE

INTERFACE get
  MODULE PROCEDURE intVec_get_1, intVec_get_2, intVec_get_3, &
    & intVec_get_4, intVec_get_5, intVec_get_6, intVec_get_7, &
    & intVec_get_8, intVec_get_9, intVec_get_10, intVec_get_11, &
    & intVec_get_12, intVec_get_13
END INTERFACE get

PUBLIC :: get

!----------------------------------------------------------------------------
!                                                           assign@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE IntVec_assign_a( Val, obj )
  INTEGER(I4B), ALLOCATABLE, INTENT( INOUT ) :: Val( : )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
END SUBROUTINE IntVec_assign_a
END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE IntVec_assign_a
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                     getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION intVec_getPointer_1( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ), TARGET :: obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ), POINTER :: Val
END FUNCTION intVec_getPointer_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION intVec_getPointer_2( obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ), TARGET :: obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), POINTER :: Val( : )
END FUNCTION intVec_getPointer_2
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE intVec_getPointer_1, intVec_getPointer_2
END INTERFACE getPointer

PUBLIC :: getPointer

!----------------------------------------------------------------------------
!                                                         Convert@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE obj_convert_int( From, To )
  CLASS( IntVector_ ), INTENT( IN ) :: From
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT) :: To( : )
END SUBROUTINE obj_convert_int
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE obj_convert_int
END INTERFACE Convert

PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_getIndex1( obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value
  INTEGER( I4B ) :: Ans
END FUNCTION intVec_getIndex1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_getIndex2( obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION intVec_getIndex2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_getIndex3( obj, Value ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Value
  INTEGER( I4B ) :: Ans
END FUNCTION intVec_getIndex3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_getIndex4( obj, Value ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION intVec_getIndex4
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE intVec_getIndex1, intVec_getIndex2, &
    & intVec_getIndex3, intVec_getIndex4
END INTERFACE getIndex

PUBLIC :: getIndex

!----------------------------------------------------------------------------
!                                                        isPresent@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_isPresent1( obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value
  LOGICAL( LGT ) :: Ans
END FUNCTION intVec_isPresent1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        isPresent@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION intVec_isPresent2( obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION intVec_isPresent2
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE intVec_isPresent1, intVec_isPresent2
END INTERFACE isPresent

PUBLIC :: isPresent

!----------------------------------------------------------------------------
!                                                        setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE
MODULE PURE SUBROUTINE intVec_set1( obj, Indx, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE intVec_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        setValue@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: set the value in IntVector

INTERFACE
MODULE PURE SUBROUTINE intVec_set2( obj, Indx, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Indx
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE intVec_set2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE intVec_set1, intVec_set2
END INTERFACE set

PUBLIC :: set

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE IntVec_Append_1( obj, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE IntVec_Append_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE IntVec_Append_2( obj, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE IntVec_Append_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Append@setMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE IntVec_Append_3( obj, Anotherobj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
  CLASS( IntVector_ ), INTENT( IN ) :: Anotherobj
END SUBROUTINE IntVec_Append_3
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE IntVec_Append_1, IntVec_Append_2, IntVec_Append_3
END INTERFACE Append

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                 RemoveDuplicates@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE IntVec_RemoveDuplicates_1( obj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: obj
END SUBROUTINE IntVec_RemoveDuplicates_1
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE IntVec_RemoveDuplicates_2( obj )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
END SUBROUTINE IntVec_RemoveDuplicates_2
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE IntVec_RemoveDuplicates_1, IntVec_RemoveDuplicates_2
END INTERFACE RemoveDuplicates

PUBLIC :: RemoveDuplicates

!----------------------------------------------------------------------------
!                                                           Repeat@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION IntVec_Repeat_1( Val, rtimes ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: rtimes
  INTEGER( I4B ) :: Ans( SIZE( Val ) * rtimes )
END FUNCTION IntVec_Repeat_1
END INTERFACE

INTERFACE
MODULE PURE FUNCTION IntVec_Repeat_2( obj, rtimes ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: rtimes
  INTEGER( I4B ) :: Ans( SIZE( obj%Val ) * rtimes )
END FUNCTION IntVec_Repeat_2
END INTERFACE

INTERFACE Repeat
  MODULE PROCEDURE IntVec_Repeat_1, IntVec_Repeat_2
END INTERFACE Repeat

PUBLIC :: Repeat

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Horizontally concat two integer vectors and returns a new integer vector

INTERFACE
MODULE PURE FUNCTION IntVec_H_CONCAT_1( vec1, vec2 ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: vec1( : )
  INTEGER( I4B ), INTENT( IN ) :: vec2( : )
  INTEGER( I4B ) :: ans( size(vec1) + size(vec2) )
END FUNCTION IntVec_H_CONCAT_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Horizontally concat a IntVec data type to another IntVec datatype.

INTERFACE
MODULE PURE FUNCTION IntVec_H_CONCAT_2( obj1, obj2 ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: obj1
  TYPE( IntVector_ ), INTENT( IN ) :: obj2
  TYPE( IntVector_ ) :: ans
END FUNCTION IntVec_H_CONCAT_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Horizontally concat a integer vector to a IntVec datatype.

INTERFACE
MODULE PURE FUNCTION IntVec_H_CONCAT_3( vec1, obj2 ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: vec1( : )
  TYPE( IntVector_ ), INTENT( IN ) :: obj2
  TYPE( IntVector_ ) :: ans
END FUNCTION IntVec_H_CONCAT_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                        H_CONCAT@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Horizontally concat a integer vector to a IntVec datatype.

INTERFACE
MODULE PURE FUNCTION IntVec_H_CONCAT_4( obj1, vec2 ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: vec2( : )
  TYPE( IntVector_ ), INTENT( IN ) :: obj1
  TYPE( IntVector_ ) :: ans
END FUNCTION IntVec_H_CONCAT_4
END INTERFACE


INTERFACE H_CONCAT
  MODULE PROCEDURE IntVec_H_CONCAT_1, IntVec_H_CONCAT_2, &
    & IntVec_H_CONCAT_3, IntVec_H_CONCAT_4
END INTERFACE H_CONCAT

PUBLIC :: H_CONCAT

END MODULE IntVector_Method
