
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
! This module contains methods of [[IntVector_]] data type. This module only contains the definition of the interfaces of these methods. The actual implementation is given inside the submodules. This modules has following submodules:
!

MODULE IntVector_Method
USE GlobalData, ONLY: DFP, I4B, LGT, stdout
USE BaseType, ONLY : IntVector_
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns shape of the vector

INTERFACE
MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION get_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                             SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns size of the vector

INTERFACE
MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: Obj
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

!> authors: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary: 	Allocate memory for the vector

INTERFACE
MODULE PURE SUBROUTINE Allocate_Data( Obj, Dims )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
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

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Deallocate memory occupied by IntVector

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                         Display@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE
MODULE SUBROUTINE IntVectorDisplay ( Obj, msg, UnitNo )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE IntVectorDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: Display the content of [[IntVector_]]

INTERFACE
MODULE SUBROUTINE IntscalarDisplay ( Obj, msg, UnitNo )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE IntscalarDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 FEb 2021
! summary: 	This is a generic subroutine that displays the content of [[IntVector_]]
INTERFACE Display
  MODULE PROCEDURE IntVectorDisplay, &
    & IntscalarDisplay
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
  MODULE PURE SUBROUTINE initiate_obj( Obj, tSize )
    CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: tSize
  END SUBROUTINE initiate_obj
END INTERFACE

!----------------------------------------------------------------------------
!
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
  MODULE PURE SUBROUTINE initiate_obj_vector( Obj, tSize )
    TYPE( IntVector_ ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
    INTEGER( I4B ), INTENT( IN ) :: tSize( : )
  END SUBROUTINE initiate_obj_vector
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE initiate_obj_ab( Obj, a, b )
    CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: a, b
  END SUBROUTINE initiate_obj_ab

END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj, initiate_obj_vector, initiate_obj_ab
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          Vector@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor1( tSize ) RESULT( Obj )
  TYPE( IntVector_ ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor1
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE Constructor1
END INTERFACE IntVector

PUBLIC :: IntVector

!----------------------------------------------------------------------------
!                                                 Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( tSize ) RESULT( Obj )
  CLASS( IntVector_ ), POINTER :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tSize
END FUNCTION Constructor_1
END INTERFACE


INTERFACE IntVector_Pointer
  MODULE PROCEDURE Constructor_1
END INTERFACE IntVector_Pointer

PUBLIC :: IntVector_Pointer

!----------------------------------------------------------------------------
!                                                   Vector_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_Int( Val ) RESULT( Obj )
CLASS( IntVector_ ), POINTER :: Obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Int

MODULE PURE FUNCTION ConstructorInt( Val ) RESULT( Obj )
  TYPE( IntVector_ ) :: Obj
  INTEGER(I4B), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorInt
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE ConstructorInt
END INTERFACE IntVector

INTERFACE IntVector_Pointer
  MODULE PROCEDURE Constructor_Int
END INTERFACE IntVector_Pointer

INTERFACE
MODULE PURE FUNCTION Constructor_Real( Val ) RESULT( Obj )
  CLASS( IntVector_ ), POINTER :: Obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION Constructor_Real

MODULE PURE FUNCTION ConstructorReal( Val ) RESULT( Obj )
  TYPE( IntVector_ ) :: Obj
  REAL(DFP), INTENT( IN ) :: Val( : )
END FUNCTION ConstructorReal
END INTERFACE

INTERFACE IntVector
  MODULE PROCEDURE ConstructorReal
END INTERFACE IntVector

INTERFACE IntVector_Pointer
  MODULE PROCEDURE Constructor_Real
END INTERFACE IntVector_Pointer

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getValues_self( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
END FUNCTION f_getValues_self

MODULE PURE FUNCTION f_getSectionValues_self( Obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( IntVector_ ) :: Val
END FUNCTION f_getSectionValues_self

MODULE PURE FUNCTION f_getValuesFromTriplet_self( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( IntVector_ ) :: Val
END FUNCTION f_getValuesFromTriplet_self

MODULE PURE FUNCTION f_getARRAYValues_self( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
END FUNCTION f_getARRAYValues_self

MODULE PURE FUNCTION f_getARRAYSectionValues_self( Obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  TYPE( IntVector_ ) :: Val
END FUNCTION f_getARRAYSectionValues_self

MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_self( Obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ) :: Val
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

INTERFACE getValues
  MODULE PROCEDURE f_getValues_self, &
    & f_getSectionValues_self, &
    & f_getValuesFromTriplet_self, &
    & f_getARRAYValues_self, &
    & f_getARRAYSectionValues_self, &
    & f_getARRAYValuesFromTriplet_self
END INTERFACE getValues

PUBLIC :: getValues

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getSectionValues_Int( Obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getSectionValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getValuesFromTriplet_Int( Obj, iStart, iEnd, Stride,&
  & DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getValuesFromTriplet_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getARRAYValues_Int( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getARRAYSectionValues_Int( Obj, Indx, DataType ) &
  & RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYSectionValues_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION f_getARRAYValuesFromTriplet_Int( Obj, iStart, iEnd, &
  & Stride, &
  & DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  INTEGER(I4B), INTENT( IN ):: DataType
  INTEGER(I4B), ALLOCATABLE :: Val( : )
END FUNCTION f_getARRAYValuesFromTriplet_Int
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE ArrayValues
  MODULE PROCEDURE f_getValues_Int, &
    & f_getSectionValues_Int, &
    & f_getValuesFromTriplet_Int, &
    & f_getARRAYValues_Int, &
    & f_getARRAYSectionValues_Int, &
    & f_getARRAYValuesFromTriplet_Int
END INTERFACE ArrayValues

!----------------------------------------------------------------------------
!                                                       ArrayValues@GetMethod
!----------------------------------------------------------------------------

INTERFACE getValues
  MODULE PROCEDURE f_getValues_Int, &
    & f_getSectionValues_Int, &
    & f_getValuesFromTriplet_Int, &
    & f_getARRAYValues_Int, &
    & f_getARRAYSectionValues_Int, &
    & f_getARRAYValuesFromTriplet_Int
END INTERFACE getValues

!----------------------------------------------------------------------------
!                                                   ArrayPointers@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION f_getPointer_self( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ), TARGET :: Obj
  TYPE( IntVector_ ), INTENT( IN ) :: DataType
  TYPE( IntVector_ ), POINTER :: Val
END FUNCTION f_getPointer_self

MODULE FUNCTION f_getPointer_Int( Obj, DataType ) RESULT( Val )
  CLASS( IntVector_ ), INTENT( IN ), TARGET :: Obj
  INTEGER(I4B), INTENT( IN ) :: DataType
  INTEGER(I4B), POINTER :: Val( : )
END FUNCTION f_getPointer_Int
END INTERFACE

INTERFACE ArrayPointer
  MODULE PROCEDURE f_getPointer_self, f_getPointer_Int
END INTERFACE

PUBLIC :: ArrayPointer

!----------------------------------------------------------------------------
!                                                                 Convert
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
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION IndexOf_1( Obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value
  INTEGER( I4B ) :: Ans
END FUNCTION IndexOf_1
END INTERFACE

INTERFACE
MODULE PURE FUNCTION IndexOf_2( Obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION IndexOf_2
END INTERFACE

INTERFACE
MODULE PURE FUNCTION IndexOf_3( Obj, Value ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Value
  INTEGER( I4B ) :: Ans
END FUNCTION IndexOf_3
END INTERFACE

INTERFACE
MODULE PURE FUNCTION IndexOf_4( Obj, Value ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: Obj( : )
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION IndexOf_4
END INTERFACE

INTERFACE IndexOF
  MODULE PROCEDURE IndexOf_1, IndexOf_2, IndexOf_3, IndexOf_4
END INTERFACE

PUBLIC :: IndexOf

INTERFACE LOC
  MODULE PROCEDURE IndexOf_1, IndexOf_2, IndexOf_3, IndexOf_4
END INTERFACE LOC

PUBLIC :: LOC

!----------------------------------------------------------------------------
!                                                        isPresent@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isPresent_1( Obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value
  LOGICAL( LGT ) :: Ans
END FUNCTION isPresent_1
END INTERFACE

INTERFACE
MODULE PURE FUNCTION isPresent_2( Obj, Value ) RESULT( Ans )
  CLASS( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION isPresent_2
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE isPresent_1, isPresent_2
END INTERFACE isPresent

PUBLIC :: isPresent

! INTERFACE OPERATOR( .EQ. )
!   MODULE PROCEDURE isPresent_1, isPresent_2
! END INTERFACE

! PUBLIC :: OPERATOR( .EQ. )

!----------------------------------------------------------------------------
!                                                                      Append
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Append_1( Obj, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE Append_1
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE Append_2( Obj, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE Append_2
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE Append_3( Obj, AnotherObj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
  CLASS( IntVector_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE Append_3
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE Append_1, Append_2, Append_3
END INTERFACE Append

PUBLIC :: Append

!----------------------------------------------------------------------------
!                                                         setValue@SetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setValue_1( Obj, Indx, Value )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Indx( : )
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE setValue_1
END INTERFACE

INTERFACE SetValue
  MODULE PROCEDURE setValue_1
END INTERFACE

PUBLIC :: SetValue

!----------------------------------------------------------------------------
!                                                 RemoveDuplicate@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE remove_duplicates( Obj )
  CLASS( IntVector_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE remove_duplicates
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE remove_dupl_intvec( Obj )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
END SUBROUTINE remove_dupl_intvec
END INTERFACE

INTERFACE RemoveDuplicates
  MODULE PROCEDURE remove_duplicates, remove_dupl_intvec
END INTERFACE RemoveDuplicates

PUBLIC :: RemoveDuplicates

!----------------------------------------------------------------------------
!                                                           Repeat@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION repeat_int( Val, rtimes ) RESULT( Ans )
  INTEGER( I4B ), INTENT( IN ) :: Val( : )
  INTEGER( I4B ), INTENT( IN ) :: rtimes
  INTEGER( I4B ) :: Ans( SIZE( Val ) * rtimes )
END FUNCTION repeat_int
END INTERFACE

INTERFACE
MODULE PURE FUNCTION repeat_obj( Obj, rtimes ) RESULT( Ans )
  TYPE( IntVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: rtimes
  INTEGER( I4B ) :: Ans( SIZE( Obj % Val ) * rtimes )
END FUNCTION repeat_obj
END INTERFACE

INTERFACE Repeat
  MODULE PROCEDURE repeat_int, repeat_obj
END INTERFACE Repeat

PUBLIC :: Repeat

END MODULE IntVector_Method
