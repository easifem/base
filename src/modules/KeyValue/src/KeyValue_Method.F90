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
! date: 4 March 2021
! summary: This module contains interfaces of methods related to [[keyvalue_]]
!
!### Usage
!
! ```fortran
! program main
! use easifem
! implicit none
! type( keyvalue_ ) :: obj
! real( dfp ) :: vec( 3 ), mat( 3, 3 )
! call random_number( vec )
! call random_number( mat )
! obj = keyvalue( 'real-rank-0', 1.0_dfp )
! obj = 1.0_dfp
! call display( obj, 'obj' )
! obj = keyvalue( 'real-rank-1', vec )
! obj = [1.0_dfp, 1.0_dfp, 1.0_dfp]
! call display( obj, 'obj' )
! obj = keyvalue( 'real-rank-2', mat )
! call display( obj, 'obj' )
! end program main
! ```

MODULE KeyValue_Method
USE BaseType
USE GlobalData
USE String_Class, ONLY:String
IMPLICIT NONE
PRIVATE
INTEGER, PARAMETER :: REAL_RANK_0 = 0
INTEGER, PARAMETER :: REAL_RANK_1 = 1
INTEGER, PARAMETER :: REAL_RANK_2 = 2
INTEGER, PARAMETER :: INT_RANK_0 = 3
INTEGER, PARAMETER :: INT_RANK_1 = 4
INTEGER, PARAMETER :: INT_RANK_2 = 5

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Real Rank 0`
!
!### Usage
!
! ```fortran
!	call initiate( obj, Key = 'E', Value = 1.0_dfp )
! ```

MODULE PURE SUBROUTINE Initiate1( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = string`
! - `Value = Real Rank 0`
!
!### Usage
!
! ```fortran
!	call initiate( obj, Key = string('E'), Value = 1.0_dfp )
! ```

MODULE PURE SUBROUTINE Initiate2( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Real Rank 1`

MODULE PURE SUBROUTINE Initiate3( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = String`
! - `Value = Real Rank 1`

MODULE PURE SUBROUTINE Initiate4( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Real Rank 2`

MODULE PURE SUBROUTINE Initiate5( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE Initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = String`
! - `Value = Real Rank 2`

MODULE PURE SUBROUTINE Initiate6( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE Initiate6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Int Rank 0`

MODULE PURE SUBROUTINE Initiate7( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE Initiate7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = String`
! - `Value = Int Rank 0`

MODULE PURE SUBROUTINE Initiate8( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE Initiate8
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Int Rank 1`

MODULE PURE SUBROUTINE Initiate9( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE Initiate9
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = string`
! - `Value = Int Rank 1`

MODULE PURE SUBROUTINE Initiate10( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE Initiate10
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = char`
! - `Value = Int Rank 2`

MODULE PURE SUBROUTINE Initiate11( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE Initiate11
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object
! - `Key = string`
! - `Value = Int Rank 2`

MODULE PURE SUBROUTINE Initiate12( obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE Initiate12
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that initiate instance of [[KeyValue_]]

!> authors: Dr. Vikas Sharma
!
! This suborutine constructs [[keyvalue_]] object

MODULE PURE SUBROUTINE Initiate13( obj, obj2 )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  CLASS( keyValue_ ), INTENT( IN ) :: obj2
END SUBROUTINE Initiate13
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE Initiate1, Initiate2, Initiate3, Initiate4, &
    & Initiate5, Initiate6, Initiate7, Initiate8, Initiate9, &
    & Initiate10, Initiate11, Initiate12, Initiate13
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Constructor1( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Constructor2( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END FUNCTION Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor3( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor4( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor5( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor5
end INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor6( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Constructor7( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END FUNCTION Constructor7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Constructor8( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END FUNCTION Constructor8
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor9( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor9
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor10( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor10
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor11( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor11
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  KeyValue
!----------------------------------------------------------------------------

INTERFACE
!! Function that constructs [[keyvalue_]]

MODULE PURE FUNCTION Constructor12( Key, Value ) RESULT( Ans )
  TYPE( keyValue_ ) :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor12
END INTERFACE

INTERFACE KeyValue
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3, &
    & Constructor4, Constructor5, Constructor6, Constructor7, &
    & Constructor8, Constructor9, Constructor10, Constructor11, &
    & Constructor12
END INTERFACE KeyValue

PUBLIC :: KeyValue


INTERFACE
MODULE PURE FUNCTION Contains2( obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Contains2
END INTERFACE

INTERFACE OPERATOR( .CONTAINS. )
  MODULE PROCEDURE Present1, Present2, Contains1, Contains2
END INTERFACE OPERATOR( .CONTAINS. )

PUBLIC :: OPERATOR( .CONTAINS. )

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_1( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END FUNCTION Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_2( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value
END FUNCTION Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_3( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor_3
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_4( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor_4
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_5( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor_5
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_6( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor_6
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_7( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END FUNCTION Constructor_7
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_8( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value
END FUNCTION Constructor_8
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_9( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor_9
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_10( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END FUNCTION Constructor_10
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_11( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor_11
END INTERFACE

!----------------------------------------------------------------------------
!                                               KeyValue_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_12( Key, Value ) RESULT( Ans )
  CLASS( keyValue_ ), POINTER :: Ans
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END FUNCTION Constructor_12
END INTERFACE

INTERFACE KeyValue_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2, Constructor_3, &
    & Constructor_4, Constructor_5, Constructor_6, Constructor_7, &
    & Constructor_8, Constructor_9, Constructor_10, Constructor_11, &
    & Constructor_12
END INTERFACE KeyValue_Pointer

PUBLIC :: KeyValue_Pointer

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine display contents of [[keyvalue_]]

MODULE SUBROUTINE keyvalue_display( obj, msg, UnitNo )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE keyvalue_display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

INTERFACE
!! Display content of vector of [[keyvalue_]]

MODULE SUBROUTINE mp_display( obj, msg, unitno )
  TYPE( keyvalue_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE mp_display
END INTERFACE

!> Generic subroutine to display content of [[keyvalue_]]
INTERFACE Display
  MODULE PROCEDURE keyvalue_display, mp_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal1( obj, Key ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal2( Key, obj ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal3( obj, Key ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal4( Key, obj ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal4
END INTERFACE

INTERFACE OPERATOR( .EQ. )
  MODULE PROCEDURE Equal1, Equal2, Equal3, Equal4
END INTERFACE OPERATOR( .EQ. )

PUBLIC :: OPERATOR( .EQ. )

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE keyvalue_deallocate( obj )
  CLASS( KeyValue_ ), INTENT( INOUT ) :: obj
END SUBROUTINE keyvalue_deallocate
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE keyvalue_deallocate
END INTERFACE DeallocateData

PUBLIC :: DeallocateData



!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set real scalar value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = 1.0_dfp
! ```

MODULE PURE SUBROUTINE SetValue1( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value
END SUBROUTINE SetValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set real vector value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = vec1
! ```

MODULE PURE SUBROUTINE SetValue2( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( : )
END SUBROUTINE SetValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set real matrix value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = mat
! ```

MODULE PURE SUBROUTINE SetValue3( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE SetValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set integer scalar value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = intval
! ```

MODULE PURE SUBROUTINE SetValue4( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value
END SUBROUTINE SetValue4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set integer vector value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = intvec
! ```

MODULE PURE SUBROUTINE SetValue5( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value( : )
END SUBROUTINE SetValue5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set value in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set integer matrix value in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = intmat
! ```

MODULE PURE SUBROUTINE SetValue6( obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Value( :, : )
END SUBROUTINE SetValue6
END INTERFACE

!> Generic subroutine to set values in [[keyvalue_]]
INTERFACE setValue
  MODULE PROCEDURE SetValue1, SetValue2, SetValue3, SetValue4, &
    & SetValue5, SetValue6
END INTERFACE setValue

PUBLIC :: setValue

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE SetValue1, SetValue2, SetValue3, SetValue4, &
    & SetValue5, SetValue6
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set key in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set key in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = "hello"
! ```

MODULE PURE SUBROUTINE setKey1( obj, Key )
  CLASS( KeyValue_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
END SUBROUTINE setKey1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

INTERFACE
!! Set key in [[keyvalue_]] object

!> authors: Dr. Vikas Sharma
!
! This subroutine set key in [[keyvalue_]]
!
!### Usage
!
! ```fortran
!	obj = string( "hello" )
! ```

MODULE PURE SUBROUTINE setKey2( obj, Key )
  CLASS( KeyValue_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Key
END SUBROUTINE setKey2
END INTERFACE

!> Generic subroutine to set key in [[keyvalue_]]
INTERFACE setKey
  MODULE PROCEDURE setKey1, setKey2
END INTERFACE setKey

PUBLIC :: setKey

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE setKey1, setKey2
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

INTERFACE
!! Subroutine that append `keyvalue_` instance to an array of [[keyvalue_]]
!! type

MODULE PURE SUBROUTINE keyvalue_append( obj, KeyValobj )
  TYPE( KeyValue_ ), ALLOCATABLE, INTENT( INOUT ) :: obj( : )
  TYPE( KeyValue_), INTENT( IN ) :: KeyValobj
END SUBROUTINE keyvalue_append
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE keyvalue_append
END INTERFACE Append

PUBLIC :: Append


!----------------------------------------------------------------------------
!                                                                    getKey
!----------------------------------------------------------------------------

INTERFACE
!! get key from [[keyvalue_]]

MODULE PURE SUBROUTINE getKey1( Key, obj )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( INOUT ) :: Key
END SUBROUTINE getKey1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    getKey
!----------------------------------------------------------------------------

INTERFACE
!! get key from [[keyvalue_]]

MODULE PURE SUBROUTINE getKey2( Key, obj )
  CLASS( KeyValue_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( INOUT ) :: Key
END SUBROUTINE getKey2
END INTERFACE

!> Generic subroutine to get key from [[keyvalue_]]
INTERFACE getKey
  MODULE PROCEDURE getKey1, getKey2
END INTERFACE getKey

PUBLIC :: getKey

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE getKey1, getKey2
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue1( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( INOUT ) :: Value
END SUBROUTINE getValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue2( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Value( : )
END SUBROUTINE getValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue3( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Value( :, : )
END SUBROUTINE getValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue4( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( INOUT ) :: Value
END SUBROUTINE getValue4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue5( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Value( : )
END SUBROUTINE getValue5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue6( Value, obj )
  CLASS( keyValue_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Value( :, : )
END SUBROUTINE getValue6
END INTERFACE

!> Generic subroutine to get value from [[keyvalue_]]
INTERFACE getValue
  MODULE PROCEDURE getValue1, getValue2, getValue3, getValue4, &
    & getValue5, getValue6
END INTERFACE getValue

PUBLIC :: getValue

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE getValue1, getValue2, getValue3, getValue4, &
    & getValue5, getValue6
END INTERFACE ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                      Index
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Index1( obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ) :: Ans
END FUNCTION Index1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Index
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Index2( obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  TYPE( String ), INTENT( IN ) :: Key
  INTEGER( I4B ) :: Ans
END FUNCTION Index2
END INTERFACE

INTERFACE IndexOf
  MODULE PROCEDURE Index1, Index2
END INTERFACE IndexOf

PUBLIC :: IndexOf

INTERFACE OPERATOR( .INDEX. )
  MODULE PROCEDURE Index1, Index2
END INTERFACE OPERATOR( .INDEX. )

PUBLIC :: OPERATOR( .INDEX. )

!----------------------------------------------------------------------------
!                                                                     Present
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Present1( Key, obj ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Present1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Present
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Present2( Key, obj ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Present2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Contains1( obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Contains1
END INTERFACE

!------------------------------------------------------------------------------
!                                                                     Contains2
!------------------------------------------------------------------------------
END MODULE KeyValue_Method