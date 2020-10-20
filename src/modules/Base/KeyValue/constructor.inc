
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
!	call initiate( Obj, Key = 'E', Value = 1.0_dfp )
! ```

MODULE PURE SUBROUTINE Initiate1( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	call initiate( Obj, Key = string('E'), Value = 1.0_dfp )
! ```

MODULE PURE SUBROUTINE Initiate2( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate3( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate4( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate5( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate6( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate7( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate8( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate9( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate10( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate11( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate12( Obj, Key, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE Initiate13( Obj, Obj2 )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
  CLASS( keyValue_ ), INTENT( IN ) :: Obj2
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

!----------------------------------------------------------------------------
!                                                           KeyValue_Pointer
!----------------------------------------------------------------------------

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

MODULE SUBROUTINE keyvalue_display( Obj, msg, UnitNo )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE keyvalue_display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Display
!----------------------------------------------------------------------------

INTERFACE
!! Display content of vector of [[keyvalue_]]

MODULE SUBROUTINE mp_display( Obj, msg, unitno )
  TYPE( keyvalue_ ), INTENT( IN ) :: Obj( : )
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

MODULE ELEMENTAL FUNCTION Equal1( Obj, Key ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal2( Key, Obj ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal3( Obj, Key ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Equal3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Equal
!----------------------------------------------------------------------------

INTERFACE
!! Function to check equality in [[keyvalue_]]

MODULE ELEMENTAL FUNCTION Equal4( Key, Obj ) RESULT( Ans )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
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
MODULE PURE SUBROUTINE keyvalue_deallocate( Obj )
  CLASS( KeyValue_ ), INTENT( INOUT) :: Obj
END SUBROUTINE keyvalue_deallocate
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE keyvalue_deallocate
END INTERFACE DeallocateData

PUBLIC :: DeallocateData
