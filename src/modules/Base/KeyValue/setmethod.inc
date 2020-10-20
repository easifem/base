
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
!	Obj = 1.0_dfp
! ```

MODULE PURE SUBROUTINE SetValue1( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = vec1
! ```

MODULE PURE SUBROUTINE SetValue2( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = mat
! ```

MODULE PURE SUBROUTINE SetValue3( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = intval
! ```

MODULE PURE SUBROUTINE SetValue4( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = intvec
! ```

MODULE PURE SUBROUTINE SetValue5( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = intmat
! ```

MODULE PURE SUBROUTINE SetValue6( Obj, Value )
  CLASS( keyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = "hello"
! ```

MODULE PURE SUBROUTINE setKey1( Obj, Key )
  CLASS( KeyValue_ ), INTENT( INOUT ) :: Obj
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
!	Obj = string( "hello" )
! ```

MODULE PURE SUBROUTINE setKey2( Obj, Key )
  CLASS( KeyValue_ ), INTENT( INOUT ) :: Obj
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

MODULE PURE SUBROUTINE keyvalue_append( Obj, KeyValObj )
  TYPE( KeyValue_ ), ALLOCATABLE, INTENT( INOUT ) :: Obj( : )
  TYPE( KeyValue_), INTENT( IN ) :: KeyValObj
END SUBROUTINE keyvalue_append
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE keyvalue_append
END INTERFACE Append

PUBLIC :: Append
