
!----------------------------------------------------------------------------
!                                                                    getKey
!----------------------------------------------------------------------------

INTERFACE
!! get key from [[keyvalue_]]

MODULE PURE SUBROUTINE getKey1( Key, Obj )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( INOUT ) :: Key
END SUBROUTINE getKey1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    getKey
!----------------------------------------------------------------------------

INTERFACE
!! get key from [[keyvalue_]]

MODULE PURE SUBROUTINE getKey2( Key, Obj )
  CLASS( KeyValue_ ), INTENT( IN ) :: Obj
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

MODULE PURE SUBROUTINE getValue1( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: Value
END SUBROUTINE getValue1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue2( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Value( : )
END SUBROUTINE getValue2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue3( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Value( :, : )
END SUBROUTINE getValue3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue4( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( INOUT ) :: Value
END SUBROUTINE getValue4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue5( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: Value( : )
END SUBROUTINE getValue5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getValue
!----------------------------------------------------------------------------

INTERFACE
!! getValue from [[keyvalue_]]

MODULE PURE SUBROUTINE getValue6( Value, Obj )
  CLASS( keyValue_ ), INTENT( IN ) :: Obj
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
MODULE PURE FUNCTION Index1( Obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  INTEGER( I4B ) :: Ans
END FUNCTION Index1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Index
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Index2( Obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
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
MODULE PURE FUNCTION Present1( Key, Obj ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Present1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Present
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Present2( Key, Obj ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Present2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Contains
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Contains1( Obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Contains1
END INTERFACE

!------------------------------------------------------------------------------
!                                                                     Contains2
!------------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Contains2( Obj, Key ) RESULT( Ans )
  TYPE( KeyValue_ ), INTENT( IN ) :: Obj( : )
  TYPE( String ), INTENT( IN ) :: Key
  LOGICAL( LGT ) :: Ans
END FUNCTION Contains2
END INTERFACE

INTERFACE OPERATOR( .CONTAINS. )
  MODULE PROCEDURE Present1, Present2, Contains1, Contains2
END INTERFACE OPERATOR( .CONTAINS. )

PUBLIC :: OPERATOR( .CONTAINS. )
