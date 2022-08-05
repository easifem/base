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

!> author: Vikas Sharma, Ph. D.
! date: 	4 March 2021
! summary: This submodule contains implementation of construction methods [[keyvalue_]]

SUBMODULE(KeyValue_Method) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS
!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate1
  obj%DataType = Real_Rank_0
  obj%Key = Key
  obj%Value = RESHAPE( [Value], [1,1] )
END PROCEDURE Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate2
  obj%DataType = Real_Rank_0
  obj%Key = Key
  obj%Value = RESHAPE( [Value], [1,1] )
END PROCEDURE Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate3
  obj%DataType =  Real_Rank_1
  obj%Key = Key
  obj%Value = RESHAPE( Value, [SIZE( Value ), 1] )
END PROCEDURE Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate4
  obj%DataType = Real_Rank_1
  obj%Key = Key
  obj%Value = RESHAPE( Value, [SIZE( Value ), 1] )
END PROCEDURE Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate5
  obj%DataType = Real_Rank_2
  obj%Key = Key
  obj%Value = Value
END PROCEDURE Initiate5

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate6
  obj%DataType = Real_Rank_2
  obj%Key = Key
  obj%Value = Value
END PROCEDURE Initiate6

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate7
  obj%DataType = Int_Rank_0
  obj%Key = Key
  obj%Value = RESHAPE( [Value], [1,1] )
END PROCEDURE Initiate7

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate8
  obj%DataType = Int_Rank_0
  obj%Key = Key
  obj%Value = REAL( RESHAPE( [Value], [1,1] ), DFP )
END PROCEDURE Initiate8

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate9
  obj%DataType = Int_Rank_1
  obj%Key = Key
  obj%Value = REAL( RESHAPE( Value, [SIZE( Value ), 1 ] ), DFP )
END PROCEDURE Initiate9

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate10
  obj%DataType = Int_Rank_1
  obj%Key = Key
  obj%Value = REAL( RESHAPE( Value, [SIZE( Value ), 1 ] ), DFP )
END PROCEDURE Initiate10

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate11
  obj%DataType = Int_Rank_2
  obj%Key = Key
  obj%Value = REAL( Value, DFP )
END PROCEDURE Initiate11

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate12
  obj%DataType = Int_Rank_2
  obj%Key = Key
  obj%Value = REAL( Value, DFP )
END PROCEDURE Initiate12

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate13
  obj%DataType = obj2%DataType
  obj%Key = obj2%Key
  IF( ALLOCATED( obj2%Value ) ) THEN
    obj%Value = obj2%Value
  END IF
END PROCEDURE Initiate13

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor3

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor4
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor4

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor5
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor5

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor6
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor6

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor7
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor7

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor8
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor8

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor9
  CALL Initiate( Ans, Key, Value )
END PROCEDURE Constructor9

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor10
  CALL  Initiate(Ans,  Key, Value )
END PROCEDURE Constructor10

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor11
  CALL  Initiate(Ans,  Key, Value )
END PROCEDURE Constructor11

!----------------------------------------------------------------------------
!                                                                 KeyValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor12
  CALL  Initiate(Ans,  Key, Value )
END PROCEDURE Constructor12

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE keyvalue_display
  INTEGER( I4B ) :: I
  CHARACTER( LEN = 6 ) :: s

  I = stdout

  IF( PRESENT( UnitNo ) ) I = UnitNo
  SELECT CASE( obj%DataType )
  CASE( REAL_RANK_0 )
    s = "Rank0"
  CASE( REAL_RANK_1 )
    s = "Rank1"
  CASE( REAL_RANK_2 )
    s = "Rank2"
  CASE( INT_RANK_0 )
    s = "Rank0"
  CASE( INT_RANK_1 )
    s = "Rank1"
  CASE( INT_RANK_2 )
    s = "Rank2"
  END SELECT

  IF( LEN_TRIM( msg ) .NE.  0 ) CALL Display( msg, I )
  IF( ALLOCATED( obj%Value ) ) THEN
    CALL Display( obj%Value, &
      & s // " :: " // TRIM( obj%Key%Raw ) // " :: ", UnitNo = I )
  ELSE
    CALL Display(  s // " :: " // TRIM( obj%Key%Raw ) // " :: ", UnitNo = I )
  END IF
END PROCEDURE keyvalue_display

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE mp_display
  INTEGER( I4B ) :: n, i, j

  I = stdout
  IF( PRESENT( UnitNo ) ) I = UnitNo
  n = SIZE( obj )
  CALL BlankLines( UnitNo = I )
  DO j = 1, n
    CALL display( obj( j ), msg, UnitNo = I )
    CALL BlankLines( UnitNo = I )
  END DO

END PROCEDURE mp_display

!----------------------------------------------------------------------------
!                                                                      Equal
!----------------------------------------------------------------------------

MODULE PROCEDURE Equal1
  Ans = obj%Key .EQ. String( Key )
END PROCEDURE Equal1

!----------------------------------------------------------------------------
!                                                                      Equal
!----------------------------------------------------------------------------

MODULE PROCEDURE Equal2
  Ans = obj%Key .EQ. String( Key )
END PROCEDURE Equal2

!----------------------------------------------------------------------------
!                                                                      Equal
!----------------------------------------------------------------------------

MODULE PROCEDURE Equal3
  Ans = obj%Key .EQ. Key
END PROCEDURE Equal3

!----------------------------------------------------------------------------
!                                                                      Equal
!----------------------------------------------------------------------------

MODULE PROCEDURE Equal4
  Ans = obj%Key .EQ. Key
END PROCEDURE Equal4

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE keyvalue_deallocate
  IF(  ALLOCATED( obj%Value )  ) DEALLOCATE( obj%Value )
END PROCEDURE keyvalue_deallocate

END SUBMODULE Constructor

! CONTAINS

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_1( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_1

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_2( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_2

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_3( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value( : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_3

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_4( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value( : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_4

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_5( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value( :, : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_5

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_6( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   REAL( DFP ), INTENT( IN ) :: Value( :, : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_6

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_7( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_7

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_8( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_8

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_9( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value( : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_9

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_10( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value( : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_10

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_11( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   CHARACTER( LEN = * ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value( :, : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_11

! !----------------------------------------------------------------------------
! !                                                            KeyValue_Pointer
! !----------------------------------------------------------------------------

! FUNCTION Constructor_12( Key, Value ) RESULT( Ans )
!   CLASS( keyValue_ ), POINTER :: Ans
!   TYPE( String ), INTENT( IN ) :: Key
!   INTEGER( I4B ), INTENT( IN ) :: Value( :, : )

!   ALLOCATE( Ans )
!   CALL Initiate( Ans, Key, Value )
! END FUNCTION Constructor_12