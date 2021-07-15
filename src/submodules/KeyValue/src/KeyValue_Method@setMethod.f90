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

SUBMODULE( KeyValue_Method ) setMethod
  !! This submodule includes implementation of method to set values in
  !! [[keyvalue_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setKey1
  obj%Key = Key
END PROCEDURE setKey1

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE setKey2
  obj%Key = Key
END PROCEDURE setKey2

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue1
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue1

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue2
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue2

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue3
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue3

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue4
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue4

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue5
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue5

!----------------------------------------------------------------------------
!                                                                   setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE SetValue6
  CALL Initiate( obj, obj%Key, Value )
END PROCEDURE SetValue6

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE keyvalue_append
  INTEGER( I4B ) :: I, Indx, tSize
  LOGICAL( LGT ) :: isPresent

  IF( .NOT. ALLOCATED( obj ) ) THEN
    ALLOCATE( obj( 1 ) )
    obj( 1 ) = KeyValobj
  ELSE
    tSize = SIZE( obj )
    DO  I = 1, tSize
      isPresent = obj( I ) .EQ. KeyValobj%Key
      IF( isPresent ) THEN
        Indx = I
        EXIT
      END IF
    END DO

    IF( isPresent ) THEN

      obj( Indx ) = KeyValobj

    ELSE

      BLOCK
        TYPE( keyvalue_ ) :: Dummyobj( tSize )

        DO I = 1, tSize
          Dummyobj( I ) = obj( I )
        END DO

        DEALLOCATE( obj )
        ALLOCATE( obj( tSize + 1 ) )

        DO I = 1, tSize
          obj( I ) = Dummyobj( I )
        END DO

        obj( tSize + 1 ) = KeyValobj

      END BLOCK
    END IF
  END IF

END PROCEDURE keyvalue_append

END SUBMODULE setMethod