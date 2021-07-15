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

SUBMODULE( KeyValue_Method ) getMethod
  !! This submodule includes implementation of method to set values in
  !! [[keyvalue_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                     getKey
!----------------------------------------------------------------------------

MODULE PROCEDURE getKey1
  Key = TRIM( obj%Key%Raw )
END PROCEDURE getKey1

!----------------------------------------------------------------------------
!                                                                     getKey
!----------------------------------------------------------------------------

MODULE PROCEDURE getKey2
  Key = obj%Key
END PROCEDURE getKey2

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue1
  Value = obj%Value( 1, 1 )
END PROCEDURE getValue1

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue2
  Value = obj%Value( :, 1 )
END PROCEDURE getValue2

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue3
  Value = obj%Value
END PROCEDURE getValue3

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue4
  Value = INT( obj%Value( 1, 1 ) )
END PROCEDURE getValue4

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue5
  Value = INT( obj%Value( :, 1 ) )
END PROCEDURE getValue5

!----------------------------------------------------------------------------
!                                                                   getValue
!----------------------------------------------------------------------------

MODULE PROCEDURE getValue6
  Value = INT( obj%Value )
END PROCEDURE getValue6

!----------------------------------------------------------------------------
!                                                                     INDEX
!----------------------------------------------------------------------------

MODULE PROCEDURE Index1
  INTEGER( I4B ) :: I
  Ans = 0
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = I
      EXIT
    END IF
  END DO
END PROCEDURE Index1

!----------------------------------------------------------------------------
!                                                                     INDEX
!----------------------------------------------------------------------------

MODULE PROCEDURE Index2
  INTEGER( I4B ) :: I
  Ans = 0
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = I
      EXIT
    END IF
  END DO
END PROCEDURE Index2

!----------------------------------------------------------------------------
!                                                                   Present
!----------------------------------------------------------------------------

MODULE PROCEDURE Present1
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Present1

!----------------------------------------------------------------------------
!                                                                   Present
!----------------------------------------------------------------------------

MODULE PROCEDURE Present2
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Present2

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

MODULE PROCEDURE Contains1
  INTEGER( I4B ) :: I

  Ans = .FALSE.
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO

END PROCEDURE Contains1

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

MODULE PROCEDURE Contains2
  INTEGER( I4B ) :: I
  Ans = .FALSE.
  DO I = 1, SIZE( obj )
    IF( obj( I ) .EQ. Key  ) THEN
      Ans = .TRUE.
      EXIT
    END IF
  END DO
END PROCEDURE Contains2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE getMethod
