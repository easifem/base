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

SUBMODULE( FEVariable_Method ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Display_Obj
  INTEGER( I4B ) :: I

  I = stdout
  IF( PRESENT( UnitNo ) ) I = UnitNo
  IF( LEN_TRIM( Msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( Msg )

  SELECT CASE( Obj%Rank )
  CASE( Scalar )
    WRITE( I, "(A)") "RANK :: 0 (SCALAR)"

    SELECT CASE( Obj%VarType )
    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj%R0, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj%R1, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj%R2, 'VALUE' )
    END SELECT

  CASE( Vector )
    WRITE( I, "(A)") "RANK :: 1 (VECTOR)"

    SELECT CASE( Obj%VarType )
    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj%R1, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj%R2, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj%R3, 'VALUE' )
    END SELECT

  CASE( Matrix )
    WRITE( I, "(A)") "RANK :: 2 (MATRIX)"

    SELECT CASE( Obj%VarType )
    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj%R2, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj%R3, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj%R4, 'VALUE' )
    END SELECT
  END SELECT
END PROCEDURE Display_Obj

END SUBMODULE IO