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
! date: 3 March 2021
! summary: This submodule contains the IO method for [[QuadraturePoint_]]

SUBMODULE (QuadraturePoint_Method) IO
Use BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Display
  INTEGER( I4B ) :: I, j

  IF( .NOT. ALLOCATED( obj%Points ) ) RETURN
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = stdout
  END IF
  CALL Blanklines( UnitNo = I )
  CALL Display( msg, UnitNo = I )
  CALL Blanklines( UnitNo = I )
  WRITE( I, "(A)" ) "          Weights    |      Points       "
  WRITE( I, "(A)" ) "-----------------------------------------"
  DO j = 1, SIZE( obj%Points, 2 )
    WRITE( I,"( 2X, G15.8, 2X" // TRIM( INT2STR( obj%tXi ) ) // "G15.8 )")&
      & obj%Points( obj%tXi + 1, j ), &
      & obj%Points( 1 : obj%tXi, j )
  END DO
  WRITE( I, "(A)" ) "-----------------------------------------"
END PROCEDURE quad_Display

END SUBMODULE IO