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
! date: 	3 April 2021
! summary: 	This method contains the input method

SUBMODULE( Utility ) Input
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Int
  IF(PRESENT(option) )THEN
    Ans=option
  ELSE
    Ans=default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Real
  IF(PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_IntVec
  IF( PRESENT( option ) ) THEN
    val=option
  ELSE
    val=default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_Realvec
  IF( PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_IntArray
  IF(PRESENT(option) )THEN
    val = option
  ELSE
    val = default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_RealArray
  IF(PRESENT(option) )THEN
    val = option
  ELSE
    val = default
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_String
  IF(PRESENT(option) )THEN
    val=TRIM(option)
  ELSE
    val=TRIM(default)
  ENDIF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      Input
!----------------------------------------------------------------------------

MODULE PROCEDURE input_logical
  IF(PRESENT(option) )THEN
    val=option
  ELSE
    val=default
  ENDIF
END PROCEDURE


END SUBMODULE Input
