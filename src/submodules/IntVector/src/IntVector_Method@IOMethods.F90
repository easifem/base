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

!> author: Vikas Sharma, Ph. D.
! date:         28 Feb 2021
! summary:  This contains Input/Output methods for [[IntVector_]]

SUBMODULE(IntVector_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_Display1
INTEGER(I4B) :: j
CALL Display(msg="# "//TRIM(msg), unitNo=unitNo)
CALL Display(msg="# size : ", val=SIZE(obj), unitNo=unitNo)
DO j = 1, SIZE(obj)
  CALL Display(obj(j),  &
    & msg="# "//TRIM(msg)//"( "   &
    & //TOSTRING(j)//" ) ", &
    & unitNo=UnitNo, orient=orient)
END DO
END PROCEDURE intVec_Display1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE intVec_Display2
IF (isAllocated(obj)) THEN
  CALL Display(msg="# "//TRIM(msg), unitNo=unitNo)
  CALL Display(msg="# size : ", val=SIZE(obj), unitNo=unitNo)
  CALL Display(Val=obj%Val, msg='', unitNo=unitNo, orient=orient)
END IF
END PROCEDURE intVec_Display2

END SUBMODULE IOMethods
