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
! date:         7 March 2021
! summary: This module contains IO methods for [[RealMatrix_]]

SUBMODULE(RealMatrix_Method) IOMethods
USE Display_Method, ONLY: UtilDisplay => Display
USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display1
CALL UtilDisplay(obj%val, msg, unitno=unitno)
END PROCEDURE obj_Display1

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display2
INTEGER(I4B) :: j

DO j = 1, SIZE(obj)
  CALL UtilDisplay(obj(j)%val, &
                   TRIM(msg)//' ('//ToString(j)//'): ', &
                   unitno=unitno)
END DO
END PROCEDURE obj_Display2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
