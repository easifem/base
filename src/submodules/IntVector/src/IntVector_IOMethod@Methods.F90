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

SUBMODULE(IntVector_IOMethod) Methods
USE Display_Method, ONLY: ToString
USE Display_Method, ONLY: Display0 => Display
USE IntVector_EnquireMethod, ONLY: IsAllocated
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display1
INTEGER(I4B) :: j
CALL Display0(msg=msg, unitno=unitno)
CALL Display0(val=SIZE(obj), msg="size: ", unitno=unitno)
DO j = 1, SIZE(obj)
  CALL Display(obj(j), msg= "("//ToString(j)//")", &
               unitno=unitno, orient=orient)
END DO
END PROCEDURE obj_Display1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display2
CALL Display0(msg, unitno=unitno)
CALL Display0(IsAllocated(obj), "ALLOCATED: ", unitno=unitno)
IF (IsAllocated(obj)) THEN
  CALL Display0(val=obj%tsize, msg="tsize: ", unitno=unitno)
  CALL Display0(val=obj%capacity, msg="capacity: ", unitno=unitno)
  CALL Display0(val=obj%tDimension, msg="tDimension: ", unitno=unitno)
  CALL Display0(val=obj%val, msg='', unitno=unitno, orient=orient)
END IF
END PROCEDURE obj_Display2

END SUBMODULE Methods
