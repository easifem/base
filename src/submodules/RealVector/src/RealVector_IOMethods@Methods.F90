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
! date:         25 Feb 2021
! summary:         This submodule implements IO methods of [[RealVector_]]

SUBMODULE(RealVector_IOMethods) Methods
USE Display_Method, ONLY: Util_Display => Display, &
                          tostring

USE RealVector_ConstructorMethods, ONLY: size

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_display1
INTEGER(I4B) :: tsize
CALL Util_Display(msg=msg, unitno=unitno)
tsize = SIZE(obj)
CALL Util_Display(msg="size: "//tostring(tsize), unitno=unitno)
CALL Util_Display(val=obj%val, msg='', unitno=unitno, orient='col', &
                  full=.TRUE.)
END PROCEDURE obj_display1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_display2
INTEGER(I4B) :: j, tsize

tsize = SIZE(obj)
CALL Util_Display(msg=msg, unitno=unitno)
CALL Util_Display(msg="size : "//tostring(tsize), unitno=unitno)

DO j = 1, tsize
  CALL Display(obj(j), msg="("//tostring(j)//"): ", unitno=unitno)
END DO

END PROCEDURE obj_display2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
