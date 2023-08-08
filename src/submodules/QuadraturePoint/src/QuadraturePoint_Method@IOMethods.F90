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
! date: 3 March 2021
! summary: This submodule contains the IO method for [[QuadraturePoint_]]

SUBMODULE(QuadraturePoint_Method) IOMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE quad_Display
CALL Display(msg, unitno=unitno)
IF (.NOT. ALLOCATED(obj%points)) THEN
  RETURN
END IF
CALL Display(obj%points, msg="# points :", unitno=unitno)
CALL Display(obj%txi, msg="# txi :", unitno=unitno)
END PROCEDURE quad_Display

!----------------------------------------------------------------------------
!                                                                 MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_MdEncode
INTEGER(I4B) :: ii, n, jj

n = SIZE(obj%points, 2)

ans = "|  |  "
DO ii = 1, n
  ans = ans//" | "
END DO
ans = ans//CHAR_LF

ans = ans//"| --- | "
DO ii = 1, n
  ans = ans//" --- | "
END DO
ans = ans//CHAR_LF

DO ii = 1, obj%txi
  ans = ans//"| x"//tostring(ii)//" | "
  DO jj = 1, n
    ans = ans//TOSTRING(obj%points(ii, jj))//" | "
  END DO
  ans = ans//CHAR_LF
END DO

ans = ans//"| w | "
DO jj = 1, n
  ans = ans//TOSTRING(obj%points(obj%txi + 1, jj))//" | "
END DO
ans = ans//CHAR_LF

END PROCEDURE QuadraturePoint_MdEncode

END SUBMODULE IOMethods
