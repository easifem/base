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
USE Display_Method, ONLY: Util_Display => Display, Tostring
USE MdEncode_Method, ONLY: Util_MdEncode => MdEncode

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
LOGICAL(LGT) :: isok

CALL Util_Display(msg, unitno=unitno)

isok = ALLOCATED(obj%points)
IF (.NOT. isok) RETURN

CALL Util_Display(obj%points, msg="points:", unitno=unitno)
CALL Util_Display(obj%txi, msg="txi:", unitno=unitno)

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 MdEncode
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MdEncode
INTEGER(I4B) :: ii, n, jj
TYPE(String), ALLOCATABLE :: rh(:), ch(:)

IF (.NOT. ALLOCATED(obj%points)) THEN
  ans = ""
  RETURN
END IF

n = SIZE(obj%points, 2)
ii = SIZE(obj, 1)
jj = SIZE(obj, 2)

ALLOCATE (rh(ii), ch(jj))

DO ii = 1, SIZE(rh) - 1
  rh(ii) = "`x"//tostring(ii)//"`"
END DO
rh(obj%txi + 1) = "w"

DO ii = 1, SIZE(ch)
  ch(ii) = "`p"//tostring(ii)//"`"
END DO

ans = Util_MdEncode(obj%points, rh=rh, ch=ch)

END PROCEDURE obj_MdEncode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
