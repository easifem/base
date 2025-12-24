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

MODULE FPL_Utils
USE PENF, ONLY: I1P, I4P
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-02
! summary: Procedure for computing the number of bytes of a logical variable.

ELEMENTAL FUNCTION byte_size_logical(l) RESULT(bytes)
  LOGICAL, INTENT(IN) :: l
  !! Character variable whose number of bits must be computed.
  INTEGER(I4P) :: bytes
  !! Number of bits of l.
  INTEGER(I1P) :: mold(1)
  !! "Molding" dummy variable for bits counting.
  bytes = SIZE(TRANSFER(l, mold), dim=1, kind=I1P)
  RETURN
END FUNCTION byte_size_logical

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FPL_Utils
