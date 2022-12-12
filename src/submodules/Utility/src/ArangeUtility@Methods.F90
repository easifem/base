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

SUBMODULE(ArangeUtility) Methods
USE BaseMethod, ONLY: INPUT
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_int
! Internal var
integer(i4b) :: incr
integer(i4b) :: i
integer(i4b) :: n
incr = INPUT(default=1, option=increment)
n = (iend - istart) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
end do
END PROCEDURE arange_int

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real64
! internal var
REAL(Real64) :: incr
INTEGER(I4B) :: i
INTEGER(I4B) :: n
  !!
incr = INPUT(Default=1.0_Real64, Option=increment)
  !!
n = (iend - istart + 0.5_Real64 * incr) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_real64

!----------------------------------------------------------------------------
!                                                                     arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real32
! internal var
REAL(Real32) :: incr
INTEGER(I4B) :: i
INTEGER(I4B) :: n
  !!
incr = INPUT(Default=1.0_Real32, Option=increment)
  !!
n = (iend - istart + 0.5_Real32 * incr) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_real32

END SUBMODULE Methods
