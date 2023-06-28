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

MODULE PROCEDURE arange_Int8
! Internal var
INTEGER(INT8) :: incr
INTEGER(INT8) :: i
INTEGER(INT8) :: n
incr = INPUT(default=1_Int8, option=increment)
n = (iend - istart) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_Int8

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_Int16
! Internal var
INTEGER(INT16) :: incr
INTEGER(INT16) :: i
INTEGER(INT16) :: n
incr = INPUT(default=1_Int16, option=increment)
n = (iend - istart) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_Int16

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_Int32
! Internal var
INTEGER(INT32) :: incr
INTEGER(INT32) :: i
INTEGER(INT32) :: n
incr = INPUT(default=1_Int32, option=increment)
n = (iend - istart) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_Int32
!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_Int64
! Internal var
INTEGER(INT64) :: incr
INTEGER(INT64) :: i
INTEGER(INT64) :: n
incr = INPUT(default=1_Int64, option=increment)
n = (iend - istart) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_Int64

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real64
! internal var
REAL(REAL64) :: incr
INTEGER(I4B) :: i
INTEGER(I4B) :: n
  !!
incr = INPUT(Default=1.0_REAL64, Option=increment)
  !!
n = (iend - istart + 0.5_REAL64 * incr) / incr + 1
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
REAL(REAL32) :: incr
INTEGER(I4B) :: i
INTEGER(I4B) :: n
  !!
incr = INPUT(Default=1.0_REAL32, Option=increment)
  !!
n = (iend - istart + 0.5_REAL32 * incr) / incr + 1
ALLOCATE (Ans(n))
DO CONCURRENT(i=1:n)
  Ans(i) = istart + (i - 1) * incr
END DO
END PROCEDURE arange_real32

END SUBMODULE Methods
