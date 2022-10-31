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

INTEGER(I4B) :: n, ii
  !!
n = SIZE(vec)
!!
IF (n .EQ. 1) RETURN
!!
IF (pos .GT. n) THEN
  ans = vec(1:n - 1)
  RETURN
END IF
!!
IF (pos .LT. 1_I4B) THEN
  ans = vec(2:n)
  RETURN
END IF
!!
DO ii = 1, pos - 1
  ans(ii) = vec(ii)
END DO

DO ii = pos, n - 1
  ans(ii) = vec(ii + 1)
END DO
