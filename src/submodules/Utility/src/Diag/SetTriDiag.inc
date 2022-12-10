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

INTEGER(I4B) :: m, n, ii
!!
n = SIZE(mat, 1)
!!
IF (SIZE(d) .EQ. 1) THEN
  DO CONCURRENT(ii=1:n)
    mat(ii, ii) = REAL(d(1), kind=DFP)
  END DO
ELSE
  DO CONCURRENT(ii=1:n)
    mat(ii, ii) = REAL(d(ii), kind=DFP)
  END DO
END IF
!!
m = n - 1
!!
IF (SIZE(da) .EQ. 1) THEN
  DO CONCURRENT(ii=1:m)
    mat(ii, ii + 1) = REAL(da(1), kind=DFP)
  END DO
ELSE
  DO CONCURRENT(ii=1:m)
    mat(ii, ii + 1) = REAL(da(ii), kind=DFP)
  END DO
END IF
!!
m = n - 1
!!
IF (SIZE(db) .EQ. 1) THEN
  DO CONCURRENT(ii=1:m)
    mat(ii + 1, ii) = REAL(db(1), kind=DFP)
  END DO
ELSE
  DO CONCURRENT(ii=1:m)
    mat(ii + 1, ii) = REAL(db(ii), kind=DFP)
  END DO
END IF
!!