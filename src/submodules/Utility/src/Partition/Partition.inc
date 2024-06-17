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


pivot = this(left)
lo = left; hi = right
DO WHILE (lo <= hi)
  DO WHILE (this(hi) > pivot)
    hi = hi - 1
  END DO

  DO WHILE (lo <= hi .AND. this(lo) <= pivot)
    lo = lo + 1
  END DO
  IF (lo <= hi) THEN
    CALL swap(this(lo), this(hi))
    lo = lo + 1; hi = hi - 1
  END IF
END DO
CALL swap(this(left), this(hi))
iPivot = hi
