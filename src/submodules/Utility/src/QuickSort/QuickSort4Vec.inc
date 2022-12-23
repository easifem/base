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

INTEGER(I4B) i, iPivot

iPivot = high
i = low
DO WHILE (iPivot > i)
  IF (vect1(i) > vect1(iPivot)) THEN
    CALL Swap(vect1(i), vect1(iPivot - 1))
    CALL Swap(vect2(i), vect2(iPivot - 1))
    CALL Swap(vect3(i), vect3(iPivot - 1))
    CALL Swap(vect4(i), vect4(iPivot - 1))
    CALL Swap(vect1(iPivot - 1), vect1(iPivot))
    CALL Swap(vect2(iPivot - 1), vect2(iPivot))
    CALL Swap(vect3(iPivot - 1), vect3(iPivot))
    CALL Swap(vect4(iPivot - 1), vect4(iPivot))
    iPivot = iPivot - 1
  ELSE
    i = i + 1
  END IF
END DO
IF (low < high) THEN
  CALL QuickSort(vect1, vect2, vect3, vect4, low, iPivot - 1)
  CALL QuickSort(vect1, vect2, vect3, vect4, iPivot + 1, high)
END IF
