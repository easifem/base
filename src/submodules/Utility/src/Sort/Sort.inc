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

CHARACTER(LEN=120) :: name0

IF (PRESENT(name)) THEN
  name0 = UpperCase(name)
ELSE
  name0 = "INTROSORT"
END IF

ans = x

SELECT CASE (TRIM(name0))
CASE ("QUICKSORT")
  CALL QuickSort(vect1=ans, low=1_I4B, high=SIZE(ans, kind=I4B))
CASE ("HEAPSORT")
  CALL HeapSort(array=ans)
CASE ("INTROSORT")
  CALL IntroSort(array=ans)
CASE ("INSERTIONSORT")
  CALL InsertionSort(array=ans, low=1_I4B, high=SIZE(ans, kind=I4B))
END SELECT
