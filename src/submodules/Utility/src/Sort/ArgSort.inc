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

CHARACTER(LEN=4) :: name0

IF (PRESENT(name)) THEN
  name0 = UpperCase(name(1:4))
ELSE
  name0 = "INTR"
END IF

ans = arange(1_I4B, SIZE(x, kind=I4B), 1_I4B)

SELECT CASE (name0)
CASE ("HEAP")
  CALL ArgHeapSort(array=x, arg=ans)
CASE ("INTR", "QUIC")
  CALL ArgIntroSort(array=x, arg=ans)
CASE ("INSE")
  CALL ArgInsertionSort(array=x, arg=ans, low=1_I4B, high=SIZE(x, kind=I4B))
CASE DEFAULT
  CALL ArgIntroSort(array=x, arg=ans)
END SELECT
