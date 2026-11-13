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

! Find the order of vect1(low:high) with ArgIntroSort and apply it
! to every vector: O(n log n) time and O(log n) recursion depth for
! every input. The old version used the last entry as pivot and
! was O(n**2), n levels deep, for sorted or constant input.
INTEGER(I4B) :: arg(MAX(high - low + 1, 0)), ii

IF (high - low + 1 .LE. 1) RETURN

arg = [(ii, ii=1, high - low + 1)]
CALL ArgIntroSort(vect1(low:high), arg)
arg = arg + (low - 1)

vect1(low:high) = vect1(arg)
vect2(low:high) = vect2(arg)
vect3(low:high) = vect3(arg)
vect4(low:high) = vect4(arg)
