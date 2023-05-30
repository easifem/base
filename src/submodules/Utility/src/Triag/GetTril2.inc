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

INTEGER(I4B), ALLOCATABLE :: indx(:, :)
INTEGER(I4B) :: tsize, ii, jj
!
indx = TrilIndx(A, diagNo)
tsize = SIZE(indx, 1)
!
CALL Reallocate(lu, tsize)
!
jj = 0
!
DO ii = 1, tsize
  jj = jj + 1
  lu(jj) = A(indx(ii, 1), indx(ii, 2))
END DO
!
DEALLOCATE (indx)

