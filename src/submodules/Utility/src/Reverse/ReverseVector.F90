! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

! INTEGER(INT8) :: temp
INTEGER(I4B) :: ii, jj, tsize, halfSize, indx

tsize = n2 - n1 + 1
halfSize = tsize / 2

DO indx = 1, halfSize
  ii = n1 + indx - 1
  jj = n2 - indx + 1
  temp = ans(jj)
  ans(jj) = ans(ii)
  ans(ii) = temp
END DO
