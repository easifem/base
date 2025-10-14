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

INTEGER(I4B) :: tsize, halfSize, indx, indx1, indx2, ii

SELECT CASE (dim)
CASE (1)
  ! dim = 1
  tsize = r2 - r1 + 1
  halfSize = tsize / 2

  DO ii = c1, c2
    DO indx = 1, halfSize
      indx1 = r1 + indx - 1
      indx2 = r2 - indx + 1
      temp = ans(indx2, ii)
      ans(indx2, ii) = ans(indx1, ii)
      ans(indx1, ii) = temp
    END DO
  END DO

CASE (2)
  ! dim = 2
  tsize = c2 - c1 + 1
  halfSize = tsize / 2

  DO indx = 1, halfSize
    indx1 = c1 + indx - 1
    indx2 = c2 - indx + 1
    DO ii = r1, r2
      temp = ans(ii, indx2)
      ans(ii, indx2) = ans(ii, indx1)
      ans(ii, indx1) = temp
    END DO
  END DO
END SELECT

