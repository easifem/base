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

SUBMODULE(Plane_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE plane_normal_line_exp_int_3d
integer(i4b), parameter :: dim_num = 3
real(dfp) :: direction(dim_num)
real(dfp) :: temp
!
!  Make sure the line is not degenerate.
!
if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
  return
end if
!
!  Make sure the plane normal vector is a unit vector.
!
temp = sqrt(sum(normal(1:dim_num)**2))
!
if (temp == 0.0D+00) then
  return
end if
!
normal(1:dim_num) = normal(1:dim_num) / temp
!
!  Determine the unit direction vector of the line.
!
direction(1:dim_num) = p2(1:dim_num) - p1(1:dim_num)
temp = sqrt(sum(direction(1:dim_num)**2))
direction(1:dim_num) = direction(1:dim_num) / temp
!
!  If the normal and direction vectors are orthogonal, then
!  we have a special case to deal with.
!
if (dot_product(normal(1:dim_num), direction(1:dim_num)) == 0.0D+00) then

  temp = dot_product(normal(1:dim_num), p1(1:dim_num) - pp(1:dim_num))

  if (temp == 0.0D+00) then
    ival = 2
    pint(1:dim_num) = p1(1:dim_num)
  else
    ival = 0
    pint(1:dim_num) = huge(temp)
  end if

  return
end if
!
!  Determine the distance along the direction vector
!  to the intersection point.
!
temp = dot_product(pp(1:dim_num) - p1(1:dim_num), normal(1:dim_num)) &
    & / dot_product(direction(1:dim_num), normal(1:dim_num))

ival = 1
pint(1:dim_num) = p1(1:dim_num) + temp * direction(1:dim_num)

END PROCEDURE plane_normal_line_exp_int_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
