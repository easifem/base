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

MODULE Plane_Method
USE GlobalData
IMPLICIT NONE

PRIVATE

PUBLIC :: plane_normal_line_exp_int_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         intersection of plane and line in 3D.
!
!# Introduction
!
!    The normal form of a plane in 3D is:
!
!      PP is a point on the plane,
!      N is a normal vector to the plane.
!
!    The explicit form of a line in 3D is:
!
!      P1, P2 are two points on the line.
!
!# Parameters:
!
!    Input, real ( kind = 8 ) PP(3), a point on the plane.
!
!    Input, real ( kind = 8 ) NORMAL(3), a normal vector to the plane.
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two distinct points on the line.
!
!    Output, integer ( kind = 4 ) IVAL, the kind of intersection;
!    0, the line and plane seem to be parallel and separate;
!    1, the line and plane intersect at a single point;
!    2, the line and plane seem to be parallel and joined.
!
!    Output, real ( kind = 8 ) PINT(3), the coordinates of a
!    common point of the plane and line, when IVAL is 1 or 2.

INTERFACE
  MODULE PURE SUBROUTINE plane_normal_line_exp_int_3d(pp, normal, &
                                                      p1, p2, ival, pint)
    REAL(dfp), INTENT(in) :: pp(3)
    REAL(dfp), INTENT(inout) :: normal(3)
    REAL(dfp), INTENT(in) :: p1(3)
    REAL(dfp), INTENT(in) :: p2(3)
    INTEGER(i4b), INTENT(out) :: ival
    REAL(dfp), INTENT(out) :: pint(3)
  END SUBROUTINE
END INTERFACE

END MODULE Plane_Method
