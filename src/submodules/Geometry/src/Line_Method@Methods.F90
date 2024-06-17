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

SUBMODULE(Line_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_exp_is_degenerate_nd
ans = (all(p1(1:dim_num) == p2(1:dim_num)))
END PROCEDURE line_exp_is_degenerate_nd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_exp2imp_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) norm
!
!  Take care of degenerate cases.
!
if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
  return
end if

a = p2(2) - p1(2)
b = p1(1) - p2(1)
c = p2(1) * p1(2) - p1(1) * p2(2)

norm = a * a + b * b + c * c

if (0.0D+00 < norm) then
  a = a / norm
  b = b / norm
  c = c / norm
end if

if (a < 0.0D+00) then
  a = -a
  b = -b
  c = -c
end if

END PROCEDURE line_exp2imp_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure line_imp_is_degenerate_2d
ans = (a * a + b * b == 0.0D+00)
end procedure line_imp_is_degenerate_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure lines_imp_int_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) a(dim_num, dim_num + 1)
integer(kind=4) info
!
p(1:dim_num) = 0.0D+00
!
!  Refuse to handle degenerate lines.
!
if (line_imp_is_degenerate_2d(a1, b1, c1)) then
  ival = -1
  return
end if
!
if (line_imp_is_degenerate_2d(a2, b2, c2)) then
  ival = -2
  return
end if
!
!  Set up and solve a linear system.
!
a(1, 1) = a1
a(1, 2) = b1
a(1, 3) = -c1
a(2, 1) = a2
a(2, 2) = b2
a(2, 3) = -c2
!
call r8mat_solve(2, 1, a, info)
!
!  If the inverse exists, then the lines intersect at the solution point.
!
if (info == 0) then

  ival = 1
  p(1:dim_num) = a(1:dim_num, 3)
!
!  If the inverse does not exist, then the lines are parallel
!  or coincident.  Check for parallelism by seeing if the
!  C entries are in the same ratio as the A or B entries.
!
else
  ival = 0
  if (a1 == 0.0D+00) then
    if (b2 * c1 == c2 * b1) then
      ival = 2
    end if
  else
    if (a2 * c1 == c2 * a1) then
      ival = 2
    end if
  end if
end if
!
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure line_exp_perp_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) bot
real(kind=8) t
!
flag = .false.
if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
  flag = .true.
  p4(1:2) = r8_huge()
  return
end if
!
bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)
!
!  (P3-P1) dot (P2-P1) = Norm(P3-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P3-P1) dot (P2-P1) / Norm(P3-P1)^2 = normalized coordinate T
!  of the projection of (P3-P1) onto (P2-P1).
!
t = sum((p1(1:dim_num) - p3(1:dim_num)) &
        * (p1(1:dim_num) - p2(1:dim_num))) / bot
!
p4(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
!
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure lines_exp_int_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) a1
real(kind=8) a2
real(kind=8) b1
real(kind=8) b2
real(kind=8) c1
real(kind=8) c2
logical(kind=4) point_1
logical(kind=4) point_2
!
ival = 0
p(1:dim_num) = 0.0D+00
!
!  Check whether either line is a point.
!
if (all(p1(1:dim_num) == p2(1:dim_num))) then
  point_1 = .true.
else
  point_1 = .false.
end if

if (all(q1(1:dim_num) == q2(1:dim_num))) then
  point_2 = .true.
else
  point_2 = .false.
end if
!
!  Convert the lines to ABC format.
!
if (.not. point_1) then
  call line_exp2imp_2d(p1, p2, a1, b1, c1)
end if

if (.not. point_2) then
  call line_exp2imp_2d(q1, q2, a2, b2, c2)
end if
!
!  Search for intersection of the lines.
!
if (point_1 .and. point_2) then
  if (all(p1(1:dim_num) == q1(1:dim_num))) then
    ival = 1
    p(1:dim_num) = p1(1:dim_num)
  end if
else if (point_1) then
  if (a2 * p1(1) + b2 * p1(2) == c2) then
    ival = 1
    p(1:dim_num) = p1(1:dim_num)
  end if
else if (point_2) then
  if (a1 * q1(1) + b1 * q1(2) == c1) then
    ival = 1
    p(1:dim_num) = q1(1:dim_num)
  end if
else
  call lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)
end if
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure segment_point_dist_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) bot
real(kind=8) pn(dim_num)
real(kind=8) t
!
!  If the line segment is actually a point, then the answer is easy.
!
if (all(p1(1:dim_num) == p2(1:dim_num))) then
  t = 0.0D+00
else
  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)
  t = sum((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = max(t, 0.0D+00)
  t = min(t, 1.0D+00)
end if
!
pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = sqrt(sum((p(1:dim_num) - pn(1:dim_num))**2))
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure segment_point_dist_3d
integer(i4b), parameter :: dim_num = 3
real(dfp) bot
real(dfp) pn(dim_num)
real(dfp) t
!
!  If the line segment is actually a point, then the answer is easy.
!
if (all(p1(1:dim_num) == p2(1:dim_num))) then
  t = 0.0D+00
else
  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)
  t = sum((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = max(t, 0.0D+00)
  t = min(t, 1.0D+00)
end if

pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = sqrt(sum((p(1:dim_num) - pn(1:dim_num))**2))
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure line_exp_point_dist_signed_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) a
real(kind=8) b
real(kind=8) c
!
!  If the explicit line degenerates to a point, the computation is easy.
!
if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
  dist_signed = sqrt(sum((p1(1:dim_num) - p(1:dim_num))**2))
!
!  Convert the explicit line to the implicit form A * P(1) + B * P(2) + C = 0.
!  This makes the computation of the signed distance to (X,Y) easy.
!
else
  a = p2(2) - p1(2)
  b = p1(1) - p2(1)
  c = p2(1) * p1(2) - p1(1) * p2(2)
  dist_signed = (a * p(1) + b * p(2) + c) / sqrt(a * a + b * b)
end if
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module procedure segment_point_near_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) bot
!
!  If the line segment is actually a point, then the answer is easy.
!
if (all(p1(1:dim_num) == p2(1:dim_num))) then
  t = 0.0D+00
else
  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)
  t = sum((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = max(t, 0.0D+00)
  t = min(t, 1.0D+00)
end if
!
pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = sqrt(sum((p(1:dim_num) - pn(1:dim_num))**2))
end procedure

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./inc/aux.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
