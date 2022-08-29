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

SUBMODULE(Triangle_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_angles_2d
INTEGER(I4B), PARAMETER :: dim_num = 2
REAL(DFP), PARAMETER :: r8_pi = 3.141592653589793D+00
REAL(DFP) :: a
REAL(DFP) :: b
REAL(DFP) :: c
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
!
!  Take care of ridiculous special cases.
!
if (a == 0.0D+00 .and. b == 0.0D+00 .and. c == 0.0D+00) then
  angle(1:3) = 2.0D+00 * r8_pi / 3.0D+00
  return
end if
!
if (c == 0.0D+00 .or. a == 0.0D+00) then
  angle(1) = r8_pi
else
  angle(1) = safe_ACOS((c * c + a * a - b * b) / (2.0D+00 * c * a))
end if
!
if (a == 0.0D+00 .or. b == 0.0D+00) then
  angle(2) = r8_pi
else
  angle(2) = safe_ACOS((a * a + b * b - c * c) / (2.0D+00 * a * b))
end if
!
if (b == 0.0D+00 .or. c == 0.0D+00) then
  angle(3) = r8_pi
else
  angle(3) = safe_ACOS((b * b + c * c - a * a) / (2.0D+00 * b * c))
end if
END PROCEDURE triangle_angles_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_angles_3d
INTEGER(I4B), PARAMETER :: dim_num = 3
REAL(DFP) :: a
REAL(DFP) :: b
REAL(DFP) :: c
REAL(DFP), PARAMETER :: r8_pi = 3.141592653589793D+00
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
!
!  Take care of a ridiculous special case.
!
if (a == 0.0_DFP .and. b == 0.0_DFP .and. c == 0.0_DFP) then
  angle(1:3) = 2.0_DFP * r8_pi / 3.0_DFP
  return
end if
!
if (c == 0.0_DFP .or. a == 0.0_DFP) then
  angle(1) = r8_pi
else
  angle(1) = safe_acos((c * c + a * a - b * b) / (2.0_DFP * c * a))
end if
!
if (a == 0.0_DFP .or. b == 0.0_DFP) then
  angle(2) = r8_pi
else
  angle(2) = safe_acos((a * a + b * b - c * c) / (2.0_DFP * a * b))
end if
!
if (b == 0.0_DFP .or. c == 0.0_DFP) then
  angle(3) = r8_pi
else
  angle(3) = safe_acos((b * b + c * c - a * a) / (2.0_DFP * b * c))
end if
!
END PROCEDURE triangle_angles_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_2d
area = 0.5_DFP * ( &
       t(1, 1) * (t(2, 2) - t(2, 3)) &
       + t(1, 2) * (t(2, 3) - t(2, 1)) &
       + t(1, 3) * (t(2, 1) - t(2, 2)))
END PROCEDURE triangle_area_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_3d
INTEGER(I4B), PARAMETER :: dim_num = 3_I4B
real(DFP) :: cross(dim_num)
!
!  Compute the cross product vector.
!
cross(1) = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
           - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))
!
cross(2) = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
           - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))
!
cross(3) = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
           - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))
!
area = 0.5D+00 * sqrt(sum(cross(1:3)**2))
END PROCEDURE triangle_area_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_3d_2
integer(I4B), parameter :: dim_num = 3
real(DFP) :: alpha
real(DFP) :: base
real(DFP) :: dot
real(DFP) :: height
!
!  Find the projection of (P3-P1) onto (P2-P1).
!
dot = (t(1, 2) - t(1, 1)) * (t(1, 3) - t(1, 1)) &
      + (t(2, 2) - t(2, 1)) * (t(2, 3) - t(2, 1)) &
      + (t(3, 2) - t(3, 1)) * (t(3, 3) - t(3, 1))
!
!  Find the length of (P2-P1).
!
base = sqrt((t(1, 2) - t(1, 1))**2 &
      & + (t(2, 2) - t(2, 1))**2 &
      & + (t(3, 2) - t(3, 1))**2)
!
!  The height of the triangle is the length of (P3-P1) after its
!  projection onto (P2-P1) has been subtracted.
!
if (base == 0.0_DFP) then
  height = 0.0_DFP
else
  alpha = dot / (base * base)
  height = sqrt( &
           (t(1, 1) + alpha * (t(1, 2) - t(1, 1)) - t(1, 3))**2 &
           + (t(2, 1) + alpha * (t(2, 2) - t(2, 1)) - t(2, 3))**2 &
           + (t(3, 1) + alpha * (t(3, 2) - t(3, 1)) - t(3, 3))**2)
end if
!
area = 0.5_DFP * base * height
!
END PROCEDURE triangle_area_3d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_3d_3
integer(i4b), parameter :: dim_num = 3
integer(i4b) :: i
integer(i4b) :: j
integer(i4b) :: jp1
real(dfp) :: s(3)
!!
do j = 1, 3
  jp1 = mod(j, 3) + 1
  s(j) = 0.0D+00
  do i = 1, dim_num
    s(j) = s(j) + (t(i, j) - t(i, jp1))**2
  end do
  s(j) = sqrt(s(j))
end do
!!
area = (s(1) + s(2) + s(3)) &
       * (-s(1) + s(2) + s(3)) &
       * (s(1) - s(2) + s(3)) &
       * (s(1) + s(2) - s(3))
!!
if (area < 0.0D+00) then
  area = -1.0D+00
  return
end if
!!
area = 0.25D+00 * sqrt(area)
END PROCEDURE triangle_area_3d_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_heron
area = (s(1) + s(2) + s(3)) &
       * (-s(1) + s(2) + s(3)) &
       * (s(1) - s(2) + s(3)) &
       * (s(1) + s(2) - s(3))
if (area < 0.0D+00) then
  area = -1.0D+00
  return
end if
area = 0.25D+00 * sqrt(area)
END PROCEDURE triangle_area_heron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_vector_3d
integer(i4b), parameter :: dim_num = 3

area_vector(1) = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
                 - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))

area_vector(2) = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
                 - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))

area_vector(3) = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
                 - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))
END PROCEDURE triangle_area_vector_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_barycentric_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b), parameter :: rhs_num = 1
real(dfp) :: a(dim_num, dim_num + rhs_num)
integer(i4b) :: info
!
!  Set up the linear system
!
!    ( X2-X1  X3-X1 ) XSI(1)  = X-X1
!    ( Y2-Y1  Y3-Y1 ) XSI(2)    Y-Y1
!
!  which is satisfied by the barycentric coordinates of P.
!
a(1, 1) = t(1, 2) - t(1, 1)
a(1, 2) = t(1, 3) - t(1, 1)
a(1, 3) = p(1) - t(1, 1)
!
a(2, 1) = t(2, 2) - t(2, 1)
a(2, 2) = t(2, 3) - t(2, 1)
a(2, 3) = p(2) - t(2, 1)
!
!  Solve the linear system.
!
call r8mat_solve(n=dim_num, rhs_num=rhs_num, a=a, info=info)
!
xsi(1) = a(1, 3)
xsi(2) = a(2, 3)
xsi(3) = 1.0D+00 - xsi(1) - xsi(2)
END PROCEDURE triangle_barycentric_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b) :: i
do i = 1, dim_num
  centroid(i) = sum(t(i, 1:3)) / 3.0D+00
end do
END PROCEDURE triangle_centroid_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid_3d
integer(i4b), parameter :: dim_num = 3
integer(i4b) :: i
do i = 1, dim_num
  centroid(i) = sum(t(i, 1:3)) / 3.0D+00
end do
END PROCEDURE triangle_centroid_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcenter_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: avar
real(dfp) :: f(2)
real(dfp) :: top(dim_num)

f(1) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2
f(2) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2

top(1) = (t(2, 3) - t(2, 1)) * f(1) - (t(2, 2) - t(2, 1)) * f(2)
top(2) = -(t(1, 3) - t(1, 1)) * f(1) + (t(1, 2) - t(1, 1)) * f(2)

avar = (t(2, 3) - t(2, 1)) * (t(1, 2) - t(1, 1)) &
       - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

pc(1:2) = t(1:2, 1) + 0.5D+00 * top(1:2) / avar
END PROCEDURE triangle_circumcenter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcenter_2d_2
integer(i4b), parameter :: dim_num = 2
integer(i4b), parameter :: rhs_num = 1
real(dfp) :: a(dim_num, dim_num + rhs_num)
integer(i4b) :: info
!
!  Set up the linear system.
!
a(1, 1) = t(1, 2) - t(1, 1)
a(1, 2) = t(2, 2) - t(2, 1)
a(1, 3) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2
!
a(2, 1) = t(1, 3) - t(1, 1)
a(2, 2) = t(2, 3) - t(2, 1)
a(2, 3) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2
!
!  Solve the linear system.
!
call r8mat_solve(dim_num, rhs_num, a, info)
!
!  Compute the center
!
if (info /= 0) then
  pc(1:dim_num) = 0.0D+00
else
  pc(1:dim_num) = t(1:dim_num, 1) + 0.5D+00 * a(1:dim_num, dim_num + 1)
end if
END PROCEDURE triangle_circumcenter_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcenter
real(dfp) :: a
real(dfp) :: abp
real(dfp) :: apc
real(dfp) :: b
real(dfp) :: c
real(dfp) :: pbc
!!
a = r8vec_normsq_affine(n, t(1:n, 2), t(1:n, 3))
b = r8vec_normsq_affine(n, t(1:n, 3), t(1:n, 1))
c = r8vec_normsq_affine(n, t(1:n, 1), t(1:n, 2))
!!
pbc = a * (-a + b + c)
apc = b * (a - b + c)
abp = c * (a + b - c)
!!
p(1:n) = (pbc * t(1:n, 1) + apc * t(1:n, 2) + abp * t(1:n, 3)) &
      & / (pbc + apc + abp)
END PROCEDURE triangle_circumcenter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcircle_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: b
real(dfp) :: bot
real(dfp) :: c
real(dfp) :: f(2)
real(dfp) :: top(dim_num)
real(dfp) :: avar
!
!  Circumradius.
!
a = sqrt((t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2)
b = sqrt((t(1, 3) - t(1, 2))**2 + (t(2, 3) - t(2, 2))**2)
c = sqrt((t(1, 1) - t(1, 3))**2 + (t(2, 1) - t(2, 3))**2)
!
bot = (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)
!
if (bot <= 0.0D+00) then
  r = -1.0D+00
  pc(1:2) = 0.0D+00
  return
end if
!
r = a * b * c / sqrt(bot)
!
!  Circumcenter.
!
f(1) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2
f(2) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2
!
top(1) = (t(2, 3) - t(2, 1)) * f(1) - (t(2, 2) - t(2, 1)) * f(2)
top(2) = -(t(1, 3) - t(1, 1)) * f(1) + (t(1, 2) - t(1, 1)) * f(2)
!
avar = (t(2, 3) - t(2, 1)) * (t(1, 2) - t(1, 1)) &
    & - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))
!
pc(1:2) = t(1:2, 1) + 0.5D+00 * top(1:2) / avar
!
END PROCEDURE triangle_circumcircle_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcircle_2d_2
integer(i4b), parameter :: dim_num = 2
integer(i4b), parameter :: rhs_num = 1
real(dfp) :: a(dim_num, dim_num + rhs_num)
integer(i4b) :: info
!
!  Set up the linear system.
!
a(1, 1) = t(1, 2) - t(1, 1)
a(1, 2) = t(2, 2) - t(2, 1)
a(1, 3) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2

a(2, 1) = t(1, 3) - t(1, 1)
a(2, 2) = t(2, 3) - t(2, 1)
a(2, 3) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2
!
!  Solve the linear system.
!
call r8mat_solve(dim_num, rhs_num, a, info)

if (info /= 0) then
  r = -1.0D+00
  pc(1:dim_num) = 0.0D+00
end if

r = 0.5D+00 * sqrt(a(1, dim_num + 1) * a(1, dim_num + 1) &
                   + a(2, dim_num + 1) * a(2, dim_num + 1))
pc(1:dim_num) = t(1:dim_num, 1) + 0.5D+00 * a(1:dim_num, dim_num + 1)

END PROCEDURE triangle_circumcircle_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumradius_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: b
real(dfp) :: bot
real(dfp) :: c
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
bot = (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)
if (bot <= 0.0D+00) then
  r = -1.0D+00
  return
end if
r = a * b * c / sqrt(bot)
END PROCEDURE triangle_circumradius_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line_exp_3d
integer(i4b), parameter :: dim_num = 3
integer(i4b) :: ival
real(dfp) :: normal(dim_num)
real(dfp) :: normal2(dim_num)
real(dfp) :: temp
real(dfp) :: v1(dim_num)
real(dfp) :: v2(dim_num)
!
!  Make sure the line is not degenerate.
!
if (line_exp_is_degenerate_nd(dim_num, p1, p2)) return
!
!  Make sure the triangle is not degenerate.
!
if (triangle_is_degenerate_nd(dim_num, t)) return
!
!  Determine a unit normal vector associated with the plane of
!  the triangle.
!
v1(1:dim_num) = t(1:dim_num, 2) - t(1:dim_num, 1)
v2(1:dim_num) = t(1:dim_num, 3) - t(1:dim_num, 1)
!
normal(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal(3) = v1(1) * v2(2) - v1(2) * v2(1)
!
temp = sqrt(sum(normal(1:dim_num)**2))
normal(1:dim_num) = normal(1:dim_num) / temp
!
!  Find the intersection of the plane and the line.
!
call plane_normal_line_exp_int_3d(t(1:dim_num, 1), normal, p1, p2, &
  & ival, pint)
!
if (ival == 0) then
  inside = .false.
  pint(1:dim_num) = huge(temp)
  return
else if (ival == 2) then
  inside = .false.
  pint(1:dim_num) = p1(1:dim_num)
  return
end if
!
!  Now, check that all three triangles made by two vertices and
!  the intersection point have the same "clock sense" as the
!  triangle's normal vector.
!
v1(1:dim_num) = t(1:dim_num, 2) - t(1:dim_num, 1)
v2(1:dim_num) = pint(1:dim_num) - t(1:dim_num, 1)
!
normal2(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal2(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal2(3) = v1(1) * v2(2) - v1(2) * v2(1)

if (dot_product(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) then
  inside = .false.
  return
end if

v1(1:dim_num) = t(1:dim_num, 3) - t(1:dim_num, 2)
v2(1:dim_num) = pint(1:dim_num) - t(1:dim_num, 2)

normal2(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal2(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal2(3) = v1(1) * v2(2) - v1(2) * v2(1)

if (dot_product(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) then
  inside = .false.
  return
end if

v1(1:dim_num) = t(1:dim_num, 1) - t(1:dim_num, 3)
v2(1:dim_num) = pint(1:dim_num) - t(1:dim_num, 3)

normal2(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal2(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal2(3) = v1(1) * v2(2) - v1(2) * v2(1)

if (dot_product(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) then
  inside = .false.
  return
end if

inside = .true.

END PROCEDURE triangle_contains_line_exp_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line_par_3d
integer(i4b), parameter :: dim_num = 3
real(dfp) :: a
real(dfp) :: angle_sum
real(dfp) :: b
real(dfp) :: c
real(dfp) :: d
real(dfp) :: denom
logical(lgt) :: intersect
real(dfp) :: norm
real(dfp) :: norm1
real(dfp) :: norm2
real(dfp), parameter :: r8_pi = 3.141592653589793D+00
real(dfp) :: t_int
real(dfp), parameter :: tol = 0.00001D+00
real(dfp) :: v1(dim_num)
real(dfp) :: v2(dim_num)
real(dfp) :: v3(dim_num)
!
!  Determine the implicit form (A,B,C,D) of the plane containing the
!  triangle.
!
a = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
    - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))

b = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
    - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))

c = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
    - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

d = -t(1, 2) * a - t(2, 2) * b - t(3, 2) * c
!
!  Make sure the plane is well-defined.
!
norm1 = sqrt(a * a + b * b + c * c)

if (norm1 == 0.0D+00) then
  inside = .false.
  p(1:dim_num) = 0.0D+00
  return
end if
!
!  Make sure the implicit line is well defined.
!
norm2 = sqrt(sum(pd(1:dim_num)**2))

if (norm2 == 0.0D+00) then
  inside = .false.
  p(1:dim_num) = 0.0D+00
  return
end if
!
!  Determine the denominator of the parameter in the
!  implicit line definition that determines the intersection
!  point.
!
denom = a * pd(1) + b * pd(2) + c * pd(3)
!
!  If DENOM is zero, or very small, the line and the plane may be
!  parallel or almost so.
!
if (abs(denom) < tol * norm1 * norm2) then
!
!  The line may actually lie in the plane.  We're not going
!  to try to address this possibility.
!
  if (a * p0(1) + b * p0(2) + c * p0(3) + d == 0.0D+00) then

    intersect = .true.
    inside = .false.
    p(1:dim_num) = p0(1:dim_num)
!
!  The line and plane are parallel and disjoint.
!
  else

    intersect = .false.
    inside = .false.
    p(1:dim_num) = 0.0D+00

  end if
!
!  The line and plane intersect at a single point P.
!
else

  intersect = .true.
  t_int = -(a * p0(1) + b * p0(2) + c * p0(3) + d) / denom
  p(1:dim_num) = p0(1:dim_num) + t_int * pd(1:dim_num)
!
!  To see if P is included in the triangle, sum the angles
!  formed by P and pairs of the vertices.  If the point is in the
!  triangle, we get a total 360 degree view.  Otherwise, we
!  get less than 180 degrees.
!
  v1(1:dim_num) = t(1:dim_num, 1) - p(1:dim_num)
  v2(1:dim_num) = t(1:dim_num, 2) - p(1:dim_num)
  v3(1:dim_num) = t(1:dim_num, 3) - p(1:dim_num)

  norm = sqrt(sum(v1(1:dim_num)**2))

  if (norm == 0.0D+00) then
    inside = .true.
    return
  end if

  v1(1:dim_num) = v1(1:dim_num) / norm

  norm = sqrt(sum(v2(1:dim_num)**2))

  if (norm == 0.0D+00) then
    inside = .true.
    return
  end if

  v2(1:dim_num) = v2(1:dim_num) / norm

  norm = sqrt(sum(v3(1:dim_num)**2))

  if (norm == 0.0D+00) then
    inside = .true.
    return
  end if

  v3(1:dim_num) = v3(1:dim_num) / norm

  angle_sum = safe_acos(dot_product(v1(1:3), v2(1:3))) &
              + safe_acos(dot_product(v2(1:3), v3(1:3))) &
              + safe_acos(dot_product(v3(1:3), v1(1:3)))

  if (nint(angle_sum / r8_pi) == 2) then
    inside = .true.
  else
    inside = .false.
  end if

end if

return
END PROCEDURE triangle_contains_line_par_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_1
integer(i4b), parameter :: dim_num = 2
real(dfp) :: xsi(dim_num + 1)
xsi = triangle_barycentric_2d(t, p)
if (any(xsi(1:3) < 0.0D+00)) then
  inside = .false.
else
  inside = .true.
end if
END PROCEDURE triangle_contains_point_2d_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_2
integer(i4b), parameter :: dim_num = 2
integer(i4b) :: j
integer(i4b) :: k
do j = 1, 3
  k = mod(j, 3) + 1
  if (0.0D+00 < (p(1) - t(1, j)) * (t(2, k) - t(2, j)) &
      - (p(2) - t(2, j)) * (t(1, k) - t(1, j))) then
    inside = .false.
    return
  end if
end do
inside = .true.
END PROCEDURE triangle_contains_point_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_3
integer(i4b), parameter :: dim_num = 2
real(dfp) :: dir_new
real(dfp) :: dir_old
integer(i4b) :: j
integer(i4b) :: k

dir_old = 0.0D+00

do j = 1, 3
  k = mod(j, 3) + 1
  dir_new = (p(1) - t(1, j)) * (t(2, k) - t(2, j)) &
            - (p(2) - t(2, j)) * (t(1, k) - t(1, j))
  if (dir_new * dir_old < 0.0D+00) then
    inside = .false.
    return
  end if
  if (dir_new /= 0.0D+00) then
    dir_old = dir_new
  end if
end do
inside = .true.
END PROCEDURE triangle_contains_point_2d_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_diameter_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: asq
real(dfp) :: b
real(dfp) :: bsq
real(dfp) :: c
real(dfp) :: csq
!
!  Compute the squared length of each side.
!
asq = sum(t(1:dim_num, 1) - t(1:dim_num, 2))**2
bsq = sum(t(1:dim_num, 2) - t(1:dim_num, 3))**2
csq = sum(t(1:dim_num, 3) - t(1:dim_num, 1))**2
!
!  Take care of a zero side.
!
if (asq == 0.0D+00) then
  diameter = sqrt(bsq)
  return
else if (bsq == 0.0D+00) then
  diameter = sqrt(csq)
  return
else if (csq == 0.0D+00) then
  diameter = sqrt(asq)
  return
end if
!
!  Make ASQ the largest.
!
if (asq < bsq) then
  call swap(asq, bsq)
end if

if (asq < csq) then
  call swap(asq, csq)
end if
!
!  If ASQ is very large...
!
if (bsq + csq < asq) then
  diameter = sqrt(asq)
else
  a = sqrt(asq)
  b = sqrt(bsq)
  c = sqrt(csq)
  diameter = 2.0D+00 * a * b * c / sqrt((a + b + c) * (-a + b + c) &
                                        * (a - b + c) * (a + b - c))
end if
END PROCEDURE triangle_diameter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_edge_length_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b) :: j1, j2
do j1 = 1, 3
  j2 = i4_wrap(j1 + 1, 1, 3)
  edge_length(j1) = NORM2(t(1:dim_num, j2) - t(1:dim_num, j1))
end do
END PROCEDURE triangle_edge_length_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_gridpoints_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b) :: i
integer(i4b) :: j
!
grid_num = 0
!
!  Special case, SUB_NUM = 0.
!
if (sub_num == 0) then
  if (1 <= grid_max) then
    grid_num = 1
    g(1, 1) = (t(1, 1) + t(1, 2) + t(1, 3)) / 3.0D+00
    g(2, 1) = (t(2, 1) + t(2, 2) + t(2, 3)) / 3.0D+00
  end if
  return
end if
!
do i = 0, sub_num
  do j = 0, sub_num - i
    if (grid_num < grid_max) then
      grid_num = grid_num + 1
      g(1, grid_num) = (real(i, kind=8) * t(1, 1) &
                      & + real(j, kind=8) * t(1, 2) &
                      & + real(sub_num - i - j, kind=8) * t(1, 3)) &
                      & / real(sub_num, kind=8)
      !
      g(2, grid_num) = (real(i, kind=8) * t(2, 1) &
                      & + real(j, kind=8) * t(2, 2) &
                      & + real(sub_num - i - j, kind=8) * t(2, 3)) &
                      & / real(sub_num, kind=8)
    end if
  end do
end do
!
END PROCEDURE triangle_gridpoints_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incenter_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: b
real(dfp) :: c
real(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

if (perimeter == 0.0D+00) then
  pc(1:dim_num) = t(1:dim_num, 1)
else
  pc(1:dim_num) = (b * t(1:dim_num, 1) &
                   + c * t(1:dim_num, 2) &
                   + a * t(1:dim_num, 3)) / perimeter
end if
END PROCEDURE triangle_incenter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incircle_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: b
real(dfp) :: c
real(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

if (perimeter == 0.0D+00) then
  pc(1:dim_num) = t(1:dim_num, 1)
  r = 0.0D+00
  return
end if

pc(1:dim_num) = ( &
                b * t(1:dim_num, 1) &
                + c * t(1:dim_num, 2) &
                + a * t(1:dim_num, 3)) / perimeter

r = 0.5D+00 * sqrt( &
    (-a + b + c) &
    * (+a - b + c) &
    * (+a + b - c) / perimeter)
END PROCEDURE triangle_incircle_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_inradius_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) :: a
real(dfp) :: b
real(dfp) :: c
real(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

if (perimeter == 0.0D+00) then
  r = 0.0D+00
  return
end if

r = 0.5D+00 * sqrt( &
    (-a + b + c) &
    * (+a - b + c) &
    * (+a + b - c) / perimeter)
END PROCEDURE triangle_inradius_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_is_degenerate_nd
ans = &
  & (all(t(1:dim_num, 1) == t(1:dim_num, 2)) .or. &
  & all(t(1:dim_num, 2) == t(1:dim_num, 3)) .or. &
  & all(t(1:dim_num, 3) == t(1:dim_num, 1)))
END PROCEDURE triangle_is_degenerate_nd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_lattice_layer_point_next
integer(i4b) :: c1n
integer(i4b), parameter :: n = 2
integer(i4b) :: rhs1
integer(i4b) :: rhs2
!
!  Treat layer C(N+1) = 0 specially.
!
if (c(n + 1) == 0) then
  if (.not. more) then
    v(1:n) = 0
    more = .true.
  else
    more = .false.
  end if
  return
end if
!
!  Compute first point.
!
if (.not. more) then
  v(1) = (c(n + 1) - 1) * c(1) + 1
  v(2) = 0
  more = .true.
else
  c1n = i4vec_lcm(n, c)
  rhs1 = c1n * (c(n + 1) - 1)
  rhs2 = c1n * c(n + 1)
  if (c(2) * (v(1) + 1) + c(1) * v(2) <= rhs2) then
    v(1) = v(1) + 1
  else
    v(1) = (rhs1 - c(1) * (v(2) + 1)) / c(2)
    v(1) = max(v(1), 0)
    v(2) = v(2) + 1
    if (c(2) * v(1) + c(1) * v(2) <= rhs1) then
      v(1) = v(1) + 1
    end if
    if (c(2) * v(1) + c(1) * v(2) <= rhs2) then
    else
      v(1:n) = 0
      more = .false.
    end if
  end if
end if
END PROCEDURE triangle_lattice_layer_point_next

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_lattice_point_next
integer(i4b) c1n
integer(i4b), parameter :: n = 2
integer(i4b) rhs

if (.not. more) then

  v(1:n) = 0
  more = .true.

else

  c1n = i4vec_lcm(n, c)

  rhs = c1n * c(n + 1)

  if (c(2) * (v(1) + 1) + c(1) * v(2) <= rhs) then
    v(1) = v(1) + 1
  else
    v(1) = 0
    if (c(2) * v(1) + c(1) * (v(2) + 1) <= rhs) then
      v(2) = v(2) + 1
    else
      v(2) = 0
      more = .false.
    end if
  end if
end if
END PROCEDURE triangle_lattice_point_next

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_line_imp_int_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) a1
real(dfp) b1
real(dfp) c1
integer(i4b) i
integer(i4b) ival
integer(i4b) j
real(dfp) p(dim_num)
real(dfp) test1
real(dfp) test2

int_num = 0

do i = 1, 3
  j = i4_wrap(i + 1, 1, 3)
  !
  ! Get the implicit form of the line through vertices I and I+1.
  !
  call line_exp2imp_2d(t(1:2, i), t(1:2, j), a1, b1, c1)
  !
  ! Seek an intersection with the original line.
  !
  call lines_imp_int_2d(a, b, c, a1, b1, c1, ival, p)
  !
  ! If there is an intersection, determine if it
  ! lies between the two vertices.
  !
  if (ival == 1) then
    test1 = sum((p(1:dim_num) - t(1:dim_num, i)) &
                * (t(1:dim_num, j) - t(1:dim_num, i)))
    test2 = sum((t(1:dim_num, j) - t(1:dim_num, i)) &
                * (t(1:dim_num, j) - t(1:dim_num, i)))
    !
    if (0 <= test1 .and. test1 <= test2) then
      int_num = int_num + 1
      pint(1:dim_num, int_num) = p(1:dim_num)
    end if
  end if
end do

END PROCEDURE triangle_line_imp_int_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orientation_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) avar

if (all(t(1:dim_num, 1) == t(1:dim_num, 2)) .or. &
    all(t(1:dim_num, 2) == t(1:dim_num, 3)) .or. &
    all(t(1:dim_num, 3) == t(1:dim_num, 1))) then
  ans = 3
  return
end if

avar = (t(1, 1) - t(1, 3)) * (t(2, 2) - t(2, 3)) &
      & - (t(1, 2) - t(1, 3)) * (t(2, 1) - t(2, 3))

if (avar == 0.0D+00) then
  ans = 2
else if (avar < 0.0D+00) then
  ans = 1
else if (0.0D+00 < avar) then
  ans = 0
end if

END PROCEDURE triangle_orientation_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orthocenter_2d
integer(i4b), parameter :: dim_num = 2
logical(lgt) flag
integer(i4b) ival
real(dfp) p23(dim_num)
real(dfp) p31(dim_num)
!
!  Determine a point P23 common to the line (P2,P3) and
!  its perpendicular through P1.
!
call line_exp_perp_2d(t(1:2, 2), t(1:2, 3), t(1:2, 1), p23, flag)

if (flag) then
  pc(1:2) = r8_huge()
  return
end if
!
!  Determine a point P31 common to the line (P3,P1) and
!  its perpendicular through P2.
!
call line_exp_perp_2d(t(1:2, 3), t(1:2, 1), t(1:2, 2), p31, flag)

if (flag) then
  pc(1:2) = r8_huge()
  return
end if
!
!  Determine PC, the intersection of the lines (P1,P23) and (P2,P31).
!
call lines_exp_int_2d(t(1:2, 1), p23(1:2), t(1:2, 2), p31(1:2), ival, pc)

if (ival /= 1) then
  pc(1:2) = r8_huge()
  flag = .true.
  return
end if

return
END PROCEDURE triangle_orthocenter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b), parameter :: side_num = 3
real(dfp) dist2
integer(i4b) j
integer(i4b) jp1
!
!  Find the distance to each of the line segments.
!
dist = huge(dist)
!
do j = 1, side_num
  jp1 = i4_wrap(j + 1, 1, side_num)
  dist2 = segment_point_dist_2d(t(1:dim_num, j), t(1:dim_num, jp1), p)
  if (dist2 < dist) then
    dist = dist2
  end if
end do
END PROCEDURE triangle_point_dist_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_3d
integer(i4b), parameter :: dim_num = 3
real(dfp) dist2
!
!  Compute the distances from the point to each of the sides.
!
dist2 = segment_point_dist_3d(t(1:dim_num, 1), t(1:dim_num, 2), p)
dist = dist2
dist2 = segment_point_dist_3d(t(1:dim_num, 2), t(1:dim_num, 3), p)
dist = min(dist, dist2)
dist2 = segment_point_dist_3d(t(1:dim_num, 3), t(1:dim_num, 1), p)
dist = min(dist, dist2)
END PROCEDURE triangle_point_dist_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_signed_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) dis12
real(dfp) dis23
real(dfp) dis31
!
!  Compute the signed line distances to the point.
!
dis12 = line_exp_point_dist_signed_2d(t(1:2, 1), t(1:2, 2), p)
dis23 = line_exp_point_dist_signed_2d(t(1:2, 2), t(1:2, 3), p)
dis31 = line_exp_point_dist_signed_2d(t(1:2, 3), t(1:2, 1), p)
!
!  If the point is inside the triangle, all the line distances are negative.
!  The largest (negative) line distance has the smallest magnitude,
!  and is the signed triangle distance.
!
if (dis12 <= 0.0D+00 .and. dis23 <= 0.0D+00 .and. dis31 <= 0.0D+00) then
  dist_signed = max(dis12, dis23, dis31)
!
!  If the point is outside the triangle, then we have to compute
!  the (positive) line segment distances and take the minimum.
!
else
  dis12 = segment_point_dist_2d(t(1:2, 1), t(1:2, 2), p)
  dis23 = segment_point_dist_2d(t(1:2, 2), t(1:2, 3), p)
  dis31 = segment_point_dist_2d(t(1:2, 3), t(1:2, 1), p)
  dist_signed = min(dis12, dis23, dis31)
end if
!
END PROCEDURE triangle_point_dist_signed_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_near_2d
integer(i4b), parameter :: dim_num = 2
integer(i4b), parameter :: side_num = 3
integer(i4b) j
integer(i4b) jp1
real(dfp) dist2
real(dfp) pn2(dim_num)
real(dfp) tval
!
!  Find the distance to each of the line segments that make up the edges
!  of the triangle.
!
dist = huge(dist)
pn(1:dim_num) = 0.0D+00
do j = 1, side_num
  jp1 = i4_wrap(j + 1, 1, side_num)
  call segment_point_near_2d(t(1:dim_num, j), t(1:dim_num, jp1), p, &
    & pn2, dist2, tval)
  if (dist2 < dist) then
    dist = dist2
    pn(1:dim_num) = pn2(1:dim_num)
  end if
end do
END PROCEDURE triangle_point_near_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_quality_2d
integer(i4b), parameter :: dim_num = 2
real(dfp) a
real(dfp) b
real(dfp) c
!
!  Compute the length of each side.
!
a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
if (a * b * c == 0.0D+00) then
  quality = 0.0D+00
else
  quality = (-a + b + c) * (a - b + c) * (a + b - c) &
            / (a * b * c)
end if
END PROCEDURE triangle_quality_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_right_lattice_point_num_2d
n = ((a + 1) * (b + 1) + i4_gcd(a, b) + 1) / 2
END PROCEDURE triangle_right_lattice_point_num_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_sample
integer(i4b), parameter :: dim_num = 2
real(dfp) alpha(n)
integer(i4b) dim
real(dfp) p12(dim_num, n)
real(dfp) p13(dim_num, n)
!
alpha = rvec_uniform_01(n, seed)
!
!  Interpret R as a percentage of the triangle's area.
!
!  Imagine a line L, parallel to side 1, so that the area between
!  vertex 1 and line L is R percent of the full triangle's area.
!
!  The line L will intersect sides 2 and 3 at a fraction
!  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
!
alpha(1:n) = sqrt(alpha(1:n))
!
!  Determine the coordinates of the points on sides 2 and 3 intersected
!  by line L.
!
do dim = 1, dim_num
  p12(dim, 1:n) = (1.0D+00 - alpha(1:n)) * t(dim, 1) &
                  + alpha(1:n) * t(dim, 2)

  p13(dim, 1:n) = (1.0D+00 - alpha(1:n)) * t(dim, 1) &
                  + alpha(1:n) * t(dim, 3)
end do
!
!  Now choose, uniformly at random, a point on the line L.
!
alpha = rvec_uniform_01(n, seed)

do dim = 1, dim_num
  p(dim, 1:n) = (1.0D+00 - alpha(1:n)) * p12(dim, 1:n) &
                + alpha(1:n) * p13(dim, 1:n)
end do

END PROCEDURE triangle_sample

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle01_lattice_point_num_2d
n = ((s + 2) * (s + 1)) / 2
END PROCEDURE triangle01_lattice_point_num_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_xsi_to_xy_2d
integer(kind=4), parameter :: dim_num = 2
p(1:dim_num) = matmul(t(1:dim_num, 1:3), xsi(1:dim_num + 1))
END PROCEDURE triangle_xsi_to_xy_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_xy_to_xsi_2d
integer(kind=4), parameter :: dim_num = 2
real(kind=8) avar
avar = (t(1, 1) - t(1, 3)) * (t(2, 2) - t(2, 3)) &
       - (t(1, 2) - t(1, 3)) * (t(2, 1) - t(2, 3))
xsi(1) = ((t(2, 2) - t(2, 3)) * (p(1) - t(1, 3)) &
          - (t(1, 2) - t(1, 3)) * (p(2) - t(2, 3))) / avar

xsi(2) = (-(t(2, 1) - t(2, 3)) * (p(1) - t(1, 3)) &
          + (t(1, 1) - t(1, 3)) * (p(2) - t(2, 3))) / avar

xsi(3) = 1.0D+00 - xsi(1) - xsi(2)
END PROCEDURE triangle_xy_to_xsi_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "./inc/aux.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
