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
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
!
!  Take care of ridiculous special cases.
!
IF (a == 0.0D+00 .AND. b == 0.0D+00 .AND. c == 0.0D+00) THEN
  angle(1:3) = 2.0D+00 * r8_pi / 3.0D+00
  RETURN
END IF
!
IF (c == 0.0D+00 .OR. a == 0.0D+00) THEN
  angle(1) = r8_pi
ELSE
  angle(1) = safe_ACOS((c * c + a * a - b * b) / (2.0D+00 * c * a))
END IF
!
IF (a == 0.0D+00 .OR. b == 0.0D+00) THEN
  angle(2) = r8_pi
ELSE
  angle(2) = safe_ACOS((a * a + b * b - c * c) / (2.0D+00 * a * b))
END IF
!
IF (b == 0.0D+00 .OR. c == 0.0D+00) THEN
  angle(3) = r8_pi
ELSE
  angle(3) = safe_ACOS((b * b + c * c - a * a) / (2.0D+00 * b * c))
END IF
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
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
!
!  Take care of a ridiculous special case.
!
IF (a == 0.0_DFP .AND. b == 0.0_DFP .AND. c == 0.0_DFP) THEN
  angle(1:3) = 2.0_DFP * r8_pi / 3.0_DFP
  RETURN
END IF
!
IF (c == 0.0_DFP .OR. a == 0.0_DFP) THEN
  angle(1) = r8_pi
ELSE
  angle(1) = safe_acos((c * c + a * a - b * b) / (2.0_DFP * c * a))
END IF
!
IF (a == 0.0_DFP .OR. b == 0.0_DFP) THEN
  angle(2) = r8_pi
ELSE
  angle(2) = safe_acos((a * a + b * b - c * c) / (2.0_DFP * a * b))
END IF
!
IF (b == 0.0_DFP .OR. c == 0.0_DFP) THEN
  angle(3) = r8_pi
ELSE
  angle(3) = safe_acos((b * b + c * c - a * a) / (2.0_DFP * b * c))
END IF
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
REAL(DFP) :: cross(dim_num)
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
area = 0.5D+00 * SQRT(SUM(cross(1:3)**2))
END PROCEDURE triangle_area_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_3d_2
INTEGER(I4B), PARAMETER :: dim_num = 3
REAL(DFP) :: alpha
REAL(DFP) :: base
REAL(DFP) :: dot
REAL(DFP) :: height
!
!  Find the projection of (P3-P1) onto (P2-P1).
!
dot = (t(1, 2) - t(1, 1)) * (t(1, 3) - t(1, 1)) &
      + (t(2, 2) - t(2, 1)) * (t(2, 3) - t(2, 1)) &
      + (t(3, 2) - t(3, 1)) * (t(3, 3) - t(3, 1))
!
!  Find the length of (P2-P1).
!
base = SQRT((t(1, 2) - t(1, 1))**2 &
      & + (t(2, 2) - t(2, 1))**2 &
      & + (t(3, 2) - t(3, 1))**2)
!
!  The height of the triangle is the length of (P3-P1) after its
!  projection onto (P2-P1) has been subtracted.
!
IF (base == 0.0_DFP) THEN
  height = 0.0_DFP
ELSE
  alpha = dot / (base * base)
  height = SQRT( &
           (t(1, 1) + alpha * (t(1, 2) - t(1, 1)) - t(1, 3))**2 &
           + (t(2, 1) + alpha * (t(2, 2) - t(2, 1)) - t(2, 3))**2 &
           + (t(3, 1) + alpha * (t(3, 2) - t(3, 1)) - t(3, 3))**2)
END IF
!
area = 0.5_DFP * base * height
!
END PROCEDURE triangle_area_3d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_3d_3
INTEGER(i4b), PARAMETER :: dim_num = 3
INTEGER(i4b) :: i
INTEGER(i4b) :: j
INTEGER(i4b) :: jp1
REAL(dfp) :: s(3)
!!
DO j = 1, 3
  jp1 = MOD(j, 3) + 1
  s(j) = 0.0D+00
  DO i = 1, dim_num
    s(j) = s(j) + (t(i, j) - t(i, jp1))**2
  END DO
  s(j) = SQRT(s(j))
END DO
!!
area = (s(1) + s(2) + s(3)) &
       * (-s(1) + s(2) + s(3)) &
       * (s(1) - s(2) + s(3)) &
       * (s(1) + s(2) - s(3))
!!
IF (area < 0.0D+00) THEN
  area = -1.0D+00
  RETURN
END IF
!!
area = 0.25D+00 * SQRT(area)
END PROCEDURE triangle_area_3d_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_heron
area = (s(1) + s(2) + s(3)) &
       * (-s(1) + s(2) + s(3)) &
       * (s(1) - s(2) + s(3)) &
       * (s(1) + s(2) - s(3))
IF (area < 0.0D+00) THEN
  area = -1.0D+00
  RETURN
END IF
area = 0.25D+00 * SQRT(area)
END PROCEDURE triangle_area_heron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_area_vector_3d
INTEGER(i4b), PARAMETER :: dim_num = 3

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
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b), PARAMETER :: rhs_num = 1
REAL(dfp) :: a(dim_num, dim_num + rhs_num)
INTEGER(i4b) :: info
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
CALL r8mat_solve(n=dim_num, rhs_num=rhs_num, a=a, info=info)
!
xsi(1) = a(1, 3)
xsi(2) = a(2, 3)
xsi(3) = 1.0D+00 - xsi(1) - xsi(2)
END PROCEDURE triangle_barycentric_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b) :: i
DO i = 1, dim_num
  centroid(i) = SUM(t(i, 1:3)) / 3.0D+00
END DO
END PROCEDURE triangle_centroid_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_centroid_3d
INTEGER(i4b), PARAMETER :: dim_num = 3
INTEGER(i4b) :: i
DO i = 1, dim_num
  centroid(i) = SUM(t(i, 1:3)) / 3.0D+00
END DO
END PROCEDURE triangle_centroid_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcenter_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: avar
REAL(dfp) :: f(2)
REAL(dfp) :: top(dim_num)

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
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b), PARAMETER :: rhs_num = 1
REAL(dfp) :: a(dim_num, dim_num + rhs_num)
INTEGER(i4b) :: info
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
CALL r8mat_solve(dim_num, rhs_num, a, info)
!
!  Compute the center
!
IF (info /= 0) THEN
  pc(1:dim_num) = 0.0D+00
ELSE
  pc(1:dim_num) = t(1:dim_num, 1) + 0.5D+00 * a(1:dim_num, dim_num + 1)
END IF
END PROCEDURE triangle_circumcenter_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumcenter
REAL(dfp) :: a
REAL(dfp) :: abp
REAL(dfp) :: apc
REAL(dfp) :: b
REAL(dfp) :: c
REAL(dfp) :: pbc
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
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: b
REAL(dfp) :: bot
REAL(dfp) :: c
REAL(dfp) :: f(2)
REAL(dfp) :: top(dim_num)
REAL(dfp) :: avar
!
!  Circumradius.
!
a = SQRT((t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2)
b = SQRT((t(1, 3) - t(1, 2))**2 + (t(2, 3) - t(2, 2))**2)
c = SQRT((t(1, 1) - t(1, 3))**2 + (t(2, 1) - t(2, 3))**2)
!
bot = (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)
!
IF (bot <= 0.0D+00) THEN
  r = -1.0D+00
  pc(1:2) = 0.0D+00
  RETURN
END IF
!
r = a * b * c / SQRT(bot)
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
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b), PARAMETER :: rhs_num = 1
REAL(dfp) :: a(dim_num, dim_num + rhs_num)
INTEGER(i4b) :: info
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
CALL r8mat_solve(dim_num, rhs_num, a, info)

IF (info /= 0) THEN
  r = -1.0D+00
  pc(1:dim_num) = 0.0D+00
END IF

r = 0.5D+00 * SQRT(a(1, dim_num + 1) * a(1, dim_num + 1) &
                   + a(2, dim_num + 1) * a(2, dim_num + 1))
pc(1:dim_num) = t(1:dim_num, 1) + 0.5D+00 * a(1:dim_num, dim_num + 1)

END PROCEDURE triangle_circumcircle_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_circumradius_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: b
REAL(dfp) :: bot
REAL(dfp) :: c
!
!  Compute the length of each side.
!
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
bot = (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)
IF (bot <= 0.0D+00) THEN
  r = -1.0D+00
  RETURN
END IF
r = a * b * c / SQRT(bot)
END PROCEDURE triangle_circumradius_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line_exp_3d
INTEGER(i4b), PARAMETER :: dim_num = 3
INTEGER(i4b) :: ival
REAL(dfp) :: normal(dim_num)
REAL(dfp) :: normal2(dim_num)
REAL(dfp) :: temp
REAL(dfp) :: v1(dim_num)
REAL(dfp) :: v2(dim_num)
!
!  Make sure the line is not degenerate.
!
IF (line_exp_is_degenerate_nd(dim_num, p1, p2)) RETURN
!
!  Make sure the triangle is not degenerate.
!
IF (triangle_is_degenerate_nd(dim_num, t)) RETURN
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
temp = SQRT(SUM(normal(1:dim_num)**2))
normal(1:dim_num) = normal(1:dim_num) / temp
!
!  Find the intersection of the plane and the line.
!
CALL plane_normal_line_exp_int_3d(t(1:dim_num, 1), normal, p1, p2, &
  & ival, pint)
!
IF (ival == 0) THEN
  inside = .FALSE.
  pint(1:dim_num) = HUGE(temp)
  RETURN
ELSE IF (ival == 2) THEN
  inside = .FALSE.
  pint(1:dim_num) = p1(1:dim_num)
  RETURN
END IF
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

IF (DOT_PRODUCT(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) THEN
  inside = .FALSE.
  RETURN
END IF

v1(1:dim_num) = t(1:dim_num, 3) - t(1:dim_num, 2)
v2(1:dim_num) = pint(1:dim_num) - t(1:dim_num, 2)

normal2(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal2(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal2(3) = v1(1) * v2(2) - v1(2) * v2(1)

IF (DOT_PRODUCT(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) THEN
  inside = .FALSE.
  RETURN
END IF

v1(1:dim_num) = t(1:dim_num, 1) - t(1:dim_num, 3)
v2(1:dim_num) = pint(1:dim_num) - t(1:dim_num, 3)

normal2(1) = v1(2) * v2(3) - v1(3) * v2(2)
normal2(2) = v1(3) * v2(1) - v1(1) * v2(3)
normal2(3) = v1(1) * v2(2) - v1(2) * v2(1)

IF (DOT_PRODUCT(normal(1:dim_num), normal2(1:dim_num)) < 0.0D+00) THEN
  inside = .FALSE.
  RETURN
END IF

inside = .TRUE.

END PROCEDURE triangle_contains_line_exp_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_line_par_3d
INTEGER(i4b), PARAMETER :: dim_num = 3
REAL(dfp) :: a
REAL(dfp) :: angle_sum
REAL(dfp) :: b
REAL(dfp) :: c
REAL(dfp) :: d
REAL(dfp) :: denom
LOGICAL(lgt) :: intersect
REAL(dfp) :: norm
REAL(dfp) :: norm1
REAL(dfp) :: norm2
REAL(dfp), PARAMETER :: r8_pi = 3.141592653589793D+00
REAL(dfp) :: t_int
REAL(dfp), PARAMETER :: tol = 0.00001D+00
REAL(dfp) :: v1(dim_num)
REAL(dfp) :: v2(dim_num)
REAL(dfp) :: v3(dim_num)
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
norm1 = SQRT(a * a + b * b + c * c)

IF (norm1 == 0.0D+00) THEN
  inside = .FALSE.
  p(1:dim_num) = 0.0D+00
  RETURN
END IF
!
!  Make sure the implicit line is well defined.
!
norm2 = SQRT(SUM(pd(1:dim_num)**2))

IF (norm2 == 0.0D+00) THEN
  inside = .FALSE.
  p(1:dim_num) = 0.0D+00
  RETURN
END IF
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
IF (ABS(denom) < tol * norm1 * norm2) THEN
!
!  The line may actually lie in the plane.  We're not going
!  to try to address this possibility.
!
  IF (a * p0(1) + b * p0(2) + c * p0(3) + d == 0.0D+00) THEN

    intersect = .TRUE.
    inside = .FALSE.
    p(1:dim_num) = p0(1:dim_num)
!
!  The line and plane are parallel and disjoint.
!
  ELSE

    intersect = .FALSE.
    inside = .FALSE.
    p(1:dim_num) = 0.0D+00

  END IF
!
!  The line and plane intersect at a single point P.
!
ELSE

  intersect = .TRUE.
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

  norm = SQRT(SUM(v1(1:dim_num)**2))

  IF (norm == 0.0D+00) THEN
    inside = .TRUE.
    RETURN
  END IF

  v1(1:dim_num) = v1(1:dim_num) / norm

  norm = SQRT(SUM(v2(1:dim_num)**2))

  IF (norm == 0.0D+00) THEN
    inside = .TRUE.
    RETURN
  END IF

  v2(1:dim_num) = v2(1:dim_num) / norm

  norm = SQRT(SUM(v3(1:dim_num)**2))

  IF (norm == 0.0D+00) THEN
    inside = .TRUE.
    RETURN
  END IF

  v3(1:dim_num) = v3(1:dim_num) / norm

  angle_sum = safe_acos(DOT_PRODUCT(v1(1:3), v2(1:3))) &
              + safe_acos(DOT_PRODUCT(v2(1:3), v3(1:3))) &
              + safe_acos(DOT_PRODUCT(v3(1:3), v1(1:3)))

  IF (NINT(angle_sum / r8_pi) == 2) THEN
    inside = .TRUE.
  ELSE
    inside = .FALSE.
  END IF

END IF

RETURN
END PROCEDURE triangle_contains_line_par_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_1
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: xsi(dim_num + 1)
xsi = triangle_barycentric_2d(t, p)
IF (ANY(xsi(1:3) < 0.0D+00)) THEN
  inside = .FALSE.
ELSE
  inside = .TRUE.
END IF
END PROCEDURE triangle_contains_point_2d_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_2
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b) :: j
INTEGER(i4b) :: k
DO j = 1, 3
  k = MOD(j, 3) + 1
  IF (0.0D+00 < (p(1) - t(1, j)) * (t(2, k) - t(2, j)) &
      - (p(2) - t(2, j)) * (t(1, k) - t(1, j))) THEN
    inside = .FALSE.
    RETURN
  END IF
END DO
inside = .TRUE.
END PROCEDURE triangle_contains_point_2d_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_contains_point_2d_3
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: dir_new
REAL(dfp) :: dir_old
INTEGER(i4b) :: j
INTEGER(i4b) :: k

dir_old = 0.0D+00

DO j = 1, 3
  k = MOD(j, 3) + 1
  dir_new = (p(1) - t(1, j)) * (t(2, k) - t(2, j)) &
            - (p(2) - t(2, j)) * (t(1, k) - t(1, j))
  IF (dir_new * dir_old < 0.0D+00) THEN
    inside = .FALSE.
    RETURN
  END IF
  IF (dir_new /= 0.0D+00) THEN
    dir_old = dir_new
  END IF
END DO
inside = .TRUE.
END PROCEDURE triangle_contains_point_2d_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_diameter_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: asq
REAL(dfp) :: b
REAL(dfp) :: bsq
REAL(dfp) :: c
REAL(dfp) :: csq
!
!  Compute the squared length of each side.
!
asq = SUM(t(1:dim_num, 1) - t(1:dim_num, 2))**2
bsq = SUM(t(1:dim_num, 2) - t(1:dim_num, 3))**2
csq = SUM(t(1:dim_num, 3) - t(1:dim_num, 1))**2
!
!  Take care of a zero side.
!
IF (asq == 0.0D+00) THEN
  diameter = SQRT(bsq)
  RETURN
ELSE IF (bsq == 0.0D+00) THEN
  diameter = SQRT(csq)
  RETURN
ELSE IF (csq == 0.0D+00) THEN
  diameter = SQRT(asq)
  RETURN
END IF
!
!  Make ASQ the largest.
!
IF (asq < bsq) THEN
  CALL swap(asq, bsq)
END IF

IF (asq < csq) THEN
  CALL swap(asq, csq)
END IF
!
!  If ASQ is very large...
!
IF (bsq + csq < asq) THEN
  diameter = SQRT(asq)
ELSE
  a = SQRT(asq)
  b = SQRT(bsq)
  c = SQRT(csq)
  diameter = 2.0D+00 * a * b * c / SQRT((a + b + c) * (-a + b + c) &
                                        * (a - b + c) * (a + b - c))
END IF
END PROCEDURE triangle_diameter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_edge_length_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b) :: j1, j2
DO j1 = 1, 3
  j2 = i4_wrap(j1 + 1, 1, 3)
  edge_length(j1) = NORM2(t(1:dim_num, j2) - t(1:dim_num, j1))
END DO
END PROCEDURE triangle_edge_length_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_gridpoints_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b) :: i
INTEGER(i4b) :: j
!
grid_num = 0
!
!  Special case, SUB_NUM = 0.
!
IF (sub_num == 0) THEN
  IF (1 <= grid_max) THEN
    grid_num = 1
    g(1, 1) = (t(1, 1) + t(1, 2) + t(1, 3)) / 3.0D+00
    g(2, 1) = (t(2, 1) + t(2, 2) + t(2, 3)) / 3.0D+00
  END IF
  RETURN
END IF
!
DO i = 0, sub_num
  DO j = 0, sub_num - i
    IF (grid_num < grid_max) THEN
      grid_num = grid_num + 1
      g(1, grid_num) = (REAL(i, kind=8) * t(1, 1) &
                      & + REAL(j, kind=8) * t(1, 2) &
                      & + REAL(sub_num - i - j, kind=8) * t(1, 3)) &
                      & / REAL(sub_num, kind=8)
      !
      g(2, grid_num) = (REAL(i, kind=8) * t(2, 1) &
                      & + REAL(j, kind=8) * t(2, 2) &
                      & + REAL(sub_num - i - j, kind=8) * t(2, 3)) &
                      & / REAL(sub_num, kind=8)
    END IF
  END DO
END DO
!
END PROCEDURE triangle_gridpoints_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incenter_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: b
REAL(dfp) :: c
REAL(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

IF (perimeter == 0.0D+00) THEN
  pc(1:dim_num) = t(1:dim_num, 1)
ELSE
  pc(1:dim_num) = (b * t(1:dim_num, 1) &
                   + c * t(1:dim_num, 2) &
                   + a * t(1:dim_num, 3)) / perimeter
END IF
END PROCEDURE triangle_incenter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_incircle_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: b
REAL(dfp) :: c
REAL(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

IF (perimeter == 0.0D+00) THEN
  pc(1:dim_num) = t(1:dim_num, 1)
  r = 0.0D+00
  RETURN
END IF

pc(1:dim_num) = ( &
                b * t(1:dim_num, 1) &
                + c * t(1:dim_num, 2) &
                + a * t(1:dim_num, 3)) / perimeter

r = 0.5D+00 * SQRT( &
    (-a + b + c) &
    * (+a - b + c) &
    * (+a + b - c) / perimeter)
END PROCEDURE triangle_incircle_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_inradius_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) :: a
REAL(dfp) :: b
REAL(dfp) :: c
REAL(dfp) :: perimeter
!
!  Compute the length of each side.
!
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))

perimeter = a + b + c

IF (perimeter == 0.0D+00) THEN
  r = 0.0D+00
  RETURN
END IF

r = 0.5D+00 * SQRT( &
    (-a + b + c) &
    * (+a - b + c) &
    * (+a + b - c) / perimeter)
END PROCEDURE triangle_inradius_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_is_degenerate_nd
ans = &
  & (ALL(t(1:dim_num, 1) == t(1:dim_num, 2)) .OR. &
  & ALL(t(1:dim_num, 2) == t(1:dim_num, 3)) .OR. &
  & ALL(t(1:dim_num, 3) == t(1:dim_num, 1)))
END PROCEDURE triangle_is_degenerate_nd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_lattice_layer_point_next
INTEGER(i4b) :: c1n
INTEGER(i4b), PARAMETER :: n = 2
INTEGER(i4b) :: rhs1
INTEGER(i4b) :: rhs2
!
!  Treat layer C(N+1) = 0 specially.
!
IF (c(n + 1) == 0) THEN
  IF (.NOT. more) THEN
    v(1:n) = 0
    more = .TRUE.
  ELSE
    more = .FALSE.
  END IF
  RETURN
END IF
!
!  Compute first point.
!
IF (.NOT. more) THEN
  v(1) = (c(n + 1) - 1) * c(1) + 1
  v(2) = 0
  more = .TRUE.
ELSE
  c1n = i4vec_lcm(n, c)
  rhs1 = c1n * (c(n + 1) - 1)
  rhs2 = c1n * c(n + 1)
  IF (c(2) * (v(1) + 1) + c(1) * v(2) <= rhs2) THEN
    v(1) = v(1) + 1
  ELSE
    v(1) = (rhs1 - c(1) * (v(2) + 1)) / c(2)
    v(1) = MAX(v(1), 0)
    v(2) = v(2) + 1
    IF (c(2) * v(1) + c(1) * v(2) <= rhs1) THEN
      v(1) = v(1) + 1
    END IF
    IF (c(2) * v(1) + c(1) * v(2) <= rhs2) THEN
    ELSE
      v(1:n) = 0
      more = .FALSE.
    END IF
  END IF
END IF
END PROCEDURE triangle_lattice_layer_point_next

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_lattice_point_next
INTEGER(i4b) c1n
INTEGER(i4b), PARAMETER :: n = 2
INTEGER(i4b) rhs

IF (.NOT. more) THEN

  v(1:n) = 0
  more = .TRUE.

ELSE

  c1n = i4vec_lcm(n, c)

  rhs = c1n * c(n + 1)

  IF (c(2) * (v(1) + 1) + c(1) * v(2) <= rhs) THEN
    v(1) = v(1) + 1
  ELSE
    v(1) = 0
    IF (c(2) * v(1) + c(1) * (v(2) + 1) <= rhs) THEN
      v(2) = v(2) + 1
    ELSE
      v(2) = 0
      more = .FALSE.
    END IF
  END IF
END IF
END PROCEDURE triangle_lattice_point_next

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_line_imp_int_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) a1
REAL(dfp) b1
REAL(dfp) c1
INTEGER(i4b) i
INTEGER(i4b) ival
INTEGER(i4b) j
REAL(dfp) p(dim_num)
REAL(dfp) test1
REAL(dfp) test2

int_num = 0

DO i = 1, 3
  j = i4_wrap(i + 1, 1, 3)
  !
  ! Get the implicit form of the line through vertices I and I+1.
  !
  CALL line_exp2imp_2d(t(1:2, i), t(1:2, j), a1, b1, c1)
  !
  ! Seek an intersection with the original line.
  !
  CALL lines_imp_int_2d(a, b, c, a1, b1, c1, ival, p)
  !
  ! If there is an intersection, determine if it
  ! lies between the two vertices.
  !
  IF (ival == 1) THEN
    test1 = SUM((p(1:dim_num) - t(1:dim_num, i)) &
                * (t(1:dim_num, j) - t(1:dim_num, i)))
    test2 = SUM((t(1:dim_num, j) - t(1:dim_num, i)) &
                * (t(1:dim_num, j) - t(1:dim_num, i)))
    !
    IF (0 <= test1 .AND. test1 <= test2) THEN
      int_num = int_num + 1
      pint(1:dim_num, int_num) = p(1:dim_num)
    END IF
  END IF
END DO

END PROCEDURE triangle_line_imp_int_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orientation_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) avar

IF (ALL(t(1:dim_num, 1) == t(1:dim_num, 2)) .OR. &
    ALL(t(1:dim_num, 2) == t(1:dim_num, 3)) .OR. &
    ALL(t(1:dim_num, 3) == t(1:dim_num, 1))) THEN
  ans = 3
  RETURN
END IF

avar = (t(1, 1) - t(1, 3)) * (t(2, 2) - t(2, 3)) &
      & - (t(1, 2) - t(1, 3)) * (t(2, 1) - t(2, 3))

IF (avar == 0.0D+00) THEN
  ans = 2
ELSE IF (avar < 0.0D+00) THEN
  ans = 1
ELSE IF (0.0D+00 < avar) THEN
  ans = 0
END IF

END PROCEDURE triangle_orientation_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_orthocenter_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
LOGICAL(lgt) flag
INTEGER(i4b) ival
REAL(dfp) p23(dim_num)
REAL(dfp) p31(dim_num)
!
!  Determine a point P23 common to the line (P2,P3) and
!  its perpendicular through P1.
!
CALL line_exp_perp_2d(t(1:2, 2), t(1:2, 3), t(1:2, 1), p23, flag)

IF (flag) THEN
  pc(1:2) = r8_huge()
  RETURN
END IF
!
!  Determine a point P31 common to the line (P3,P1) and
!  its perpendicular through P2.
!
CALL line_exp_perp_2d(t(1:2, 3), t(1:2, 1), t(1:2, 2), p31, flag)

IF (flag) THEN
  pc(1:2) = r8_huge()
  RETURN
END IF
!
!  Determine PC, the intersection of the lines (P1,P23) and (P2,P31).
!
CALL lines_exp_int_2d(t(1:2, 1), p23(1:2), t(1:2, 2), p31(1:2), ival, pc)

IF (ival /= 1) THEN
  pc(1:2) = r8_huge()
  flag = .TRUE.
  RETURN
END IF

RETURN
END PROCEDURE triangle_orthocenter_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b), PARAMETER :: side_num = 3
REAL(dfp) dist2
INTEGER(i4b) j
INTEGER(i4b) jp1
!
!  Find the distance to each of the line segments.
!
dist = HUGE(dist)
!
DO j = 1, side_num
  jp1 = i4_wrap(j + 1, 1, side_num)
  dist2 = segment_point_dist_2d(t(1:dim_num, j), t(1:dim_num, jp1), p)
  IF (dist2 < dist) THEN
    dist = dist2
  END IF
END DO
END PROCEDURE triangle_point_dist_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_3d
INTEGER(i4b), PARAMETER :: dim_num = 3
REAL(dfp) dist2
!
!  Compute the distances from the point to each of the sides.
!
dist2 = segment_point_dist_3d(t(1:dim_num, 1), t(1:dim_num, 2), p)
dist = dist2
dist2 = segment_point_dist_3d(t(1:dim_num, 2), t(1:dim_num, 3), p)
dist = MIN(dist, dist2)
dist2 = segment_point_dist_3d(t(1:dim_num, 3), t(1:dim_num, 1), p)
dist = MIN(dist, dist2)
END PROCEDURE triangle_point_dist_3d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_dist_signed_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) dis12
REAL(dfp) dis23
REAL(dfp) dis31
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
IF (dis12 <= 0.0D+00 .AND. dis23 <= 0.0D+00 .AND. dis31 <= 0.0D+00) THEN
  dist_signed = MAX(dis12, dis23, dis31)
!
!  If the point is outside the triangle, then we have to compute
!  the (positive) line segment distances and take the minimum.
!
ELSE
  dis12 = segment_point_dist_2d(t(1:2, 1), t(1:2, 2), p)
  dis23 = segment_point_dist_2d(t(1:2, 2), t(1:2, 3), p)
  dis31 = segment_point_dist_2d(t(1:2, 3), t(1:2, 1), p)
  dist_signed = MIN(dis12, dis23, dis31)
END IF
!
END PROCEDURE triangle_point_dist_signed_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_point_near_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
INTEGER(i4b), PARAMETER :: side_num = 3
INTEGER(i4b) j
INTEGER(i4b) jp1
REAL(dfp) dist2
REAL(dfp) pn2(dim_num)
REAL(dfp) tval
!
!  Find the distance to each of the line segments that make up the edges
!  of the triangle.
!
dist = HUGE(dist)
pn(1:dim_num) = 0.0D+00
DO j = 1, side_num
  jp1 = i4_wrap(j + 1, 1, side_num)
  CALL segment_point_near_2d(t(1:dim_num, j), t(1:dim_num, jp1), p, &
    & pn2, dist2, tval)
  IF (dist2 < dist) THEN
    dist = dist2
    pn(1:dim_num) = pn2(1:dim_num)
  END IF
END DO
END PROCEDURE triangle_point_near_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_quality_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) a
REAL(dfp) b
REAL(dfp) c
!
!  Compute the length of each side.
!
a = SQRT(SUM((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
b = SQRT(SUM((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
c = SQRT(SUM((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
IF (a * b * c == 0.0D+00) THEN
  quality = 0.0D+00
ELSE
  quality = (-a + b + c) * (a - b + c) * (a + b - c) &
            / (a * b * c)
END IF
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
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) alpha(n)
INTEGER(i4b) dim
REAL(dfp) p12(dim_num, n)
REAL(dfp) p13(dim_num, n)
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
alpha(1:n) = SQRT(alpha(1:n))
!
!  Determine the coordinates of the points on sides 2 and 3 intersected
!  by line L.
!
DO dim = 1, dim_num
  p12(dim, 1:n) = (1.0D+00 - alpha(1:n)) * t(dim, 1) &
                  + alpha(1:n) * t(dim, 2)

  p13(dim, 1:n) = (1.0D+00 - alpha(1:n)) * t(dim, 1) &
                  + alpha(1:n) * t(dim, 3)
END DO
!
!  Now choose, uniformly at random, a point on the line L.
!
alpha = rvec_uniform_01(n, seed)

DO dim = 1, dim_num
  p(dim, 1:n) = (1.0D+00 - alpha(1:n)) * p12(dim, 1:n) &
                + alpha(1:n) * p13(dim, 1:n)
END DO

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
INTEGER(kind=4), PARAMETER :: dim_num = 2
p(1:dim_num) = MATMUL(t(1:dim_num, 1:3), xsi(1:dim_num + 1))
END PROCEDURE triangle_xsi_to_xy_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE triangle_xy_to_xsi_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) avar
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
