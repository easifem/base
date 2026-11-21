! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https://www.gnu.org/licenses/>
!

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Line geometry and interpolation utilities.
!
! Explicit infinite lines, implicit line equations, and finite segments are
! distinct inputs. Signed line distance is negative to the left of the
! oriented line. Intersection status codes are -1/-2 for degenerate implicit
! inputs, 0 for no unique intersection, 1 for a unique point, and 2 for
! coincident lines.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$q=p_1+t(p_2-p_1),\quad0\le t\le1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: segment_point_near_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: nearest(2), d, parameter
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! CALL segment_point_near_2d(p1, p2, [3.0_DFP, 1.0_DFP], &
!   nearest, d, parameter)
! CALL OK(ABS(parameter-1.0_DFP) < 1.0E-12_DFP, "Parameter clamped")
! CALL OK(MAXVAL(ABS(nearest-p2)) < 1.0E-12_DFP, &
!   "Nearest point is the right endpoint")
! CALL OK(ABS(d-SQRT(2.0_DFP)) < 1.0E-12_DFP, "Euclidean distance")
! END PROGRAM main
!```

MODULE Line_Method
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: line_exp_is_degenerate_nd, &
          line_exp2imp_2d, &
          line_imp_is_degenerate_2d, &
          lines_imp_int_2d, &
          line_exp_perp_2d, &
          lines_exp_int_2d, &
          segment_point_dist_2d, &
          segment_point_dist_3d, &
          line_exp_point_dist_signed_2d, &
          segment_point_near_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checks if an explicit line is degenerate in N dimensions.
!
! An explicit line is given by two points, `p1` and `p2`.
! The line is degenerate when the two points are equal.
! The comparison is exact, with no tolerance.
!
! Returns:
!   ans : LOGICAL(lgt) - `.TRUE.` if `p1` equals `p2`
!
! Formula:
!   $$\mathrm{degenerate}\iff p_1=p_2$$
!
! Examples
!
! - [test 1](|base|Line/examples/line_exp_is_degenerate_nd_test_1.F90)
!
! See also: [[line_exp2imp_2d]], [[line_imp_is_degenerate_2d]]

INTERFACE
  MODULE PURE FUNCTION line_exp_is_degenerate_nd(dim_num, p1, p2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    !! number of space dimensions
    REAL(dfp), INTENT(IN) :: p1(dim_num)
    !! first point of the line
    REAL(dfp), INTENT(IN) :: p2(dim_num)
    !! second point of the line
    LOGICAL(lgt) :: ans
    !! `.TRUE.` if the line is degenerate
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Converts an explicit line to implicit form in 2D.
!
! The explicit line passes through `p1` and `p2`.
! The routine returns `a`, `b` and `c` of the implicit equation
! `a*x + b*y + c = 0`.
!
! The routine divides the coefficients by `a**2 + b**2 + c**2`.
! If that gives `a < 0`, it flips all three signs.
! Hence `a` is never negative.
!
! @warning
! If `p1` equals `p2`, the routine returns without setting `a`, `b`
! and `c`. Check the line with [[line_exp_is_degenerate_nd]] first.
! @endwarning
!
! Formula:
!   $$ax+by+c=0$$
!
! Examples
!
! - [test 1](|base|Line/examples/line_exp2imp_2d_test_1.F90)
!
! See also: [[line_exp_is_degenerate_nd]], [[lines_exp_int_2d]]

INTERFACE
  MODULE PURE SUBROUTINE line_exp2imp_2d(p1, p2, a, b, c)
    REAL(kind=8), INTENT(out) :: a, b, c
    !! coefficients of the implicit line equation
    REAL(kind=8), INTENT(in) :: p1(:)
    !! first point of the line, the first two entries are used
    REAL(kind=8), INTENT(in) :: p2(:)
    !! second point of the line, the first two entries are used
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checks if an implicit line is degenerate in 2D.
!
! The implicit line is `a*x + b*y + c = 0`.
! The line is degenerate when `a` and `b` are both zero.
! The value of `c` does not matter.
! The comparison is exact, with no tolerance.
!
! Returns:
!   ans : LOGICAL(lgt) - `.TRUE.` if the line is degenerate
!
! Formula:
!   $$\mathrm{degenerate}\iff a=b=0$$
!
! Examples
!
! - [test 1](|base|Line/examples/line_imp_is_degenerate_2d_test_1.F90)
!
! See also: [[line_exp_is_degenerate_nd]], [[lines_imp_int_2d]]

INTERFACE
  MODULE PURE FUNCTION line_imp_is_degenerate_2d(a, b, c) RESULT(ans)
    REAL(dfp), INTENT(in) :: a, b, c
    !! coefficients of the implicit line equation
    LOGICAL(lgt) :: ans
    !! `.TRUE.` if the line is degenerate
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds where two implicit lines intersect in 2D.
!
! The lines are `a1*x + b1*y + c1 = 0` and `a2*x + b2*y + c2 = 0`.
! The routine returns a status code `ival` and, for a unique
! intersection, the point `p`.
!
! The values of `ival` are:
!
! - `-1`: the first line is degenerate.
! - `-2`: the second line is degenerate.
! - `0`: the lines are parallel and distinct, so there is no intersection.
! - `1`: the lines meet in one point, returned in `p`.
! - `2`: the lines are coincident.
!
! Unless `ival` is 1, `p` is set to zero.
! The parallel and coincident checks use exact comparisons.
!
! Formula:
!   $$a_i x+b_i y+c_i=0,\quad i=1,2$$
!
! Examples
!
! - [test 1](|base|Line/examples/lines_imp_int_2d_test_1.F90)
! - [test 2](|base|Line/examples/lines_imp_int_2d_test_2.F90)
!
! See also: [[line_imp_is_degenerate_2d]], [[lines_exp_int_2d]]

INTERFACE
  MODULE PURE SUBROUTINE lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)
    IMPLICIT NONE
    REAL(dfp), INTENT(in) :: a1, b1, c1, a2, b2, c2
    !! coefficients of the first line (`a1`, `b1`, `c1`) and
    !! of the second line (`a2`, `b2`, `c2`)
    REAL(dfp), INTENT(out) :: p(2)
    !! intersection point, valid only when `ival` is 1
    INTEGER(i4b), INTENT(out) :: ival
    !! status code: -1, -2, 0, 1 or 2, see the description
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Projects a point onto an explicit line in 2D.
!
! The line passes through `p1` and `p2` and is infinite.
! The routine returns the foot `p4` of the perpendicular from `p3`
! to the line.
! The foot can lie outside the segment from `p1` to `p2`.
!
! If `p1` equals `p2`, the line is degenerate.
! Then `flag` is `.TRUE.` and every entry of `p4` is `1.0E+30`.
! Otherwise `flag` is `.FALSE.`.
!
! Formula:
!   $$p_4=p_1+t(p_2-p_1),\quad
!   t=\frac{(p_3-p_1)\cdot(p_2-p_1)}{(p_2-p_1)\cdot(p_2-p_1)}$$
!
! Examples
!
! - [test 1](|base|Line/examples/line_exp_perp_2d_test_1.F90)
!
! See also: [[segment_point_near_2d]], [[line_exp_point_dist_signed_2d]]

INTERFACE
  MODULE PURE SUBROUTINE line_exp_perp_2d(p1, p2, p3, p4, flag)
    REAL(dfp), INTENT(in) :: p1(2)
    !! first point of the line
    REAL(dfp), INTENT(in) :: p2(2)
    !! second point of the line
    REAL(dfp), INTENT(in) :: p3(2)
    !! point to project
    REAL(dfp), INTENT(out) :: p4(2)
    !! foot of the perpendicular from `p3`
    LOGICAL(lgt), INTENT(out) :: flag
    !! `.TRUE.` if the line is degenerate
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds where two explicit lines intersect in 2D.
!
! The first line passes through `p1` and `p2`.
! The second line passes through `q1` and `q2`.
! Both lines are infinite.
! The routine returns a status code `ival` and, for an intersection,
! the point `p`.
!
! A line whose two points are equal is treated as a single point.
! Such a point intersects the other line only if it lies on it.
!
! The values of `ival` are:
!
! - `0`: no intersection, or the lines are parallel and distinct.
! - `1`: the lines meet in one point, returned in `p`.
! - `2`: the lines are coincident.
!
! Unless `ival` is 1, `p` is set to zero.
! Two lines that are both points give `ival` 1 when the points are equal.
!
! @note
! Two lines given as implicit equations are handled by
! [[lines_imp_int_2d]]. That routine also returns `-1` and `-2` for
! degenerate lines. This routine never does.
! @endnote
!
! Formula:
!   $$p=p_1+s(p_2-p_1)=q_1+t(q_2-q_1)$$
!
! Examples
!
! - [test 1](|base|Line/examples/lines_exp_int_2d_test_1.F90)
! - [test 2](|base|Line/examples/lines_exp_int_2d_test_2.F90)
!
! See also: [[lines_imp_int_2d]], [[line_exp2imp_2d]]

INTERFACE
  MODULE PURE SUBROUTINE lines_exp_int_2d(p1, p2, q1, q2, ival, p)
    REAL(kind=8), INTENT(in) :: p1(2)
    !! first point of the first line
    REAL(kind=8), INTENT(in) :: p2(2)
    !! second point of the first line
    REAL(kind=8), INTENT(in) :: q1(2)
    !! first point of the second line
    REAL(kind=8), INTENT(in) :: q2(2)
    !! second point of the second line
    REAL(kind=8), INTENT(out) :: p(2)
    !! intersection point, valid only when `ival` is 1
    INTEGER(i4b), INTENT(out) :: ival
    !! status code: 0, 1 or 2, see the description
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Distance between a line segment and a point in 2D.
!
! The segment runs from `p1` to `p2`.
! The routine returns the distance from `p` to the closest point of
! the segment.
! That point can be an endpoint.
! If `p1` equals `p2`, the result is the distance from `p` to `p1`.
!
! Returns:
!   dist : REAL(dfp) - distance from `p` to the segment
!
! Formula:
!   $$d=\min_{0\le t\le1}\|p-p_1-t(p_2-p_1)\|$$
!
! Examples
!
! - [test 1](|base|Line/examples/segment_point_dist_2d_test_1.F90)
!
! See also: [[segment_point_dist_3d]], [[segment_point_near_2d]]

INTERFACE
  MODULE PURE FUNCTION segment_point_dist_2d(p1, p2, p) RESULT(dist)
    REAL(dfp), INTENT(in) :: p1(2)
    !! first endpoint of the segment
    REAL(dfp), INTENT(in) :: p2(2)
    !! second endpoint of the segment
    REAL(dfp), INTENT(in) :: p(2)
    !! query point
    REAL(dfp) :: dist
    !! distance from `p` to the segment
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Distance between a line segment and a point in 3D.
!
! The segment runs from `p1` to `p2`.
! The routine returns the distance from `p` to the closest point of
! the segment.
! That point can be an endpoint.
! If `p1` equals `p2`, the result is the distance from `p` to `p1`.
!
! Returns:
!   dist : REAL(dfp) - distance from `p` to the segment
!
! Formula:
!   $$d=\min_{0\le t\le1}\|p-p_1-t(p_2-p_1)\|$$
!
! Examples
!
! - [test 1](|base|Line/examples/segment_point_dist_3d_test_1.F90)
!
! See also: [[segment_point_dist_2d]]

INTERFACE
  MODULE PURE FUNCTION segment_point_dist_3d(p1, p2, p) RESULT(dist)
    REAL(dfp), INTENT(in) :: p1(3)
    !! first endpoint of the segment
    REAL(dfp), INTENT(in) :: p2(3)
    !! second endpoint of the segment
    REAL(dfp), INTENT(in) :: p(3)
    !! query point
    REAL(dfp) :: dist
    !! distance from `p` to the segment
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Signed distance between an explicit line and a point in 2D.
!
! The line is infinite and passes through `p1` and `p2`.
! It is oriented from `p1` to `p2`.
! The result is negative if `p` is to the left of the line.
! It is positive if `p` is to the right.
! It is zero if `p` is on the line.
!
! If `p1` equals `p2`, the line is degenerate.
! Then the result is the plain distance from `p` to `p1`.
! It is never negative in this case.
!
! Returns:
!   `dist_signed` : REAL(dfp) - signed distance from `p` to the line
!
! Formula:
!   $$d=\frac{ax+by+c}{\sqrt{a^2+b^2}}$$
!   with $a=y_2-y_1$, $b=x_1-x_2$ and $c=x_2y_1-x_1y_2$.
!
! Examples
!
! - [test 1](|base|Line/examples/line_exp_point_dist_signed_2d_test_1.F90)
!
! See also: [[segment_point_dist_2d]], [[line_exp_perp_2d]]

INTERFACE
  MODULE PURE FUNCTION line_exp_point_dist_signed_2d(p1, p2, p) &
    & RESULT(dist_signed)
    REAL(dfp), INTENT(in) :: p(2)
    !! query point
    REAL(dfp), INTENT(in) :: p1(2)
    !! first point of the line
    REAL(dfp), INTENT(in) :: p2(2)
    !! second point of the line
    REAL(dfp) :: dist_signed
    !! signed distance from `p` to the line
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds the point of a line segment nearest to a point in 2D.
!
! The segment runs from `p1` to `p2`.
! The routine returns the nearest point `pn` on the segment.
! It also returns the distance `dist` from `p` to `pn`.
! The parameter `t` locates `pn` on the segment, with `pn = p1` at
! `t = 0` and `pn = p2` at `t = 1`.
!
! If the projection of `p` falls outside the segment, `t` is clamped
! to 0 or 1. Then `pn` is an endpoint.
! If `p1` equals `p2`, then `t` is 0 and `pn` is `p1`.
!
! Formula:
!   $$q=p_1+t(p_2-p_1),\quad0\le t\le1,\quad d=\|p-q\|$$
!
! Examples
!
! - [test 1](|base|Line/examples/segment_point_near_2d_test_1.F90)
!
! See also: [[segment_point_dist_2d]], [[line_exp_perp_2d]]

INTERFACE
  MODULE PURE SUBROUTINE segment_point_near_2d(p1, p2, p, pn, dist, t)
    REAL(dfp), INTENT(in) :: p1(2)
    !! first endpoint of the segment
    REAL(dfp), INTENT(in) :: p2(2)
    !! second endpoint of the segment
    REAL(dfp), INTENT(in) :: p(2)
    !! query point
    REAL(dfp), INTENT(out) :: pn(2)
    !! nearest point on the segment
    REAL(dfp), INTENT(out) :: dist
    !! distance from `p` to `pn`
    REAL(dfp), INTENT(out) :: t
    !! position of `pn` on the segment, between 0 and 1
  END SUBROUTINE
END INTERFACE

END MODULE Line_Method
