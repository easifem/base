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
! summary: Finds if an explicit line is degenerate in ND.
!
!
! Arguments:
!   dim_num : INTEGER(I4B), INTENT(IN). Use the declaration and description
! for this overload.
!   p1 : REAL(dfp), INTENT(IN)(dim_num). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(IN)(dim_num). Second explicit line endpoint.
!
! Returns:
!   ans : LOGICAL(lgt).
!
! Formula:
! $$\mathrm{degenerate}\iff p_1=p_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Line_Method, ONLY: line_exp_is_degenerate_nd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! LOGICAL(LGT) :: deg
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! deg = line_exp_is_degenerate_nd(2_I4B, p1, p1)
! CALL OK(deg, "Coincident endpoints are degenerate")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION line_exp_is_degenerate_nd(dim_num, p1, p2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    REAL(dfp), INTENT(IN) :: p1(dim_num)
    REAL(dfp), INTENT(IN) :: p2(dim_num)
    LOGICAL(lgt) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Converts an explicit line to implicit form in 2D.
!
!
! Arguments:
!   a : REAL(kind=8), INTENT(out). Implicit line coefficient of x, or scalar
! output as declared.
!   b : REAL(kind=8), INTENT(out). Implicit line coefficient of y.
!   c : REAL(kind=8), INTENT(out). Implicit line constant coefficient.
!   p1 : REAL(kind=8), INTENT(in)(:). First explicit line endpoint.
!   p2 : REAL(kind=8), INTENT(in)(:). Second explicit line endpoint.
!
! Returns:
!   Updates a, b, c.
!
! Formula:
! $$ax+by+c=0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: line_exp2imp_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: a, b, c
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! CALL line_exp2imp_2d(p1, p2, a, b, c)
! CALL OK(ABS(a*p1(1)+b*p1(2)+c) < 1.0E-12_DFP .AND. &
!   ABS(a*p2(1)+b*p2(2)+c) < 1.0E-12_DFP, "Both endpoints lie on line")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE line_exp2imp_2d(p1, p2, a, b, c)
    REAL(kind=8), INTENT(out) :: a, b, c
    REAL(kind=8), INTENT(in) :: p1(:)
    REAL(kind=8), INTENT(in) :: p2(:)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds if an implicit point is degenerate in 2D.
!
!
! Arguments:
!   a : REAL(dfp), INTENT(in). Implicit line coefficient of x, or scalar
! output as declared.
!   b : REAL(dfp), INTENT(in). Implicit line coefficient of y.
!   c : REAL(dfp), INTENT(in). Implicit line constant coefficient.
!
! Returns:
!   ans : LOGICAL(lgt).
!
! Formula:
! $$\mathrm{degenerate}\iff a=b=0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Line_Method, ONLY: line_imp_is_degenerate_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! LOGICAL(LGT) :: deg
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! deg = line_imp_is_degenerate_2d(0.0_DFP, 0.0_DFP, 1.0_DFP)
! CALL OK(deg, "Zero normal is degenerate")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION line_imp_is_degenerate_2d(a, b, c) RESULT(ans)
    REAL(dfp), INTENT(in) :: a, b, c
    LOGICAL(lgt) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Determines where two implicit lines intersect in 2D.
!
!
! Arguments:
!   a1 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   b1 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   c1 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   a2 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   b2 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   c2 : REAL(dfp), INTENT(in). Use the declaration and description for this
! overload.
!   p : REAL(dfp), INTENT(out)(2). Query point or output point as indicated by
! INTENT.
!   ival : INTEGER(i4b), INTENT(out). Intersection status: see the line
! intersection convention.
!
! Returns:
!   Updates p(2), ival.
!
! Formula:
! $$a_i x+b_i y+c_i=0,\quad i=1,2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: lines_imp_int_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: p(2)
! INTEGER(I4B) :: status
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! CALL lines_imp_int_2d(1.0_DFP, 0.0_DFP, -1.0_DFP, &
!   0.0_DFP, 1.0_DFP, -2.0_DFP, status, p)
! CALL OK(status == 1, "Unique intersection")
! CALL OK(MAXVAL(ABS(p-[1.0_DFP, 2.0_DFP])) < 1.0E-12_DFP, &
!   "Intersection solves both line equations")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)
    IMPLICIT NONE
    REAL(dfp), INTENT(in) :: a1, b1, c1, a2, b2, c2
    REAL(dfp), INTENT(out) :: p(2)
    INTEGER(i4b), INTENT(out) :: ival
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes a line perpendicular to a line and through a point.
!
!
! Arguments:
!   p1 : REAL(dfp), INTENT(in)(2). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(in)(2). Second explicit line endpoint.
!   p3 : REAL(dfp), INTENT(in)(2). Use the declaration and description for
! this overload.
!   p4 : REAL(dfp), INTENT(out)(2). Use the declaration and description for
! this overload.
!   flag : LOGICAL(lgt), INTENT(out). Failure flag; .TRUE. indicates a
! degenerate line.
!
! Returns:
!   Updates p4(2), flag.
!
! Formula:
! $$q=p_1+\frac{(p-p_1)\cdot d}{d\cdot d}d$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Line_Method, ONLY: line_exp_perp_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: foot(2)
! LOGICAL(LGT) :: failed
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! CALL line_exp_perp_2d(p1, p2, [1.0_DFP, 1.0_DFP], foot, failed)
! CALL OK(.NOT. failed, "Projection succeeds")
! CALL OK(MAXVAL(ABS(foot-[1.0_DFP, 0.0_DFP])) < 1.0E-12_DFP, &
!   "Orthogonal projection onto infinite line")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE line_exp_perp_2d(p1, p2, p3, p4, flag)
    REAL(dfp), INTENT(in) :: p1(2)
    REAL(dfp), INTENT(in) :: p2(2)
    REAL(dfp), INTENT(in) :: p3(2)
    REAL(dfp), INTENT(out) :: p4(2)
    LOGICAL(lgt), INTENT(out) :: flag
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Determines where two explicit lines intersect in 2D.
!
!
! Arguments:
!   p1 : REAL(kind=8), INTENT(in)(2). First explicit line endpoint.
!   p2 : REAL(kind=8), INTENT(in)(2). Second explicit line endpoint.
!   q1 : REAL(kind=8), INTENT(in)(2). Use the declaration and description for
! this overload.
!   q2 : REAL(kind=8), INTENT(in)(2). Use the declaration and description for
! this overload.
!   p : REAL(kind=8), INTENT(out)(2). Query point or output point as indicated
! by INTENT.
!   ival : INTEGER(i4b), INTENT(out). Intersection status: see the line
! intersection convention.
!
! Returns:
!   Updates p(2), ival.
!
! Formula:
! $$p=p_1+s(p_2-p_1)=q_1+t(q_2-q_1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: lines_exp_int_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: p(2)
! INTEGER(I4B) :: status
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! CALL lines_exp_int_2d(p1, p2, [1.0_DFP, -1.0_DFP], &
!   [1.0_DFP, 1.0_DFP], status, p)
! CALL OK(status == 1, "Unique intersection")
! CALL OK(MAXVAL(ABS(p-[1.0_DFP, 0.0_DFP])) < 1.0E-12_DFP, &
!   "Horizontal and vertical lines intersect")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE lines_exp_int_2d(p1, p2, q1, q2, ival, p)
    REAL(kind=8), INTENT(in) :: p1(2)
    REAL(kind=8), INTENT(in) :: p2(2)
    REAL(kind=8), INTENT(in) :: q1(2)
    REAL(kind=8), INTENT(in) :: q2(2)
    REAL(kind=8), INTENT(out) :: p(2)
    INTEGER(i4b), INTENT(out) :: ival
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Distance ( line segment, point ) in 2D.
!
!
! Arguments:
!   p1 : REAL(dfp), INTENT(in)(2). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(in)(2). Second explicit line endpoint.
!   p : REAL(dfp), INTENT(in)(2). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   dist : REAL(dfp).
!
! Formula:
! $$d=\min_{0\le t\le1}\|p-p_1-t(p_2-p_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: segment_point_dist_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: d
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! d = segment_point_dist_2d(p1, p2, [3.0_DFP, 0.0_DFP])
! CALL OK(ABS(d-1.0_DFP) < 1.0E-12_DFP, "Nearest endpoint distance")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION segment_point_dist_2d(p1, p2, p) RESULT(dist)
    REAL(dfp), INTENT(in) :: p1(2)
    REAL(dfp), INTENT(in) :: p2(2)
    REAL(dfp), INTENT(in) :: p(2)
    REAL(dfp) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Distance ( line segment, point ) in 3D.
!
!
! Arguments:
!   p1 : REAL(dfp), INTENT(in)(3). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(in)(3). Second explicit line endpoint.
!   p : REAL(dfp), INTENT(in)(3). Query point or output point as indicated by
! INTENT.
!
! Returns:
!   dist : REAL(dfp).
!
! Formula:
! $$d=\min_{0\le t\le1}\|p-p_1-t(p_2-p_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: segment_point_dist_3d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: d
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! d = segment_point_dist_3d([0.0_DFP, 0.0_DFP, 0.0_DFP], &
!   [2.0_DFP, 0.0_DFP, 0.0_DFP], [1.0_DFP, 0.0_DFP, 1.0_DFP])
! CALL OK(ABS(d-1.0_DFP) < 1.0E-12_DFP, "Spatial segment distance")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION segment_point_dist_3d(p1, p2, p) RESULT(dist)
    REAL(dfp), INTENT(in) :: p1(3)
    REAL(dfp), INTENT(in) :: p2(3)
    REAL(dfp), INTENT(in) :: p(3)
    REAL(dfp) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Signed distance ( exp line, point ) in 2D.
!
!
! Arguments:
!   p : REAL(dfp), INTENT(in)(2). Query point or output point as indicated by
! INTENT.
!   p1 : REAL(dfp), INTENT(in)(2). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(in)(2). Second explicit line endpoint.
!
! Returns:
!   dist_signed : REAL(dfp).
!
! Formula:
! $$d=\frac{ax+by+c}{\sqrt{a^2+b^2}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Line_Method, ONLY: line_exp_point_dist_signed_2d
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: p1(2), p2(2)
! REAL(DFP) :: d
!
! p1 = [0.0_DFP, 0.0_DFP]
! p2 = [2.0_DFP, 0.0_DFP]
! d = line_exp_point_dist_signed_2d(p1, p2, [1.0_DFP, 1.0_DFP])
! CALL OK(ABS(d+1.0_DFP) < 1.0E-12_DFP, &
!   "Left side of the oriented horizontal line is negative")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION line_exp_point_dist_signed_2d(p1, p2, p) &
    & RESULT(dist_signed)
    REAL(dfp), INTENT(in) :: p(2)
    REAL(dfp), INTENT(in) :: p1(2)
    REAL(dfp), INTENT(in) :: p2(2)
    REAL(dfp) :: dist_signed
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Nearest point on line segment to point in 2D.
!
!
! Arguments:
!   p1 : REAL(dfp), INTENT(in)(2). First explicit line endpoint.
!   p2 : REAL(dfp), INTENT(in)(2). Second explicit line endpoint.
!   p : REAL(dfp), INTENT(in)(2). Query point or output point as indicated by
! INTENT.
!   pn : REAL(dfp), INTENT(out)(2). Nearest point on the segment.
!   dist : REAL(dfp), INTENT(out). Distance from the query point to the
! segment or triangle boundary.
!   t : REAL(dfp), INTENT(out). Clamped segment interpolation parameter in
! [0,1].
!
! Returns:
!   Updates pn(2), dist, t.
!
! Formula:
! $$q=p_1+t(p_2-p_1),\quad0\le t\le1,\quad d=\|p-q\|$$
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

INTERFACE
  MODULE PURE SUBROUTINE segment_point_near_2d(p1, p2, p, pn, dist, t)
    REAL(dfp), INTENT(in) :: p1(2)
    REAL(dfp), INTENT(in) :: p2(2)
    REAL(dfp), INTENT(in) :: p(2)
    REAL(dfp), INTENT(out) :: pn(2)
    REAL(dfp), INTENT(out) :: dist
    REAL(dfp), INTENT(out) :: t
  END SUBROUTINE
END INTERFACE

END MODULE Line_Method
