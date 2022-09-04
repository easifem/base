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

MODULE Line_Method
USE GlobalData
IMPLICIT NONE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: finds if an explicit line is degenerate in ND.
!
!# Introduction
!
!    The explicit form of a line in ND is:
!
!    the line through the points P1 and P2.
!
!    An explicit line is degenerate if the two defining points are equal.
!
!# Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) P1(DIM_NUM), P2(DIM_NUM), two points on the
! line.
!
!    Output, logical ( kind = 4 ) LINE_EXP_IS_DEGENERATE_ND, is TRUE if the
! line is degenerate.
!

interface
  module pure function line_exp_is_degenerate_nd(dim_num, p1, p2) result(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    real(dfp), INTENT(IN) :: p1(dim_num)
    real(dfp), INTENT(IN) :: p2(dim_num)
    logical(lgt) :: ans
  end function
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         converts an explicit line to implicit form in 2D.
!
!# Introduction
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Output, real ( kind = 8 ) A, B, C, the implicit form of the line.
!

interface
  module pure subroutine line_exp2imp_2d(p1, p2, a, b, c)
    real(kind=8), intent(out) :: a, b, c
    real(kind=8), intent(in) :: p1(:)
    real(kind=8), intent(in) :: p2(:)
  end subroutine
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         finds if an implicit point is degenerate in 2D.
!
!# Introduction
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Output, logical ( kind = 4 ) LINE_IMP_IS_DEGENERATE_2D, is true if the
!    line is degenerate.
!

interface
  module pure function line_imp_is_degenerate_2d(a, b, c) result(ans)
    real(dfp), intent(in) :: a, b, c
    logical(lgt) :: ans
  end function
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         determines where two implicit lines intersect in 2D.
!
!# Introduction
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1, B1, C1, define the first line.
!    At least one of A1 and B1 must be nonzero.
!
!    Input, real ( kind = 8 ) A2, B2, C2, define the second line.
!    At least one of A2 and B2 must be nonzero.
!
!    Output, integer ( kind = 4 ) IVAL, reports on the intersection.
!
!    -1, both A1 and B1 were zero.
!    -2, both A2 and B2 were zero.
!     0, no intersection, the lines are parallel.
!     1, one intersection point, returned in P.
!     2, infinitely many intersections, the lines are identical.
!
!    Output, real ( kind = 8 ) P(2), if IVAL = 1, then P is
!    the intersection point.  Otherwise, P = 0.
!

interface
  module pure subroutine lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)
    implicit none
    real(dfp), intent(in) :: a1, b1, c1, a2, b2, c2
    real(dfp), intent(out) :: p(2)
    integer(i4b), intent(out) :: ival
  end subroutine
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: computes a line perpendicular to a line and through a point.
!
!# Introduction
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The input point P3 should NOT lie on the line (P1,P2).  If it
!    does, then the output value P4 will equal P3.
!
!    P1-----P4-----------P2
!            |
!            |
!           P3
!
!    P4 is also the nearest point on the line (P1,P2) to the point P3.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Input, real ( kind = 8 ) P3(2), a point (presumably not on the
!    line (P1,P2)), through which the perpendicular must pass.
!
!    Output, real ( kind = 8 ) P4(2), a point on the line (P1,P2),
!    such that the line (P3,P4) is perpendicular to the line (P1,P2).
!
!    Output, logical ( kind = 4 ) FLAG, is TRUE if the value could
!    not be computed.

interface
  module pure subroutine line_exp_perp_2d(p1, p2, p3, p4, flag)
    real(dfp), intent(in) :: p1(2)
    real(dfp), intent(in) :: p2(2)
    real(dfp), intent(in) :: p3(2)
    real(dfp), intent(out) :: p4(2)
    logical(lgt), intent(out) :: flag
  end subroutine
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: determines where two explicit lines intersect in 2D.
!
!# Introduction
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(2), Q2(2), two points on the second line.
!
!    Output, integer ( kind = 4 ) IVAL, reports on the intersection:
!    0, no intersection, the lines may be parallel or degenerate.
!    1, one intersection point, returned in P.
!    2, infinitely many intersections, the lines are identical.
!
!    Output, real ( kind = 8 ) P(2), if IVAl = 1, P is
!    the intersection point.  Otherwise, P = 0.

interface
  module pure subroutine lines_exp_int_2d(p1, p2, q1, q2, ival, p)
    real(kind=8), intent(in) :: p1(2)
    real(kind=8), intent(in) :: p2(2)
    real(kind=8), intent(in) :: q1(2)
    real(kind=8), intent(in) :: q2(2)
    real(kind=8), intent(out) :: p(2)
    integer(i4b), intent(out) :: ival
  end subroutine
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: distance ( line segment, point ) in 2D.
!
!# Introduction
!
!    A line segment is the finite portion of a line that lies between
!    two points P1 and P2.
!
!    The nearest point will satisfy the condition
!
!      PN = (1-T) * P1 + T * P2.
!
!    T will always be between 0 and 1.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the endpoints of the line segment.
!
!    Input, real ( kind = 8 ) P(2),
! the point whose nearest neighbor on the line
!    segment is to be determined.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    line segment.

interface
  module pure function segment_point_dist_2d(p1, p2, p) result(dist)
    real(dfp), intent(in) :: p1(2)
    real(dfp), intent(in) :: p2(2)
    real(dfp), intent(in) :: p(2)
    real(dfp) :: dist
  end function
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: distance ( line segment, point ) in 3D.
!
!# Introduction
!
!  Discussion:
!
!    A line segment is the finite portion of a line that lies between
!    two points P1 and P2.
!
!    The nearest point will satisfy the condition
!
!      PN = (1-T) * P1 + T * P2.
!
!    T will always be between 0 and 1.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), the endpoints of the segment.
!
!    Input, real ( kind = 8 ) P(3), the point whose nearest neighbor on
!    the line segment is to be determined.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    line segment.
!

interface
  module pure function segment_point_dist_3d(p1, p2, p) result(dist)
    real(dfp), intent(in) :: p1(3)
    real(dfp), intent(in) :: p2(3)
    real(dfp), intent(in) :: p(3)
    real(dfp) :: dist
  end function
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary:         signed distance ( exp line, point ) in 2D.
!
!# Introduction
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The signed distance has two interesting properties:
!
!    *  The absolute value of the signed distance is the
!        usual (Euclidean) distance.
!
!    *  Points with signed distance 0 lie on the line,
!       points with a negative signed distance lie on one side
!         of the line,
!       points with a positive signed distance lie on the
!         other side of the line.
!
!    Assuming that C is nonnegative, then if a point is a positive
!    distance away from the line, it is on the same side of the
!    line as the point (0,0), and if it is a negative distance
!    from the line, it is on the opposite side from (0,0).
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Input, real ( kind = 8 ) P(2), the point whose signed distance is
! desired.
!
!    Output, real ( kind = 8 ) DIST_SIGNED, the signed distance from the
!    point to the line.

interface
  module pure function line_exp_point_dist_signed_2d(p1, p2, p) &
    & result(dist_signed)
    real(dfp), intent(in) :: p(2)
    real(dfp), intent(in) :: p1(2)
    real(dfp), intent(in) :: p2(2)
    real(dfp) :: dist_signed
  end function
end interface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 29 Aug 2022
! summary: nearest point on line segment to point in 2D.
!
!# Introduction
!
!
!    A line segment is the finite portion of a line that lies between
!    two points P1 and P2.
!
!    The nearest point will satisfy the condition
!
!      PN = (1-T) * P1 + T * P2.
!
!    T will always be between 0 and 1.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), the endpoints of the line segment.
!
!    Input, real ( kind = 8 ) P(2), the point whose nearest neighbor
!    on the line segment is to be determined.
!
!    Output, real ( kind = 8 ) PN(2), the point on the line segment which is
!    nearest the point P.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    nearest point on the line segment.
!
!    Output, real ( kind = 8 ) T, the relative position of the point PN
!    to the points P1 and P2.
!

interface
  module pure subroutine segment_point_near_2d(p1, p2, p, pn, dist, t)
    real(dfp), intent(in) :: p1(2)
    real(dfp), intent(in) :: p2(2)
    real(dfp), intent(in) :: p(2)
    real(dfp), intent(out) :: pn(2)
    real(dfp), intent(out) :: dist
    real(dfp), intent(out) :: t
  end subroutine
end interface

END MODULE Line_Method
