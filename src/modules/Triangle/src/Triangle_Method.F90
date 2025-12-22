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

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Triangle geometry MODULE
!
!# Introduction
!
! This MODULE is just a carbon copy of the MODULE written by
! professor John Burkardt. The original code is kept in the directory
! named "./assets/geometry_burkardt_triangle.inc".
!
! I have just restructured the code according to the code style of
! easifem.

MODULE Triangle_Method
USE GlobalData, ONLY: I4B, LGT, DFP
IMPLICIT NONE
PRIVATE
PUBLIC :: triangle_angles_2d
PUBLIC :: triangle_angles_3d
PUBLIC :: triangle_area_2d
PUBLIC :: triangle_area_3d
PUBLIC :: triangle_area_3d_2
PUBLIC :: triangle_area_3d_3
PUBLIC :: triangle_area_heron
PUBLIC :: triangle_area_vector_3d
PUBLIC :: triangle_barycentric_2d
PUBLIC :: triangle_centroid_2d
PUBLIC :: triangle_centroid_3d
PUBLIC :: triangle_circumcenter_2d
PUBLIC :: triangle_circumcenter_2d_2
PUBLIC :: triangle_circumcenter
PUBLIC :: triangle_circumcircle_2d
PUBLIC :: triangle_circumcircle_2d_2
PUBLIC :: triangle_circumradius_2d
PUBLIC :: triangle_contains_line_exp_3d
PUBLIC :: triangle_contains_line_par_3d
PUBLIC :: triangle_contains_point_2d_1
PUBLIC :: triangle_contains_point_2d_2
PUBLIC :: triangle_contains_point_2d_3
PUBLIC :: triangle_diameter_2d
PUBLIC :: triangle_edge_length_2d
PUBLIC :: triangle_incenter_2d
PUBLIC :: triangle_incircle_2d
PUBLIC :: triangle_inradius_2d
PUBLIC :: triangle_orthocenter_2d
PUBLIC :: triangle_point_dist_2d
PUBLIC :: triangle_point_dist_3d
PUBLIC :: triangle_point_dist_signed_2d
PUBLIC :: triangle_point_near_2d
PUBLIC :: triangle_quality_2d
PUBLIC :: triangle_sample

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Computes the angles of a triangle in 2D.
!
!# Introduction
!
!    The law of cosines is used:
!
!$$
! C^2 = A^2 + B^2 - 2 * A * B * COS ( GAMMA )
!$$
!
!    where GAMMA is the angle opposite side C.

INTERFACE
  MODULE PURE FUNCTION triangle_angles_2d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex in xij format
    REAL(DFP) :: angle(3)
    !! The angles opposite sides P1-P2, P2-P3 and P3-P1, in radians.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Computes the angles of a triangle in 3D.
!
!# Introduction
!
!    The law of cosines is used:
!
!$$
!  C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
!$$
!
!    where GAMMA is the angle opposite side C.

INTERFACE
  MODULE PURE FUNCTION triangle_angles_3d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertices in xij format
    REAL(DFP) :: angle(3)
    !! angle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: compute area of the triangle
!
!# Introduction
!
!    If the triangle's vertices are given in counter clockwise order,
!    the area will be positive.  If the triangle's vertices are given
!    in clockwise order, the area will be negative!
!
!    An earlier version of this routine always returned the absolute
!    value of the computed area.  I am convinced now that that is
!    a less useful RESULT!  For instance, by returning the signed
!    area of a triangle, it is possible to easily compute the area
!    of a nonconvex polygon as the sum of the (possibly negative)
!    areas of triangles formed by node 1 and successive pairs of vertices.
!

INTERFACE
  MODULE PURE FUNCTION triangle_area_2d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         compute area of triangle in 3D
!
!# Introduction
!
!    This routine uses the fact that the norm of the cross product
!    of two vectors is the area of the parallelogram they form.
!
!    Therefore, the area of the triangle is half of that value.

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: area of triangle in 3D
!
!# Introduction
!
!    This routine computes the area "the hard way".

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_2(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: area of triangle using Heron's formula

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_3(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Area of triangle using Herons formula
!
!# Introduction
!
!    The formula is valid for any spatial dimension, depENDing only
!    on the lengths of the sides, and not the coordinates of the vertices.

INTERFACE
  MODULE PURE FUNCTION triangle_area_heron(s) RESULT(area)
    REAL(DFP), INTENT(IN) :: s(3)
    REAL(DFP) :: area
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:  compute the area vector of a tri in 3D
!
!# Introduction
!
!    The "area vector" of a triangle is simply a cross product of,
!    for instance, the vectors (V2-V1) and (V3-V1), where V1, V2
!    and V3 are the vertices of the triangle.
!
!    The norm of the cross product vector of two vectors is the area
!    of the parallelogram they form.
!
!    Therefore, the area of the triangle is half of the norm of the
!    area vector:
!
!      area = 0.5 * sqrt ( sum ( area_vector(1:3)^2 ) )
!
!    The reason for looking at the area vector rather than the area
!    is that this makes it possible to compute the area of a flat
!    polygon in 3D by summing the areas of the triangles that form
!    a decomposition of the polygon, while allowing for both positive
!    and negative areas.  (Sum the vectors, THEN take the norm and
!    multiply by 1/2).

INTERFACE
  MODULE PURE FUNCTION triangle_area_vector_3d(t) RESULT(area_vector)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: area_vector(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         Find the barycentric coordinates of a point in 2D
!
!# Introduction
!
!    The barycentric coordinate of point P related to vertex A can be
!    interpreted as the ratio of the area of the triangle with
!    vertex A replaced by vertex P to the area of the original
!    triangle.
!
!    This routine assumes that the triangle vertices are given in
!    counter clockwise order.

INTERFACE
  MODULE PURE FUNCTION triangle_barycentric_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex
    REAL(DFP), INTENT(IN) :: p(2)
    !! point
    REAL(DFP) :: xsi(3)
    !! barycentric points
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         compute the centroid of a triangle in 2D
!
!# Introduction
!
!    The centroid of a triangle can also be considered the
!    center of gravity, or center of mass, assuming that the triangle
!    is made of a thin uniform sheet of massy material.
!
!    The centroid of a triangle is the intersection of the medians.
!
!    A median of a triangle is a line connecting a vertex to the
!    midpoint of the opposite side.
!
!    In barycentric coordinates, in which the vertices of the triangle
!    have the coordinates (1,0,0), (0,1,0) and (0,0,1), the centroid
!    has coordinates (1/3,1/3,1/3).
!
!    In geometry, the centroid of a triangle is often symbolized by "G".

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_2d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: centroid(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: computes the centroid of a triangle in 3D
!
!# Introduction
!
!    The centroid of a triangle can also be considered the
!    center of gravity or center of mass, assuming that the triangle
!    is made of a thin uniform sheet of massy material.
!
!    The centroid of a triangle is the intersection of the medians.
!    A median of a triangle is a line connecting any vertex to the
!    midpoint of the opposite side.

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_3d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: centroid(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         cirumcenter of a triangle in 2D
!
!# Introduction
!
!    The circumcenter of a triangle is the center of the circumcircle, the
!    circle that passes through the three vertices of the triangle.
!
!    The circumcircle contains the triangle, but it is not necessarily the
!    smallest triangle to do so.
!
!    If all angles of the triangle are no greater than 90 degrees, then
!    the center of the circumscribed circle will lie inside the triangle.
!    Otherwise, the center will lie outside the triangle.
!
!    The circumcenter is the intersection of the perpENDicular bisectors
!    of the sides of the triangle.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: circum center in 2d
!
!# Introduction
!
!    The circumcenter of a triangle is the center of the circumcircle, the
!    circle that passes through the three vertices of the triangle.
!
!    The circumcircle contains the triangle, but it is not necessarily the
!    smallest triangle to do so.
!
!    If all angles of the triangle are no greater than 90 degrees, then
!    the center of the circumscribed circle will lie inside the triangle.
!    Otherwise, the center will lie outside the triangle.
!
!    The circumcenter is the intersection of the perpENDicular bisectors
!    of the sides of the triangle.
!
!    Surprisingly, the diameter of the circle can be found by solving
!    a 2 by 2 linear system.  If we label the vertices of the triangle
!    P1, P2 and P3, then the vectors P2 - P1 and P3 - P1 are secants of
!    the circle, and each forms a right triangle with the diameter
!    vector through P1.
!
!    Hence, the dot product of P2 - P1 with the diameter vector is equal
!    to the square of the length of P2 - P1, and similarly for P3 - P1.
!    This determines the diameter vector originating at P1.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".
!

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d_2(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: computes the circumcenter of a triangle in ND.
!
!# Introduction
!
!    Three ND points A, B and C lie on a circle.
!
!    The circumcenter P has the formula
!
!      P = ( Area ( PBC ) * A + Area ( APC) * B + Area ( ABP ) * C )
!        / ( Area ( PBC )     + Area ( APC )    + Area ( ABP ) )
!
!    The details of the formula rely on information supplied
!    by Oscar Lanzi III.

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter(n, t) RESULT(p)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! shape (n,3)
    REAL(DFP) :: p(n)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28  Aug 2022
! summary: computes the circumcircle of a triangle in 2D
!
!# Introduction
!
!    The circumcenter of a triangle is the center of the circumcircle, the
!    circle that passes through the three vertices of the triangle.
!
!    The circumcircle contains the triangle, but it is not necessarily the
!    smallest triangle to do so.
!
!    If all angles of the triangle are no greater than 90 degrees, then
!    the center of the circumscribed circle will lie inside the triangle.
!    Otherwise, the center will lie outside the triangle.
!
!    The circumcenter is the intersection of the perpENDicular bisectors
!    of the sides of the triangle.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circum center
    REAL(DFP), INTENT(OUT) :: r
    !! circum radius
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         circumcircle
!
!# Introduction
!
!    The circumscribed circle of a triangle is the circle that passes through
!    the three vertices of the triangle.  The circumscribed circle contains
!    the triangle, but it is not necessarily the smallest triangle to do so.
!
!    Surprisingly, the diameter of the circle can be found by solving
!    a 2 by 2 linear system.  This is because the vectors P2 - P1
!    and P3 - P1 are secants of the circle, and each forms a right
!    triangle with the diameter.  Hence, the dot product of
!    P2 - P1 with the diameter is equal to the square of the length
!    of P2 - P1, and similarly for P3 - P1.  This determines the
!    diameter vector originating at P1.
!
!    If all angles of the triangle are no greater than 90 degrees, then
!    the center of the circumscribed circle will lie inside the triangle.
!    Otherwise, the center will lie outside the triangle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d_2(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circum center
    REAL(DFP), INTENT(OUT) :: r
    !! circum radius
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: triangle circumradius in 2d
!
!# Introduction
!
!    The circumscribed circle of a triangle is the circle that passes through
!    the three vertices of the triangle.  The circumscribed circle contains
!    the triangle, but it is not necessarily the smallest triangle to do so.
!
!    The circumradius of a triangle is the radius of the circumscribed
!    circle.

INTERFACE
  MODULE PURE FUNCTION triangle_circumradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: r
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         is a line inside the triangle in 3D
!
!# Introduction
!
!    A line will "intersect" the plane of a triangle in 3D if
!    * the line does not lie in the plane of the triangle
!      (there would be infinitely many intersections), AND
!    * the line does not lie parallel to the plane of the triangle
!      (there are no intersections at all).
!
!    Therefore, if a line intersects the plane of a triangle, it does so
!    at a single point.  We say the line is "inside" the triangle if,
!    regarded as 2D objects, the intersection point of the line and the plane
!    is inside the triangle.
!
!    A triangle in 3D is determined by three points:
!
!      T(1:3,1), T(1:3,2) and T(1:3,3).
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1(1:3), P2(1:3).
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P1(3), P2(3), two points on the line.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if
! (the intersection point of)
!    the line is inside the triangle.
!
!    Output, REAL ( kind = 8 ) PINT(3), the point where the line
!    intersects the plane of the triangle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_exp_3d(t, p1, p2, &
    & inside, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p1(3)
    REAL(DFP), INTENT(IN) :: p2(3)
    LOGICAL(LGT), INTENT(OUT) :: inside
    REAL(DFP), INTENT(OUT) :: pint(3)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         finds if a line is inside a triangle in 3D.
!
!# Introduction
!
!    A line will "intersect" the plane of a triangle in 3D if
!    * the line does not lie in the plane of the triangle
!      (there would be infinitely many intersections), AND
!    * the line does not lie parallel to the plane of the triangle
!      (there are no intersections at all).
!
!    Therefore, if a line intersects the plane of a triangle, it does so
!    at a single point.  We say the line is "inside" the triangle if,
!    regarded as 2D objects, the intersection point of the line and the plane
!    is inside the triangle.
!
!    A triangle in 3D is determined by three points:
!
!      T(1:3,1), T(1:3,2) and T(1:3,3).
!
!    The parametric form of a line in 3D is:
!
!      P(1:3) = P0(1:3) + PD(1:3) * T
!
!    We can normalize by requiring PD to have euclidean norm 1,
!    and the first nonzero entry positive.
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(3,3), the three points that define
!    the triangle.
!
!    Input, REAL ( kind = 8 ) P0(3), PD(3), parameters that define the
!    parametric line.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if
!     (the intersection point of)
!    the line is inside the triangle.
!
!    Output, REAL ( kind = 8 ) P(3), is the point of intersection of the line
!    and the plane of the triangle, unless they are parallel.

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_par_3d(t, p0, pd, inside, p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p0(3)
    REAL(DFP), INTENT(IN) :: pd(3)
    REAL(DFP), INTENT(OUT) :: p(3)
    LOGICAL(LGT), INTENT(OUT) :: inside
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         finds if a point is inside the triangle in 2D
!
!# Introduction
!
!    It is conventional to list the triangle vertices in counter clockwise
!    order.  However, this routine does not require a particular order
!    for the vertices.
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is inside
!    the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_1(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         finds if a poiint is inside a triangle in 2D
!
!# Introduction
!
!    The routine assumes that the vertices are given in counter clockwise
!    order.  If the triangle vertices are actually given in clockwise
!    order, this routine will behave as though the triangle contains
!    no points whatsoever!
!
!    The routine determines if a point P is "to the right of" each of the
! lines
!    that bound the triangle.  It does this by computing the cross product
!    of vectors from a vertex to its next vertex, and to P.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!    The vertices should be given in counter clockwise order.
!
!    Input, REAL ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is
!    inside the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_2(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns true if the point is contained inside the triangle
!
!# Introduction
!
!    This routine is the same as TRIANGLE_CONTAINS_POINT_2D_2, except
!    that it does not assume an ordering of the points.  It should
!    work correctly whether the vertices of the triangle are listed
!    in clockwise or counter clockwise order.
!
!    The routine determines if a point P is "to the right of" each of the
! lines
!    that bound the triangle.  It does this by computing the cross product
!    of vectors from a vertex to its next vertex, and to P.
!
!    The point is inside the triangle if it is to the right of all
!    the lines, or to the left of all the lines.
!
!    This version was suggested by Paulo Ernesto of Maptek Brasil.
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is
!    inside the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_3(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    LOGICAL(LGT) :: inside
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns the triangle diameter in 2d
!
!# Introduction
!
!    The diameter of a triangle is the diameter of the smallest circle
!    that can be drawn around the triangle.  At least two of the vertices
!    of the triangle will intersect the circle, but not necessarily
!    all three!
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, REAL ( kind = 8 ) DIAMETER, the diameter of the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_diameter_2d(t) RESULT(diameter)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: diameter
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns edge lengths of a triangle in 2D
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, REAL ( kind = 8 ) EDGE_LENGTH(3), the length of the edges.

INTERFACE
  MODULE PURE FUNCTION triangle_edge_length_2d(t) RESULT(edge_length)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: edge_length(3)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         Returns grid points within a triangle in 2D
!
!# Introduction
!
!    The gridpoints are computed by repeated halving of the triangle.
!    The 0-th set of grid points is the vertices themselves.
!    The first set of grid points is the midpoints of the sides.
!    These points can be used to draw 4 triangles that make up the original
!    triangle.  The second set of grid points is the side midpoints and
!     centers
!    of these four triangles.
!
!     SUB_NUM                     GRID_NUM
!    -----                        -----
!        0      1                  =  1  (centroid)
!        1      1 + 2              =  3  (vertices)
!        2      1 + 2 + 3          =  6
!        3      1 + 2 + 3 + 4      = 10
!        4      1 + 2 + 3 + 4 + 5  = 15
!
!    GRID_NUM is the sum of the integers from 1 to SUB_NUM+1 or
!
!      GRID_NUM = (SUB_NUM+1) * (SUB_NUM+2) / 2
!
!# Parameters:
!
!- Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!- Input, integer ( kind = 4 ) SUB_NUM, the number of subdivisions.
!- Input, integer ( kind = 4 ) GRID_MAX, the maximum number of grid points.
!- Output, integer ( kind = 4 ) GRID_NUM, the number of grid points
! returned.
!- Output, REAL ( kind = 8 ) G(2,GRID_MAX), the grid points.
!

INTERFACE
  MODULE PURE SUBROUTINE triangle_gridpoints_2d(t, sub_num, grid_max, &
      & grid_num, g)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B), INTENT(IN) :: sub_num
    INTEGER(I4B), INTENT(IN) :: grid_max
    INTEGER(I4B), INTENT(OUT) :: grid_num
    REAL(DFP), INTENT(OUT) :: g(2, grid_max)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: computes the incenter of a triangle in 2D.
!
!# Introduction
!
!    The incenter of a triangle is the center of the inscribed circle.
!
!    The inscribed circle of a triangle is the largest circle that can
!    be drawn inside the triangle.
!
!    The inscribed circle is tangent to all three sides of the triangle.
!
!    The angle bisectors of the triangle intersect at the center of the
!    inscribed circle.
!
!    In geometry, the incenter is often represented by "I".
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, REAL ( kind = 8 ) PC(2), the incenter.

INTERFACE
  MODULE PURE FUNCTION triangle_incenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         computes the inscribed circle of a triangle in 2D.
!
!# Introduction
!
!    The inscribed circle of a triangle is the largest circle that can
!    be drawn inside the triangle.  It is tangent to all three sides,
!    and the lines from its center to the vertices bisect the angles
!    made by each vertex.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, REAL ( kind = 8 ) R, PC(2), the radius and center of the
!    inscribed circle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_incircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(OUT) :: pc(2)
    REAL(DFP), INTENT(OUT) :: r
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Radius of the inscribed circle of a triangle in 2D
!
!# Introduction
!
!    The inscribed circle of a triangle is the largest circle that can
!    be drawn inside the triangle.  It is tangent to all three sides,
!    and the lines from its center to the vertices bisect the angles
!    made by each vertex.
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!    Output, REAL ( kind = 8 ) R, the radius of the inscribed circle.

INTERFACE
  MODULE PURE FUNCTION triangle_inradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: r
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         finds if a triangle is degenerate in ND.
!
!# Introduction
!
!    A triangle in ND is described by the coordinates of its 3 vertices.
!    A triangle in ND is degenerate if any two vertices are equal.
!
!# Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!    Input, REAL ( kind = 8 ) T(DIM_NUM,3), the triangle vertices.
!    Output, logical ( kind = 4 ) TRIANGLE_IS_DEGENERATE_ND, is TRUE if the
!    triangle is degenerate.

INTERFACE
  MODULE PURE FUNCTION triangle_is_degenerate_nd(dim_num, t) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    REAL(DFP), INTENT(IN) :: t(dim_num, 3)
    LOGICAL(LGT) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: next triangle lattice layer point.
!
!# Introduction
!
!    The triangle lattice layer L is bounded by the lines
!
!      0 <= X,
!      0 <= Y,
!      L - 1 < X / C(1) + Y / C(2) <= L.
!
!    In particular, layer L = 0 always contains the single point (0,0).
!
!    This FUNCTION returns, one at a time, the points that lie within
!    a given triangle lattice layer.
!
!    Thus, if we set C(1) = 2, C(2) = 3, then we get the following layers:
!
!    L = 0: (0,0)
!    L = 1: (1,0), (2,0), (0,1), (1,1), (0,2), (0,3)
!    L = 2: (3,0), (4,0), (2,1), (3,1), (1,2), (2,2), (1,3), (2,3),
!           (0,4), (1,4), (0,5), (0,6).
!
!# Parameters:
!
!- Input, integer ( kind = 4 ) C(3), coefficients defining the
! lattice layer.  Entry C(3) contains the layer index. C(1) and C(2) should
! be positive, and C(3) must be nonnegative.
!- Input/output, integer ( kind = 4 ) V(2).  On first call for a given layer,
!    the input value of V is not important.  On a repeated call for the same
!    layer, the input value of V should be the output value from the previous
!    call.  On output, V contains the next lattice layer point.
!- Input/output, logical ( kind = 4 ) MORE.  On input, set MORE to FALSE to
!    indicate that this is the first call for a given layer.  Thereafter, the
!    input value should be the output value from the previous call.
! On output, MORE is TRUE if the returned value V is a new point.
!    If the output value is FALSE, then no more points were found,
!    and V was reset to 0, and the lattice layer has been exhausted.

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_layer_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    LOGICAL(LGT), INTENT(INOUT) :: more
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns the next triangle lattice point
!
!# Introduction
!
!    The lattice triangle is defined by the vertices:
!
!      (0,0), (C(3)/C(1), 0) and (0,C(3)/C(2))
!
!    The lattice triangle is bounded by the lines
!
!      0 <= X,
!      0 <= Y
!      X / C(1) + Y / C(2) <= C(3)
!
!    Lattice points are listed one at a time, starting at the origin,
!    with X increasing first.
!
!# Parameters:
!
!    Input, integer ( kind = 4 ) C(3), coefficients defining the
!    lattice triangle.  These should be positive.
!
!    Input/output, integer ( kind = 4 ) V(2).  On first call, the input
!    value is not important.  On a repeated call, the input value should
!    be the output value from the previous call.  On output, V contains
!    the next lattice point.
!
!    Input/output, logical ( kind = 4 ) MORE.  On input, set MORE to FALSE to
!    indicate that this is the first call for a given triangle.  Thereafter,
!    the input value should be the output value from the previous call.  On
!    output, MORE is TRUE if the returned value V is a new lattice point.
!    If the output value is FALSE, then no more lattice points were found,
!    and V was reset to 0, and the routine should not be called further
!    for this triangle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    LOGICAL(LGT), INTENT(INOUT) :: more
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         implicit line intersects a triangle in 2D.
!
!# Introduction
!
!    An implicit line is the set of points ( X, Y ) satisfying
!
!      A * X + B * Y + C = 0
!
!    where at least one of A and B is not zero.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) A, B, C, determine the equation of the line:
!    A*X + B*Y + C = 0.
!
!    Output, integer ( kind = 4 ) INT_NUM, the number of points of
! intersection
!    of the line with the triangle.  INT_NUM may be 0, 1, 2 or 3.
!
!    Output, REAL ( kind = 8 ) PINT(2,3), contains the intersection points.

INTERFACE
  MODULE PURE SUBROUTINE triangle_line_imp_int_2d(t, a, b, c, int_num, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: a, b, c
    INTEGER(I4B), INTENT(OUT) :: int_num
    INTEGER(I4B), INTENT(OUT) :: pint(2, 3)
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         determines the orientation of a triangle in 2D.
!
!# Introduction
!
!    Three distinct non-colinear points in the plane define a circle.
!    If the points are visited in the order P1, P2, and then
!    P3, this motion defines a clockwise or counter clockwise
!    rotation along the circle.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, integer ( kind = 4 ) TRIANGLE_ORIENTATION_2D, reports if the
!    three points lie clockwise on the circle that passes through them.
!    The possible return values are:
!    0, the points are distinct, noncolinear, and lie counter clockwise
!    on their circle.
!    1, the points are distinct, noncolinear, and lie clockwise
!    on their circle.
!    2, the points are distinct and colinear.
!    3, at least two of the points are identical.

INTERFACE
  MODULE PURE FUNCTION triangle_orientation_2d(t) RESULT(ans)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B) :: ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         computes the orthocenter of a triangle in 2D.
!
!# Introduction
!
!    The orthocenter is defined as the intersection of the three altitudes
!    of a triangle.
!
!    An altitude of a triangle is the line through a vertex of the triangle
!    and perpENDicular to the opposite side.
!
!    In geometry, the orthocenter of a triangle is often symbolized by "H".
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, REAL ( kind = 8 ) PC(2), the orthocenter of the triangle.
!
!    Output, logical ( kind = 4 ) FLAG, is TRUE if the value could not
!    be computed.

INTERFACE
  MODULE PURE FUNCTION triangle_orthocenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: pc(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         distance ( triangle, point ) in 2D
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the point to be checked.
!
!    Output, REAL ( kind = 8 ) DIST, the distance from the point to the
!    triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_2d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(:)
    REAL(DFP) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         distance ( triangle, point ) in 3D.
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(3), the point which is to be checked.
!
!    Output, REAL ( kind = 8 ) DIST, the distance from the point to the
!    triangle.  DIST is zero if the point lies exactly on the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_3d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(:)
    REAL(DFP) :: dist
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         signed distance in 2D
!
!# Introduction
!
!    If the signed distance is:
!    0, the point is on the boundary of the triangle;
!    negative, the point is in the triangle;
!    positive, the point is outside the triangle.
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!    These should be given in counter clockwise order.
!
!    Input, REAL ( kind = 8 ) P(2), the point which is to be checked.
!
!    Output, REAL ( kind = 8 ) DIST_SIGNED, the signed distance from the
!    point to the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_signed_2d(t, p) RESULT(dist_signed)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP) :: dist_signed
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         computes the nearest point on a triangle in 2D.
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the point whose nearest triangle point
!    is to be determined.
!
!    Output, REAL ( kind = 8 ) PN(2), the nearest point to P.
!
!    Output, REAL ( kind = 8 ) DIST, the distance from the point to the
!    triangle.

INTERFACE
  MODULE PURE SUBROUTINE triangle_point_near_2d(t, p, pn, dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP), INTENT(OUT) :: pn(2)
    REAL(DFP), INTENT(OUT) :: dist
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         "quality" of a triangle in 2D.
!
!# Introduction
!
!    The quality of a triangle is 2.0 times the ratio of the radius of
!    the inscribed circle divided by that of the circumscribed circle.
!    An equilateral triangle achieves the maximum possible quality of 1.
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!    Output, REAL ( kind = 8 ) QUALITY, the quality of the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_quality_2d(t) RESULT(quality)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP) :: quality
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: count lattice points.
!
!# Introduction
!
!    The triangle is assumed to be a right triangle which, without loss
!    of generality, has the coordinates:
!
!    ( (0,0), (a,0), (0,b) )
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!    The formula for this FUNCTION occurred to me (JVB) after some thought,
!    on 06 July 2009.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, define the vertices.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.

INTERFACE
  MODULE PURE FUNCTION triangle_right_lattice_point_num_2d(a, b) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: a
    INTEGER(I4B), INTENT(IN) :: b
    INTEGER(I4B) :: n
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         returns random points in a triangle.
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, integer ( kind = 4 ) N, the number of points to generate.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, REAL ( kind = 8 ) P(2,N), random points in the triangle.

INTERFACE
  MODULE PURE FUNCTION triangle_sample(t, n, seed) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), INTENT(IN) :: seed
    REAL(DFP) :: p(2, n)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:          count lattice points.
!
!# Introduction
!
!    The triangle is assumed to be the unit triangle:
!
!$$
!    ( (0,0), (1,0), (0,1) )
!$$
!
!    or a copy of this triangle scaled by an integer S:
!
!$$
!    ( (0,0), (S,0), (0,S) ).
!$$
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the scale factor.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.
!

INTERFACE
  MODULE PURE FUNCTION triangle01_lattice_point_num_2d(s) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: s
    INTEGER(I4B) :: n
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:         converts from barycentric to XY coordinates in 2D.
!
!# Introduction
!
!  Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) XSI(3), the barycentric coordinates of a point.
!    XSI(1) + XSI(2) + XSI(3) should equal 1, but this is not checked.
!
!    Output, REAL ( kind = 8 ) P(2), the XY coordinates of the point.
!

INTERFACE
  MODULE PURE FUNCTION triangle_xsi_to_xy_2d(t, xsi) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: xsi(3)
    REAL(DFP) :: p(2)
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary:          converts from XY to barycentric in 2D.
!
!# Introduction
!
!# Parameters:
!
!    Input, REAL ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, REAL ( kind = 8 ) P(2), the XY coordinates of a point.
!
!    Output, REAL ( kind = 8 ) XSI(3), the barycentric coordinates of the
! point.
!    XSI1 + XSI2 + XSI3 should equal 1.
!

INTERFACE
  MODULE PURE FUNCTION triangle_xy_to_xsi_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    REAL(DFP), INTENT(IN) :: p(2)
    REAL(DFP) :: xsi(3)
  END FUNCTION
END INTERFACE

END MODULE Triangle_Method
