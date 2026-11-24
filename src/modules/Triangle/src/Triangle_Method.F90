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
! summary: Geometry routines for a triangle given by its vertices.
!
! This module holds geometry routines for a triangle given by its
! vertices.
! John Burkardt wrote the original kernels.
! Vikas Sharma adapted them.
! The source file is `assets/geometry_burkardt_triangle.inc`.
!
! The routines cover these groups.
!
! 1. Area: `triangle_area_2d`, `triangle_area_3d`, `triangle_area_heron`
!    and `triangle_area_vector_3d`.
! 2. Angles, edges and quality: `triangle_angles_2d`,
!    `triangle_edge_length_2d` and `triangle_quality_2d`.
! 3. Centers and circles: centroid, circumcenter, incenter, orthocenter,
!    circumcircle and incircle.
! 4. Point tests: containment, distance and nearest point.
! 5. Sampling and barycentric coordinates: `triangle_sample` and
!    `triangle_barycentric_2d`.
!
! Each vertex is one column of the input array `t`.
! The planar area has a sign.
! The spatial area is never negative.
! The distance routines measure the distance to the edges, not to the
! filled face.
!
! @warning
! `triangle_diameter_2d` gives a wrong result for most triangles.
! `triangle_area_vector_3d` returns a vector, not a scalar area.
! Its length is twice the area.
! Read the notes of each routine before you use it.
! @endwarning
!
! Formula:
!   $$ A = \tfrac{1}{2} \| (v_2 - v_1) \times (v_3 - v_1) \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_2d_test_1.F90)

MODULE Triangle_Method
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: DFP
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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the interior angles of a triangle in 2D.
!
! Returns the three angles of the triangle in radians.
! The angle `angle(i)` is the angle at vertex `i`.
! The three angles add up to $\pi$.
!
! The routine handles degenerate triangles as follows.
!
! 1. If all three vertices coincide, every angle is $2\pi/3$.
! 2. If a vertex coincides with another one, the angle at that vertex
!    is $\pi$.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_angles_2d_test_1.F90)
!
! See also: [[triangle_angles_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_angles_2d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:2, 1:3)`.
    REAL(DFP) :: angle(3)
    !! Angles at vertex 1, 2 and 3, in radians.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the interior angles of a triangle in 3D.
!
! Returns the three angles of the triangle in radians.
! The angle `angle(i)` is the angle at vertex `i`.
! The three angles add up to $\pi$.
!
! The routine handles degenerate triangles as follows.
!
! 1. If all three vertices coincide, every angle is $2\pi/3$.
! 2. If a vertex coincides with another one, the angle at that vertex
!    is $\pi$.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_angles_3d_test_1.F90)
!
! See also: [[triangle_angles_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_angles_3d(t) RESULT(angle)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:3, 1:3)`.
    REAL(DFP) :: angle(3)
    !! Angles at vertex 1, 2 and 3, in radians.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the signed area of a triangle in 2D.
!
! The area is positive when the vertices run counterclockwise.
! It is negative when they run clockwise.
! Reversing the vertex order changes the sign.
!
! Formula:
!   $$ A = \frac{1}{2}\left[x_1(y_2-y_3) + x_2(y_3-y_1)
!   + x_3(y_1-y_2)\right] $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_2d_test_1.F90)
!
! See also: [[triangle_area_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_2d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:2, 1:3)`.
    REAL(DFP) :: area
    !! Signed area of the triangle.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a triangle in 3D.
!
! The area is half the length of the cross product of two edges.
! It is never negative.
!
! Formula:
!   $$ A = \frac{1}{2}\|(v_2-v_1)\times(v_3-v_1)\| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_3d_test_1.F90)
!
! See also: [[triangle_area_3d_2]], [[triangle_area_3d_3]],
! [[triangle_area_vector_3d]], [[triangle_area_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:3, 1:3)`.
    REAL(DFP) :: area
    !! Area of the triangle.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a triangle in 3D from base and height.
!
! The routine takes the edge from vertex 1 to vertex 2 as the base.
! It computes the height of vertex 3 above the base line.
! The area is half the base times the height.
! If vertices 1 and 2 coincide, the area is zero.
!
! Formula:
!   $$ A = \frac{1}{2}\, b\, h $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_3d_2_test_1.F90)
!
! See also: [[triangle_area_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_2(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:3, 1:3)`.
    REAL(DFP) :: area
    !! Area of the triangle.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a triangle in 3D with Heron's formula.
!
! The routine first computes the three side lengths.
! It then applies Heron's formula.
! If the side lengths cannot form a triangle, the routine returns `-1`.
!
! Formula:
!   $$ A = \frac{1}{4}\sqrt{(s_1+s_2+s_3)(-s_1+s_2+s_3)
!   (s_1-s_2+s_3)(s_1+s_2-s_3)} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_3d_3_test_1.F90)
!
! See also: [[triangle_area_heron]], [[triangle_area_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_3d_3(t) RESULT(area)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:3, 1:3)`.
    REAL(DFP) :: area
    !! Area of the triangle, or `-1` for invalid side lengths.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a triangle from its side lengths.
!
! The routine uses Heron's formula.
! If the side lengths cannot form a triangle, the routine returns `-1`.
!
! Formula:
!   $$ A = \frac{1}{4}\sqrt{(s_1+s_2+s_3)(-s_1+s_2+s_3)
!   (s_1-s_2+s_3)(s_1+s_2-s_3)} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_heron_test_1.F90)
!
! See also: [[triangle_area_3d_3]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_heron(s) RESULT(area)
    REAL(DFP), INTENT(IN) :: s(3)
    !! Lengths of the three sides.
    REAL(DFP) :: area
    !! Area of the triangle, or `-1` for invalid side lengths.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area vector of a triangle in 3D.
!
! The area vector is the cross product of two edges of the triangle.
! It is normal to the triangle plane.
! Its direction follows the vertex order by the right-hand rule.
!
! @note
! The length of the area vector is twice the area of the triangle.
! @endnote
!
! Formula:
!   $$ \mathbf{a} = (v_2-v_1)\times(v_3-v_1) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_area_vector_3d_test_1.F90)
!
! See also: [[triangle_area_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_area_vector_3d(t) RESULT(area_vector)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:3, 1:3)`.
    REAL(DFP) :: area_vector(3)
    !! Cross product of the edges `v2 - v1` and `v3 - v1`.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the barycentric coordinates of a point in 2D.
!
! Returns the weights of the three vertices that give the point `p`.
! The weights add up to 1.
! All weights are in the range 0 to 1 when `p` lies inside the triangle.
! Otherwise, at least one weight is negative.
!
! @warning
! The triangle must not be degenerate. For a degenerate triangle the
! result is not valid, and the routine reports no error.
! @endwarning
!
! Formula:
!   $$ p = \sum_{i=1}^{3} \xi_i v_i, \quad \sum_{i=1}^{3} \xi_i = 1 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_barycentric_2d_test_1.F90)
!
! See also: [[triangle_area_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_barycentric_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads `t(1:2, 1:3)`.
    REAL(DFP), INTENT(IN) :: p(2)
    !! Coordinates of the point.
    REAL(DFP) :: xsi(3)
    !! Barycentric coordinates `xi_1`, `xi_2` and `xi_3` of `p`.
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the centroid of a triangle in 2D.
!
! The centroid is the mean of the three vertices.
! The vertices are the columns of `t`.
!
! Formula:
!   $$c = (v_1 + v_2 + v_3) / 3$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_centroid_2d_test_1.F90)
!
! See also: [[triangle_centroid_3d]]

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_2d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: centroid(2)
    !! centroid of the triangle
  END FUNCTION triangle_centroid_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the centroid of a triangle in 3D.
!
! The centroid is the mean of the three vertices.
! The vertices are the columns of `t`.
!
! Formula:
!   $$c = (v_1 + v_2 + v_3) / 3$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_centroid_3d_test_1.F90)
!
! See also: [[triangle_centroid_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_centroid_3d(t) RESULT(centroid)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 3` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: centroid(3)
    !! centroid of the triangle
  END FUNCTION triangle_centroid_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcenter of a triangle in 2D.
!
! The circumcenter is the point at equal distance from the three
! vertices. It is the center of the circle through them.
! The vertices are the columns of `t`.
!
! @warning
! The triangle must not be degenerate. For collinear vertices, the
! function divides by zero and returns invalid values.
! @endwarning
!
! Formula:
!   $$\|c - v_1\| = \|c - v_2\| = \|c - v_3\|$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumcenter_2d_test_1.F90)
!
! See also: [[triangle_circumcenter_2d_2]], [[triangle_circumcenter]]

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: pc(2)
    !! circumcenter of the triangle
  END FUNCTION triangle_circumcenter_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcenter of a triangle in 2D by a linear solve.
!
! The circumcenter is the point at equal distance from the three
! vertices. It is the center of the circle through them.
! The vertices are the columns of `t`.
!
! If the vertices are collinear, the function returns the origin.
!
! Formula:
!   $$\|c - v_1\| = \|c - v_2\| = \|c - v_3\|$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumcenter_2d_2_test_1.F90)
!
! See also: [[triangle_circumcenter_2d]], [[triangle_circumcenter]]

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter_2d_2(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: pc(2)
    !! circumcenter of the triangle
  END FUNCTION triangle_circumcenter_2d_2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcenter of a triangle in `n` dimensions.
!
! The circumcenter is the point at equal distance from the three
! vertices. It is the center of the circle through them.
! The vertices are the columns of `t`.
!
! @warning
! The triangle must not be degenerate. For collinear vertices, the
! function divides by zero and returns invalid values.
! @endwarning
!
! Formula:
!   $$\|c - v_1\| = \|c - v_2\| = \|c - v_3\|$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumcenter_test_1.F90)
!
! See also: [[triangle_circumcenter_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_circumcenter(n, t) RESULT(p)
    INTEGER(I4B), INTENT(IN) :: n
    !! spatial dimension of the vertices
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= n` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: p(n)
    !! circumcenter of the triangle
  END FUNCTION triangle_circumcenter
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcenter and circumradius of a triangle in 2D.
!
! The circumcircle is the circle through the three vertices.
! The vertices are the columns of `t`.
!
! If the vertices are collinear, `r` is -1 and `pc` is the origin.
!
! Formula:
!   $$R = abc / (4A)$$
!
! Here `a`, `b` and `c` are the side lengths. `A` is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumcircle_2d_test_1.F90)
!
! See also: [[triangle_circumcircle_2d_2]], [[triangle_circumradius_2d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP), INTENT(OUT) :: r
    !! circumradius, or -1 for a degenerate triangle
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circumcenter
  END SUBROUTINE triangle_circumcircle_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcenter and circumradius of a triangle in 2D
!   by a linear solve.
!
! The circumcircle is the circle through the three vertices.
! The vertices are the columns of `t`.
!
! @warning
! The triangle must not be degenerate. For collinear vertices, the
! routine sets `r` to -1 and `pc` to the origin. It then overwrites
! both with values from the failed solve, so they are not valid.
! @endwarning
!
! Formula:
!   $$R = abc / (4A)$$
!
! Here `a`, `b` and `c` are the side lengths. `A` is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumcircle_2d_2_test_1.F90)
! - [test 2](|base|Triangle/examples/triangle_circumcircle_2d_2_test_2.F90)
!
! See also: [[triangle_circumcircle_2d]], [[triangle_circumradius_2d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_circumcircle_2d_2(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP), INTENT(OUT) :: r
    !! circumradius
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! circumcenter
  END SUBROUTINE triangle_circumcircle_2d_2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumradius of a triangle in 2D.
!
! The circumradius is the radius of the circle through the three
! vertices. The vertices are the columns of `t`.
!
! If the vertices are collinear, the function returns -1.
!
! Formula:
!   $$R = abc / (4A)$$
!
! Here `a`, `b` and `c` are the side lengths. `A` is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_circumradius_2d_test_1.F90)
!
! See also: [[triangle_circumcircle_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_circumradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 2` and `SIZE(t, 2) >= 3`
    REAL(DFP) :: r
    !! circumradius, or -1 for a degenerate triangle
  END FUNCTION triangle_circumradius_2d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds where a line through two points meets a triangle in 3D.
!
! The line passes through `p1` and `p2`.
! The routine finds where it crosses the plane of the triangle.
! It sets `inside` to `.TRUE.` if that point lies in the triangle.
!
! If the line is parallel to the plane, `inside` is `.FALSE.`.
! Then every component of `pint` is `HUGE(1.0_DFP)`.
!
! If the line lies in the plane, `inside` is `.FALSE.` and `pint` is `p1`.
!
! @warning
! If `p1` equals `p2`, or the triangle is degenerate, the routine
! returns at once. Then `inside` and `pint` are not set.
! @endwarning
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_contains_line_exp_3d_test_1.F90)
!
! See also: [[triangle_contains_line_par_3d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_exp_3d(t, p1, p2, &
    & inside, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 3` and `SIZE(t, 2) >= 3`
    REAL(DFP), INTENT(IN) :: p1(3)
    !! first point on the line
    REAL(DFP), INTENT(IN) :: p2(3)
    !! second point on the line
    LOGICAL(LGT), INTENT(OUT) :: inside
    !! `.TRUE.` if the intersection point lies in the triangle
    REAL(DFP), INTENT(OUT) :: pint(3)
    !! intersection of the line and the plane of the triangle
  END SUBROUTINE triangle_contains_line_exp_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds where a line with a point and direction meets a triangle
!   in 3D.
!
! The line is `p0 + s * pd` for a real number `s`.
! The routine finds where it crosses the plane of the triangle.
! It sets `inside` to `.TRUE.` if that point lies in the triangle.
!
! If the line is parallel to the plane, `inside` is `.FALSE.`.
! Then `p` is `p0` if the line lies in the plane, and zero otherwise.
! The routine treats the line as parallel when the angle is very small.
!
! If the triangle is degenerate or `pd` is zero, `inside` is `.FALSE.`
! and `p` is zero.
!
! Formula:
!   $$p = p_0 + s\, p_d, \quad p \in \mathrm{triangle}$$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_contains_line_par_3d_test_1.F90)
!
! See also: [[triangle_contains_line_exp_3d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line_par_3d(t, p0, pd, inside, p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) >= 3` and `SIZE(t, 2) >= 3`
    REAL(DFP), INTENT(IN) :: p0(3)
    !! point on the line
    REAL(DFP), INTENT(IN) :: pd(3)
    !! direction of the line
    REAL(DFP), INTENT(OUT) :: p(3)
    !! intersection of the line and the plane of the triangle
    LOGICAL(LGT), INTENT(OUT) :: inside
    !! `.TRUE.` if the intersection point lies in the triangle
  END SUBROUTINE triangle_contains_line_par_3d
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Tests if a point lies in a triangle in 2D.
!
! Returns `.TRUE.` if the point `p` is inside the triangle `t` or on its
! boundary.
!
! The routine computes the barycentric coordinates of `p`.
! The point is inside if none of them is negative.
! The triangle must not be degenerate.
!
! Returns:
!   inside : `.TRUE.` if the point lies in the triangle
!
! Formula:
!   $$ p \in T \iff \lambda_i(p) \ge 0, \quad i = 1, 2, 3 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_contains_point_2d_1_test_1.F90)
!
! See also: [[triangle_contains_point_2d_2]],
! [[triangle_contains_point_2d_3]]

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_1(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! coordinates of the query point
    LOGICAL(LGT) :: inside
    !! `.TRUE.` if `p` is inside the triangle or on its boundary
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Tests if a point lies in a triangle in 2D, counterclockwise.
!
! Returns `.TRUE.` if the point `p` is inside the triangle `t` or on its
! boundary.
!
! The routine checks that `p` is not on the right of any directed edge.
!
! @note
! List the vertices of `t` counterclockwise. For other orders, use
! [[triangle_contains_point_2d_3]].
! @endnote
!
! Returns:
!   inside : `.TRUE.` if the point lies in the triangle
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_contains_point_2d_2_test_1.F90)
!
! See also: [[triangle_contains_point_2d_1]],
! [[triangle_contains_point_2d_3]]

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_2(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! coordinates of the query point
    LOGICAL(LGT) :: inside
    !! `.TRUE.` if `p` is inside the triangle or on its boundary
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Tests if a point lies in a triangle in 2D, any vertex order.
!
! Returns `.TRUE.` if the point `p` is inside the triangle `t` or on its
! boundary.
!
! The routine checks that `p` lies on the same side of all three edges.
! It works for clockwise and counterclockwise vertex order.
!
! Returns:
!   inside : `.TRUE.` if the point lies in the triangle
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_contains_point_2d_3_test_1.F90)
!
! See also: [[triangle_contains_point_2d_1]],
! [[triangle_contains_point_2d_2]]

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point_2d_3(t, p) RESULT(inside)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! coordinates of the query point
    LOGICAL(LGT) :: inside
    !! `.TRUE.` if `p` is inside the triangle or on its boundary
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the diameter of a triangle in 2D.
!
! Returns the diameter of the smallest circle that encloses the
! triangle `t`.
! For an acute triangle, this is the circle through the three vertices.
! For an obtuse triangle, it is the longest edge.
!
! @warning
! The code sums the coordinate differences of an edge and then squares
! the sum. It should square each difference first. The result is wrong
! for most triangles.
! @endwarning
!
! Returns:
!   diameter : diameter of the enclosing circle
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_diameter_2d_test_1.F90)
! - [test 2](|base|Triangle/examples/triangle_diameter_2d_test_2.F90)
!
! See also: [[triangle_edge_length_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_diameter_2d(t) RESULT(diameter)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: diameter
    !! diameter of the enclosing circle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the edge lengths of a triangle in 2D.
!
! Returns the length of each edge of the triangle `t`.
! Edge `i` joins vertex `i` to vertex `i+1`.
! Edge 3 joins vertex 3 to vertex 1.
!
! Returns:
!   `edge_length` : the three edge lengths
!
! Formula:
!   $$ \ell_i = \| v_{i+1} - v_i \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_edge_length_2d_test_1.F90)

INTERFACE
  MODULE PURE FUNCTION triangle_edge_length_2d(t) RESULT(edge_length)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: edge_length(3)
    !! length of edge `i`, from vertex `i` to vertex `i+1`
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Returns grid points within a triangle in 2D.
!
! Fills `g` with points that subdivide the triangle `t`.
! The routine splits each edge of `t` into `sub_num` equal parts.
!
! 1. If `sub_num` is 0, the only point is the centroid.
! 2. Otherwise the points are `(i*v1 + j*v2 + k*v3) / sub_num`.
!    Here `i`, `j`, `k` are non-negative integers with
!    `i + j + k = sub_num`.
!
! The full set has `(sub_num+1)*(sub_num+2)/2` points.
! The routine stores at most `grid_max` of them.
! Then `grid_num` is the number of points stored.
!
! @note
! Columns of `g` after `grid_num` keep undefined values.
! @endnote

INTERFACE
  MODULE PURE SUBROUTINE triangle_gridpoints_2d(t, sub_num, grid_max, &
      & grid_num, g)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    INTEGER(I4B), INTENT(IN) :: sub_num
    !! number of subdivisions of each edge, `sub_num >= 0`
    INTEGER(I4B), INTENT(IN) :: grid_max
    !! maximum number of points to store, second size of `g`
    INTEGER(I4B), INTENT(OUT) :: grid_num
    !! number of points stored in `g`
    REAL(DFP), INTENT(OUT) :: g(2, grid_max)
    !! grid points, one per column; columns `1:grid_num` hold points
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the incenter of a triangle in 2D.
!
! Returns the incenter of the triangle `t`.
! The incenter is the center of the inscribed circle.
! It is the average of the vertices, weighted by the length of the
! opposite edges.
! If all vertices coincide, it returns the first vertex.
!
! Returns:
!   pc : coordinates of the incenter
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_incenter_2d_test_1.F90)
!
! See also: [[triangle_incircle_2d]], [[triangle_inradius_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_incenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: pc(2)
    !! coordinates of the incenter
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the inscribed circle of a triangle in 2D.
!
! Returns the center `pc` and the radius `r` of the circle inscribed in
! the triangle `t`.
! If all vertices coincide, the center is the first vertex and the
! radius is 0.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_incircle_2d_test_1.F90)
!
! See also: [[triangle_incenter_2d]], [[triangle_inradius_2d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_incircle_2d(t, r, pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(OUT) :: pc(2)
    !! coordinates of the center
    REAL(DFP), INTENT(OUT) :: r
    !! radius of the inscribed circle
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the radius of the inscribed circle of a triangle in 2D.
!
! Returns the radius of the circle inscribed in the triangle `t`.
! If all vertices coincide, it returns 0.
!
! Returns:
!   r : radius of the inscribed circle
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_inradius_2d_test_1.F90)
!
! See also: [[triangle_incircle_2d]], [[triangle_incenter_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_inradius_2d(t) RESULT(r)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: r
    !! radius of the inscribed circle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Tests if a triangle in N dimensions is degenerate.
!
! Returns `.TRUE.` if two vertices of the triangle `t` are equal.
! The test compares coordinates exactly.
!
! @note
! Three distinct points on a line are not reported as degenerate.
! @endnote
!
! Returns:
!   ans : `.TRUE.` if the triangle is degenerate

INTERFACE
  MODULE PURE FUNCTION triangle_is_degenerate_nd(dim_num, t) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim_num
    !! spatial dimension
    REAL(DFP), INTENT(IN) :: t(dim_num, 3)
    !! vertex coordinates, one vertex per column
    LOGICAL(LGT) :: ans
    !! `.TRUE.` if two vertices coincide
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Returns the next point of a triangle lattice layer.
!
! Returns the points of one triangle lattice layer, one at a time.
! Layer `L` holds the points `(x, y)` with `x >= 0`, `y >= 0` and
! `L - 1 < x/c(1) + y/c(2) <= L`.
! The layer index is `L = c(3)`.
! Layer 0 holds only the point `(0,0)`.
!
! 1. On the first call for a layer, set `more` to `.FALSE.`.
!    The value of `v` on input does not matter.
! 2. On each later call, pass the `v` and `more` of the previous call.
! 3. Stop when a call returns `more = .FALSE.`.
!    Then `v` is 0 and the layer has no more points.
!
! For example, take `c = [2, 3, L]`. The layers are:
!
! - `L = 0`: `(0,0)`
! - `L = 1`: `(1,0), (2,0), (0,1), (1,1), (0,2), (0,3)`
! - `L = 2`: `(3,0), (4,0), (2,1), (3,1), (1,2), (2,2), (1,3), (2,3),
!   (0,4), (1,4), (0,5), (0,6)`
!
! See also: [[triangle_lattice_point_next]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_layer_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    !! `c(1)` and `c(2)` shape the layer and must be positive.
    !! `c(3)` is the layer index and must not be negative.
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    !! on output, the next point of the layer
    LOGICAL(LGT), INTENT(INOUT) :: more
    !! `.FALSE.` on the first call, then the value from the last call.
    !! On output it is `.TRUE.` if `v` holds a new point.
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Returns the next lattice point of a triangle.
!
! Returns the lattice points of a triangle one at a time.
! The lattice triangle has the vertices `(0,0)`, `(c(3)/c(1), 0)` and
! `(0, c(3)/c(2))`.
! A point `(x, y)` belongs to it if `x >= 0`, `y >= 0` and
! `x/c(1) + y/c(2) <= c(3)`.
!
! The routine starts at the origin and lists `x` first.
!
! 1. On the first call, set `more` to `.FALSE.`.
!    The value of `v` on input does not matter.
! 2. On each later call, pass the `v` and `more` of the previous call.
! 3. Stop when a call returns `more = .FALSE.`.
!    Then `v` is 0 and there are no more points.
!
! Do not call the routine again after that.
!
! See also: [[triangle_lattice_layer_point_next]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_lattice_point_next(c, v, more)
    INTEGER(I4B), INTENT(IN) :: c(3)
    !! coefficients of the lattice triangle, all positive
    INTEGER(I4B), INTENT(INOUT) :: v(2)
    !! on output, the next lattice point
    LOGICAL(LGT), INTENT(INOUT) :: more
    !! `.FALSE.` on the first call, then the value from the last call.
    !! On output it is `.TRUE.` if `v` holds a new point.
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Finds where an implicit line meets a triangle in 2D.
!
! Finds the points where the line `a*x + b*y + c = 0` crosses the
! edges of the triangle `t`.
! At least one of `a` and `b` must be nonzero.
!
! The routine stores the points in the first `int_num` columns of
! `pint`.
! A line through a vertex can report that vertex once for each edge.
! A line along an edge reports no point for that edge.
!
! @bug
! `pint` has type `INTEGER(I4B)`, but it holds real coordinates.
! The routine truncates them.
! @endbug

INTERFACE
  MODULE PURE SUBROUTINE triangle_line_imp_int_2d(t, a, b, c, int_num, pint)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: a, b, c
    !! coefficients of the line `a*x + b*y + c = 0`
    INTEGER(I4B), INTENT(OUT) :: int_num
    !! number of intersection points, 0 to 3
    INTEGER(I4B), INTENT(OUT) :: pint(2, 3)
    !! intersection points, one per column; columns `1:int_num` are valid
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Determines the orientation of a triangle in 2D.
!
! Returns an integer code for the vertex order of the triangle `t`.
! Three distinct points that are not on a line lie on one circle.
! Visiting them as vertex 1, 2, 3 turns clockwise or counterclockwise
! along that circle.
!
! The codes are:
!
! - 0: the points are not on a line and turn counterclockwise.
! - 1: the points are not on a line and turn clockwise.
! - 2: the points are distinct and on a line.
! - 3: at least two points coincide.
!
! The test compares values exactly, with no tolerance.
!
! Returns:
!   `ans` : orientation code, 0 to 3

INTERFACE
  MODULE PURE FUNCTION triangle_orientation_2d(t) RESULT(ans)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    INTEGER(I4B) :: ans
    !! orientation code, 0 to 3
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the orthocenter of a triangle in 2D.
!
! Returns the orthocenter of the triangle `t`.
! The orthocenter is the point where the three altitudes meet.
!
! If the routine cannot find the point, every entry of the result is
! `HUGE(1.0_DFP)`.
! This happens for a degenerate triangle.
!
! Returns:
!   `pc` : coordinates of the orthocenter
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_orthocenter_2d_test_1.F90)
!
! See also: [[triangle_circumcenter_2d]], [[triangle_centroid_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_orthocenter_2d(t) RESULT(pc)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: pc(2)
    !! coordinates of the orthocenter
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the distance from a point to a triangle in 2D.
!
! Returns the shortest distance from the point `p` to the three edges
! of the triangle `t`.
! A point inside the triangle gets its distance to the nearest edge.
! The result is never negative.
!
! Formula:
!   $$ d = \min_{q \in \partial T} \| p - q \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_point_dist_2d_test_1.F90)
!
! See also: [[triangle_point_dist_signed_2d]], [[triangle_point_near_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_2d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(:)
    !! coordinates of the query point, `SIZE(p) = 2`
    REAL(DFP) :: dist
    !! distance from `p` to the nearest edge
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the distance from a point to a triangle in 3D.
!
! Returns the shortest distance from the point `p` to the three edges
! of the triangle `t`.
! The routine does not treat the triangle as a filled surface.
! A point above the interior gets its distance to the nearest edge.
!
! Formula:
!   $$ d = \min_{q \in \partial T} \| p - q \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_point_dist_3d_test_1.F90)
!
! See also: [[triangle_point_dist_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_3d(t, p) RESULT(dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 1) = 3` and `SIZE(t, 2) = 3`
    REAL(DFP), INTENT(IN) :: p(:)
    !! coordinates of the query point, `SIZE(p) = 3`
    REAL(DFP) :: dist
    !! distance from `p` to the nearest edge
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the signed distance from a point to a triangle in 2D.
!
! Returns the distance from the point `p` to the triangle `t`.
! The distance is positive if `p` is outside the triangle.
! It is negative if `p` is inside.
! Its magnitude is the distance from `p` to the nearest edge.
!
! @note
! The sign is correct only if the vertices of `t` are in
! counterclockwise order.
! @endnote
!
! Formula:
!   $$ d = \pm \min_{q \in \partial T} \| p - q \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_point_dist_signed_2d_test_1.F90)
!
! See also: [[triangle_point_dist_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist_signed_2d(t, p) RESULT(dist_signed)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! coordinates of the query point
    REAL(DFP) :: dist_signed
    !! signed distance, negative inside the triangle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the nearest point on a triangle in 2D.
!
! Finds the point `pn` on the boundary of the triangle `t` that is
! closest to the point `p`.
! It also returns the distance `dist` from `p` to `pn`.
!
! If `p` is inside the triangle, `pn` is still on an edge.
!
! Formula:
!   $$ pn = \arg\min_{q \in \partial T} \| p - q \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_point_near_2d_test_1.F90)
!
! See also: [[triangle_point_dist_2d]]

INTERFACE
  MODULE PURE SUBROUTINE triangle_point_near_2d(t, p, pn, dist)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! coordinates of the query point
    REAL(DFP), INTENT(OUT) :: pn(2)
    !! coordinates of the nearest point on the boundary
    REAL(DFP), INTENT(OUT) :: dist
    !! distance from `p` to `pn`
  END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the quality of a triangle in 2D.
!
! Returns a shape quality measure of the triangle `t`.
! The value is 1 for an equilateral triangle.
! It gets smaller as the triangle becomes thin.
! It is 0 if two vertices coincide.
!
! Returns:
!   `quality` : quality of the triangle, between 0 and 1
!
! Formula:
!   $$ Q = \frac{2r}{R} $$
!
!   Here `r` is the inradius and `R` is the circumradius.
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_quality_2d_test_1.F90)
!
! See also: [[triangle_inradius_2d]], [[triangle_circumradius_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_quality_2d(t) RESULT(quality)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP) :: quality
    !! quality of the triangle
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Counts the lattice points of a right triangle.
!
! Returns the number of integer lattice points in the right triangle
! with vertices `(0,0)`, `(a,0)` and `(0,b)`.
! Points on the edges and at the vertices count.
!
! Returns:
!   `n` : number of lattice points
!
! Formula:
!   $$ n = \frac{(a+1)(b+1) + \gcd(a,b) + 1}{2} $$
!
! See also: [[triangle01_lattice_point_num_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_right_lattice_point_num_2d(a, b) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: a
    !! length of the side along the x axis, `a >= 0`
    INTEGER(I4B), INTENT(IN) :: b
    !! length of the side along the y axis, `b >= 0`
    INTEGER(I4B) :: n
    !! number of lattice points
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns random points in a triangle in 2D.
!
! Returns `n` points drawn uniformly at random from the triangle `t`.
! The same `seed` always gives the same points.
! The routine does not return an updated seed.
!
! Returns:
!   `p` : sample points, one point per column
!
! Examples
!
! - [test 1](|base|Triangle/examples/triangle_sample_test_1.F90)
! - [test 2](|base|Triangle/examples/triangle_sample_test_2.F90)
!   checks that the samples are uniform
!
! See also: [[triangle_xsi_to_xy_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_sample(t, n, seed) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    INTEGER(I4B), INTENT(IN) :: n
    !! number of points, `n >= 0`
    INTEGER(I4B), INTENT(IN) :: seed
    !! seed of the random number generator
    REAL(DFP) :: p(2, n)
    !! XY coordinates of the sample points
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Counts the lattice points of a scaled unit triangle.
!
! Returns the number of integer lattice points in the triangle with
! vertices `(0,0)`, `(s,0)` and `(0,s)`.
! Points on the edges and at the vertices count.
! For `s = 1` this is the unit triangle.
!
! Returns:
!   `n` : number of lattice points
!
! Formula:
!   $$ n = \frac{(s+1)(s+2)}{2} $$
!
! See also: [[triangle_right_lattice_point_num_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle01_lattice_point_num_2d(s) RESULT(n)
    INTEGER(I4B), INTENT(IN) :: s
    !! integer scale factor, `s >= 0`
    INTEGER(I4B) :: n
    !! number of lattice points
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Converts barycentric coordinates to XY coordinates in 2D.
!
! Returns the XY coordinates of the point with barycentric coordinates
! `xsi` in the triangle `t`.
! The result is the sum of `xsi(i)` times vertex `i`.
!
! @note
! The routine does not check that the entries of `xsi` sum to 1.
! @endnote
!
! Returns:
!   `p` : XY coordinates of the point
!
! Formula:
!   $$ p = \sum_{i=1}^{3} \xi_i t_i $$
!
! See also: [[triangle_xy_to_xsi_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_xsi_to_xy_2d(t, xsi) RESULT(p)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: xsi(3)
    !! barycentric coordinates of the point
    REAL(DFP) :: p(2)
    !! XY coordinates of the point
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-28
! summary: Converts XY coordinates to barycentric coordinates in 2D.
!
! Returns the barycentric coordinates of the point `p` in the triangle
! `t`.
! The coordinates sum to 1.
! A point inside the triangle has three nonnegative coordinates.
!
! @note
! The triangle must not be degenerate.
! The routine does not check this and divides by the signed area.
! @endnote
!
! Returns:
!   `xsi` : barycentric coordinates of `p`
!
! See also: [[triangle_xsi_to_xy_2d]]

INTERFACE
  MODULE PURE FUNCTION triangle_xy_to_xsi_2d(t, p) RESULT(xsi)
    REAL(DFP), INTENT(IN) :: t(:, :)
    !! vertex coordinates, one vertex per column,
    !! `SIZE(t, 2) = 3`; the routine reads only rows 1 and 2
    REAL(DFP), INTENT(IN) :: p(2)
    !! XY coordinates of the point
    REAL(DFP) :: xsi(3)
    !! barycentric coordinates of `p`
  END FUNCTION
END INTERFACE

END MODULE Triangle_Method
