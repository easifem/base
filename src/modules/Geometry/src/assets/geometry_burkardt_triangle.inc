
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_angles_2d(t, angle)

!***************************************************************************80
!
!! TRIANGLE_ANGLES_2D computes the angles of a triangle in 2D.
!
!  Discussion:
!
!    The law of cosines is used:
!
!      C^2 = A^2 + B^2 - 2 * A * B * COS ( GAMMA )
!
!    where GAMMA is the angle opposite side C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) ANGLE(3), the angles opposite
!    sides P1-P2, P2-P3 and P3-P1, in radians.
!
  implicit none

  real(kind=8) t(dim_num, 3)
  real(kind=8) angle(3)

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8), parameter :: r8_pi = 3.141592653589793D+00
  real(kind=8) r8_acos
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

  if (c == 0.0D+00 .or. a == 0.0D+00) then
    angle(1) = r8_pi
  else
    angle(1) = r8_acos((c * c + a * a - b * b) / (2.0D+00 * c * a))
  end if

  if (a == 0.0D+00 .or. b == 0.0D+00) then
    angle(2) = r8_pi
  else
    angle(2) = r8_acos((a * a + b * b - c * c) / (2.0D+00 * a * b))
  end if

  if (b == 0.0D+00 .or. c == 0.0D+00) then
    angle(3) = r8_pi
  else
    angle(3) = r8_acos((b * b + c * c - a * a) / (2.0D+00 * b * c))
  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_angles_3d(t, angle)

!***************************************************************************80
!
!! TRIANGLE_ANGLES_3D computes the angles of a triangle in 3D.
!
!  Discussion:
!
!    The law of cosines is used:
!
!      C * C = A * A + B * B - 2 * A * B * COS ( GAMMA )
!
!    where GAMMA is the angle opposite side C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) ANGLE(3), the angles opposite
!    sides P1-P2, P2-P3 and P3-P1, in radians.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) a
  real(kind=8) angle(3)
  real(kind=8) b
  real(kind=8) c
  real(kind=8) r8_acos
  real(kind=8), parameter :: r8_pi = 3.141592653589793D+00
  real(kind=8) t(dim_num, 3)
!
!  Compute the length of each side.
!
  a = sqrt(sum((t(1:dim_num, 1) - t(1:dim_num, 2))**2))
  b = sqrt(sum((t(1:dim_num, 2) - t(1:dim_num, 3))**2))
  c = sqrt(sum((t(1:dim_num, 3) - t(1:dim_num, 1))**2))
!
!  Take care of a ridiculous special case.
!
  if (a == 0.0D+00 .and. b == 0.0D+00 .and. c == 0.0D+00) then
    angle(1:3) = 2.0D+00 * r8_pi / 3.0D+00
    return
  end if

  if (c == 0.0D+00 .or. a == 0.0D+00) then
    angle(1) = r8_pi
  else
    angle(1) = r8_acos((c * c + a * a - b * b) / (2.0D+00 * c * a))
  end if

  if (a == 0.0D+00 .or. b == 0.0D+00) then
    angle(2) = r8_pi
  else
    angle(2) = r8_acos((a * a + b * b - c * c) / (2.0D+00 * a * b))
  end if

  if (b == 0.0D+00 .or. c == 0.0D+00) then
    angle(3) = r8_pi
  else
    angle(3) = r8_acos((b * b + c * c - a * a) / (2.0D+00 * b * c))
  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_area_2d(t, area)

!***************************************************************************80
!
!! TRIANGLE_AREA_2D computes the area of a triangle in 2D.
!
!  Discussion:
!
!    If the triangle's vertices are given in counter clockwise order,
!    the area will be positive.  If the triangle's vertices are given
!    in clockwise order, the area will be negative!
!
!    An earlier version of this routine always returned the absolute
!    value of the computed area.  I am convinced now that that is
!    a less useful result!  For instance, by returning the signed
!    area of a triangle, it is possible to easily compute the area
!    of a nonconvex polygon as the sum of the (possibly negative)
!    areas of triangles formed by node 1 and successive pairs of vertices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) AREA, the area of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) area
  real(kind=8) t(dim_num, 3)

  area = 0.5D+00 * ( &
         t(1, 1) * (t(2, 2) - t(2, 3)) &
         + t(1, 2) * (t(2, 3) - t(2, 1)) &
         + t(1, 3) * (t(2, 1) - t(2, 2)))

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_area_3d(t, area)

!*****************************************************************************80
!
!! TRIANGLE_AREA_3D computes the area of a triangle in 3D.
!
!  Discussion:
!
!    This routine uses the fact that the norm of the cross product
!    of two vectors is the area of the parallelogram they form.
!
!    Therefore, the area of the triangle is half of that value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) AREA, the area of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) area
  real(kind=8) cross(dim_num)
  real(kind=8) t(dim_num, 3)
!
!  Compute the cross product vector.
!
  cross(1) = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
             - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))

  cross(2) = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
             - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))

  cross(3) = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
             - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

  area = 0.5D+00 * sqrt(sum(cross(1:3)**2))

  return
end
subroutine triangle_area_3d_2(t, area)

!*****************************************************************************80
!
!! TRIANGLE_AREA_3D_2 computes the area of a triangle in 3D.
!
!  Discussion:
!
!    This routine computes the area "the hard way".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) AREA, the area of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) alpha
  real(kind=8) area
  real(kind=8) base
  real(kind=8) dot
  real(kind=8) height
  real(kind=8) t(dim_num, 3)
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
              + (t(2, 2) - t(2, 1))**2 &
              + (t(3, 2) - t(3, 1))**2)
!
!  The height of the triangle is the length of (P3-P1) after its
!  projection onto (P2-P1) has been subtracted.
!
  if (base == 0.0D+00) then

    height = 0.0D+00

  else

    alpha = dot / (base * base)

    height = sqrt( &
             (t(1, 1) + alpha * (t(1, 2) - t(1, 1)) - t(1, 3))**2 &
             + (t(2, 1) + alpha * (t(2, 2) - t(2, 1)) - t(2, 3))**2 &
             + (t(3, 1) + alpha * (t(3, 2) - t(3, 1)) - t(3, 3))**2)

  end if

  area = 0.5D+00 * base * height

  return
end
subroutine triangle_area_3d_3(t, area)

!*****************************************************************************80
!
!! TRIANGLE_AREA_3D_3 computes the area of a triangle in 3D.
!
!  Discussion:
!
!    This routine uses Heron's formula
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) AREA, the area of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) area
  integer(kind=4) i
  integer(kind=4) j
  integer(kind=4) jp1
  real(kind=8) s(3)
  real(kind=8) t(dim_num, 3)

  do j = 1, 3
    jp1 = mod(j, 3) + 1
    s(j) = 0.0D+00
    do i = 1, dim_num
      s(j) = s(j) + (t(i, j) - t(i, jp1))**2
    end do
    s(j) = sqrt(s(j))
  end do

  area = (s(1) + s(2) + s(3)) &
         * (-s(1) + s(2) + s(3)) &
         * (s(1) - s(2) + s(3)) &
         * (s(1) + s(2) - s(3))

  if (area < 0.0D+00) then
    area = -1.0D+00
    return
  end if

  area = 0.25D+00 * sqrt(area)

  return
end
subroutine triangle_area_heron(s, area)

!*****************************************************************************80
!
!! TRIANGLE_AREA_HERON computes the area of a triangle using Heron's formula.
!
!  Discussion:
!
!    The formula is valid for any spatial dimension, depending only
!    on the lengths of the sides, and not the coordinates of the vertices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S(3), the lengths of the three sides.
!
!    Output, real ( kind = 8 ) AREA, the area of the triangle, or -1.0 if the
!    sides cannot constitute a triangle.
!
  implicit none

  real(kind=8) area
  real(kind=8) s(3)

  area = (s(1) + s(2) + s(3)) &
         * (-s(1) + s(2) + s(3)) &
         * (s(1) - s(2) + s(3)) &
         * (s(1) + s(2) - s(3))

  if (area < 0.0D+00) then
    area = -1.0D+00
    return
  end if

  area = 0.25D+00 * sqrt(area)

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_area_vector_3d(t, area_vector)
!
!! TRIANGLE_AREA_VECTOR_3D computes the area vector of a triangle in 3D.
!
!  Discussion:
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
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) AREA_VECTOR(3), the area vector of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) area_vector(dim_num)
  real(kind=8) t(dim_num, 3)

  area_vector(1) = (t(2, 2) - t(2, 1)) * (t(3, 3) - t(3, 1)) &
                   - (t(3, 2) - t(3, 1)) * (t(2, 3) - t(2, 1))

  area_vector(2) = (t(3, 2) - t(3, 1)) * (t(1, 3) - t(1, 1)) &
                   - (t(1, 2) - t(1, 1)) * (t(3, 3) - t(3, 1))

  area_vector(3) = (t(1, 2) - t(1, 1)) * (t(2, 3) - t(2, 1)) &
                   - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_barycentric_2d(t, p, xsi)

!
!! TRIANGLE_BARYCENTRIC_2D finds the barycentric coordinates of a point in 2D.
!
!  Discussion:
!
!    The barycentric coordinate of point P related to vertex A can be
!    interpreted as the ratio of the area of the triangle with
!    vertex A replaced by vertex P to the area of the original
!    triangle.
!
!    This routine assumes that the triangle vertices are given in
!    counter clockwise order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!    The vertices should be given in counter clockwise order.
!
!    Input, real ( kind = 8 ) P(2), the point to be checked.
!
!    Output, real ( kind = 8 ) XSI(3), the barycentric coordinates of P
!    with respect to the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4), parameter :: rhs_num = 1

  real(kind=8) a(dim_num, dim_num + rhs_num)
  integer(kind=4) info
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) xsi(dim_num + 1)
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

  a(2, 1) = t(2, 2) - t(2, 1)
  a(2, 2) = t(2, 3) - t(2, 1)
  a(2, 3) = p(2) - t(2, 1)
!
!  Solve the linear system.
!
  call r8mat_solve(dim_num, rhs_num, a, info)

  if (info /= 0) then
    write (*, '(a)') ' '
    write (*, '(a)') 'TRIANGLE_BARYCENTRIC_2D - Fatal error!'
    write (*, '(a)') '  The linear system is singular.'
    write (*, '(a)') '  The input data does not form a proper triangle.'
    stop 1
  end if

  xsi(1) = a(1, 3)
  xsi(2) = a(2, 3)
  xsi(3) = 1.0D+00 - xsi(1) - xsi(2)

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_centroid_2d(t, centroid)

!
!! TRIANGLE_CENTROID_2D computes the centroid of a triangle in 2D.
!
!  Discussion:
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
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) CENTROID(2), the coordinates of the centroid.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) centroid(dim_num)
  integer(kind=4) i
  real(kind=8) t(dim_num, 3)

  do i = 1, dim_num
    centroid(i) = sum(t(i, 1:3)) / 3.0D+00
  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_centroid_3d(t, centroid)

!
!! TRIANGLE_CENTROID_3D computes the centroid of a triangle in 3D.
!
!  Discussion:
!
!    The centroid of a triangle can also be considered the
!    center of gravity or center of mass, assuming that the triangle
!    is made of a thin uniform sheet of massy material.
!
!    The centroid of a triangle is the intersection of the medians.
!    A median of a triangle is a line connecting any vertex to the
!    midpoint of the opposite side.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) CENTROID(3), the coordinates of the centroid.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) centroid(dim_num)
  integer(kind=4) i
  real(kind=8) t(dim_num, 3)

  do i = 1, dim_num
    centroid(i) = sum(t(i, 1:3)) / 3.0D+00
  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumcenter_2d(t, pc)
!
!! TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
!
!  Discussion:
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
!    The circumcenter is the intersection of the perpendicular bisectors
!    of the sides of the triangle.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) PC(2), the circumcenter of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) det
  real(kind=8) f(2)
  real(kind=8) pc(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) top(dim_num)

  f(1) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2
  f(2) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2

  top(1) = (t(2, 3) - t(2, 1)) * f(1) - (t(2, 2) - t(2, 1)) * f(2)
  top(2) = -(t(1, 3) - t(1, 1)) * f(1) + (t(1, 2) - t(1, 1)) * f(2)

  det = (t(2, 3) - t(2, 1)) * (t(1, 2) - t(1, 1)) &
        - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

  pc(1:2) = t(1:2, 1) + 0.5D+00 * top(1:2) / det

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumcenter_2d_2(t, pc)

!*****************************************************************************80
!
!! TRIANGLE_CIRCUMCENTER_2D_2 computes the circumcenter of a triangle in 2D.
!
!  Discussion:
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
!    The circumcenter is the intersection of the perpendicular bisectors
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) PC(2), the circumcenter of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4), parameter :: rhs_num = 1

  real(kind=8) a(dim_num, dim_num + rhs_num)
  integer(kind=4) info
  real(kind=8) pc(dim_num)
  real(kind=8) t(dim_num, 3)
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
!
!  Compute the center
!
  if (info /= 0) then
    pc(1:dim_num) = 0.0D+00
  else
    pc(1:dim_num) = t(1:dim_num, 1) + 0.5D+00 * a(1:dim_num, dim_num + 1)
  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumcenter(n, t, p)

!*****************************************************************************80
!
!! TRIANGLE_CIRCUMCENTER computes the circumcenter of a triangle in ND.
!
!  Discussion:
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
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) T(N,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) P(N), the circumcenter of the triangle.
!
  implicit none

  integer(kind=4) n

  real(kind=8) a
  real(kind=8) abp
  real(kind=8) apc
  real(kind=8) b
  real(kind=8) c
  real(kind=8) p(n)
  real(kind=8) pbc
  real(kind=8) r8vec_normsq_affine
  real(kind=8) t(n, 3)

  a = r8vec_normsq_affine(n, t(1:n, 2), t(1:n, 3))
  b = r8vec_normsq_affine(n, t(1:n, 3), t(1:n, 1))
  c = r8vec_normsq_affine(n, t(1:n, 1), t(1:n, 2))

  pbc = a * (-a + b + c)
  apc = b * (a - b + c)
  abp = c * (a + b - c)

  p(1:n) = (pbc * t(1:n, 1) + apc * t(1:n, 2) + abp * t(1:n, 3)) &
           / (pbc + apc + abp)

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumcircle_2d(t, r, pc)

!*****************************************************************************80
!
!! TRIANGLE_CIRCUMCIRCLE_2D computes the circumcircle of a triangle in 2D.
!
!  Discussion:
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
!    The circumcenter is the intersection of the perpendicular bisectors
!    of the sides of the triangle.
!
!    In geometry, the circumcenter of a triangle is often symbolized by "O".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) R, PC(2), the circumradius and circumcenter
!    of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) bot
  real(kind=8) c
  real(kind=8) det
  real(kind=8) f(2)
  real(kind=8) pc(dim_num)
  real(kind=8) r
  real(kind=8) top(dim_num)
  real(kind=8) t(dim_num, 3)
!
!  Circumradius.
!
  a = sqrt((t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2)
  b = sqrt((t(1, 3) - t(1, 2))**2 + (t(2, 3) - t(2, 2))**2)
  c = sqrt((t(1, 1) - t(1, 3))**2 + (t(2, 1) - t(2, 3))**2)

  bot = (a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)

  if (bot <= 0.0D+00) then
    r = -1.0D+00
    pc(1:2) = 0.0D+00
    return
  end if

  r = a * b * c / sqrt(bot)
!
!  Circumcenter.
!
  f(1) = (t(1, 2) - t(1, 1))**2 + (t(2, 2) - t(2, 1))**2
  f(2) = (t(1, 3) - t(1, 1))**2 + (t(2, 3) - t(2, 1))**2

  top(1) = (t(2, 3) - t(2, 1)) * f(1) - (t(2, 2) - t(2, 1)) * f(2)
  top(2) = -(t(1, 3) - t(1, 1)) * f(1) + (t(1, 2) - t(1, 1)) * f(2)

  det = (t(2, 3) - t(2, 1)) * (t(1, 2) - t(1, 1)) &
        - (t(2, 2) - t(2, 1)) * (t(1, 3) - t(1, 1))

  pc(1:2) = t(1:2, 1) + 0.5D+00 * top(1:2) / det

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumcircle_2d_2(t, r, pc)
!
!! TRIANGLE_CIRCUMCIRCLE_2D_2 computes the circumcircle of a triangle in 2D.
!
!  Discussion:
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
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) R, PC(2), the circumradius and circumcenter.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4), parameter :: rhs_num = 1

  real(kind=8) a(dim_num, dim_num + rhs_num)
  integer(kind=4) info
  real(kind=8) pc(dim_num)
  real(kind=8) r
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_circumradius_2d(t, r)

!
!! TRIANGLE_CIRCUMRADIUS_2D computes the circumradius of a triangle in 2D.
!
!  Discussion:
!
!    The circumscribed circle of a triangle is the circle that passes through
!    the three vertices of the triangle.  The circumscribed circle contains
!    the triangle, but it is not necessarily the smallest triangle to do so.
!
!    The circumradius of a triangle is the radius of the circumscribed
!    circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) R, the circumradius of the circumscribed circle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) bot
  real(kind=8) c
  real(kind=8) r
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_contains_line_exp_3d(t, p1, p2, inside, pint)

!
!! TRIANGLE_CONTAINS_LINE_EXP_3D finds if a line is inside a triangle in 3D.
!
!  Discussion:
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Steve Marschner, Cornell University,
!    CS465 Notes: Simple Ray-Triangle Intersection.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the line.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if (the intersection point of)
!    the line is inside the triangle.
!
!    Output, real ( kind = 8 ) PINT(3), the point where the line
!    intersects the plane of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  logical(kind=4) inside
  integer(kind=4) ival
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) normal(dim_num)
  real(kind=8) normal2(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pint(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) temp
  logical(kind=4) triangle_is_degenerate_nd
  real(kind=8) v1(dim_num)
  real(kind=8) v2(dim_num)
!
!  Make sure the line is not degenerate.
!
  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'TRIANGLE_CONTAINS_LINE_EXP_3D - Fatal error!'
    write (*, '(a)') '  The explicit line is degenerate.'
    stop 1
  end if
!
!  Make sure the triangle is not degenerate.
!
  if (triangle_is_degenerate_nd(dim_num, t)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'TRIANGLE_CONTAINS_LINE_EXP_3D - Fatal error!'
    write (*, '(a)') '  The triangle is degenerate.'
    stop 1
  end if
!
!  Determine a unit normal vector associated with the plane of
!  the triangle.
!
  v1(1:dim_num) = t(1:dim_num, 2) - t(1:dim_num, 1)
  v2(1:dim_num) = t(1:dim_num, 3) - t(1:dim_num, 1)

  normal(1) = v1(2) * v2(3) - v1(3) * v2(2)
  normal(2) = v1(3) * v2(1) - v1(1) * v2(3)
  normal(3) = v1(1) * v2(2) - v1(2) * v2(1)

  temp = sqrt(sum(normal(1:dim_num)**2))
  normal(1:dim_num) = normal(1:dim_num) / temp
!
!  Find the intersection of the plane and the line.
!
  call plane_normal_line_exp_int_3d(t(1:dim_num, 1), normal, p1, p2, &
                                    ival, pint)

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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_contains_line_par_3d(t, p0, pd, inside, p)

!
!! TRIANGLE_CONTAINS_LINE_PAR_3D: finds if a line is inside a triangle in 3D.
!
!  Discussion:
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420,
!    page 111.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the three points that define
!    the triangle.
!
!    Input, real ( kind = 8 ) P0(3), PD(3), parameters that define the
!    parametric line.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if (the intersection point of)
!    the line is inside the triangle.
!
!    Output, real ( kind = 8 ) P(3), is the point of intersection of the line
!    and the plane of the triangle, unless they are parallel.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) a
  real(kind=8) angle_sum
  real(kind=8) b
  real(kind=8) c
  real(kind=8) d
  real(kind=8) denom
  logical(kind=4) inside
  logical(kind=4) intersect
  real(kind=8) norm
  real(kind=8) norm1
  real(kind=8) norm2
  real(kind=8) p(dim_num)
  real(kind=8) p0(dim_num)
  real(kind=8) pd(dim_num)
  real(kind=8) r8_acos
  real(kind=8), parameter :: r8_pi = 3.141592653589793D+00
  real(kind=8) t(dim_num, 3)
  real(kind=8) t_int
  real(kind=8), parameter :: tol = 0.00001D+00
  real(kind=8) v1(dim_num)
  real(kind=8) v2(dim_num)
  real(kind=8) v3(dim_num)
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
    write (*, '(a)') ' '
    write (*, '(a)') 'TRIANGLE_LINE_PAR_INT_3D - Fatal error!'
    write (*, '(a)') '  The plane normal vector is null.'
    inside = .false.
    p(1:dim_num) = 0.0D+00
    stop 1
  end if
!
!  Make sure the implicit line is well defined.
!
  norm2 = sqrt(sum(pd(1:dim_num)**2))

  if (norm2 == 0.0D+00) then
    write (*, '(a)') ' '
    write (*, '(a)') 'TRIANGLE_LINE_PAR_INT_3D - Fatal error!'
    write (*, '(a)') '  The line direction vector is null.'
    inside = .false.
    p(1:dim_num) = 0.0D+00
    stop 1
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

    angle_sum = r8_acos(dot_product(v1(1:3), v2(1:3))) &
                + r8_acos(dot_product(v2(1:3), v3(1:3))) &
                + r8_acos(dot_product(v3(1:3), v1(1:3)))

    if (nint(angle_sum / r8_pi) == 2) then
      inside = .true.
    else
      inside = .false.
    end if

  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_contains_point_2d_1(t, p, inside)

!
!! TRIANGLE_CONTAINS_POINT_2D_1 finds if a point is inside a triangle in 2D.
!
!  Discussion:
!
!    It is conventional to list the triangle vertices in counter clockwise
!    order.  However, this routine does not require a particular order
!    for the vertices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is inside
!    the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) inside
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) xsi(dim_num + 1)

  call triangle_barycentric_2d(t, p, xsi)

  if (any(xsi(1:3) < 0.0D+00)) then
    inside = .false.
  else
    inside = .true.
  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_contains_point_2d_2(t, p, inside)

!
!! TRIANGLE_CONTAINS_POINT_2D_2 finds if a point is inside a triangle in 2D.
!
!  Discussion:
!
!    The routine assumes that the vertices are given in counter clockwise
!    order.  If the triangle vertices are actually given in clockwise
!    order, this routine will behave as though the triangle contains
!    no points whatsoever!
!
!    The routine determines if a point P is "to the right of" each of the lines
!    that bound the triangle.  It does this by computing the cross product
!    of vectors from a vertex to its next vertex, and to P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!    The vertices should be given in counter clockwise order.
!
!    Input, real ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is
!    inside the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) inside
  integer(kind=4) j
  integer(kind=4) k
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)

  do j = 1, 3

    k = mod(j, 3) + 1

    if (0.0D+00 < (p(1) - t(1, j)) * (t(2, k) - t(2, j)) &
        - (p(2) - t(2, j)) * (t(1, k) - t(1, j))) then
      inside = .false.
      return
    end if

  end do

  inside = .true.

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_contains_point_2d_3(t, p, inside)

!
!! TRIANGLE_CONTAINS_POINT_2D_3 finds if a point is inside a triangle in 2D.
!
!  Discussion:
!
!    This routine is the same as TRIANGLE_CONTAINS_POINT_2D_2, except
!    that it does not assume an ordering of the points.  It should
!    work correctly whether the vertices of the triangle are listed
!    in clockwise or counter clockwise order.
!
!    The routine determines if a point P is "to the right of" each of the lines
!    that bound the triangle.  It does this by computing the cross product
!    of vectors from a vertex to its next vertex, and to P.
!
!    The point is inside the triangle if it is to the right of all
!    the lines, or to the left of all the lines.
!
!    This version was suggested by Paulo Ernesto of Maptek Brasil.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(2), the point to be checked.
!
!    Output, logical ( kind = 4 ) INSIDE, is TRUE if the point is
!    inside the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) dir_new
  real(kind=8) dir_old
  logical(kind=4) inside
  integer(kind=4) j
  integer(kind=4) k
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)

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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_diameter_2d(t, diameter)

!
!! TRIANGLE_DIAMETER_2D computes the diameter of a triangle in 2D.
!
!  Discussion:
!
!    The diameter of a triangle is the diameter of the smallest circle
!    that can be drawn around the triangle.  At least two of the vertices
!    of the triangle will intersect the circle, but not necessarily
!    all three!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) DIAMETER, the diameter of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) asq
  real(kind=8) b
  real(kind=8) bsq
  real(kind=8) c
  real(kind=8) csq
  real(kind=8) diameter
  real(kind=8) t(dim_num, 3)
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
    call r8_swap(asq, bsq)
  end if

  if (asq < csq) then
    call r8_swap(asq, csq)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_edge_length_2d(t, edge_length)

!
!! TRIANGLE_EDGE_LENGTH_2D returns edge lengths of a triangle in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) EDGE_LENGTH(3), the length of the edges.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) edge_length(3)
  integer(kind=4) i4_wrap
  integer(kind=4) j1
  integer(kind=4) j2
  real(kind=8) r8vec_norm
  real(kind=8) t(dim_num, 3)

  do j1 = 1, 3
    j2 = i4_wrap(j1 + 1, 1, 3)
    edge_length(j1) = &
      r8vec_norm(dim_num, t(1:dim_num, j2) - t(1:dim_num, j1))
  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_gridpoints_2d(t, sub_num, grid_max, grid_num, g)

!
!! TRIANGLE_GRIDPOINTS_2D computes gridpoints within a triangle in 2D.
!
!  Discussion:
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, integer ( kind = 4 ) SUB_NUM, the number of subdivisions.
!
!    Input, integer ( kind = 4 ) GRID_MAX, the maximum number of grid points.
!
!    Output, integer ( kind = 4 ) GRID_NUM, the number of grid points returned.
!
!    Output, real ( kind = 8 ) G(2,GRID_MAX), the grid points.
!
  implicit none

  integer(kind=4) grid_max
  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) g(dim_num, grid_max)
  integer(kind=4) i
  integer(kind=4) j
  integer(kind=4) grid_num
  integer(kind=4) sub_num
  real(kind=8) t(dim_num, 3)

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

  do i = 0, sub_num

    do j = 0, sub_num - i

      if (grid_num < grid_max) then

        grid_num = grid_num + 1

        g(1, grid_num) = (real(i, kind=8) * t(1, 1) &
                          + real(j, kind=8) * t(1, 2) &
                          + real(sub_num - i - j, kind=8) * t(1, 3)) &
                         / real(sub_num, kind=8)

        g(2, grid_num) = (real(i, kind=8) * t(2, 1) &
                          + real(j, kind=8) * t(2, 2) &
                          + real(sub_num - i - j, kind=8) * t(2, 3)) &
                         / real(sub_num, kind=8)
      end if

    end do
  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_incenter_2d(t, pc)

!
!! TRIANGLE_INCENTER_2D computes the incenter of a triangle in 2D.
!
!  Discussion:
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) PC(2), the incenter.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) pc(dim_num)
  real(kind=8) perimeter
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_incircle_2d(t, r, pc)

!
!! TRIANGLE_INCIRCLE_2D computes the inscribed circle of a triangle in 2D.
!
!  Discussion:
!
!    The inscribed circle of a triangle is the largest circle that can
!    be drawn inside the triangle.  It is tangent to all three sides,
!    and the lines from its center to the vertices bisect the angles
!    made by each vertex.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) R, PC(2), the radius and center of the
!    inscribed circle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) pc(dim_num)
  real(kind=8) perimeter
  real(kind=8) r
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!

!----------------------------------------------------------------------------
subroutine triangle_inradius_2d(t, r)

!
!! TRIANGLE_INRADIUS_2D: radius of the inscribed circle of a triangle in 2D.
!
!  Discussion:
!
!    The inscribed circle of a triangle is the largest circle that can
!    be drawn inside the triangle.  It is tangent to all three sides,
!    and the lines from its center to the vertices bisect the angles
!    made by each vertex.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) R, the radius of the inscribed circle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) perimeter
  real(kind=8) r
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function triangle_is_degenerate_nd(dim_num, t)

!
!! TRIANGLE_IS_DEGENERATE_ND finds if a triangle is degenerate in ND.
!
!  Discussion:
!
!    A triangle in ND is described by the coordinates of its 3 vertices.
!
!    A triangle in ND is degenerate if any two vertices are equal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) T(DIM_NUM,3), the triangle vertices.
!
!    Output, logical ( kind = 4 ) TRIANGLE_IS_DEGENERATE_ND, is TRUE if the
!    triangle is degenerate.
!
  implicit none

  integer(kind=4) dim_num

  real(kind=8) t(dim_num, 3)
  logical(kind=4) triangle_is_degenerate_nd

  triangle_is_degenerate_nd = &
    (all(t(1:dim_num, 1) == t(1:dim_num, 2)) .or. &
     all(t(1:dim_num, 2) == t(1:dim_num, 3)) .or. &
     all(t(1:dim_num, 3) == t(1:dim_num, 1)))

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_lattice_layer_point_next(c, v, more)

!*****************************************************************************80
!
!! TRIANGLE_LATTICE_LAYER_POINT_NEXT: next triangle lattice layer point.
!
!  Discussion:
!
!    The triangle lattice layer L is bounded by the lines
!
!      0 <= X,
!      0 <= Y,
!      L - 1 < X / C(1) + Y / C(2) <= L.
!
!    In particular, layer L = 0 always contains the single point (0,0).
!
!    This function returns, one at a time, the points that lie within
!    a given triangle lattice layer.
!
!    Thus, if we set C(1) = 2, C(2) = 3, then we get the following layers:
!
!    L = 0: (0,0)
!    L = 1: (1,0), (2,0), (0,1), (1,1), (0,2), (0,3)
!    L = 2: (3,0), (4,0), (2,1), (3,1), (1,2), (2,2), (1,3), (2,3),
!           (0,4), (1,4), (0,5), (0,6).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) C(3), coefficients defining the
!    lattice layer.  Entry C(3) contains the layer index.
!    C(1) and C(2) should be positive, and C(3) must be nonnegative.
!
!    Input/output, integer ( kind = 4 ) V(2).  On first call for a given layer,
!    the input value of V is not important.  On a repeated call for the same
!    layer, the input value of V should be the output value from the previous
!    call.  On output, V contains the next lattice layer point.
!
!    Input/output, logical ( kind = 4 ) MORE.  On input, set MORE to FALSE to
!    indicate that this is the first call for a given layer.  Thereafter, the
!    input value should be the output value from the previous call.  On output,
!    MORE is TRUE if the returned value V is a new point.
!    If the output value is FALSE, then no more points were found,
!    and V was reset to 0, and the lattice layer has been exhausted.
!
  implicit none

  integer(kind=4) c(3)
  integer(kind=4) c1n
  integer(kind=4) i4vec_lcm
  logical(kind=4) more
  integer(kind=4), parameter :: n = 2
  integer(kind=4) rhs1
  integer(kind=4) rhs2
  integer(kind=4) v(2)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_lattice_point_next(c, v, more)

!! TRIANGLE_LATTICE_POINT_NEXT returns the next triangle lattice point.
!
!  Discussion:
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
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
!
  implicit none

  integer(kind=4) c(3)
  integer(kind=4) c1n
  integer(kind=4) i4vec_lcm
  logical(kind=4) more
  integer(kind=4), parameter :: n = 2
  integer(kind=4) rhs
  integer(kind=4) v(2)

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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_line_imp_int_2d(t, a, b, c, int_num, pint)

!*****************************************************************************80
!
!! TRIANGLE_LINE_IMP_INT_2D: implicit line intersects a triangle in 2D.
!
!  Discussion:
!
!    An implicit line is the set of points ( X, Y ) satisfying
!
!      A * X + B * Y + C = 0
!
!    where at least one of A and B is not zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) A, B, C, determine the equation of the line:
!    A*X + B*Y + C = 0.
!
!    Output, integer ( kind = 4 ) INT_NUM, the number of points of intersection
!    of the line with the triangle.  INT_NUM may be 0, 1, 2 or 3.
!
!    Output, real ( kind = 8 ) PINT(2,3), contains the intersection points.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) a1
  real(kind=8) b
  real(kind=8) b1
  real(kind=8) c
  real(kind=8) c1
  integer(kind=4) i
  integer(kind=4) i4_wrap
  integer(kind=4) int_num
  integer(kind=4) ival
  integer(kind=4) j
  real(kind=8) p(dim_num)
  real(kind=8) pint(dim_num, 3)
  real(kind=8) t(dim_num, 3)
  real(kind=8) test1
  real(kind=8) test2

  int_num = 0

  do i = 1, 3

    j = i4_wrap(i + 1, 1, 3)
!
!  Get the implicit form of the line through vertices I and I+1.
!
    call line_exp2imp_2d(t(1:2, i), t(1:2, j), a1, b1, c1)
!
!  Seek an intersection with the original line.
!
    call lines_imp_int_2d(a, b, c, a1, b1, c1, ival, p)
!
!  If there is an intersection, determine if it lies between the two vertices.
!
    if (ival == 1) then

      test1 = sum((p(1:dim_num) - t(1:dim_num, i)) &
                  * (t(1:dim_num, j) - t(1:dim_num, i)))
      test2 = sum((t(1:dim_num, j) - t(1:dim_num, i)) &
                  * (t(1:dim_num, j) - t(1:dim_num, i)))

      if (0 <= test1 .and. test1 <= test2) then
        int_num = int_num + 1
        pint(1:dim_num, int_num) = p(1:dim_num)
      end if

    end if

  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

function triangle_orientation_2d(t)

!
!! TRIANGLE_ORIENTATION_2D determines the orientation of a triangle in 2D.
!
!  Discussion:
!
!    Three distinct non-colinear points in the plane define a circle.
!    If the points are visited in the order P1, P2, and then
!    P3, this motion defines a clockwise or counter clockwise
!    rotation along the circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
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
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) det
  integer(kind=4) triangle_orientation_2d
  real(kind=8) t(dim_num, 3)

  if (all(t(1:dim_num, 1) == t(1:dim_num, 2)) .or. &
      all(t(1:dim_num, 2) == t(1:dim_num, 3)) .or. &
      all(t(1:dim_num, 3) == t(1:dim_num, 1))) then
    triangle_orientation_2d = 3
    return
  end if

  det = (t(1, 1) - t(1, 3)) * (t(2, 2) - t(2, 3)) &
        - (t(1, 2) - t(1, 3)) * (t(2, 1) - t(2, 3))

  if (det == 0.0D+00) then
    triangle_orientation_2d = 2
  else if (det < 0.0D+00) then
    triangle_orientation_2d = 1
  else if (0.0D+00 < det) then
    triangle_orientation_2d = 0
  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_orthocenter_2d(t, pc)

!
!! TRIANGLE_ORTHOCENTER_2D computes the orthocenter of a triangle in 2D.
!
!  Discussion:
!
!    The orthocenter is defined as the intersection of the three altitudes
!    of a triangle.
!
!    An altitude of a triangle is the line through a vertex of the triangle
!    and perpendicular to the opposite side.
!
!    In geometry, the orthocenter of a triangle is often symbolized by "H".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) PC(2), the orthocenter of the triangle.
!
!    Output, logical ( kind = 4 ) FLAG, is TRUE if the value could not
!    be computed.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) flag
  integer(kind=4) ival
  real(kind=8) p23(dim_num)
  real(kind=8) p31(dim_num)
  real(kind=8) pc(dim_num)
  real(kind=8) r8_huge
  real(kind=8) t(dim_num, 3)
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
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_point_dist_2d(t, p, dist)

!
!! TRIANGLE_POINT_DIST_2D: distance ( triangle, point ) in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(2), the point to be checked.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4), parameter :: side_num = 3

  real(kind=8) dist
  real(kind=8) dist2
  integer(kind=4) i4_wrap
  integer(kind=4) j
  integer(kind=4) jp1
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, side_num)
!
!  Find the distance to each of the line segments.
!
  dist = huge(dist)

  do j = 1, side_num

    jp1 = i4_wrap(j + 1, 1, side_num)

    call segment_point_dist_2d(t(1:dim_num, j), t(1:dim_num, jp1), p, dist2)

    if (dist2 < dist) then
      dist = dist2
    end if

  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_point_dist_3d(t, p, dist)

!
!! TRIANGLE_POINT_DIST_3D: distance ( triangle, point ) in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(3,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(3), the point which is to be checked.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    triangle.  DIST is zero if the point lies exactly on the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) dist
  real(kind=8) dist2
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
!
!  Compute the distances from the point to each of the sides.
!
  call segment_point_dist_3d(t(1:dim_num, 1), t(1:dim_num, 2), p, dist2)

  dist = dist2

  call segment_point_dist_3d(t(1:dim_num, 2), t(1:dim_num, 3), p, dist2)

  dist = min(dist, dist2)

  call segment_point_dist_3d(t(1:dim_num, 3), t(1:dim_num, 1), p, dist2)

  dist = min(dist, dist2)

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_point_dist_signed_2d(t, p, dist_signed)

!
!! TRIANGLE_POINT_DIST_SIGNED_2D: signed distance ( triangle, point ) in 2D.
!
!  Discussion:
!
!    If the signed distance is:
!    0, the point is on the boundary of the triangle;
!    negative, the point is in the triangle;
!    positive, the point is outside the triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!    These should be given in counter clockwise order.
!
!    Input, real ( kind = 8 ) P(2), the point which is to be checked.
!
!    Output, real ( kind = 8 ) DIST_SIGNED, the signed distance from the
!    point to the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) dis12
  real(kind=8) dis23
  real(kind=8) dis31
  real(kind=8) dist_signed
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
!
!  Compute the signed line distances to the point.
!
  call line_exp_point_dist_signed_2d(t(1:2, 1), t(1:2, 2), p, dis12)

  call line_exp_point_dist_signed_2d(t(1:2, 2), t(1:2, 3), p, dis23)

  call line_exp_point_dist_signed_2d(t(1:2, 3), t(1:2, 1), p, dis31)
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

    call segment_point_dist_2d(t(1:2, 1), t(1:2, 2), p, dis12)
    call segment_point_dist_2d(t(1:2, 2), t(1:2, 3), p, dis23)
    call segment_point_dist_2d(t(1:2, 3), t(1:2, 1), p, dis31)

    dist_signed = min(dis12, dis23, dis31)

  end if

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_point_near_2d(t, p, pn, dist)

!
!! TRIANGLE_POINT_NEAR_2D computes the nearest point on a triangle in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(2), the point whose nearest triangle point
!    is to be determined.
!
!    Output, real ( kind = 8 ) PN(2), the nearest point to P.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4), parameter :: side_num = 3

  real(kind=8) dist
  real(kind=8) dist2
  integer(kind=4) i4_wrap
  integer(kind=4) j
  integer(kind=4) jp1
  real(kind=8) p(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) pn2(dim_num)
  real(kind=8) t(dim_num, side_num)
  real(kind=8) tval
!
!  Find the distance to each of the line segments that make up the edges
!  of the triangle.
!
  dist = huge(dist)
  pn(1:dim_num) = 0.0D+00

  do j = 1, side_num

    jp1 = i4_wrap(j + 1, 1, side_num)

    call segment_point_near_2d(t(1:dim_num, j), t(1:dim_num, jp1), p, &
                               pn2, dist2, tval)

    if (dist2 < dist) then
      dist = dist2
      pn(1:dim_num) = pn2(1:dim_num)
    end if

  end do

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_quality_2d(t, quality)

!
!! TRIANGLE_QUALITY_2D: "quality" of a triangle in 2D.
!
!  Discussion:
!
!    The quality of a triangle is 2.0 times the ratio of the radius of
!    the inscribed circle divided by that of the circumscribed circle.
!    An equilateral triangle achieves the maximum possible quality of 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Adrian Bowyer, John Woodwark,
!    A Programmer's Geometry,
!    Butterworths, 1983,
!    ISBN: 0408012420.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Output, real ( kind = 8 ) QUALITY, the quality of the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) quality
  real(kind=8) t(dim_num, 3)
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

  return
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine triangle_right_lattice_point_num_2d(a, b, n)

!
!! TRIANGLE_RIGHT_LATTICE_POINT_NUM_2D: count lattice points.
!
!  Discussion:
!
!    The triangle is assumed to be a right triangle which, without loss
!    of generality, has the coordinates:
!
!    ( (0,0), (a,0), (0,b) )
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!    The formula for this function occurred to me (JVB) after some thought,
!    on 06 July 2009.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, define the vertices.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.
!
  implicit none

  integer(kind=4) a
  integer(kind=4) b
  integer(kind=4) i4_gcd
  integer(kind=4) n

  n = ((a + 1) * (b + 1) + i4_gcd(a, b) + 1) / 2

  return
end
subroutine triangle_sample(t, n, seed, p)

!*****************************************************************************80
!
!! TRIANGLE_SAMPLE returns random points in a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, integer ( kind = 4 ) N, the number of points to generate.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) P(2,N), random points in the triangle.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2
  integer(kind=4) n

  real(kind=8) alpha(n)
  integer(kind=4) dim
  real(kind=8) p(dim_num, n)
  real(kind=8) p12(dim_num, n)
  real(kind=8) p13(dim_num, n)
  integer(kind=4) seed
  real(kind=8) t(dim_num, 3)
!
!  For comparison between F90, C++ and MATLAB codes, call R8VEC_UNIFORM_01.
!  For faster execution, call RANDOM_NUMBER.
!
  if (.true.) then

    call r8vec_uniform_01(n, seed, alpha)

  else

    call random_number(harvest=alpha(1:n))

  end if
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
!  For comparison between F90, C++ and MATLAB codes, call R8VEC_UNIFORM_01.
!  For faster execution, call RANDOM_NUMBER.
!
  if (.true.) then

    call r8vec_uniform_01(n, seed, alpha)

  else

    call random_number(harvest=alpha(1:n))

  end if

  do dim = 1, dim_num

    p(dim, 1:n) = (1.0D+00 - alpha(1:n)) * p12(dim, 1:n) &
                  + alpha(1:n) * p13(dim, 1:n)

  end do

  return
end
subroutine triangle01_lattice_point_num_2d(s, n)

!*****************************************************************************80
!
!! TRIANGLE01_LATTICE_POINT_NUM_2D: count lattice points.
!
!  Discussion:
!
!    The triangle is assumed to be the unit triangle:
!
!    ( (0,0), (1,0), (0,1) )
!
!    or a copy of this triangle scaled by an integer S:
!
!    ( (0,0), (S,0), (0,S) ).
!
!    The routine returns the number of integer lattice points that appear
!    inside the triangle or on its edges or vertices.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Matthias Beck, Sinai Robins,
!    Computing the Continuous Discretely,
!    Springer, 2006,
!    ISBN13: 978-0387291390,
!    LC: QA640.7.B43.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the scale factor.
!
!    Output, integer ( kind = 4 ) N, the number of lattice points.
!
  implicit none

  integer(kind=4) n
  integer(kind=4) s

  n = ((s + 2) * (s + 1)) / 2

  return
end
subroutine triangle_xsi_to_xy_2d(t, xsi, p)

!*****************************************************************************80
!
!! TRIANGLE_XSI_TO_XY_2D converts from barycentric to XY coordinates in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) XSI(3), the barycentric coordinates of a point.
!    XSI(1) + XSI(2) + XSI(3) should equal 1, but this is not checked.
!
!    Output, real ( kind = 8 ) P(2), the XY coordinates of the point.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) xsi(dim_num + 1)

  p(1:dim_num) = matmul(t(1:dim_num, 1:3), xsi(1:dim_num + 1))

  return
end
subroutine triangle_xy_to_xsi_2d(t, p, xsi)

!*****************************************************************************80
!
!! TRIANGLE_XY_TO_XSI_2D converts from XY to barycentric in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, real ( kind = 8 ) P(2), the XY coordinates of a point.
!
!    Output, real ( kind = 8 ) XSI(3), the barycentric coordinates of the point.
!    XSI1 + XSI2 + XSI3 should equal 1.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) det
  real(kind=8) p(dim_num)
  real(kind=8) t(dim_num, 3)
  real(kind=8) xsi(3)

  det = (t(1, 1) - t(1, 3)) * (t(2, 2) - t(2, 3)) &
        - (t(1, 2) - t(1, 3)) * (t(2, 1) - t(2, 3))

  xsi(1) = ((t(2, 2) - t(2, 3)) * (p(1) - t(1, 3)) &
            - (t(1, 2) - t(1, 3)) * (p(2) - t(2, 3))) / det

  xsi(2) = (-(t(2, 1) - t(2, 3)) * (p(1) - t(1, 3)) &
            + (t(1, 1) - t(1, 3)) * (p(2) - t(2, 3))) / det

  xsi(3) = 1.0D+00 - xsi(1) - xsi(2)

  return
end
