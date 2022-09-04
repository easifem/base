function line_exp_is_degenerate_nd(dim_num, p1, p2)

!*****************************************************************************80
!
!! LINE_EXP_IS_DEGENERATE_ND finds if an explicit line is degenerate in ND.
!
!  Discussion:
!
!    The explicit form of a line in ND is:
!
!      the line through the points P1 and P2.
!
!    An explicit line is degenerate if the two defining points are equal.
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
!    Input, real ( kind = 8 ) P1(DIM_NUM), P2(DIM_NUM), two points on the line.
!
!    Output, logical ( kind = 4 ) LINE_EXP_IS_DEGENERATE_ND, is TRUE if the line
!    is degenerate.
!
  implicit none

  integer(kind=4) dim_num

  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)

  line_exp_is_degenerate_nd = (all(p1(1:dim_num) == p2(1:dim_num)))

  return
end
subroutine line_exp_normal_2d(p1, p2, normal)

!*****************************************************************************80
!
!! LINE_EXP_NORMAL_2D computes a unit normal vector to a line in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The sign of the normal vector N is chosen so that the normal vector
!    points "to the left" of the direction of the line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two distinct points on the line.
!
!    Output, real ( kind = 8 ) NORMAL(2), a unit normal vector to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) norm
  real(kind=8) normal(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    normal(1:dim_num) = sqrt(2.0D+00)
    return
  end if

  norm = sqrt((p2(1) - p1(1))**2 + (p2(2) - p1(2))**2)

  normal(1) = -(p2(2) - p1(2)) / norm
  normal(2) = (p2(1) - p1(1)) / norm

  return
end
subroutine line_exp_perp_2d(p1, p2, p3, p4, flag)

!*****************************************************************************80
!
!! LINE_EXP_PERP_2D computes a line perpendicular to a line and through a point.
!
!  Discussion:
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
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) bot
  logical(kind=4) flag
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) p3(dim_num)
  real(kind=8) p4(dim_num)
  real(kind=8) r8_huge
  real(kind=8) t

  flag = .false.

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    flag = .true.
    p4(1:2) = r8_huge()
    return
  end if

  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)
!
!  (P3-P1) dot (P2-P1) = Norm(P3-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P3-P1) dot (P2-P1) / Norm(P3-P1)^2 = normalized coordinate T
!  of the projection of (P3-P1) onto (P2-P1).
!
  t = sum((p1(1:dim_num) - p3(1:dim_num)) &
          * (p1(1:dim_num) - p2(1:dim_num))) / bot

  p4(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))

  return
end
subroutine line_exp_point_dist_2d(p1, p2, p, dist)

!*****************************************************************************80
!
!! LINE_EXP_POINT_DIST_2D: distance ( explicit line, point ) in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Input, real ( kind = 8 ) P(2), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) bot
  real(kind=8) dist
  real(kind=8) dot
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then

    pn(1:dim_num) = p1(1:dim_num)
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  else

    dot = sum((p(1:dim_num) - p1(1:dim_num)) &
              * (p2(1:dim_num) - p1(1:dim_num)))

    bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)

    t = dot / bot

    pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))

  end if

  dist = sqrt(sum((p(1:dim_num) - pn(1:dim_num))**2))

  return
end
subroutine line_exp_point_dist_3d(p1, p2, p, dist)

!*****************************************************************************80
!
!! LINE_EXP_POINT_DIST_3D: distance ( explicit line, point ) in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the line.
!
!    Input, real ( kind = 8 ) P(3), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) bot
  real(kind=8) dist
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then

    pn(1:dim_num) = p1(1:dim_num)
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  else

    bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)

    t = sum((p(1:dim_num) - p1(1:dim_num)) &
            * (p2(1:dim_num) - p1(1:dim_num))) / bot

    pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))

  end if
!
!  Now compute the distance between the projection point and P.
!
  dist = sqrt(sum((p(1:dim_num) - pn(1:dim_num))**2))

  return
end
subroutine line_exp_point_dist_signed_2d(p1, p2, p, dist_signed)

!*****************************************************************************80
!
!! LINE_EXP_POINT_DIST_SIGNED_2D: signed distance ( exp line, point ) in 2D.
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
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Input, real ( kind = 8 ) P(2), the point whose signed distance is desired.
!
!    Output, real ( kind = 8 ) DIST_SIGNED, the signed distance from the
!    point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) dist_signed
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
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

  return
end
subroutine line_exp_point_near_2d(p1, p2, p, pn, dist, t)

!*****************************************************************************80
!
!! LINE_EXP_POINT_NEAR_2D: point on an explicit line nearest a point in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The nearest point PN = (XN,YN) has the form:
!
!      PN = (1-T) * P1 + T * P2.
!
!    If T is less than 0, PN is furthest from P2.
!    If T is between 0 and 1, PN is between P1 and P2.
!    If T is greater than 1, PN is furthest from P1.
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
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Input, real ( kind = 8 ) P(2), the point whose nearest neighbor on the
!    line is to be determined.
!
!    Output, real ( kind = 8 ) PN(2), the nearest point on the line to P.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
!    Output, real ( kind = 8 ) T, the relative position of the point
!    PN to the points P1 and P2.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) bot
  real(kind=8) dist
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_POINT_NEAR_2D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)

  t = sum((p1(1:dim_num) - p(1:dim_num)) &
          * (p1(1:dim_num) - p2(1:dim_num))) / bot

  pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))

  dist = sqrt(sum((pn(1:dim_num) - p(1:dim_num))**2))

  return
end
subroutine line_exp_point_near_3d(p1, p2, p, pn, dist, t)

!*****************************************************************************80
!
!! LINE_EXP_POINT_NEAR_3D: nearest point on explicit line to point in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!    The nearest point PN has the form:
!
!      PN = ( 1 - T ) * P1 + T * P2.
!
!    If T is less than 0, PN is furthest away from P2.
!    If T is between 0 and 1, PN is between P1 and P2.
!    If T is greater than 1, PN is furthest away from P1.
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
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the line.
!
!    Input, real ( kind = 8 ) P(3), the point whose nearest neighbor on
!    the line is to be determined.
!
!    Output, real ( kind = 8 ) PN(3), the point which is the nearest
!    point on the line to P.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the
!    nearest point on the line.
!
!    Output, real ( kind = 8 ) T, the relative position of the point
!    PN to P1 and P2.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) bot
  real(kind=8) dist
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p(dim_num)
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_EXP_POINT_NEAR_3D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)^2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  bot = sum((p2(1:dim_num) - p1(1:dim_num))**2)

  t = sum((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
!
!  Now compute the location of the projection point.
!
  pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
!
!  Now compute the distance between the projection point and P.
!
  dist = sqrt(sum((pn(1:dim_num) - p(1:dim_num))**2))

  return
end
subroutine line_exp2imp_2d(p1, p2, a, b, c)

!*****************************************************************************80
!
!! LINE_EXP2IMP_2D converts an explicit line to implicit form in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
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
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Output, real ( kind = 8 ) A, B, C, the implicit form of the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) norm
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
!
!  Take care of degenerate cases.
!
  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_EXP2IMP_2D - Warning!'
    write (*, '(a)') '  The line is degenerate.'
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

  return
end
subroutine line_exp2par_2d(p1, p2, f, g, x0, y0)

!*****************************************************************************80
!
!! LINE_EXP2PAR_2D converts a line from explicit to parametric form in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F^2 + G^2 = 1, and F nonnegative.
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
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
!    Output, real ( kind = 8 ) F, G, X0, Y0, the parametric parameters
!    of the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) f
  real(kind=8) g
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) norm
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) x0
  real(kind=8) y0

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_EXP2PAR_2D - Warning!'
    write (*, '(a)') '  The line is degenerate.'
  end if

  x0 = p1(1)
  y0 = p1(2)

  f = p2(1) - p1(1)
  g = p2(2) - p1(2)

  norm = sqrt(f * f + g * g)

  if (norm /= 0.0D+00) then
    f = f / norm
    g = g / norm
  end if

  if (f < 0.0D+00) then
    f = -f
    g = -g
  end if

  return
end
subroutine line_exp2par_3d(p1, p2, f, g, h, x0, y0, z0)

!*****************************************************************************80
!
!! LINE_EXP2PAR_3D converts a line from explicit to parametric form in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We normalize by always choosing F^2 + G^2 + H^2 = 1, and F nonnegative.
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
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the line.
!
!    Output, real ( kind = 8 ) F, G, H, X0, Y0, Z0, the parametric parameters
!    of the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) f
  real(kind=8) g
  real(kind=8) h
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) norm
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) x0
  real(kind=8) y0
  real(kind=8) z0

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_EXP2PAR_3D - Warning!'
    write (*, '(a)') '  The line is degenerate.'
  end if

  x0 = p1(1)
  y0 = p1(2)
  z0 = p1(3)

  f = p2(1) - p1(1)
  g = p2(2) - p1(2)
  h = p2(3) - p1(3)

  norm = sqrt(f * f + g * g + h * h)

  if (norm /= 0.0D+00) then
    f = f / norm
    g = g / norm
    h = h / norm
  end if

  if (f < 0.0D+00) then
    f = -f
    g = -g
    h = -h
  end if

  return
end
function line_imp_is_degenerate_2d(a, b, c)

!*****************************************************************************80
!
!! LINE_IMP_IS_DEGENERATE_2D finds if an implicit point is degenerate in 2D.
!
!  Discussion:
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
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
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Output, logical ( kind = 4 ) LINE_IMP_IS_DEGENERATE_2D, is true if the
!    line is degenerate.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  logical(kind=4) line_imp_is_degenerate_2d

  line_imp_is_degenerate_2d = (a * a + b * b == 0.0D+00)

  return
end
subroutine line_imp_point_dist_2d(a, b, c, p, dist)

!*****************************************************************************80
!
!! LINE_IMP_POINT_DIST_2D: distance ( implicit line, point ) in 2D.
!
!  Discussion:
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Input, real ( kind = 8 ) P(2), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) dist
  logical(kind=4) line_imp_is_degenerate_2d
  real(kind=8) p(dim_num)

  if (line_imp_is_degenerate_2d(a, b, c)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_IMP_POINT_DIST_2D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if

  dist = abs(a * p(1) + b * p(2) + c) / sqrt(a * a + b * b)

  return
end
subroutine line_imp_point_dist_signed_2d(a, b, c, p, dist_signed)

!*****************************************************************************80
!
!! LINE_IMP_POINT_DIST_SIGNED_2D: signed distance ( imp line, point ) in 2D.
!
!  Discussion:
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C * Z + D = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Input, real ( kind = 8 ) P(2), the coordinates of the point.
!
!    Output, real ( kind = 8 ) DIST_SIGNED, the signed distance from the
!    point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) dist_signed
  logical(kind=4) line_imp_is_degenerate_2d
  real(kind=8) p(dim_num)

  if (line_imp_is_degenerate_2d(a, b, c)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_IMP_POINT_DIST_SIGNED_2D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if

  dist_signed = -sign(1.0D+00, c) * (a * p(1) + b * p(2) + c) / &
                sqrt(a * a + b * b)

  return
end
subroutine line_imp2exp_2d(a, b, c, p1, p2)

!*****************************************************************************80
!
!! LINE_IMP2EXP_2D converts an implicit line to explicit form in 2D.
!
!  Discussion:
!
!    The implicit form of line in 2D is:
!
!      A * X + B * Y + C = 0
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2005
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
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Output, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  logical(kind=4) line_imp_is_degenerate_2d
  real(kind=8) normsq
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)

  if (line_imp_is_degenerate_2d(a, b, c)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_IMP2EXP_2D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if

  normsq = a * a + b * b

  p1(1) = -a * c / normsq
  p1(2) = -b * c / normsq

  if (abs(b) < abs(a)) then
    p2(1) = -(a - b / a) * c / normsq
    p2(2) = -(b + 1.0D+00) * c / normsq
  else
    p2(1) = -(a + 1.0D+00) * c / normsq
    p2(2) = -(b - a / b) * c / normsq
  end if

  return
end
subroutine line_imp2par_2d(a, b, c, f, g, x0, y0)

!*****************************************************************************80
!
!! LINE_IMP2PAR_2D converts an implicit line to parametric form in 2D.
!
!  Discussion:
!
!    The implicit form of line in 2D is:
!
!      A * X + B * Y + C = 0
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We may normalize by choosing F*F + G*G = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 1999
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
!    Input, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
!    Output, real ( kind = 8 ) F, G, X0, Y0, the parametric parameters of
!    the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) f
  logical(kind=4) line_imp_is_degenerate_2d
  real(kind=8) g
  real(kind=8) x0
  real(kind=8) y0

  if (line_imp_is_degenerate_2d(a, b, c)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINE_IMP2PAR_2D - Fatal error!'
    write (*, '(a)') '  The line is degenerate.'
    stop 1
  end if

  x0 = -a * c / (a * a + b * b)
  y0 = -b * c / (a * a + b * b)

  f = b / sqrt(a * a + b * b)
  g = -a / sqrt(a * a + b * b)

  if (f < 0.0D+00) then
    f = -f
    g = -g
  end if

  return
end
subroutine line_par_point_dist_2d(f, g, x0, y0, p, dist)

!*****************************************************************************80
!
!! LINE_PAR_POINT_DIST_2D: distance ( parametric line, point ) in 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F*F + G*G = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
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
!    Input, real ( kind = 8 ) F, G, X0, Y0, the parametric line parameters.
!
!    Input, real ( kind = 8 ) P(2), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) dist
  real(kind=8) dx
  real(kind=8) dy
  real(kind=8) f
  real(kind=8) g
  real(kind=8) p(dim_num)
  real(kind=8) x0
  real(kind=8) y0

  dx = g * g * (p(1) - x0) - f * g * (p(2) - y0)
  dy = -f * g * (p(1) - x0) + f * f * (p(2) - y0)

  dist = sqrt(dx * dx + dy * dy) / (f * f + g * g)

  return
end
subroutine line_par_point_dist_3d(f, g, h, x0, y0, z0, p, dist)

!*****************************************************************************80
!
!! LINE_PAR_POINT_DIST_3D: distance ( parametric line, point ) in 3D.
!
!  Discussion:
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We normalize by always choosing F*F + G*G + H*H = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
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
!    Input, real ( kind = 8 ) F, G, H, X0, Y0, Z0, the parametric line
!    parameters.
!
!    Input, real ( kind = 8 ) P(3), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) DIST, the distance from the point to the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) dist
  real(kind=8) dx
  real(kind=8) dy
  real(kind=8) dz
  real(kind=8) f
  real(kind=8) g
  real(kind=8) h
  real(kind=8) p(dim_num)
  real(kind=8) x0
  real(kind=8) y0
  real(kind=8) z0

  dx = g * (f * (p(2) - y0) - g * (p(1) - x0)) &
       + h * (f * (p(3) - z0) - h * (p(1) - x0))

  dy = h * (g * (p(3) - z0) - h * (p(2) - y0)) &
       - f * (f * (p(2) - y0) - g * (p(1) - x0))

  dz = -f * (f * (p(3) - z0) - h * (p(1) - x0)) &
       - g * (g * (p(3) - z0) - h * (p(2) - y0))

  dist = sqrt(dx * dx + dy * dy + dz * dz) &
         / (f * f + g * g + h * h)

  return
end
subroutine line_par_point_near_2d(f, g, x0, y0, p, pn)

!*****************************************************************************80
!
!! LINE_PAR_POINT_NEAR_2D: nearest point on parametric line to given point, 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We may normalize by choosing F*F + G*G = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2013
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
!    Input, real ( kind = 8 ) F, G, X0, Y0, the parametric line parameters.
!
!    Input, real ( kind = 8 ) P(2), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) PN(2), the point on the parametric line which
!    is nearest to P.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) f
  real(kind=8) g
  real(kind=8) p(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t
  real(kind=8) x0
  real(kind=8) y0

  t = (f * (p(1) - x0) + g * (p(2) - y0)) / (f * f + g * g)

  pn(1) = x0 + t * f
  pn(2) = y0 + t * g

  return
end
subroutine line_par_point_near_3d(f, g, h, x0, y0, z0, p, pn)

!*****************************************************************************80
!
!! LINE_PAR_POINT_NEAR_3D: nearest point on parametric line to given point, 3D.
!
!  Discussion:
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We may normalize by choosing F*F + G*G + H*H = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2013
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
!    Input, real ( kind = 8 ) F, G, H, X0, Y0, Z0, the parametric
!    line parameters.
!
!    Input, real ( kind = 8 ) P(3), the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = 8 ) PN(3), the point on the parametric line which
!    is nearest to P.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) f
  real(kind=8) g
  real(kind=8) h
  real(kind=8) p(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) t
  real(kind=8) x0
  real(kind=8) y0
  real(kind=8) z0

  t = (f * (p(1) - x0) + g * (p(2) - y0) + h * (p(3) - z0)) &
      / (f * f + g * g + h * h)

  pn(1) = x0 + t * f
  pn(2) = y0 + t * g
  pn(3) = z0 + t * h

  return
end
subroutine line_par2exp_2d(f, g, x0, y0, p1, p2)

!*****************************************************************************80
!
!! LINE_PAR2EXP_2D converts a parametric line to explicit form in 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F*F + G*G = 1, and F nonnegative.
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2005
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
!    Input, real ( kind = 8 ) F, G, X0, Y0, the parametric line parameters.
!
!    Output, real ( kind = 8 ) P1(2), P2(2), two points on the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) f
  real(kind=8) g
  real(kind=8) x0
  real(kind=8) y0
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)

  p1(1) = x0
  p1(2) = y0

  p2(1) = p1(1) + f
  p2(2) = p1(2) + g

  return
end
subroutine line_par2exp_3d(f, g, h, x0, y0, z0, p1, p2)

!*****************************************************************************80
!
!! LINE_PAR2EXP_3D converts a parametric line to explicit form in 3D.
!
!  Discussion:
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We may normalize by choosing F*F + G*G + H*H = 1, and F nonnegative.
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 April 2013
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
!    Input, real ( kind = 8 ) F, G, H, X0, Y0, Z0, the parametric
!    line parameters.
!
!    Output, real ( kind = 8 ) P1(3), P2(3), two points on the line.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) f
  real(kind=8) g
  real(kind=8) h
  real(kind=8) x0
  real(kind=8) y0
  real(kind=8) z0
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)

  p1(1) = x0
  p1(2) = y0
  p1(3) = z0

  p2(1) = p1(1) + f
  p2(2) = p1(2) + g
  p2(3) = p1(3) + h

  return
end
subroutine line_par2imp_2d(f, g, x0, y0, a, b, c)

!*****************************************************************************80
!
!! LINE_PAR2IMP_2D converts a parametric line to implicit form in 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F*F + G*G = 1, and F nonnegative.
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 1999
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
!    Input, real ( kind = 8 ) F, G, X0, Y0, the parametric line parameters.
!
!    Output, real ( kind = 8 ) A, B, C, the implicit line parameters.
!
  implicit none

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) f
  real(kind=8) g
  real(kind=8) x0
  real(kind=8) y0

  a = -g
  b = f
  c = g * x0 - f * y0

  return
end
subroutine lines_exp_angle_3d(p1, p2, q1, q2, angle)

!*****************************************************************************80
!
!! LINES_EXP_ANGLE_3D finds the angle between two explicit lines in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(3), Q2(3), two points on the second line.
!
!    Output, real ( kind = 8 ) ANGLE, the angle in radians between the two
!    lines.  The angle is computed using the ACOS function, and so lies between
!    0 and PI.  But if one of the lines is degenerate, the angle is
!    returned as -1.0.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) angle
  real(kind=8) ctheta
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pdotq
  real(kind=8) pnorm
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) qnorm
  real(kind=8) r8_acos

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
!   write ( *, '(a)' ) ' '
!   write ( *, '(a)' ) 'LINES_EXP_ANGLE_3D - Fatal error!'
!   write ( *, '(a)' ) '  The line (P1,P2) is degenerate!'
    angle = -1.0D+00
    return
  end if

  if (line_exp_is_degenerate_nd(dim_num, q1, q2)) then
!   write ( *, '(a)' ) ' '
!   write ( *, '(a)' ) 'LINES_EXP_ANGLE_3D - Warning!'
!   write ( *, '(a)' ) '  The line (Q1,Q2) is degenerate!'
    angle = -1.0D+00
    return
  end if

  pnorm = sqrt(sum((p2(1:dim_num) - p1(1:dim_num))**2))

  qnorm = sqrt(sum((q2(1:dim_num) - q1(1:dim_num))**2))

  pdotq = sum((p2(1:dim_num) - p1(1:dim_num)) &
              * (q2(1:dim_num) - q1(1:dim_num)))

  ctheta = pdotq / (pnorm * qnorm)

  angle = r8_acos(ctheta)

  return
end
subroutine lines_exp_angle_nd(dim_num, p1, p2, q1, q2, angle)

!*****************************************************************************80
!
!! LINES_EXP_ANGLE_ND returns the angle between two explicit lines in ND.
!
!  Discussion:
!
!    The explicit form of a line in ND is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) P1(DIM_NUM), P2(DIM_NUM), two points
!    on the first line.
!
!    Input, real ( kind = 8 ) Q1(DIM_NUM), Q2(DIM_NUM), two points
!    on the second line.
!
!    Output, real ( kind = 8 ) ANGLE, the angle in radians between the two
!    lines.  The angle is computed using the ACOS function, and so lies
!    between 0 and PI.  But if one of the lines is degenerate, the angle
!    is returned as -1.0.
!
  implicit none

  integer(kind=4) dim_num

  real(kind=8) angle
  real(kind=8) ctheta
  logical(kind=4) line_exp_is_degenerate_nd
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pdotq
  real(kind=8) pnorm
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) qnorm
  real(kind=8) r8_acos

  if (line_exp_is_degenerate_nd(dim_num, p1, p2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINES_EXP_ANGLE_3D - Fatal error!'
    write (*, '(a)') '  The line (P1,P2) is degenerate!'
    angle = -1.0D+00
    stop 1
  end if

  if (line_exp_is_degenerate_nd(dim_num, q1, q2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINES_EXP_ANGLE_3D - Fatal error!'
    write (*, '(a)') '  The line (Q1,Q2) is degenerate!'
    angle = -1.0D+00
    stop 1
  end if

  pnorm = sqrt(sum((p2(1:dim_num) - p1(1:dim_num))**2))
  qnorm = sqrt(sum((q2(1:dim_num) - q1(1:dim_num))**2))

  pdotq = sum((p2(1:dim_num) - p1(1:dim_num)) &
              * (q2(1:dim_num) - q1(1:dim_num)))

  ctheta = pdotq / (pnorm * qnorm)
  angle = r8_acos(ctheta)

  return
end
subroutine lines_exp_dist_3d(p1, p2, q1, q2, dist)

!*****************************************************************************80
!
!! LINES_EXP_DIST_3D computes the distance between two explicit lines in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(3), Q2(3), two points on the second line.
!
!    Output, real ( kind = 8 ) DIST, the distance between the lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) a11
  real(kind=8) a12
  real(kind=8) a13
  real(kind=8) a21
  real(kind=8) a22
  real(kind=8) a23
  real(kind=8) a31
  real(kind=8) a32
  real(kind=8) a33
  real(kind=8) bot
  real(kind=8) cr1
  real(kind=8) cr2
  real(kind=8) cr3
  real(kind=8) dist
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) top
!
!  The distance is found by computing the volume of a parallelipiped,
!  and dividing by the area of its base.
!
!  But if the lines are parallel, we compute the distance by
!  finding the distance between the first line and any point
!  on the second line.
!
  a11 = q1(1) - p1(1)
  a12 = q1(2) - p1(2)
  a13 = q1(3) - p1(3)

  a21 = p2(1) - p1(1)
  a22 = p2(2) - p1(2)
  a23 = p2(3) - p1(3)

  a31 = q2(1) - q1(1)
  a32 = q2(2) - q1(2)
  a33 = q2(3) - q1(3)
!
!  Compute the cross product.
!
  cr1 = a22 * a33 - a23 * a32
  cr2 = a23 * a31 - a21 * a33
  cr3 = a21 * a32 - a22 * a31

  bot = sqrt(cr1 * cr1 + cr2 * cr2 + cr3 * cr3)

  if (bot == 0.0D+00) then

    call line_exp_point_dist_3d(p1, p2, q1, dist)

  else

    top = abs(a11 * (a22 * a33 - a23 * a32) &
              - a12 * (a21 * a33 - a23 * a31) &
              + a13 * (a21 * a32 - a22 * a31))

    dist = top / bot

  end if

  return
end
subroutine lines_exp_dist_3d_2(p1, p2, q1, q2, dist)

!*****************************************************************************80
!
!! LINES_EXP_DIST_3D_2 computes the distance between two explicit lines in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!    This routine uses a method that is essentially independent of dimension.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(3), Q2(3), two points on the second line.
!
!    Output, real ( kind = 8 ) DIST, the distance between the lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) d
  real(kind=8) det
  real(kind=8) dist
  real(kind=8) e
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) qn(dim_num)
  real(kind=8) sn
  real(kind=8) tn
  real(kind=8) u(dim_num)
  real(kind=8) v(dim_num)
  real(kind=8) w0(dim_num)
!
!  Let U = (P2-P1) and V = (Q2-Q1) be the direction vectors on
!  the two lines.
!
  u(1:dim_num) = p2(1:dim_num) - p1(1:dim_num)
  v(1:dim_num) = q2(1:dim_num) - q1(1:dim_num)
!
!  Let SN be the unknown coordinate of the nearest point PN on line 1,
!  so that PN = P(SN) = P1 + SN * (P2-P1).
!
!  Let TN be the unknown coordinate of the nearest point QN on line 2,
!  so that QN = Q(TN) = Q1 + TN * (Q2-Q1).
!
!  Let W0 = (P1-Q1).
!
  w0(1:dim_num) = p1(1:dim_num) - q1(1:dim_num)
!
!  The vector direction WC = P(SN) - Q(TC) is unique (among directions)
!  perpendicular to both U and V, so
!
!    U dot WC = 0
!    V dot WC = 0
!
!  or, equivalently:
!
!    U dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
!    V dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
!
!  or, equivalently:
!
!    (u dot u ) * sn - (u dot v ) tc = -u * w0
!    (v dot u ) * sn - (v dot v ) tc = -v * w0
!
!  or, equivalently:
!
!   ( a  -b ) * ( sn ) = ( -d )
!   ( b  -c )   ( tc )   ( -e )
!
  a = dot_product(u, u)
  b = dot_product(u, v)
  c = dot_product(v, v)
  d = dot_product(u, w0)
  e = dot_product(v, w0)
!
!  Check the determinant.
!
  det = -a * c + b * b

  if (det == 0.0D+00) then
    sn = 0.0D+00
    if (abs(b) < abs(c)) then
      tn = e / c
    else
      tn = d / b
    end if
  else
    sn = (c * d - b * e) / det
    tn = (b * d - a * e) / det
  end if

  pn(1:dim_num) = p1(1:dim_num) + sn * (p2(1:dim_num) - p1(1:dim_num))
  qn(1:dim_num) = q1(1:dim_num) + tn * (q2(1:dim_num) - q1(1:dim_num))

  dist = sqrt(sum((pn(1:dim_num) - qn(1:dim_num))**2))

  return
end
function lines_exp_equal_2d(p1, p2, q1, q2)

!*****************************************************************************80
!
!! LINES_EXP_EQUAL_2D determines if two explicit lines are equal in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    It is essentially impossible to accurately determine whether two
!    explicit lines are equal in 2D.  However, for form's sake, and
!    because occasionally the correct result can be determined, we
!    provide this routine.  Since divisions are avoided, if the
!    input data is exactly representable, the result should be
!    correct.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(2), Q2(2), two points on the second line.
!
!    Output, logical ( kind = 4 ) LINES_EXP_EQUAL_2D, is TRUE if the two lines
!    are determined to be identical.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) lines_exp_equal_2d
  real(kind=8) p1(2)
  real(kind=8) p2(2)
  real(kind=8) q1(2)
  real(kind=8) q2(2)
  real(kind=8) test1
  real(kind=8) test2
  real(kind=8) test3
  real(kind=8) test4
!
!  Slope (P1,P2) = Slope (P2,Q1).
!
  test1 = (p2(2) - p1(2)) * (q1(1) - p2(1)) &
          - (p2(1) - p1(1)) * (q1(2) - p2(2))

  if (test1 /= 0.0D+00) then
    lines_exp_equal_2d = .false.
    return
  end if
!
!  Slope (Q1,Q2) = Slope (P2,Q1).
!
  test2 = (q2(2) - q1(2)) * (q1(1) - p2(1)) &
          - (q2(1) - q1(1)) * (q1(2) - p2(2))

  if (test2 /= 0.0D+00) then
    lines_exp_equal_2d = .false.
    return
  end if
!
!  Slope (P1,P2) = Slope (P1,Q2).
!
  test3 = (p2(2) - p1(2)) * (q2(1) - p1(1)) &
          - (p2(1) - p1(1)) * (q2(2) - p1(2))

  if (test3 /= 0.0D+00) then
    lines_exp_equal_2d = .false.
    return
  end if
!
!  Slope (Q1,Q2) = Slope (P1,Q2).
!
  test4 = (q2(2) - q1(2)) * (q2(1) - p1(1)) &
          - (q2(1) - q1(1)) * (q2(2) - p1(2))

  if (test4 /= 0.0D+00) then
    lines_exp_equal_2d = .false.
    return
  end if

  lines_exp_equal_2d = .true.

  return
end
subroutine lines_exp_int_2d(p1, p2, q1, q2, ival, p)

!*****************************************************************************80
!
!! LINES_EXP_INT_2D determines where two explicit lines intersect in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
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
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a1
  real(kind=8) a2
  real(kind=8) b1
  real(kind=8) b2
  real(kind=8) c1
  real(kind=8) c2
  integer(kind=4) ival
  logical(kind=4) point_1
  logical(kind=4) point_2
  real(kind=8) p(2)
  real(kind=8) p1(2)
  real(kind=8) p2(2)
  real(kind=8) q1(2)
  real(kind=8) q2(2)

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

  return
end
subroutine lines_exp_near_3d(p1, p2, q1, q2, pn, qn)

!*****************************************************************************80
!
!! LINES_EXP_NEAR_3D computes the nearest points on two explicit lines in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!    This routine uses a method that is essentially independent of dimension.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(3), Q2(3), two points on the second line.
!
!    Output, real ( kind = 8 ) PN(3), QN(3), the points on the first and
!    second lines that are nearest.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) a
  real(kind=8) b
  real(kind=8) c
  real(kind=8) d
  real(kind=8) det
  real(kind=8) e
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pn(dim_num)
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) qn(dim_num)
  real(kind=8) sn
  real(kind=8) tn
  real(kind=8) u(dim_num)
  real(kind=8) v(dim_num)
  real(kind=8) w0(dim_num)
!
!  Let U = (P2-P1) and V = (Q2-Q1) be the direction vectors on
!  the two lines.
!
  u(1:dim_num) = p2(1:dim_num) - p1(1:dim_num)
  v(1:dim_num) = q2(1:dim_num) - q1(1:dim_num)
!
!  Let SN be the unknown coordinate of the nearest point PN on line 1,
!  so that PN = P(SN) = P1 + SN * (P2-P1).
!
!  Let TN be the unknown coordinate of the nearest point QN on line 2,
!  so that QN = Q(TN) = Q1 + TN * (Q2-Q1).
!
!  Let W0 = (P1-Q1).
!
  w0(1:dim_num) = p1(1:dim_num) - q1(1:dim_num)
!
!  The vector direction WC = P(SN) - Q(TC) is unique (among directions)
!  perpendicular to both U and V, so
!
!    U dot WC = 0
!    V dot WC = 0
!
!  or, equivalently:
!
!    U dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
!    V dot ( P1 + SN * (P2 - P1) - Q1 - TN * (Q2 - Q1) ) = 0
!
!  or, equivalently:
!
!    (u dot u ) * sn - (u dot v ) tc = -u * w0
!    (v dot u ) * sn - (v dot v ) tc = -v * w0
!
!  or, equivalently:
!
!   ( a  -b ) * ( sn ) = ( -d )
!   ( b  -c )   ( tc )   ( -e )
!
  a = dot_product(u, u)
  b = dot_product(u, v)
  c = dot_product(v, v)
  d = dot_product(u, w0)
  e = dot_product(v, w0)
!
!  Check the determinant.
!
  det = -a * c + b * b

  if (det == 0.0D+00) then
    sn = 0.0D+00
    if (abs(b) < abs(c)) then
      tn = e / c
    else
      tn = d / b
    end if
  else
    sn = (c * d - b * e) / det
    tn = (b * d - a * e) / det
  end if

  pn(1:dim_num) = p1(1:dim_num) + sn * (p2(1:dim_num) - p1(1:dim_num))
  qn(1:dim_num) = q1(1:dim_num) + tn * (q2(1:dim_num) - q1(1:dim_num))

  return
end
function lines_exp_parallel_2d(p1, p2, q1, q2)

!*****************************************************************************80
!
!! LINES_EXP_PARALLEL_2D determines if two lines are parallel in 2D.
!
!  Discussion:
!
!    The explicit form of a line in 2D is:
!
!      the line through the points P1 and P2.
!
!    The test is essentially a comparison of slopes, but should be
!    more accurate than an explicit slope comparison, and unfazed
!    by degenerate cases.
!
!    On the other hand, there is NO tolerance for error.  If the
!    slopes differ by a single digit in the last place, then the
!    lines are judged to be nonparallel.  A more robust test would
!    be to compute the angle between the lines, because then it makes
!    sense to say the lines are "almost" parallel: the angle is small.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(2), Q2(2), two points on the second line.
!
!    Output, logical ( kind = 4 ) LINES_EXP_PARALLEL_2D is TRUE if the
!    lines are parallel.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  logical(kind=4) lines_exp_parallel_2d
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)

  lines_exp_parallel_2d = (p2(1) - p1(1)) * (q2(2) - q1(2)) == &
    (q2(1) - q1(1)) * (p2(2) - p1(2))

  return
end
function lines_exp_parallel_3d(p1, p2, q1, q2)

!*****************************************************************************80
!
!! LINES_EXP_PARALLEL_3D determines if two lines are parallel in 3D.
!
!  Discussion:
!
!    The explicit form of a line in 3D is:
!
!      the line through the points P1 and P2.
!
!    The points P1, P2 define a direction (P2-P1).  Similarly, the
!    points (Q1,Q2) define a direction (Q2-Q1).  The quantity
!
!      (P2-P1) dot (Q2-Q1) = norm(P2-P1) * norm(Q2-Q1) * cos ( angle )
!
!    Therefore, the following value is between 0 and 1;
!
!      abs ( (P2-P1) dot (Q2-Q1) / ( norm(P2-P1) * norm(Q2-Q1) ) )
!
!    and the lines are parallel if
!
!      abs ( (P2-P1) dot (Q2-Q1) / ( norm(P2-P1) * norm(Q2-Q1) ) ) = 1
!
!    We can rephrase this as requiring:
!
!      ( (P2-P1)dot(Q2-Q1) )^2 = (P2-P1)dot(P2-P1) * (Q2-Q1)dot(Q2-Q1)
!
!    which avoids division and square roots.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(3), P2(3), two points on the first line.
!
!    Input, real ( kind = 8 ) Q1(3), Q2(3), two points on the second line.
!
!    Output, logical ( kind = 4 ) LINES_EXP_PARALLEL_3D is TRUE if the lines
!    are parallel.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  logical(kind=4) lines_exp_parallel_3d
  real(kind=8) p1(dim_num)
  real(kind=8) p2(dim_num)
  real(kind=8) pdotp
  real(kind=8) pdotq
  real(kind=8) q1(dim_num)
  real(kind=8) q2(dim_num)
  real(kind=8) qdotq

  pdotq = dot_product(p2(1:dim_num) - p1(1:dim_num), &
                      q2(1:dim_num) - q1(1:dim_num))

  pdotp = dot_product(p2(1:dim_num) - p1(1:dim_num), &
                      p2(1:dim_num) - p1(1:dim_num))

  qdotq = dot_product(q2(1:dim_num) - q1(1:dim_num), &
                      q2(1:dim_num) - q1(1:dim_num))

  lines_exp_parallel_3d = (pdotq * pdotq == pdotp * qdotq)

  return
end
subroutine lines_imp_angle_2d(a1, b1, c1, a2, b2, c2, theta)

!*****************************************************************************80
!
!! LINES_IMP_ANGLE_2D finds the angle between two implicit lines in 2D.
!
!  Discussion:
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 1999
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
!    Input, real ( kind = 8 ) A1, B1, C1, the implicit parameters of the
!    first line.
!
!    Input, real ( kind = 8 ) A2, B2, C2, the implicit parameters of the
!    second line.
!
!    Output, real ( kind = 8 ) THETA, the angle between the two lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a1
  real(kind=8) a2
  real(kind=8) b1
  real(kind=8) b2
  real(kind=8) c1
  real(kind=8) c2
  real(kind=8) pdotq
  real(kind=8) pnorm
  real(kind=8) qnorm
  real(kind=8) r8_acos
  real(kind=8) theta

  pdotq = a1 * a2 + b1 * b2
  pnorm = sqrt(a1 * a1 + b1 * b1)
  qnorm = sqrt(a2 * a2 + b2 * b2)

  theta = r8_acos(pdotq / (pnorm * qnorm))

  return
end
subroutine lines_imp_dist_2d(a1, b1, c1, a2, b2, c2, dist)

!*****************************************************************************80
!
!! LINES_IMP_DIST_2D determines the distance between two implicit lines in 2D.
!
!  Discussion:
!
!    If the lines intersect, then their distance is zero.
!    If the two lines are parallel, then they have a nonzero distance.
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1, B1, C1, define the first line.
!    At least one of A1 and B1 must be nonzero.
!
!    Input, real ( kind = 8 ) A2, B2, C2, define the second line.
!    At least one of A2 and B2 must be nonzero.
!
!    Output, real ( kind = 8 ) DIST, the distance between the two lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a1
  real(kind=8) a2
  real(kind=8) b1
  real(kind=8) b2
  real(kind=8) c1
  real(kind=8) c2
  real(kind=8) dist
  logical(kind=4) line_imp_is_degenerate_2d
!
!  Refuse to handle degenerate lines.
!
  if (line_imp_is_degenerate_2d(a1, b1, c1)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINES_IMP_DIST_2D - Fatal error!'
    write (*, '(a)') '  Line 1 is degenerate.'
    stop 1
  end if

  if (line_imp_is_degenerate_2d(a2, b2, c2)) then
    write (*, '(a)') ' '
    write (*, '(a)') 'LINES_IMP_DIST_2D - Fatal error!'
    write (*, '(a)') '  Line 2 is degenerate.'
    stop 1
  end if
!
!  Determine if the lines intersect.
!
  if (a1 * b2 /= a2 * b1) then
    dist = 0.0D+00
    return
  end if
!
!  Determine the distance between the parallel lines.
!
  dist = abs(c2 / sqrt(a2 * a2 + b2 * b2) &
             - c1 / sqrt(a1 * a1 + b1 * b1))

  return
end
subroutine lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)

!*****************************************************************************80
!
!! LINES_IMP_INT_2D determines where two implicit lines intersect in 2D.
!
!  Discussion:
!
!    The implicit form of a line in 2D is:
!
!      A * X + B * Y + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 2005
!
!  Author:
!
!    John Burkardt
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
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) a(dim_num, dim_num + 1)
  real(kind=8) a1
  real(kind=8) a2
  real(kind=8) b1
  real(kind=8) b2
  real(kind=8) c1
  real(kind=8) c2
  integer(kind=4) info
  integer(kind=4) ival
  logical(kind=4) line_imp_is_degenerate_2d
  real(kind=8) p(dim_num)

  p(1:dim_num) = 0.0D+00
!
!  Refuse to handle degenerate lines.
!
  if (line_imp_is_degenerate_2d(a1, b1, c1)) then
    ival = -1
    return
  end if

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

  return
end
subroutine lines_par_angle_2d(f1, g1, x01, y01, f2, g2, x02, y02, theta)

!*****************************************************************************80
!
!! LINES_PAR_ANGLE_2D finds the angle between two parametric lines in 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F*F + G*G = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 1999
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
!    Input, real ( kind = 8 ) F1, G1, X01, Y01, the parametric parameters of the
!    first line.
!
!    Input, real ( kind = 8 ) F2, G2, X02, Y02, the parametric parameters of the
!    second line.
!
!    Output, real ( kind = 8 ) THETA, the angle between the two lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) f1
  real(kind=8) f2
  real(kind=8) g1
  real(kind=8) g2
  real(kind=8) pdotq
  real(kind=8) pnorm
  real(kind=8) qnorm
  real(kind=8) r8_acos
  real(kind=8) theta
  real(kind=8) x01
  real(kind=8) x02
  real(kind=8) y01
  real(kind=8) y02

  pdotq = f1 * f2 + g1 * g2
  pnorm = sqrt(f1 * f1 + g1 * g1)
  qnorm = sqrt(f2 * f2 + g2 * g2)

  theta = r8_acos(pdotq / (pnorm * qnorm))

  return
end
subroutine lines_par_angle_3d(f1, g1, h1, x01, y01, z01, f2, g2, h2, &
                              x02, y02, z02, theta)

!*****************************************************************************80
!
!! LINES_PAR_ANGLE_3D finds the angle between two parametric lines in 3D.
!
!  Discussion:
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We normalize by always choosing F*F + G*G + H*H = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
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
!    Input, real ( kind = 8 ) F1, G1, H1, X01, Y01, Z01, the parametric
!    parameters of the first line.
!
!    Input, real ( kind = 8 ) F2, G2, H2, X02, Y02, Z02, the parametric
!    parameters of the second line.
!
!    Output, real ( kind = 8 ) THETA, the angle between the two lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) f1
  real(kind=8) f2
  real(kind=8) g1
  real(kind=8) g2
  real(kind=8) h1
  real(kind=8) h2
  real(kind=8) pdotq
  real(kind=8) pnorm
  real(kind=8) qnorm
  real(kind=8) r8_acos
  real(kind=8) theta
  real(kind=8) x01
  real(kind=8) x02
  real(kind=8) y01
  real(kind=8) y02
  real(kind=8) z01
  real(kind=8) z02

  pdotq = f1 * f2 + g1 * g2 + h1 * h2
  pnorm = sqrt(f1 * f1 + g1 * g1 + h1 * h1)
  qnorm = sqrt(f2 * f2 + g2 * g2 + h2 * h2)

  theta = r8_acos(pdotq / (pnorm * qnorm))

  return
end
subroutine lines_par_dist_3d(f1, g1, h1, x01, y01, z01, f2, g2, h2, &
                             x02, y02, z02, dist)

!*****************************************************************************80
!
!! LINES_PAR_DIST_3D finds the distance between two parametric lines in 3D.
!
!  Discussion:
!
!    The parametric form of a line in 3D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!      Z = Z0 + H * T
!
!    We normalize by always choosing F*F + G*G + H*H = 1, and F nonnegative.
!
!    This code does not work for parallel or near parallel lines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
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
!    Input, real ( kind = 8 ) F1, G1, H1, X01, Y01, Z01, the parametric
!    parameters of the first line.
!
!    Input, real ( kind = 8 ) F2, G2, H2, X02, Y02, Z02, the parametric
!    parameters of the second line.
!
!    Output, real ( kind = 8 ) DIST, the distance between the two lines.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 3

  real(kind=8) dist
  real(kind=8) f1
  real(kind=8) f2
  real(kind=8) g1
  real(kind=8) g2
  real(kind=8) h1
  real(kind=8) h2
  real(kind=8) x01
  real(kind=8) x02
  real(kind=8) y01
  real(kind=8) y02
  real(kind=8) z01
  real(kind=8) z02

  dist = abs((x02 - x01) * (g1 * h2 - g2 * h1) &
             + (y02 - y01) * (h1 * f2 - h2 * f1) &
             + (z02 - z01) * (f1 * g2 - f2 * g1)) / &
         ((f1 * g2 - f2 * g1)**2 &
          + (g1 * h2 - g2 * h1)**2 &
          + (h1 * f2 - h2 * f1)**2)

  return
end
subroutine lines_par_int_2d(f1, g1, x1, y1, f2, g2, x2, y2, t1, t2, pint)

!*****************************************************************************80
!
!! LINES_PAR_INT_2D determines where two parametric lines intersect in 2D.
!
!  Discussion:
!
!    The parametric form of a line in 2D is:
!
!      X = X0 + F * T
!      Y = Y0 + G * T
!
!    We normalize by always choosing F*F + G*G = 1, and F nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 1999
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
!    Input, real ( kind = 8 ) F1, G1, X1, Y1, define the first parametric line.
!
!    Input, real ( kind = 8 ) F2, G2, X2, Y2, define the second parametric line.
!
!    Output, real ( kind = 8 ) T1, T2, the T parameters on the first and second
!    lines of the intersection point.
!
!    Output, real ( kind = 8 ) PINT(2), the intersection point.
!
  implicit none

  integer(kind=4), parameter :: dim_num = 2

  real(kind=8) det
  real(kind=8) f1
  real(kind=8) f2
  real(kind=8) g1
  real(kind=8) g2
  real(kind=8) pint(dim_num)
  real(kind=8) t1
  real(kind=8) t2
  real(kind=8) x1
  real(kind=8) x2
  real(kind=8) y1
  real(kind=8) y2

  det = f2 * g1 - f1 * g2

  if (det == 0.0D+00) then
    t1 = 0.0D+00
    t2 = 0.0D+00
    pint(1:dim_num) = 0.0D+00
  else
    t1 = (f2 * (y2 - y1) - g2 * (x2 - x1)) / det
    t2 = (f1 * (y2 - y1) - g1 * (x2 - x1)) / det
    pint(1) = x1 + f1 * t1
    pint(2) = y1 + g1 * t1
  end if

  return
end
