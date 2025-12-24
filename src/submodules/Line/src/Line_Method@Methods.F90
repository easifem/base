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
ans = (ALL(p1(1:dim_num) == p2(1:dim_num)))
END PROCEDURE line_exp_is_degenerate_nd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_exp2imp_2d
INTEGER(i4b), PARAMETER :: dim_num = 2
REAL(dfp) norm
!
!  Take care of degenerate cases.
!
IF (line_exp_is_degenerate_nd(dim_num, p1, p2)) THEN
  RETURN
END IF

a = p2(2) - p1(2)
b = p1(1) - p2(1)
c = p2(1) * p1(2) - p1(1) * p2(2)

norm = a * a + b * b + c * c

IF (0.0D+00 < norm) THEN
  a = a / norm
  b = b / norm
  c = c / norm
END IF

IF (a < 0.0D+00) THEN
  a = -a
  b = -b
  c = -c
END IF

END PROCEDURE line_exp2imp_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_imp_is_degenerate_2d
ans = (a * a + b * b == 0.0D+00)
END PROCEDURE line_imp_is_degenerate_2d

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lines_imp_int_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) a(dim_num, dim_num + 1)
INTEGER(kind=4) info
!
p(1:dim_num) = 0.0D+00
!
!  Refuse to handle degenerate lines.
!
IF (line_imp_is_degenerate_2d(a1, b1, c1)) THEN
  ival = -1
  RETURN
END IF
!
IF (line_imp_is_degenerate_2d(a2, b2, c2)) THEN
  ival = -2
  RETURN
END IF
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
CALL r8mat_solve(2, 1, a, info)
!
!  If the inverse exists, then the lines intersect at the solution point.
!
IF (info == 0) THEN

  ival = 1
  p(1:dim_num) = a(1:dim_num, 3)
!
!  If the inverse does not exist, then the lines are parallel
!  or coincident.  Check for parallelism by seeing if the
!  C entries are in the same ratio as the A or B entries.
!
ELSE
  ival = 0
  IF (a1 == 0.0D+00) THEN
    IF (b2 * c1 == c2 * b1) THEN
      ival = 2
    END IF
  ELSE
    IF (a2 * c1 == c2 * a1) THEN
      ival = 2
    END IF
  END IF
END IF
!
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_exp_perp_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) bot
REAL(kind=8) t
!
flag = .FALSE.
IF (line_exp_is_degenerate_nd(dim_num, p1, p2)) THEN
  flag = .TRUE.
  p4(1:2) = r8_huge()
  RETURN
END IF
!
bot = SUM((p2(1:dim_num) - p1(1:dim_num))**2)
!
!  (P3-P1) dot (P2-P1) = Norm(P3-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P3-P1) dot (P2-P1) / Norm(P3-P1)^2 = normalized coordinate T
!  of the projection of (P3-P1) onto (P2-P1).
!
t = SUM((p1(1:dim_num) - p3(1:dim_num)) &
        * (p1(1:dim_num) - p2(1:dim_num))) / bot
!
p4(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
!
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE lines_exp_int_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) a1
REAL(kind=8) a2
REAL(kind=8) b1
REAL(kind=8) b2
REAL(kind=8) c1
REAL(kind=8) c2
LOGICAL(kind=4) point_1
LOGICAL(kind=4) point_2
!
ival = 0
p(1:dim_num) = 0.0D+00
!
!  Check whether either line is a point.
!
IF (ALL(p1(1:dim_num) == p2(1:dim_num))) THEN
  point_1 = .TRUE.
ELSE
  point_1 = .FALSE.
END IF

IF (ALL(q1(1:dim_num) == q2(1:dim_num))) THEN
  point_2 = .TRUE.
ELSE
  point_2 = .FALSE.
END IF
!
!  Convert the lines to ABC format.
!
IF (.NOT. point_1) THEN
  CALL line_exp2imp_2d(p1, p2, a1, b1, c1)
END IF

IF (.NOT. point_2) THEN
  CALL line_exp2imp_2d(q1, q2, a2, b2, c2)
END IF
!
!  Search for intersection of the lines.
!
IF (point_1 .AND. point_2) THEN
  IF (ALL(p1(1:dim_num) == q1(1:dim_num))) THEN
    ival = 1
    p(1:dim_num) = p1(1:dim_num)
  END IF
ELSE IF (point_1) THEN
  IF (a2 * p1(1) + b2 * p1(2) == c2) THEN
    ival = 1
    p(1:dim_num) = p1(1:dim_num)
  END IF
ELSE IF (point_2) THEN
  IF (a1 * q1(1) + b1 * q1(2) == c1) THEN
    ival = 1
    p(1:dim_num) = q1(1:dim_num)
  END IF
ELSE
  CALL lines_imp_int_2d(a1, b1, c1, a2, b2, c2, ival, p)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE segment_point_dist_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) bot
REAL(kind=8) pn(dim_num)
REAL(kind=8) t
!
!  If the line segment is actually a point, then the answer is easy.
!
IF (ALL(p1(1:dim_num) == p2(1:dim_num))) THEN
  t = 0.0D+00
ELSE
  bot = SUM((p2(1:dim_num) - p1(1:dim_num))**2)
  t = SUM((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = MAX(t, 0.0D+00)
  t = MIN(t, 1.0D+00)
END IF
!
pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = SQRT(SUM((p(1:dim_num) - pn(1:dim_num))**2))
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE segment_point_dist_3d
INTEGER(i4b), PARAMETER :: dim_num = 3
REAL(dfp) bot
REAL(dfp) pn(dim_num)
REAL(dfp) t
!
!  If the line segment is actually a point, then the answer is easy.
!
IF (ALL(p1(1:dim_num) == p2(1:dim_num))) THEN
  t = 0.0D+00
ELSE
  bot = SUM((p2(1:dim_num) - p1(1:dim_num))**2)
  t = SUM((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = MAX(t, 0.0D+00)
  t = MIN(t, 1.0D+00)
END IF

pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = SQRT(SUM((p(1:dim_num) - pn(1:dim_num))**2))
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE line_exp_point_dist_signed_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) a
REAL(kind=8) b
REAL(kind=8) c
!
!  If the explicit line degenerates to a point, the computation is easy.
!
IF (line_exp_is_degenerate_nd(dim_num, p1, p2)) THEN
  dist_signed = SQRT(SUM((p1(1:dim_num) - p(1:dim_num))**2))
!
!  Convert the explicit line to the implicit form A * P(1) + B * P(2) + C = 0.
!  This makes the computation of the signed distance to (X,Y) easy.
!
ELSE
  a = p2(2) - p1(2)
  b = p1(1) - p2(1)
  c = p2(1) * p1(2) - p1(1) * p2(2)
  dist_signed = (a * p(1) + b * p(2) + c) / SQRT(a * a + b * b)
END IF
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE segment_point_near_2d
INTEGER(kind=4), PARAMETER :: dim_num = 2
REAL(kind=8) bot
!
!  If the line segment is actually a point, then the answer is easy.
!
IF (ALL(p1(1:dim_num) == p2(1:dim_num))) THEN
  t = 0.0D+00
ELSE
  bot = SUM((p2(1:dim_num) - p1(1:dim_num))**2)
  t = SUM((p(1:dim_num) - p1(1:dim_num)) &
          * (p2(1:dim_num) - p1(1:dim_num))) / bot
  t = MAX(t, 0.0D+00)
  t = MIN(t, 1.0D+00)
END IF
!
pn(1:dim_num) = p1(1:dim_num) + t * (p2(1:dim_num) - p1(1:dim_num))
dist = SQRT(SUM((p(1:dim_num) - pn(1:dim_num))**2))
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: r8mat solve
!
!# Introduction
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) RHS_NUM, the number of right hand sides.
!    RHS_NUM must be at least 0.
!
!    Input/output, real ( kind = 8 ) A(N,N+rhs_num), contains in rows and
!    columns 1 to N the coefficient matrix, and in columns N+1 through
!    N+rhs_num, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.

PURE SUBROUTINE r8mat_solve(n, rhs_num, a, info)
  INTEGER(I4B), INTENT(IN) :: n
  INTEGER(I4B), INTENT(IN) :: rhs_num
  REAL(DFP), INTENT(INOUT) :: a(n, n + rhs_num)
  INTEGER(I4B), INTENT(OUT) :: info
  !!
  REAL(DFP) :: apivot
  REAL(DFP) :: factor
  INTEGER(I4B) :: i
  INTEGER(I4B) :: ipivot
  INTEGER(I4B) :: j
  !!
  info = 0
  !!
  DO j = 1, n
    !
    !  Choose a pivot row.
    !
    ipivot = j
    apivot = a(j, j)
    !
    DO i = j + 1, n
      IF (ABS(apivot) < ABS(a(i, j))) THEN
        apivot = a(i, j)
        ipivot = i
      END IF
    END DO
    !
    IF (apivot == 0.0D+00) THEN
      info = j
      RETURN
    END IF
    !
    !  Interchange.
    !
    DO i = 1, n + rhs_num
      CALL swap(a(ipivot, i), a(j, i))
    END DO
    !
    !  A(J,J) becomes 1.
    !
    a(j, j) = 1.0D+00
    a(j, j + 1:n + rhs_num) = a(j, j + 1:n + rhs_num) / apivot
    !
    !  A(I,J) becomes 0.
    !
    DO i = 1, n
      IF (i /= j) THEN
        factor = a(i, j)
        a(i, j) = 0.0D+00
        a(i,j+1:n+rhs_num) = a(i,j+1:n+rhs_num) - factor * a(j,j+1:n+rhs_num)
      END IF
    END DO
  END DO
END SUBROUTINE r8mat_solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION r8vec_normsq_affine(n, v0, v1) RESULT(ans)
  INTEGER(i4b), INTENT(in) :: n
  REAL(dfp), INTENT(in) :: v0(n)
  REAL(dfp), INTENT(in) :: v1(n)
  REAL(dfp) :: ans
  ans = SUM((v0(1:n) - v1(1:n))**2)
END FUNCTION r8vec_normsq_affine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION i4_wrap(ival, ilo, ihi) RESULT(ans)
  INTEGER(i4b), INTENT(in) :: ival
  INTEGER(i4b), INTENT(in) :: ilo
  INTEGER(i4b), INTENT(in) :: ihi
  INTEGER(i4b) :: ans
  !!
  INTEGER(i4b) :: jhi
  INTEGER(i4b) :: jlo
  INTEGER(i4b) :: wide
  !!
  jlo = MIN(ilo, ihi)
  jhi = MAX(ilo, ihi)
  !!
  wide = jhi - jlo + 1
  !!
  IF (wide == 1) THEN
    ans = jlo
  ELSE
    ans = jlo + i4_modp(ival - jlo, wide)
  END IF
  !!
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION i4_modp(i, j) RESULT(ans)
  INTEGER(i4b), INTENT(IN) :: i
  INTEGER(i4b), INTENT(IN) :: j
  INTEGER(i4b) :: ans
  IF (j == 0) THEN
    RETURN
  END IF
  ans = MOD(i, j)
  IF (ans < 0) THEN
    ans = ans + ABS(j)
  END IF
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION i4vec_lcm(n, v)
  INTEGER(i4b), INTENT(in) :: n
  INTEGER(i4b), INTENT(in) :: v(n)
  INTEGER(i4b) :: i4vec_lcm
  INTEGER(i4b) :: i
  INTEGER(i4b) :: lcm
  !
  lcm = 1
  DO i = 1, n
    IF (v(i) == 0) THEN
      lcm = 0
      i4vec_lcm = lcm
      RETURN
    END IF
    lcm = i4_lcm(lcm, v(i))
  END DO
  i4vec_lcm = lcm
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION i4_lcm(i, j)
  INTEGER(i4b), INTENT(in) :: i, j
  INTEGER(I4B) :: i4_lcm
  i4_lcm = ABS(i * (j / i4_gcd(i, j)))
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION i4_gcd(i, j)
  INTEGER(I4B), INTENT(IN) :: i, j
  INTEGER(I4B) :: i4_gcd
  !!
  INTEGER(kind=4) p
  INTEGER(kind=4) q
  INTEGER(kind=4) r
  !
  i4_gcd = 1
  !
  !  Return immediately if either I or J is zero.
  !
  IF (i == 0) THEN
    i4_gcd = MAX(1, ABS(j))
    RETURN
  ELSE IF (j == 0) THEN
    i4_gcd = MAX(1, ABS(i))
    RETURN
  END IF
  !
  !  Set P to the larger of I and J, Q to the smaller.
  !  This way, we can alter P and Q as we go.
  !
  p = MAX(ABS(i), ABS(j))
  q = MIN(ABS(i), ABS(j))
  !
  !  Carry out the Euclidean algorithm.
  !
  DO
    r = MOD(p, q)
    IF (r == 0) THEN
      EXIT
    END IF
    p = q
    q = r
  END DO
  i4_gcd = q
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE FUNCTION r8_huge()
  REAL(dfp) :: r8_huge
  r8_huge = 1.0D+30
END FUNCTION

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
