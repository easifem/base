subroutine p_polynomial_prime2(m, n, x, vpp)

!*****************************************************************************80
!
!! P_POLYNOMIAL_PRIME2: second derivative of Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(0,X) = 1
!    P(1,X) = X
!    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
!
!    P'(0,X) = 0
!    P'(1,X) = 1
!    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
!
!    P"(0,X) = 0
!    P"(1,X) = 0
!    P"(N,X) = ( (2*N-1)*(2*P(N-1,X)+X*P"(N-1,X)-(N-1)*P"(N-2,X) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) VPP(M,0:N), the second derivative of the
!    Legendre polynomials of order 0 through N.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) m
  integer(kind=4) n

  integer(kind=4) i
  real(kind=rk) v(m, 0:n)
  real(kind=rk) vp(m, 0:n)
  real(kind=rk) vpp(m, 0:n)
  real(kind=rk) x(m)

  if (n < 0) then
    return
  end if

  v(1:m, 0) = 1.0D+00
  vp(1:m, 0) = 0.0D+00
  vpp(1:m, 0) = 0.0D+00

  if (n < 1) then
    return
  end if

  v(1:m, 1) = x(1:m)
  vp(1:m, 1) = 1.0D+00
  vpp(1:m, 1) = 0.0D+00

  do i = 2, n

    v(1:m, i) = &
      (real(2 * i - 1, kind=rk) * x(1:m) * v(1:m, i - 1) &
       - real(i - 1, kind=rk) * v(1:m, i - 2)) &
      / real(i, kind=rk)

    vp(1:m, i) = &
      (real(2 * i - 1, kind=rk) * (v(1:m, i - 1) + x(1:m) * vp(1:m, i - 1)) &
       - real(i - 1, kind=rk) * vp(1:m, i - 2)) &
      / real(i, kind=rk)

    vpp(1:m, i) = &
      (real(2 * i - 1, kind=rk) * (2.0D+00 * vp(1:m, i - 1) &
                                   + x(1:m) * vpp(1:m, i - 1)) &
       - real(i - 1, kind=rk) * vpp(1:m, i - 2)) &
      / real(i, kind=rk)

  end do

  return
end
subroutine p_polynomial_value(m, n, x, v)

!*****************************************************************************80
!
!! P_POLYNOMIAL_VALUE evaluates the Legendre polynomials P(n,x).
!
!  Discussion:
!
!    P(n,1) = 1.
!    P(n,-1) = (-1)^N.
!    | P(n,x) | <= 1 in [-1,1].
!
!    The N zeroes of P(n,x) are the abscissas used for Gauss-Legendre
!    quadrature of the integral of a function F(X) with weight function 1
!    over the interval [-1,1].
!
!    The Legendre polynomials are orthogonal under the inner product defined
!    as integration from -1 to 1:
!
!      Integral ( -1 <= X <= 1 ) P(I,X) * P(J,X) dX
!        = 0 if I =/= J
!        = 2 / ( 2*I+1 ) if I = J.
!
!    Except for P(0,X), the integral of P(I,X) from -1 to 1 is 0.
!
!    A function F(X) defined on [-1,1] may be approximated by the series
!      C0*P(0,x) + C1*P(1,x) + ... + CN*P(n,x)
!    where
!      C(I) = (2*I+1)/(2) * Integral ( -1 <= X <= 1 ) F(X) P(I,x) dx.
!
!    The formula is:
!
!      P(n,x) = (1/2^N) * sum ( 0 <= M <= N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
!
!  Differential equation:
!
!    (1-X*X) * P(n,x)'' - 2 * X * P(n,x)' + N * (N+1) = 0
!
!  First terms:
!
!    P( 0,x) =      1
!    P( 1,x) =      1 X
!    P( 2,x) = (    3 X^2 -       1)/2
!    P( 3,x) = (    5 X^3 -     3 X)/2
!    P( 4,x) = (   35 X^4 -    30 X^2 +     3)/8
!    P( 5,x) = (   63 X^5 -    70 X^3 +    15 X)/8
!    P( 6,x) = (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
!    P( 7,x) = (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
!    P( 8,x) = ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
!    P( 9,x) = (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
!    P(10,x) = (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2-63)/256
!
!  Recursion:
!
!    P(0,x) = 1
!    P(1,x) = x
!    P(n,x) = ( (2*n-1)*x*P(n-1,x)-(n-1)*P(n-2,x) ) / n
!
!    P'(0,x) = 0
!    P'(1,x) = 1
!    P'(N,x) = ( (2*N-1)*(P(N-1,x)+X*P'(N-1,x)-(N-1)*P'(N-2,x) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) V(M,0:N), the values of the Legendre polynomials
!    of order 0 through N at the points X.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) m
  integer(kind=4) n

  integer(kind=4) i
  real(kind=rk) v(m, 0:n)
  real(kind=rk) x(m)

  if (n < 0) then
    return
  end if

  v(1:m, 0) = 1.0D+00

  if (n < 1) then
    return
  end if

  v(1:m, 1) = x(1:m)

  do i = 2, n

    v(1:m, i) = (real(2 * i - 1, kind=rk) * x(1:m) * v(1:m, i - 1) &
                 - real(i - 1, kind=rk) * v(1:m, i - 2)) &
                / real(i, kind=rk)

  end do

  return
end

subroutine p_polynomial_zeros(nt, t)

!*****************************************************************************80
!
!! P_POLYNOMIAL_ZEROS: zeros of Legendre function P(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the order of the rule.
!
!    Output, real ( kind = rk ) T(NT), the zeros.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) nt

  real(kind=rk) bj(nt)
  integer(kind=4) i
  real(kind=rk) t(nt)
  real(kind=rk) wts(nt)

  t(1:nt) = 0.0D+00

  do i = 1, nt
    bj(i) = real(i * i, kind=rk) / real(4 * i * i - 1, kind=rk)
  end do
  bj(1:nt) = sqrt(bj(1:nt))

  wts(1:nt) = 0.0D+00
  wts(1) = sqrt(2.0D+00)

  call imtqlx(nt, t, bj, wts)

  return
end
subroutine p_power_product(p, e, table)

!*****************************************************************************80
!
!! P_POWER_PRODUCT: power products for Legendre polynomial P(n,x).
!
!  Discussion:
!
!    Let P(n,x) represent the Legendre polynomial of degree n.
!
!    For polynomial chaos applications, it is of interest to know the
!    value of the integrals of products of X with every possible pair
!    of basis functions.  That is, we'd like to form
!
!      Tij = Integral ( -1.0 <= X <= +1.0 ) X^E * P(i,x) * P(j,x) dx
!
!    We will estimate these integrals using Gauss-Legendre quadrature.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the maximum degree of the polyonomial
!    factors.  0 <= P.
!
!    Input, integer ( kind = 4 ) E, the exponent of X in the integrand.
!    0 <= E.
!
!    Output, real ( kind = rk ) TABLE(0:P,0:P), the table of integrals.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) p

  integer(kind=4) e
  real(kind=rk) h_table(0:p)
  integer(kind=4) i
  integer(kind=4) j
  integer(kind=4) k
  integer(kind=4) order
  real(kind=rk) table(0:p, 0:p)
  real(kind=rk), allocatable :: w_table(:)
  real(kind=rk) x(1)
  real(kind=rk), allocatable :: x_table(:)

  table(0:p, 0:p) = 0.0D+00

  order = p + 1 + ((e + 1) / 2)

  allocate (x_table(order))
  allocate (w_table(order))

  call p_quadrature_rule(order, x_table, w_table)

  do k = 1, order

    x(1) = x_table(k)
    call p_polynomial_value(1, p, x, h_table)
!
!  The following formula is an outer product in H_TABLE.
!
    if (e == 0) then
      do i = 0, p
        do j = 0, p
          table(i, j) = table(i, j) + w_table(k) * h_table(i) * h_table(j)
        end do
      end do
    else
      do i = 0, p
        do j = 0, p
          table(i, j) = table(i, j) &
                        + w_table(k) * x(1)**e * h_table(i) * h_table(j)
        end do
      end do
    end if

  end do

  deallocate (w_table)
  deallocate (x_table)

  return
end
subroutine p_quadrature_rule(nt, t, wts)

!*****************************************************************************80
!
!! P_QUADRATURE_RULE: quadrature for Legendre function P(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the order of the rule.
!
!    Output, real ( kind = rk ) T(NT), WTS(NT), the points and weights
!    of the rule.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) nt

  real(kind=rk) bj(nt)
  integer(kind=4) i
  real(kind=rk) t(nt)
  real(kind=rk) wts(nt)

  t(1:nt) = 0.0D+00

  do i = 1, nt
    bj(i) = real(i * i, kind=rk) / real(4 * i * i - 1, kind=rk)
  end do
  bj(1:nt) = sqrt(bj(1:nt))

  wts(1) = sqrt(2.0D+00)
  wts(2:nt) = 0.0D+00

  call imtqlx(nt, t, bj, wts)

  wts(1:nt) = wts(1:nt)**2

  return
end
subroutine pm_polynomial_value(mm, n, m, x, cx)

!*****************************************************************************80
!
!! PM_POLYNOMIAL_VALUE evaluates the Legendre polynomials Pm(n,m,x).
!
!  Differential equation:
!
!    (1-X*X) * Y'' - 2 * X * Y + ( N (N+1) - (M*M/(1-X*X)) * Y = 0
!
!  First terms:
!
!    M = 0  ( = Legendre polynomials of first kind P(N,X) )
!
!    Pm(0,0,x) =    1
!    Pm(1,0,x) =    1 X
!    Pm(2,0,x) = (  3 X^2 -   1)/2
!    Pm(3,0,x) = (  5 X^3 -   3 X)/2
!    Pm(4,0,x) = ( 35 X^4 -  30 X^2 +   3)/8
!    Pm(5,0,x) = ( 63 X^5 -  70 X^3 +  15 X)/8
!    Pm(6,0,x) = (231 X^6 - 315 X^4 + 105 X^2 -  5)/16
!    Pm(7,0,x) = (429 X^7 - 693 X^5 + 315 X^3 - 35 X)/16
!
!    M = 1
!
!    Pm(0,1,x) =   0
!    Pm(1,1,x) =   1 * SQRT(1-X^2)
!    Pm(2,1,x) =   3 * SQRT(1-X^2) * X
!    Pm(3,1,x) = 1.5 * SQRT(1-X^2) * (5*X^2-1)
!    Pm(4,1,x) = 2.5 * SQRT(1-X^2) * (7*X^3-3*X)
!
!    M = 2
!
!    Pm(0,2,x) =   0
!    Pm(1,2,x) =   0
!    Pm(2,2,x) =   3 * (1-X^2)
!    Pm(3,2,x) =  15 * (1-X^2) * X
!    Pm(4,2,x) = 7.5 * (1-X^2) * (7*X^2-1)
!
!    M = 3
!
!    Pm(0,3,x) =   0
!    Pm(1,3,x) =   0
!    Pm(2,3,x) =   0
!    Pm(3,3,x) =  15 * (1-X^2)^1.5
!    Pm(4,3,x) = 105 * (1-X^2)^1.5 * X
!
!    M = 4
!
!    Pm(0,4,x) =   0
!    Pm(1,4,x) =   0
!    Pm(2,4,x) =   0
!    Pm(3,4,x) =   0
!    Pm(4,4,x) = 105 * (1-X^2)^2
!
!  Recursion:
!
!    if N < M:
!      Pm(N,M,x) = 0
!    if N = M:
!      Pm(N,M,x) = (2*M-1)!! * (1-X*X)^(M/2) where N!! means the product of
!      all the odd integers less than or equal to N.
!    if N = M+1:
!      Pm(N,M,x) = X*(2*M+1)*Pm(M,M,x)
!    if M+1 < N:
!      Pm(N,M,x) = ( X*(2*N-1)*Pm(N-1,M,x) - (N+M-1)*Pm(N-2,M,x) )/(N-M)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MM, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer ( kind = 4 ) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real ( kind = rk ) X(MM), the point at which the function is to be
!    evaluated.
!
!    Output, real ( kind = rk ) CX(MM,0:N), the function values.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) mm
  integer(kind=4) n

  real(kind=rk) cx(mm, 0:n)
  real(kind=rk) fact
  integer(kind=4) j
  integer(kind=4) m
  real(kind=rk) x(mm)

  cx(1:mm, 0:n) = 0.0D+00
!
!  J = M is the first nonzero function.
!
  if (m <= n) then
    cx(1:mm, m) = 1.0D+00

    fact = 1.0D+00
    do j = 1, m
      cx(1:mm, m) = -cx(1:mm, m) * fact * sqrt(1.0D+00 - x(1:mm)**2)
      fact = fact + 2.0D+00
    end do

  end if
!
!  J = M + 1 is the second nonzero function.
!
  if (m + 1 <= n) then
    cx(1:mm, m + 1) = x(1:mm) * real(2 * m + 1, kind=rk) * cx(1:mm, m)
  end if
!
!  Now we use a three term recurrence.
!
  do j = m + 2, n
    cx(1:mm, j) = (real(2 * j - 1, kind=rk) * x(1:mm) * cx(1:mm, j - 1) &
                   + real(-j - m + 1, kind=rk) * cx(1:mm, j - 2)) &
                  / real(j - m, kind=rk)
  end do

  return
end

subroutine pmn_polynomial_value(mm, n, m, x, cx)

!*****************************************************************************80
!
!! PMN_POLYNOMIAL_VALUE: normalized Legendre polynomial Pmn(n,m,x).
!
!  Discussion:
!
!    The unnormalized associated Legendre functions P_N^M(X) have
!    the property that
!
!      Integral ( -1 <= X <= 1 ) ( P_N^M(X) )^2 dX
!      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
!
!    By dividing the function by the square root of this term,
!    the normalized associated Legendre functions have norm 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MM, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer ( kind = 4 ) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real ( kind = rk ) X(MM), the evaluation points.
!
!    Output, real ( kind = rk ) CX(MM,0:N), the function values.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) mm
  integer(kind=4) n

  real(kind=rk) cx(mm, 0:n)
  real(kind=rk) factor
  integer(kind=4) j
  integer(kind=4) m
  real(kind=rk) r8_factorial
  real(kind=rk) x(mm)

  if (m < 0) then
    write (*, '(a)') ' '
    write (*, '(a)') 'PMN_POLYNOMIAL_VALUE - Fatal error!'
    write (*, '(a,i8)') '  Input value of M is ', m
    write (*, '(a)') '  but M must be nonnegative.'
    stop 1
  end if

  if (n < m) then
    write (*, '(a)') ' '
    write (*, '(a)') 'PMN_POLYNOMIAL_VALUE - Fatal error!'
    write (*, '(a,i8)') '  Input value of M = ', m
    write (*, '(a,i8)') '  Input value of N = ', n
    write (*, '(a)') '  but M must be less than or equal to N.'
    stop 1
  end if

  cx(1:mm, 0:n) = 0.0D+00

  if (m <= n) then
    cx(1:mm, m) = 1.0D+00
    factor = 1.0D+00
    do j = 1, m
      cx(1:mm, m) = -cx(1:mm, m) * factor * sqrt(1.0D+00 - x(1:mm)**2)
      factor = factor + 2.0D+00
    end do
  end if

  if (m + 1 <= n) then
    cx(1:mm, m + 1) = x(1:mm) * real(2 * m + 1, kind=rk) * cx(1:mm, m)
  end if

  do j = m + 2, n
    cx(1:mm, j) = (real(2 * j - 1, kind=rk) * x(1:mm) * cx(1:mm, j - 1) &
                   + real(-j - m + 1, kind=rk) * cx(1:mm, j - 2)) &
                  / real(j - m, kind=rk)
  end do
!
!  Normalization.
!
  do j = m, n
    factor = sqrt((real(2 * j + 1, kind=rk) * r8_factorial(j - m)) &
                  / (2.0D+00 * r8_factorial(j + m)))
    cx(1:mm, j) = cx(1:mm, j) * factor
  end do

  return
end

subroutine pmns_polynomial_value(mm, n, m, x, cx)

!*****************************************************************************80
!
!! PMNS_POLYNOMIAL_VALUE: sphere-normalized Legendre polynomial Pmns(n,m,x).
!
!  Discussion:
!
!    The unnormalized associated Legendre functions P_N^M(X) have
!    the property that
!
!      Integral ( -1 <= X <= 1 ) ( P_N^M(X) )^2 dX
!      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
!
!    By dividing the function by the square root of this term,
!    the normalized associated Legendre functions have norm 1.
!
!    However, we plan to use these functions to build spherical
!    harmonics, so we use a slightly different normalization factor of
!
!      sqrt ( ( ( 2 * N + 1 ) * ( N - M )! ) / ( 4 * pi * ( N + M )! ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MM, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer ( kind = 4 ) M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real ( kind = rk ) X(MM), the evaluation points.
!
!    Output, real ( kind = rk ) CX(MM,0:N), the function values.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) mm
  integer(kind=4) n

  real(kind=rk) cx(mm, 0:n)
  real(kind=rk) factor
  integer(kind=4) j
  integer(kind=4) m
  real(kind=rk) r8_factorial
  real(kind=rk), parameter :: r8_pi = 3.141592653589793D+00
  real(kind=rk) x(mm)

  cx(1:mm, 0:n) = 0.0D+00

  if (m <= n) then
    cx(1:mm, m) = 1.0D+00
    factor = 1.0D+00
    do j = 1, m
      cx(1:mm, m) = -cx(1:mm, m) * factor * sqrt(1.0D+00 - x(1:mm)**2)
      factor = factor + 2.0D+00
    end do
  end if

  if (m + 1 <= n) then
    cx(1:mm, m + 1) = x(1:mm) * real(2 * m + 1, kind=rk) * cx(1:mm, m)
  end if

  do j = m + 2, n
    cx(1:mm, j) = (real(2 * j - 1, kind=rk) * x(1:mm) * cx(1:mm, j - 1) &
                   + real(-j - m + 1, kind=rk) * cx(1:mm, j - 2)) &
                  / real(j - m, kind=rk)
  end do
!
!  Normalization.
!
  do j = m, n
    factor = sqrt((real(2 * j + 1, kind=rk) * r8_factorial(j - m)) &
                  / (4.0D+00 * r8_pi * r8_factorial(j + m)))
    cx(1:mm, j) = cx(1:mm, j) * factor
  end do

  return
end

subroutine pn_pair_product(p, table)

!*****************************************************************************80
!
!! PN_PAIR_PRODUCT: pair products for normalized Legendre polynomial Pn(n,x).
!
!  Discussion:
!
!    Let Pn(n,x) represent the normalized Legendre polynomial of degree n.
!
!    To check orthonormality, we compute
!
!      Tij = Integral ( -1.0 <= X <= +1.0 ) Pn(i,x) * Pn(j,x) dx
!
!    We will estimate these integrals using Gauss-Legendre quadrature.
!
!    The computed table should be the identity matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P, the maximum degree of the polyonomial
!    factors.  0 <= P.
!
!    Output, real ( kind = rk ) TABLE(0:P,0:P), the table of integrals.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) p

  real(kind=rk) h_table(0:p)
  integer(kind=4) i
  integer(kind=4) j
  integer(kind=4) k
  integer(kind=4) order
  real(kind=rk) table(0:p, 0:p)
  real(kind=rk), allocatable :: w_table(:)
  real(kind=rk) x(1)
  real(kind=rk), allocatable :: x_table(:)

  table(0:p, 0:p) = 0.0D+00

  order = p + 1

  allocate (x_table(order))
  allocate (w_table(order))

  call p_quadrature_rule(order, x_table, w_table)

  do k = 1, order

    x(1) = x_table(k)
    call pn_polynomial_value(1, p, x, h_table)

    do i = 0, p
      do j = 0, p
        table(i, j) = table(i, j) + w_table(k) * h_table(i) * h_table(j)
      end do
    end do

  end do

  deallocate (w_table)
  deallocate (x_table)

  return
end
subroutine pn_polynomial_coefficients(n, c)

!*****************************************************************************80
!
!! PN_POLYNOMIAL_COEFFICIENTS: coefficients of normalized Legendre Pn(n,x).
!
!  Discussion:
!
!    Pn(n,x) = P(n,x) * sqrt ( (2n+1)/2 )
!
!          1       x       x^2     x^3     x^4      x^5    x^6     x^7
!
!    0   0.707
!    1   0.000   1.224
!    2  -0.790   0.000   2.371
!    3   0.000  -2.806   0.000   4.677
!    4   0.795   0.000  -7.954   0.000   9.280
!    5   0.000   4.397   0.000 -20.520   0.000   18.468
!    6  -0.796   0.000  16.731   0.000 -50.193    0.000  36.808
!    7   0.000  -5.990   0.000  53.916   0.000 -118.616   0.000  73.429
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the
!    normalized Legendre polynomials of degree 0 through N.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) n

  real(kind=rk) c(0:n, 0:n)
  integer(kind=4) i
  real(kind=rk) t

  if (n < 0) then
    return
  end if
!
!  Compute P(i,x) coefficients.
!
  c(0:n, 0:n) = 0.0D+00

  c(0, 0) = 1.0D+00

  if (0 < n) then
    c(1, 1) = 1.0D+00
  end if

  do i = 2, n
    c(i, 0:i - 2) = real(-i + 1, kind=rk) * c(i - 2, 0:i - 2) &
                    / real(i, kind=rk)
    c(i, 1:i) = c(i, 1:i) + real(i + i - 1, kind=rk) * c(i - 1, 0:i - 1) &
                / real(i, kind=rk)
  end do
!
!  Normalize them.
!
  do i = 0, n
    t = sqrt(real(2 * i + 1, kind=rk) / 2.0D+00)
    c(i, 0:i) = c(i, 0:i) * t
  end do

  return
end
subroutine pn_polynomial_value(m, n, x, v)

!*****************************************************************************80
!
!! PN_POLYNOMIAL_VALUE evaluates the normalized Legendre polynomials Pn(n,x).
!
!  Discussion:
!
!    The normalized Legendre polynomials are orthonormal under the inner product
!    defined as integration from -1 to 1:
!
!      Integral ( -1 <= x <= +1 ) Pn(i,x) * Pn(j,x) dx = delta(i,j)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) V(M,0:N), the values of the Legendre polynomials
!    of order 0 through N at the points X.
!
  implicit none

  integer, parameter :: rk = kind(1.0D+00)

  integer(kind=4) m
  integer(kind=4) n

  integer(kind=4) j
  real(kind=rk) norm
  real(kind=rk) v(m, 0:n)
  real(kind=rk) x(m)

  call p_polynomial_value(m, n, x, v)

  do j = 0, n
    norm = sqrt(2.0D+00 / real(2 * j + 1, kind=rk))
    v(1:m, j) = v(1:m, j) / norm
  end do

  return
end
