subroutine t_mass_matrix ( n, a )

!*****************************************************************************80
!
!! T_MASS_MATRIX computes the mass matrix for the Chebyshev T polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a(0:n,0:n)
  integer i
  real ( kind = rk ), allocatable :: phi(:,:)
  real ( kind = rk ), allocatable :: phiw(:,:)
  real ( kind = rk ), allocatable :: w(:)
  real ( kind = rk ), allocatable :: x(:)

  allocate ( x(0:n) )
  allocate ( w(0:n) )

  call t_quadrature_rule ( n + 1, x, w )

  allocate ( phi(0:n,0:n) )

  call t_polynomial ( n + 1, n, x, phi )

  allocate ( phiw(0:n,0:n) )

  do i = 0, n
    phiw(0:n,i) = w(i) * phi(i,0:n)
  end do

  a(0:n,0:n) = matmul ( phiw(0:n,0:n), phi(0:n,0:n) )

  deallocate ( phi )
  deallocate ( phiw )
  deallocate ( w )
  deallocate ( x )

  return
end
function t_moment ( e )

!*****************************************************************************80
!
!! T_MOMENT: integral ( -1 <= x <= +1 ) x^e / sqrt ( 1 - x^2 ) dx.
!
!  Discussion:
!
!    Set
!      x = cos ( theta ),
!      dx = - sin ( theta ) d theta = - sqrt ( 1 - x^2 ) d theta
!    to transform the integral to
!      integral ( 0 <= theta <= pi ) - ( cos ( theta ) )^e d theta
!    which becomes
!      0 if E is odd,
!      (1/2^e) * choose ( e, e/2 ) * pi if E is even.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer E, the exponent of X.
!    0 <= E.
!
!    Output, real ( kind = rk ) T_MOMENT, the value of the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer e
  real ( kind = rk ) r8_choose
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t_moment
  real ( kind = rk ) value

  if ( mod ( e, 2 ) == 1 ) then

    value = 0.0D+00

  else

    value = r8_choose ( e, e / 2 ) * r8_pi / 2.0D+00 ** e

  end if

  t_moment = value

  return
end
subroutine t_polynomial ( m, n, x, v )

!*****************************************************************************80
!
!! T_POLYNOMIAL evaluates Chebyshev polynomials T(n,x).
!
!  Discussion:
!
!    Chebyshev polynomials are useful as a basis for representing the
!    approximation of functions since they are well conditioned, in the sense
!    that in the interval [-1,1] they each have maximum absolute value 1.
!    Hence an error in the value of a coefficient of the approximation, of
!    size epsilon, is exactly reflected in an error of size epsilon between
!    the computed approximation and the theoretical approximation.
!
!    Typical usage is as follows, where we assume for the moment
!    that the interval of approximation is [-1,1].  The value
!    of N is chosen, the highest polynomial to be used in the
!    approximation.  Then the function to be approximated is
!    evaluated at the N+1 points XJ which are the zeroes of the N+1-th
!    Chebyshev polynomial.  Let these values be denoted by F(XJ).
!
!    The coefficients of the approximation are now defined by
!
!      C(I) = 2/(N+1) * sum ( 1 <= J <= N+1 ) F(XJ) T(I,XJ)
!
!    except that C(0) is given a value which is half that assigned
!    to it by the above formula,
!
!    and the representation is
!
!    F(X) approximated by sum ( 0 <= J <= N ) C(J) T(J,X)
!
!    Now note that, again because of the fact that the Chebyshev polynomials
!    have maximum absolute value 1, if the higher order terms of the
!    coefficients C are small, then we have the option of truncating
!    the approximation by dropping these terms, and we will have an
!    exact value for maximum perturbation to the approximation that
!    this will cause.
!
!    It should be noted that typically the error in approximation
!    is dominated by the first neglected basis function (some multiple of
!    T(N+1,X) in the example above).  If this term were the exact error,
!    then we would have found the minimax polynomial, the approximating
!    polynomial of smallest maximum deviation from the original function.
!    The minimax polynomial is hard to compute, and another important
!    feature of the Chebyshev approximation is that it tends to behave
!    like the minimax polynomial while being easy to compute.
!
!    To evaluate a sum like
!
!      sum ( 0 <= J <= N ) C(J) T(J,X),
!
!    Clenshaw's recurrence formula is recommended instead of computing the
!    polynomial values, forming the products and summing.
!
!    Assuming that the coefficients C(J) have been computed
!    for J = 0 to N, then the coefficients of the representation of the
!    indefinite integral of the function may be computed by
!
!      B(I) = ( C(I-1) - C(I+1))/2*(I-1) for I=1 to N+1,
!
!    with
!
!      C(N+1)=0
!      B(0) arbitrary.
!
!    Also, the coefficients of the representation of the derivative of the
!    function may be computed by:
!
!      D(I) = D(I+2)+2*I*C(I) for I=N-1, N-2, ..., 0,
!
!    with
!
!      D(N+1) = D(N)=0.
!
!    Some of the above may have to adjusted because of the irregularity of C(0).
!
!    The formula is:
!
!      T(N,X) = COS(N*ARCCOS(X))
!
!  Differential equation:
!
!    (1-X*X) Y'' - X Y' + N N Y = 0
!
!  First terms:
!
!    T(0,X) =  1
!    T(1,X) =  1 X
!    T(2,X) =  2 X^2 -   1
!    T(3,X) =  4 X^3 -   3 X
!    T(4,X) =  8 X^4 -   8 X^2 +  1
!    T(5,X) = 16 X^5 -  20 X^3 +  5 X
!    T(6,X) = 32 X^6 -  48 X^4 + 18 X^2 - 1
!    T(7,X) = 64 X^7 - 112 X^5 + 56 X^3 - 7 X
!
!  Inequality:
!
!    abs ( T(N,X) ) <= 1 for -1 <= X <= 1
!
!  Orthogonality:
!
!    For integration over [-1,1] with weight
!
!      W(X) = 1 / sqrt(1-X*X),
!
!    if we write the inner product of T(I,X) and T(J,X) as
!
!      < T(I,X), T(J,X) > = integral ( -1 <= X <= 1 ) W(X) T(I,X) T(J,X) dX
!
!    then the result is:
!
!      < T(I,X), T(J,X) > = 0    if I /= J
!      < T(I,X), T(J,X) > = PI/2 if I == J /= 0
!      < T(I,X), T(J,X) > = PI   if I == J == 0
!
!    A discrete orthogonality relation is also satisfied at each of
!    the N zeroes of T(N,X):  sum ( 1 <= K <= N ) T(I,X) * T(J,X)
!                              = 0 if I /= J
!                              = N/2 if I == J /= 0
!                              = N if I == J == 0
!
!  Recursion:
!
!    T(0,X) = 1,
!    T(1,X) = X,
!    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
!
!    T'(N,X) = N * ( -X * T(N,X) + T(N-1,X) ) / ( 1 - X^2 )
!
!  Special values:
!
!    T(N,1) = 1
!    T(N,-1) = (-1)^N
!    T(2N,0) = (-1)^N
!    T(2N+1,0) = 0
!    T(N,X) = (-1)^N * T(N,-X)
!
!  Zeroes:
!
!    M-th zero of T(N,X) is X = cos((2*M-1)*PI/(2*N)), M = 1 to N.
!
!  Extrema:
!
!    M-th extremum of T(N,X) is X = cos(PI*M/N), M = 0 to N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2001
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
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(1:M), the evaluation points.
!
!    Output, real ( kind = rk ) V(1:M,0:N), the values of the polynomials.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  integer j
  real ( kind = rk ) v(1:m,0:n)
  real ( kind = rk ) x(1:m)

  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0D+00

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = x(1:m)

  do j = 2, n
    v(1:m,j) = 2.0D+00 * x(1:m) * v(1:m,j-1) - v(1:m,j-2)
  end do

  return
end
subroutine t_polynomial_ab ( a, b, m, n, xab, v )

!*****************************************************************************80
!
!! T_POLYNOMIAL_AB: evaluates Chebyshev polynomials TAB(n,x) in [A,B].
!
!  Discussion:
!
!    TAB(n,x) = T(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB(M), the evaluation points.
!    It must be the case that A <= XAB(*) <= B.
!
!    Output, real ( kind = rk ) V(M,N+1), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) v(1:m,0:n)
  real ( kind = rk ) x(1:m)
  real ( kind = rk ) xab(1:m)

  x(1:m) = ( 2.0D+00 * xab(1:m) - a - b ) / ( b - a )

  call t_polynomial ( m, n, x, v )

  return
end
subroutine t_polynomial_coefficients ( n, c )

!*****************************************************************************80
!
!! T_POLYNOMIAL_COEFFICIENTS: coefficients of the Chebyshev polynomial T(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     1
!     2     -1     0      2
!     3      0    -3      0      4
!     4      1     0     -8      0       8
!     5      0     5      0    -20       0    16
!     6     -1     0     18      0     -48     0     32
!     7      0    -7      0     56       0  -112      0    64
!
!  Recursion:
!
!    T(0,X) = 1,
!    T(1,X) = X,
!    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 May 2003
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
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the Chebyshev T
!    polynomials.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) c(0:n,0:n)
  integer i

  if ( n < 0 ) then
    return
  end if

  c(0:n,0:n) = 0.0D+00

  c(0,0) = 1.0D+00

  if ( n == 0 ) then
    return
  end if

  c(1,1) = 1.0D+00

  do i = 2, n
    c(i,0)     =                        - c(i-2,0)
    c(i,1:i-2) = 2.0D+00 * c(i-1,0:i-3) - c(i-2,1:i-2)
    c(i,  i-1) = 2.0D+00 * c(i-1,  i-2)
    c(i,  i  ) = 2.0D+00 * c(i-1,  i-1)
  end do

  return
end
function t_polynomial_value ( n, x )

!*****************************************************************************80
!
!! T_POLYNOMIAL_VALUE: returns the single value T(n,x).
!
!  Discussion:
!
!    In cases where calling T_POLYNOMIAL is inconvenient, because it returns
!    a vector of values for multiple arguments X, this simpler interface
!    may be appropriate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Input, real ( kind = rk ) X, the argument of the polynomial.
!
!    Output, real ( kind = rk ) T_POLYNOMIAL_VALUE, the value of T(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n
  real ( kind = rk ) t_polynomial_value
  real ( kind = rk ) value
  real ( kind = rk ), allocatable :: vec(:)
  real ( kind = rk ) x
  real ( kind = rk ) x_vec(1)

  if ( n < 0 ) then

    value = 0.0D+00

  else

    m = 1
    allocate ( vec(0:n) )

    x_vec(1) = x
    call t_polynomial ( m, n, x_vec, vec )

    value = vec(n)
    deallocate ( vec )

  end if

  t_polynomial_value = value

  return
end
subroutine t_polynomial_zeros ( n, z )

!*****************************************************************************80
!
!! T_POLYNOMIAL_ZEROS returns zeroes of the Chebyshev polynomial T(n,x).
!
!  Discussion:
!
!    The I-th zero of T(N,X) is cos((2*I-1)*PI/(2*N)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) Z(N), the zeroes of T(N,X).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) angle
  integer i
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) z(n)

  do i = 1, n
    angle = real ( 2 * i - 1, kind = rk ) * r8_pi / real ( 2 * n, kind = rk );
    z(i) = cos ( angle );
  end do

  return
end
subroutine t_project_coefficients ( n, f, c )

!*****************************************************************************80
!
!! T_PROJECT_COEFFICIENTS: function projected onto Chebyshev polynomials T(n,x).
!
!  Discussion:
!
!    It is assumed that the interval of definition is -1 <= x <= +1.
!
!    Over this interval, f(x) will be well approximated by
!
!      f(x) approx sum ( 0 <= i <= n ) c(i) * T(i,x)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!
!    Input, external real ( kind = rk ) function F ( X ), evaluates the function.
!
!    Output, real ( kind = rk ) C(0:N), the projection coefficients of f(x) onto
!    T(0,x) through T(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) c(0:n)
  real ( kind = rk ) d(0:n)
  real ( kind = rk ), external :: f
  real ( kind = rk ) fac
  integer j
  integer k
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) total
  real ( kind = rk ) y

  do k = 0, n
    y = cos ( r8_pi * ( real ( k, kind = rk ) + 0.5D+00 ) &
      / real ( n + 1, kind = rk ) )
    d(k) = f ( y )
  end do

  fac = 2.0D+00 / real ( n + 1, kind = rk )

  do j = 0, n
    total = 0.0D+00
    do k = 0, n
      total = total + d(k) * cos ( ( r8_pi * real ( j, kind = rk ) ) &
        * ( ( real ( k, kind = rk ) + 0.5D+00 ) / real ( n + 1, kind = rk ) ) )
    end do
    c(j) = fac * total
  end do

  c(0) = c(0) / 2.0D+00

  return
end
subroutine t_project_coefficients_ab ( n, f, a, b, c )

!*****************************************************************************80
!
!! T_PROJECT_COEFFICIENTS_AB: function projected onto TAB(n,x) over [a,b].
!
!  Discussion:
!
!    TAB(n,x) = T(n,(2*x-a-b)/(b-a))
!
!    It is assumed that the interval of definition is a <= x <= b.
!
!    Over this interval, f(x) will be well approximated by
!
!      f(x) approx sum ( 0 <= i <= n ) c(i) * T(i,(2x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!
!    Input, external real ( kind = rk ) function F ( X ), evaluates the function.
!
!    Input, real ( kind = rk ) A, B, the interval of definition.
!
!    Output, real ( kind = rk ) C(0:N), the projection coefficients of f(x) onto
!    T(0,x) through T(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) c(0:n)
  real ( kind = rk ) d(0:n)
  real ( kind = rk ), external :: f
  real ( kind = rk ) fac
  integer j
  integer k
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t
  real ( kind = rk ) total
  real ( kind = rk ) y

  do k = 0, n

    t = cos ( r8_pi * ( real ( k, kind = rk ) - 0.5D+00 ) &
      / real ( n + 1, kind = rk ) )

    y = ( ( 1.0D+00 + t ) * b   &
        + ( 1.0D+00 - t ) * a ) &
        /   2.0D+00

    d(k) = f ( y )

  end do

  fac = 2.0D+00 / real ( n + 1, kind = rk )

  do j = 0, n
    total = 0.0D+00
    do k = 0, n
      total = total + d(k) * cos ( ( r8_pi * real ( j, kind = rk ) ) &
        * ( ( real ( k, kind = rk ) + 0.5D+00 ) / real ( n + 1, kind = rk ) ) )
    end do
    c(j) = fac * total
  end do

  c(0) = c(0) / 2.0D+00

  return
end
subroutine t_project_coefficients_data ( a, b, m, n, x, d, c )

!*****************************************************************************80
!
!! T_PROJECT_COEFFICIENTS_DATA: project data onto Chebyshev polynomials T(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer M, the number of data values.
!
!    Input, integer N, the desired order of the Chebyshev
!    expansion.
!
!    Input, real ( kind = rk ) X(M), the data abscissas.  These need not
!    be sorted.  It must be the case that A <= X() <= B.
!
!    Input, real ( kind = rk ) D(M), the data values.
!
!    Output, real ( kind = rk ) C(0:N), the approximate Chebshev coefficients.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) c(0:n)
  real ( kind = rk ) d(m)
  logical r8vec_in_ab
  real ( kind = rk ) v(m,0:n)
  real ( kind = rk ) x(m)

  if ( .not. r8vec_in_ab ( m, x, a, b ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) ' T_PROJECT_COEFFICIENTS_DATA- Fatal error!'
    write ( *, '(a)' ) '  Some X not in [A,B].'
    stop 1
  end if
!
!  Compute the M by N+1 Chebyshev Vandermonde matrix V.
!
  call t_polynomial_ab ( a, b, m, n, x, v )
!
!  Compute the least-squares solution C.
!
  call svd_solve ( m, n + 1, v, d, c )

  return
end
subroutine t_project_value ( m, n, x, c, v )

!*****************************************************************************80
!
!! T_PROJECT_VALUE evaluates an expansion in Chebyshev polynomials T(n,x).
!
!  Discussion:
!
!    The projection is assumed to be based on the interval [-1,+1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest order polynomial to compute.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Input, real ( kind = rk ) C(0:N), the expansion coefficients.
!
!    Output, real ( kind = rk ) V(M), the value of the Chebyshev function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) b0(m)
  real ( kind = rk ) b1(m)
  real ( kind = rk ) b2(m)
  real ( kind = rk ) c(0:n)
  integer j
  real ( kind = rk ) v(m)
  real ( kind = rk ) x(m)

  b1(1:m) = 0.0D+00
  b0(1:m) = 0.0D+00

  do j = n, 0, -1
    b2(1:m) = b1(1:m)
    b1(1:m) = b0(1:m)
    b0(1:m) = c(j) + 2.0D+00 * x(1:m) * b1(1:m) - b2(1:m)
  end do

  v(1:m) = 0.5D+00 * ( c(0) + b0(1:m) - b2(1:m) )

  return
end
subroutine t_project_value_ab ( m, n, x, c, a, b, v )

!*****************************************************************************80
!
!! T_PROJECT_VALUE_AB evaluates an expansion in Chebyshev polynomials TAB(n,x).
!
!  Discussion:
!
!    TAB(n,x) = T(n,(2*x-a-b)/(b-a))
!
!    The projection is assumed to be based on the interval [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest order polynomial to compute.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Input, real ( kind = rk ) C(0:N), the expansion coefficients.
!
!    Input, real ( kind = rk ) A, B, the interval of definition.
!
!    Output, real ( kind = rk ) V(M), the value of the Chebyshev function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) b0(m)
  real ( kind = rk ) b1(m)
  real ( kind = rk ) b2(m)
  real ( kind = rk ) c(0:n)
  integer j
  real ( kind = rk ) v(m)
  real ( kind = rk ) x(m)

  b1(1:m) = 0.0D+00
  b0(1:m) = 0.0D+00

  do j = n, 0, -1
    b2(1:m) = b1(1:m)
    b1(1:m) = b0(1:m)
    b0(1:m) = c(j) + 2.0D+00 / ( b - a ) * ( 2.0D+00 * x(1:m) - a - b ) &
      * b1(1:m) - b2(1:m)
  end do

  v(1:m) = 0.5D+00 * ( c(0) + b0(1:m) - b2(1:m) )

  return
end
subroutine t_quadrature_rule ( n, t, w )

!*****************************************************************************80
!
!! T_QUADRATURE_RULE: quadrature rule for T(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the rule.
!
!    Output, real ( kind = rk ) T(N), W(N), the points and weights of the rule.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) bj(n)
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t(n)
  real ( kind = rk ) w(n)

  t(1:n) = 0.0D+00

  bj(1) = sqrt ( 0.5D+00 )
  bj(2:n) = 0.5D+00

  w(1) = sqrt ( r8_pi )
  w(2:n) = 0.0D+00

  call imtqlx ( n, t, bj, w )

  w(1:n) = w(1:n) ** 2

  return
end
function tt_product ( i, j, x )

!*****************************************************************************80
!
!! TT_PRODUCT: evaluate T(i,x)*T(j,x)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the indices.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) TT_PRODUCT, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer imj
  integer ipj
  integer j
  real ( kind = rk ) t_polynomial_value
  real ( kind = rk ) timj
  real ( kind = rk ) tipj
  real ( kind = rk ) tt_product
  real ( kind = rk ) value
  real ( kind = rk ) x

  if ( i < 0 .or. j < 0 ) then
    value = 0.0D+00
  else
    ipj = i + j
    tipj = t_polynomial_value ( ipj, x )
    imj = abs ( i - j )
    timj = t_polynomial_value ( imj, x )
    value = 0.5D+00 * ( tipj + timj )
  end if

  tt_product = value

  return
end
function tt_product_integral ( i, j )

!*****************************************************************************80
!
!! TT_PRODUCT_INTEGRAL: integral (-1<=x<=1) T(i,x)*T(j,x)/sqrt(1-x^2) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the polynomial indices.
!    0 <= I, J.
!
!    Output, real ( kind = rk ) TT_PRODUCT_INTEGRAL, the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) tt_product_integral
  real ( kind = rk ) value

  if ( i < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TT_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= I is required.'
    stop 1
  end if

  if ( j < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TT_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= J is required.'
    stop 1
  end if

  if ( i /= j ) then
    value = 0.0D+00
  elseif ( i == 0 ) then
    value = r8_pi
  elseif ( 0 < i ) then
    value = r8_pi / 2.0D+00
  end if

  tt_product_integral = value

  return
end
function ttt_product_integral ( i, j, k )

!*****************************************************************************80
!
!! TTT_PRODUCT_INTEGRAL: int (-1<=x<=1) T(i,x)*T(j,x)*T(k,x)/sqrt(1-x^2) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Mason, David Handscomb,
!    Chebyshev Polynomials,
!    CRC Press, 2002,
!    ISBN: 0-8493-035509,
!    LC: QA404.5.M37.
!
!  Parameters:
!
!    Input, integer I, J, K, the polynomial indices.
!    0 <= I, J, K.
!
!    Output, real ( kind = rk ) TTT_PRODUCT_INTEGRAL, the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  integer k
  real ( kind = rk ) tt_product_integral
  real ( kind = rk ) ttt_product_integral
  real ( kind = rk ) value

  if ( i < 0 ) then
    value = 0.0D+00
  else if ( j < 0 ) then
    value = 0.0D+00
  else if ( k < 0 ) then
    value = 0.0D+00
  else
    value = 0.5D+00 * ( &
        tt_product_integral (       i + j,   k ) &
      + tt_product_integral ( abs ( i - j ), k ) )
  end if

  ttt_product_integral = value

  return
end
function tu_product ( i, j, x )

!*****************************************************************************80
!
!! TU_PRODUCT: evaluate T(i,x)*U(j,x)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the indices.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) TU_PRODUCT, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  real ( kind = rk ) tu_product
  real ( kind = rk ) u_polynomial_value
  real ( kind = rk ) uu_product
  real ( kind = rk ) value
  real ( kind = rk ) x

  if ( i < 0 ) then
    value = 0.0D+00
  else if ( j < 0 ) then
    value = 0.0D+00
  else if ( i == 0 ) then
    value = u_polynomial_value ( j, x )
  else
    value = 0.5D+00 * ( uu_product ( i, j, x ) - uu_product ( i - 2, j, x ) )
  end if

  tu_product = value

  return
end
subroutine u_mass_matrix ( n, a )

!*****************************************************************************80
!
!! U_MASS_MATRIX computes the mass matrix for the Chebyshev U polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a(0:n,0:n)
  integer i
  real ( kind = rk ), allocatable :: phi(:,:)
  real ( kind = rk ), allocatable :: phiw(:,:)
  real ( kind = rk ), allocatable :: w(:)
  real ( kind = rk ), allocatable :: x(:)

  allocate ( x(0:n) )
  allocate ( w(0:n) )

  call u_quadrature_rule ( n + 1, x, w )

  allocate ( phi(0:n,0:n) )

  call u_polynomial ( n + 1, n, x, phi )

  allocate ( phiw(0:n,0:n) )

  do i = 0, n
    phiw(0:n,i) = w(i) * phi(i,0:n)
  end do

  a(0:n,0:n) = matmul ( phiw(0:n,0:n), phi(0:n,0:n) )

  deallocate ( phi )
  deallocate ( phiw )
  deallocate ( w )
  deallocate ( x )

  return
end
function u_moment ( e )

!*****************************************************************************80
!
!! U_MOMENT: integral ( -1 <= x <= +1 ) x^e sqrt ( 1 - x^2 ) dx.
!
!  Discussion:
!
!     E    U_INTEGRAL
!    --    --------------
!     0         pi /    2
!     2         pi /    8
!     4         pi /   16
!     6     5 * pi /  128
!     8     7 * pi /  256
!    10    21 * pi / 1024
!    12    33 * pi / 2048
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer E, the exponent of X.
!    0 <= E.
!
!    Output, real ( kind = rk ) U_MOMENT, the value of the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) arg1
  real ( kind = rk ) arg2
  integer e
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) u_moment
  real ( kind = rk ) value

  if ( mod ( e, 2 ) == 1 ) then

    value = 0.0D+00

  else

    arg1 = 0.5D+00 * real ( 1 + e, kind = rk )
    arg2 = 2.0D+00 + 0.5D+00 * real ( e, kind = rk )
    value = 0.5D+00 * sqrt ( r8_pi ) * gamma ( arg1 ) / gamma ( arg2 )

  end if

  u_moment = value

  return
end
subroutine u_polynomial ( m, n, x, v )

!*****************************************************************************80
!
!! U_POLYNOMIAL evaluates Chebyshev polynomials U(n,x).
!
!  Discussion:
!
!    The formula is:
!
!      If |X| <= 1, then
!
!        U(N,X) = sin ( (N+1) * arccos(X) ) / sqrt ( 1 - X^2 )
!               = sin ( (N+1) * arccos(X) ) / sin ( arccos(X) )
!
!      else
!
!        U(N,X) = sinh ( (N+1) * arccosh(X) ) / sinh ( arccosh(X) )
!
!  Differential equation:
!
!    (1-X*X) Y'' - 3 X Y' + N (N+2) Y = 0
!
!  First terms:
!
!    U(0,X) =   1
!    U(1,X) =   2 X
!    U(2,X) =   4 X^2 -   1
!    U(3,X) =   8 X^3 -   4 X
!    U(4,X) =  16 X^4 -  12 X^2 +  1
!    U(5,X) =  32 X^5 -  32 X^3 +  6 X
!    U(6,X) =  64 X^6 -  80 X^4 + 24 X^2 - 1
!    U(7,X) = 128 X^7 - 192 X^5 + 80 X^3 - 8X
!
!  Orthogonality:
!
!    For integration over [-1,1] with weight
!
!      W(X) = sqrt(1-X*X),
!
!    we have
!
!      < U(I,X), U(J,X) > = integral ( -1 <= X <= 1 ) W(X) U(I,X) U(J,X) dX
!
!    then the result is:
!
!      < U(I,X), U(J,X) >  =  0    if I /= J
!      < U(I,X), U(J,X) >  =  PI/2 if I == J
!
!  Recursion:
!
!    U(0,X) = 1,
!    U(1,X) = 2 * X,
!    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
!
!  Special values:
!
!    U(N,1) = N + 1
!    U(2N,0) = (-1)^N
!    U(2N+1,0) = 0
!    U(N,X) = (-1)^N * U(N,-X)
!
!  Zeroes:
!
!    M-th zero of U(N,X) is X = cos( M*PI/(N+1)), M = 1 to N
!
!  Extrema:
!
!    M-th extremum of U(N,X) is X = cos( M*PI/N), M = 0 to N
!
!  Norm:
!
!    Integral ( -1 <= X <= 1 ) ( 1 - X^2 ) * U(N,X)^2 dX = PI/2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2012
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
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) V(M,0:N), the values of the N+1 Chebyshev
!    polynomials.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  integer i
  real ( kind = rk ) v(m,0:n)
  real ( kind = rk ) x(m)

  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0D+00

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = 2.0D+00 * x(1:m)

  do i = 2, n
    v(1:m,i) = 2.0D+00 * x(1:m) * v(1:m,i-1) - v(1:m,i-2)
  end do

  return
end
subroutine u_polynomial_01_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! U_POLYNOMIAL_01_VALUES: values of shifted Chebyshev polynomials U01(n,x).
!
!  Discussion:
!
!    The shifted Chebyshev polynomial U01(n,x) = U(n,2*x-1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 25

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.000000000000000D+00, &
     1.000000000000000D+00, &
     1.400000000000000D+00, &
     0.9600000000000000D+00, &
    -0.05600000000000000D+00, &
    -1.038400000000000D+00, &
    -1.397760000000000D+00, &
    -0.9184640000000000D+00, &
     0.1119104000000000D+00, &
     1.075138560000000D+00, &
     1.393283584000000D+00, &
     0.8754584576000000D+00, &
    -0.1676417433600000D+00, &
    -1.110156898304000D+00, &
    -8.000000000000000D+00, &
     1.511014400000000D+00, &
    -1.133260800000000D+00, &
    -0.1636352000000000D+00, &
     1.019801600000000D+00, &
     0.000000000000000D+00, &
    -1.019801600000000D+00, &
     0.1636352000000000D+00, &
     1.133260800000000D+00, &
    -1.511014400000000D+00, &
     8.000000000000000D+00 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12,  7,  7, &
     7,  7,  7, &
     7,  7,  7, &
     7,  7,  7 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.00D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine u_polynomial_ab ( a, b, m, n, xab, v )

!*****************************************************************************80
!
!! U_POLYNOMIAL_AB: evaluates Chebyshev polynomials UAB(n,x) in [A,B].
!
!  Discussion:
!
!    UAB(n,x) = U(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB(M), the evaluation points.
!    It must be the case that A <= XAB(*) <= B.
!
!    Output, real ( kind = rk ) V(M,N+1), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) v(1:m,0:n)
  real ( kind = rk ) x(1:m)
  real ( kind = rk ) xab(1:m)

  x(1:m) = ( 2.0D+00 * xab(1:m) - a - b ) / ( b - a )

  call u_polynomial ( m, n, x, v )

  return
end
function u_polynomial_ab_value ( a, b, n, xab )

!*****************************************************************************80
!
!! U_POLYNOMIAL_AB_VALUE: evaluates Chebyshev polynomials UAB(n,x) in [A,B].
!
!  Discussion:
!
!    UAB(n,x) = U(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB, the evaluation point.
!    It must be the case that A <= XAB <= B.
!
!    Output, real ( kind = rk ) U_POLYNOMIAL_AB_VALUE, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) u_polynomial_ab_value
  real ( kind = rk ) u_polynomial_value
  real ( kind = rk ) value
  real ( kind = rk ) x
  real ( kind = rk ) xab

  x = ( 2.0D+00 * xab - a - b ) / ( b - a )

  value = u_polynomial_value ( n, x )

  u_polynomial_ab_value = value

  return
end
subroutine u_polynomial_coefficients ( n, c )

!*****************************************************************************80
!
!! U_POLYNOMIAL_COEFFICIENTS: coefficients of Chebyshev polynomials U(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     2
!     2     -1     0      4
!     3      0    -4      0      8
!     4      1     0    -12      0      16
!     5      0     6      0    -32       0    32
!     6     -1     0     24      0     -80     0     64
!     7      0    -8      0     80       0  -192      0   128
!
!  Recursion:
!
!    U(0,X) = 1,
!    U(1,X) = 2*X,
!    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2003
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
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) c(0:n,0:n)
  integer i

  if ( n < 0 ) then
    return
  end if

  c(0:n,0:n) = 0.0D+00

  c(0,0) = 1.0D+00

  if ( n == 0 ) then
    return
  end if

  c(1,1) = 2.0D+00

  do i = 2, n
    c(i,0)     =                        - c(i-2,0)
    c(i,1:i-2) = 2.0D+00 * c(i-1,0:i-3) - c(i-2,1:i-2)
    c(i,  i-1) = 2.0D+00 * c(i-1,  i-2)
    c(i,  i  ) = 2.0D+00 * c(i-1,  i-1)
  end do

  return
end
subroutine u_polynomial_plot ( n_num, n_val, output_filename )

!*****************************************************************************80
!
!! U_POLYNOMIAL_PLOT plots Chebyshev polynomials U(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NUM, the number of polynomials to be plotted.
!
!    Input, integer N_VAL(N_NUM), the degrees of 1 or more
!    Chebyshev polynomials to be plotted together.
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the name into which the
!    graphics information is to be stored.  Note that the PNG format will
!    be used.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: m = 501
  integer n_num

  real ( kind = rk ) a
  real ( kind = rk ) b
  integer column
  character ( len = 255 ) command_filename
  integer command_unit
  character ( len = 255 ) data_filename
  integer data_unit
  integer i
  integer i4vec_max
  integer j
  integer n
  integer n_max
  integer n_val(n_num)
  character ( len = * ) output_filename
  real ( kind = rk ), allocatable :: v(:,:)
  real ( kind = rk ) x(m)

  a = -1.0D+00
  b = +1.0D+00

  call r8vec_linspace ( m, a, b, x )
!
!  Compute all the data.
!
  n_max = i4vec_max ( n_num, n_val )
  allocate ( v(m,0:n_max) )
  call u_polynomial ( m, n_max, x, v )
!
!  Create the data file.
!
  data_filename = 'u_polynomial_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, m
    write ( data_unit, '(2x,g14.6)', advance = 'no' ) x(i)
    do j = 1, n_num
      n = n_val(j)
      write ( data_unit, '(2x,g14.6)', advance = 'no' ) v(i,n)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Plot the selected data.
!
  command_filename = 'u_polynomial_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set nokey'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---U(n,x)--->"'
  write ( command_unit, '(a)' ) &
    'set title "Chebyshev Polynomials U(n,x)"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  do j = 1, n_num
    column = n_val(j) + 1
    if ( j == 1 ) then
      write ( command_unit, '(a)', advance = 'no' ) 'plot '
    else
      write ( command_unit, '(a)', advance = 'no' ) '     '
    end if
    write ( command_unit, '(a,i2,a)', advance = 'no' ) &
      '"' // trim ( data_filename ) // &
      '" using 1:', column, ' lw 3 linecolor rgb "red"'
    if ( j < n_num ) then
      write ( command_unit, '(a)' ) ', \'
    else
      write ( command_unit, '(a)' ) ''
    end if
  end do

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  deallocate ( v )

  return
end
function u_polynomial_value ( n, x )

!*****************************************************************************80
!
!! U_POLYNOMIAL_VALUE: returns the single value U(n,x).
!
!  Discussion:
!
!    In cases where calling U_POLYNOMIAL is inconvenient, because it returns
!    a vector of values for multiple arguments X, this simpler interface
!    may be appropriate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Input, real ( kind = rk ) X, the argument of the polynomial.
!
!    Output, real ( kind = rk ) U_POLYNOMIAL_VALUE, the value of U(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n
  real ( kind = rk ) u_polynomial_value
  real ( kind = rk ) value
  real ( kind = rk ), allocatable :: vec(:)
  real ( kind = rk ) x
  real ( kind = rk ) x_vec(1)

  if ( n < 0 ) then

    value = 0.0D+00

  else

    m = 1
    allocate ( vec(0:n) )

    x_vec(1) = x
    call u_polynomial ( m, n, x_vec, vec )

    value = vec(n)
    deallocate ( vec )

  end if

  u_polynomial_value = value

  return
end
subroutine u_polynomial_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! U_POLYNOMIAL_VALUES returns values of Chebyshev polynomials U(n,x).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      ChebyshevU[n,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 14

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.0000000000000000D+00, &
     0.1000000000000000D+01, &
     0.1600000000000000D+01, &
     0.1560000000000000D+01, &
     0.8960000000000000D+00, &
    -0.1264000000000000D+00, &
    -0.1098240000000000D+01, &
    -0.1630784000000000D+01, &
    -0.1511014400000000D+01, &
    -0.7868390400000000D+00, &
     0.2520719360000000D+00, &
     0.1190154137600000D+01, &
     0.1652174684160000D+01, &
     0.1453325357056000D+01 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine u_polynomial_zeros ( n, z )

!*****************************************************************************80
!
!! U_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials U(n,x).
!
!  Discussion:
!
!    The I-th zero of U(N,X) is cos((I-1)*PI/(N-1)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) Z(N), the zeroes of U(N,X).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) angle
  integer i
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) z(n)

  do i = 1, n
    angle = real ( i, kind = rk ) * r8_pi / real ( n + 1, kind = rk )
    z(i) = cos ( angle )
  end do

  return
end
subroutine u_quadrature_rule ( n, t, w )

!*****************************************************************************80
!
!! U_QUADRATURE_RULE: quadrature rule for U(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the rule.
!
!    Output, real ( kind = rk ) T(N), W(N), the points and weights of the rule.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) bj(n)
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t(n)
  real ( kind = rk ) w(n)

  t(1:n) = 0.0D+00

  bj(1:n) = 0.5D+00

  w(1) = sqrt ( r8_pi / 2.0D+00 )
  w(2:n) = 0.0D+00

  call imtqlx ( n, t, bj, w )

  w(1:n) = w(1:n) ** 2

  return
end
function uu_product ( i, j, x )

!*****************************************************************************80
!
!! UU_PRODUCT: evaluate U(i,x)*U(j,x)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the indices.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) UU_PRODUCT, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  integer k
  real ( kind = rk ) u_polynomial_value
  real ( kind = rk ) uu_product
  real ( kind = rk ) value
  real ( kind = rk ) x

  value = 0.0D+00
  do k = abs ( i - j ), i + j, 2
    value = value + u_polynomial_value ( k, x )
  end do

  uu_product = value

  return
end
function uu_product_integral ( i, j )

!*****************************************************************************80
!
!! UU_PRODUCT_INTEGRAL: integral (-1<=x<=1) U(i,x)*U(j,x)*sqrt(1-x^2) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the polynomial indices.
!    0 <= I, J.
!
!    Output, real ( kind = rk ) UU_PRODUCT_INTEGRAL, the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) uu_product_integral
  real ( kind = rk ) value

  if ( i < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UU_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= I is required.'
    stop 1
  end if

  if ( j < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UU_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= J is required.'
    stop 1
  end if

  if ( i /= j ) then
    value = 0.0D+00
  else
    value = r8_pi / 2.0D+00
  end if

  uu_product_integral = value

  return
end
subroutine v_mass_matrix ( n, a )

!*****************************************************************************80
!
!! V_MASS_MATRIX computes the mass matrix for the Chebyshev V polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a(0:n,0:n)
  integer i
  real ( kind = rk ), allocatable :: phi(:,:)
  real ( kind = rk ), allocatable :: phiw(:,:)
  real ( kind = rk ), allocatable :: w(:)
  real ( kind = rk ), allocatable :: x(:)

  allocate ( x(0:n) )
  allocate ( w(0:n) )

  call v_quadrature_rule ( n + 1, x, w )

  allocate ( phi(0:n,0:n) )

  call v_polynomial ( n + 1, n, x, phi )

  allocate ( phiw(0:n,0:n) )

  do i = 0, n
    phiw(0:n,i) = w(i) * phi(i,0:n)
  end do

  a(0:n,0:n) = matmul ( phiw(0:n,0:n), phi(0:n,0:n) )

  deallocate ( phi )
  deallocate ( phiw )
  deallocate ( w )
  deallocate ( x )

  return
end
function v_moment ( e )

!*****************************************************************************80
!
!! V_MOMENT: integral ( -1 <= x <= +1 ) x^e sqrt(1+x) / sqrt(1-x) dx.
!
!  Discussion:
!
!     E    V_MOMENT
!    --    --------------
!     0      pi
!     1      pi / 2
!     2      pi / 2
!     3    3 pi / 8
!     4    3 pi / 8
!     5    5 pi / 16
!     6    5 pi / 16
!     7   35 pi / 128
!     8   35 pi / 128
!     9   63 pi / 256
!    10   63 pi / 256
!    11  231 pi / 1024
!    12  231 pi / 1024
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer E, the exponent of X.
!    0 <= E.
!
!    Output, real ( kind = rk ) V_MOMENT, the value of the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) f1
  real ( kind = rk ) f2
  real ( kind = rk ) f3
  real ( kind = rk ) f4
  real ( kind = rk ) f5
  real ( kind = rk ) f6
  real ( kind = rk ) f7
  real ( kind = rk ) f8
  integer e
  real ( kind = rk ) r8_e
  real ( kind = rk ) r8_factorial
  real ( kind = rk ) r8_hyper_2f1
  real ( kind = rk ) r8_mop
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) v_moment
  real ( kind = rk ) value

  r8_e = real ( e, kind = rk )

  f1 = 1.0D+00 / gamma ( 1.5D+00 + r8_e )
  f2 = r8_mop ( e )
  f3 = r8_pi * gamma ( 1.5D+00 + r8_e )
  f4 = 2.0D+00 * r8_hyper_2f1 ( 0.5D+00, -r8_e, 1.0D+00, 2.0D+00 )
  f5 = ( -1.0D+00 + r8_mop ( e ) ) &
    * r8_hyper_2f1 ( 0.5D+00, -r8_e, 2.0D+00, 2.0D+00 )
  f6 = sqrt ( r8_pi ) * r8_factorial ( e )
  f7 = ( -1.0D+00 + r8_mop ( e ) ) &
    * r8_hyper_2f1 ( -0.5D+00, 1.0D+00 + r8_e, 1.5D+00 + r8_e, - 1.0D+00 )
  f8 = 2.0D+00 &
    * r8_hyper_2f1 ( 0.5D+00, 1.0D+00 + r8_e, 1.5D+00 + r8_e, -1.0D+00 )

  value = f1 * f2 * ( f3 * ( f4 + f5 ) - f6 * ( f7 + f8 ) )

  v_moment = value

  return
end
subroutine v_polynomial ( m, n, x, v )

!*****************************************************************************80
!
!! V_POLYNOMIAL evaluates Chebyshev polynomials V(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2012
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
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) V(M,0:N), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  integer i
  real ( kind = rk ) v(m,0:n)
  real ( kind = rk ) x(m)

  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0D+00

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = 2.0D+00 * x(1:m) - 1.0D+00

  do i = 2, n
    v(1:m,i) = 2.0D+00 * x(1:m) * v(1:m,i-1) - v(1:m,i-2)
  end do

  return
end
subroutine v_polynomial_01_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! V_POLYNOMIAL_01_VALUES: values of shifted Chebyshev polynomials V01(n,x).
!
!  Discussion:
!
!    The shifted Chebyshev polynomial V01(n,x) = V(n,2*x-1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 25

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.0000000000000000D+00, &
     1.0000000000000000D+00, &
     0.4000000000000000D+00, &
    -0.4400000000000000D+00, &
    -1.0160000000000000D+00, &
    -0.9824000000000000D+00, &
    -0.3593600000000000D+00, &
     0.4792960000000000D+00, &
     1.0303744000000000D+00, &
     0.9632281600000000D+00, &
     0.3181450240000000D+00, &
    -0.5178251264000000D+00, &
    -1.0431002009600000D+00, &
    -0.9425151549440000D+00, &
    -15.000000000000000D+00, &
     3.1417984000000000D+00, &
    -1.3912448000000000D+00, &
    -1.2177792000000000D+00, &
     1.1837056000000000D+00, &
     1.0000000000000000D+00, &
    -0.8558976000000000D+00, &
    -0.8905088000000000D+00, &
     0.8752768000000000D+00, &
     0.1197696000000000D+00, &
     1.0000000000000000D+00 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12,  7,  7, &
     7,  7,  7, &
     7,  7,  7, &
     7,  7,  7 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.00D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine v_polynomial_ab ( a, b, m, n, xab, v )

!*****************************************************************************80
!
!! V_POLYNOMIAL_AB: evaluates Chebyshev polynomials VAB(n,x) in [A,B].
!
!  Discussion:
!
!    VAB(n,x) = V(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB(M), the evaluation points.
!    It must be the case that A <= XAB(*) <= B.
!
!    Output, real ( kind = rk ) V(M,N+1), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) v(1:m,0:n)
  real ( kind = rk ) x(1:m)
  real ( kind = rk ) xab(1:m)

  x(1:m) = ( 2.0D+00 * xab(1:m) - a - b ) / ( b - a )

  call v_polynomial ( m, n, x, v )

  return
end
function v_polynomial_ab_value ( a, b, n, xab )

!*****************************************************************************80
!
!! V_POLYNOMIAL_AB_VALUE: evaluates Chebyshev polynomials VAB(n,x) in [A,B].
!
!  Discussion:
!
!    VAB(n,x) = V(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB, the evaluation point.
!    It must be the case that A <= XAB <= B.
!
!    Output, real ( kind = rk ) V_POLYNOMIAL_AB_VALUE, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) v_polynomial_ab_value
  real ( kind = rk ) v_polynomial_value
  real ( kind = rk ) value
  real ( kind = rk ) x
  real ( kind = rk ) xab

  x = ( 2.0D+00 * xab - a - b ) / ( b - a )

  value = v_polynomial_value ( n, x )

  v_polynomial_ab_value = value

  return
end
subroutine v_polynomial_coefficients ( n, c )

!*****************************************************************************80
!
!! V_POLYNOMIAL_COEFFICIENTS: coefficients of Chebyshev polynomials V(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1     -1     2
!     2     -1    -2      4
!     3      1    -4     -4      8
!     4      1    +4    -12     -8      16
!     5     -1     6    +12    -32     -16    32
!     6     -1    -6     24    +32     -80   -32     64
!     7     +1    -8    -24     80     +80  -192    -64   128
!
!  Recursion:
!
!    V(0,X) = 1,
!    V(1,X) = 2 * X - 1,
!    V(N,X) = 2 * X * V(N-1,X) - V(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
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
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) c(0:n,0:n)
  integer i

  if ( n < 0 ) then
    return
  end if

  c(0:n,0:n) = 0.0D+00

  c(0,0) = 1.0D+00

  if ( n == 0 ) then
    return
  end if

  c(1,0) = -1.0D+00
  c(1,1) =  2.0D+00

  do i = 2, n
    c(i,0)     =                        - c(i-2,0)
    c(i,1:i-2) = 2.0D+00 * c(i-1,0:i-3) - c(i-2,1:i-2)
    c(i,  i-1) = 2.0D+00 * c(i-1,  i-2)
    c(i,  i  ) = 2.0D+00 * c(i-1,  i-1)
  end do

  return
end
subroutine v_polynomial_plot ( n_num, n_val, output_filename )

!*****************************************************************************80
!
!! V_POLYNOMIAL_PLOT plots Chebyshev polynomials V(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NUM, the number of polynomials to be plotted.
!
!    Input, integer N_VAL(N_NUM), the degrees of 1 or more
!    Chebyshev polynomials to be plotted together.
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the name into which the
!    graphics information is to be stored.  Note that the PNG format will
!    be used.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: m = 501
  integer n_num

  real ( kind = rk ) a
  real ( kind = rk ) b
  integer column
  character ( len = 255 ) command_filename
  integer command_unit
  character ( len = 255 ) data_filename
  integer data_unit
  integer i
  integer i4vec_max
  integer j
  integer n
  integer n_max
  integer n_val(n_num)
  character ( len = * ) output_filename
  real ( kind = rk ), allocatable :: v(:,:)
  real ( kind = rk ) x(m)

  a = -1.0D+00
  b = +1.0D+00

  call r8vec_linspace ( m, a, b, x )
!
!  Compute all the data.
!
  n_max = i4vec_max ( n_num, n_val )
  allocate ( v(m,0:n_max) )
  call v_polynomial ( m, n_max, x, v )
!
!  Create the data file.
!
  data_filename = 'v_polynomial_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, m
    write ( data_unit, '(2x,g14.6)', advance = 'no' ) x(i)
    do j = 1, n_num
      n = n_val(j)
      write ( data_unit, '(2x,g14.6)', advance = 'no' ) v(i,n)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Plot the selected data.
!
  command_filename = 'v_polynomial_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set nokey'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---T(n,x)--->"'
  write ( command_unit, '(a)' ) &
    'set title "Chebyshev Polynomials V(n,x)"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  do j = 1, n_num
    column = n_val(j) + 1
    if ( j == 1 ) then
      write ( command_unit, '(a)', advance = 'no' ) 'plot '
    else
      write ( command_unit, '(a)', advance = 'no' ) '     '
    end if
    write ( command_unit, '(a,i2,a)', advance = 'no' ) &
      '"' // trim ( data_filename ) // &
      '" using 1:', column, ' lw 3 linecolor rgb "red"'
    if ( j < n_num ) then
      write ( command_unit, '(a)' ) ', \'
    else
      write ( command_unit, '(a)' ) ''
    end if
  end do

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  deallocate ( v )

  return
end
function v_polynomial_value ( n, x )

!*****************************************************************************80
!
!! V_POLYNOMIAL_VALUE: returns the single value V(n,x).
!
!  Discussion:
!
!    In cases where calling V_POLYNOMIAL is inconvenient, because it returns
!    a vector of values for multiple arguments X, this simpler interface
!    may be appropriate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Input, real ( kind = rk ) X, the argument of the polynomial.
!
!    Output, real ( kind = rk ) V_POLYNOMIAL_VALUE, the value of V(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n
  real ( kind = rk ) v_polynomial_value
  real ( kind = rk ) value
  real ( kind = rk ), allocatable :: vec(:)
  real ( kind = rk ) x
  real ( kind = rk ) x_vec(1)

  if ( n < 0 ) then

    value = 0.0D+00

  else

    m = 1
    allocate ( vec(0:n) )

    x_vec(1) = x
    call v_polynomial ( m, n, x_vec, vec )

    value = vec(n)
    deallocate ( vec )

  end if

  v_polynomial_value = value

  return
end
subroutine v_polynomial_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! V_POLYNOMIAL_VALUES returns values of Chebyshev polynomials V(n,x).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      u = Sqrt[(x+1)/2],
!      ChebyshevT[2*n+1,u] / u
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 14

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.0000000000000000D+00, &
     1.0000000000000000D+00, &
     0.6000000000000000D+00, &
    -0.0400000000000000D+00, &
    -0.6640000000000000D+00, &
    -1.0224000000000000D+00, &
    -0.9718400000000000D+00, &
    -0.5325440000000000D+00, &
     0.1197696000000000D+00, &
     0.7241753600000000D+00, &
     1.0389109760000000D+00, &
     0.9380822016000000D+00, &
     0.4620205465600000D+00, &
    -0.1988493271040000D+00 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine v_polynomial_zeros ( n, z )

!*****************************************************************************80
!
!! V_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials V(n,x).
!
!  Discussion:
!
!    The I-th zero of U(N,X) is cos((I-1/2)*PI/(N+1/2)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) Z(N), the zeroes.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) angle
  integer i
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) z(n)

  do i = 1, n
    angle = real ( 2 * n - 2 * i + 1, kind = rk ) * r8_pi &
      / real ( 2 * n + 1, kind = rk )
    z(i) = cos ( angle )
  end do

  return
end
subroutine v_quadrature_rule ( n, t, w )

!*****************************************************************************80
!
!! V_QUADRATURE_RULE: quadrature rule for V(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the rule.
!
!    Output, real ( kind = rk ) T(N), W(N), the points and weights of the rule.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) bj(n)
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t(n)
  real ( kind = rk ) w(n)

  t(1:n) = 0.0D+00
  t(1) = + 0.5D+00

  bj(1:n) = 0.5D+00

  w(1) = sqrt ( r8_pi )
  w(2:n) = 0.0D+00

  call imtqlx ( n, t, bj, w )

  w(1:n) = w(1:n) ** 2

  return
end
function vv_product_integral ( i, j )

!*****************************************************************************80
!
!! VV_PRODUCT_INTEGRAL: int (-1<x<1) V(i,x)*V(j,x)*sqrt(1+x)/sqrt(1-x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the polynomial indices.
!    0 <= I, J.
!
!    Output, real ( kind = rk ) VV_PRODUCT_INTEGRAL, the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) vv_product_integral
  real ( kind = rk ) value

  if ( i < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VV_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= I is required.'
    stop 1
  end if

  if ( j < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VV_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= J is required.'
    stop 1
  end if

  if ( i /= j ) then
    value = 0.0D+00
  else
    value = r8_pi
  end if

  vv_product_integral = value

  return
end
subroutine w_mass_matrix ( n, a )

!*****************************************************************************80
!
!! W_MASS_MATRIX computes the mass matrix for the Chebyshev W polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a(0:n,0:n)
  integer i
  real ( kind = rk ), allocatable :: phi(:,:)
  real ( kind = rk ), allocatable :: phiw(:,:)
  real ( kind = rk ), allocatable :: w(:)
  real ( kind = rk ), allocatable :: x(:)

  allocate ( x(0:n) )
  allocate ( w(0:n) )

  call w_quadrature_rule ( n + 1, x, w )

  allocate ( phi(0:n,0:n) )

  call w_polynomial ( n + 1, n, x, phi )

  allocate ( phiw(0:n,0:n) )

  do i = 0, n
    phiw(0:n,i) = w(i) * phi(i,0:n)
  end do

  a(0:n,0:n) = matmul ( phiw(0:n,0:n), phi(0:n,0:n) )

  deallocate ( phi )
  deallocate ( phiw )
  deallocate ( w )
  deallocate ( x )

  return
end
function w_moment ( e )

!*****************************************************************************80
!
!! W_MOMENT: integral ( -1 <= x <= +1 ) x^e sqrt(1-x) / sqrt(1+x) dx.
!
!  Discussion:
!
!     E    W_MOMENT
!    --    --------------
!     0        pi
!     1  -     pi / 2
!     2        pi / 2
!     3  -   3 pi / 8
!     4      3 pi / 8
!     5  -   5 pi / 16
!     6      5 pi / 16
!     7  -  35 pi / 128
!     8     35 pi / 128
!     9  -  63 pi / 256
!    10     63 pi / 256
!    11  - 231 pi / 1024
!    12    231 pi / 1024
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer E, the exponent of X.
!    0 <= E.
!
!    Output, real ( kind = rk ) W_MOMENT, the value of the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) f1
  real ( kind = rk ) f2
  real ( kind = rk ) f3
  real ( kind = rk ) f4
  real ( kind = rk ) f5
  real ( kind = rk ) f6
  real ( kind = rk ) f7
  real ( kind = rk ) f8
  integer e
  real ( kind = rk ) r8_e
  real ( kind = rk ) r8_factorial
  real ( kind = rk ) r8_hyper_2f1
  real ( kind = rk ) r8_mop
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) w_moment
  real ( kind = rk ) value

  r8_e = real ( e, kind = rk )

  f1 = 1.0D+00 / gamma ( 1.5D+00 + r8_e )
  f2 = r8_mop ( e )
  f3 = r8_pi * gamma ( 1.5D+00 + r8_e )
  f4 = 2.0D+00 * r8_mop ( e ) &
    * r8_hyper_2f1 ( 0.5D+00, -r8_e, 1.0D+00, 2.0D+00 )
  f5 = ( -1.0D+00 + r8_mop ( e ) ) &
    * r8_hyper_2f1 ( 0.5D+00, -r8_e, 2.0D+00, 2.0D+00 )
  f6 = sqrt ( r8_pi ) * r8_factorial ( e )
  f7 = ( -1.0D+00 + r8_mop ( e ) ) &
    * r8_hyper_2f1 ( -0.5D+00, 1.0D+00 + r8_e, 1.5D+00 + r8_e, - 1.0D+00 )
  f8 = 2.0D+00 * r8_mop ( e ) &
    * r8_hyper_2f1 ( 0.5D+00, 1.0D+00 + r8_e, 1.5D+00 + r8_e, -1.0D+00 )

  value = f1 * f2 * ( f3 * ( f4 - f5 ) + f6 * ( f7 - f8 ) )

  w_moment = value

  return
end
subroutine w_polynomial ( m, n, x, v )

!*****************************************************************************80
!
!! W_POLYNOMIAL evaluates Chebyshev polynomials W(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2012
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
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(M), the evaluation points.
!
!    Output, real ( kind = rk ) V(M,0:N), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  integer i
  real ( kind = rk ) v(m,0:n)
  real ( kind = rk ) x(m)

  if ( n < 0 ) then
    return
  end if

  v(1:m,0) = 1.0D+00

  if ( n < 1 ) then
    return
  end if

  v(1:m,1) = 2.0D+00 * x(1:m) + 1.0D+00

  do i = 2, n
    v(1:m,i) = 2.0D+00 * x(1:m) * v(1:m,i-1) - v(1:m,i-2)
  end do

  return
end
subroutine w_polynomial_01_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! W_POLYNOMIAL_01_VALUES: values of shifted Chebyshev polynomials W01(n,x).
!
!  Discussion:
!
!    The shifted Chebyshev polynomial W01(n,x) = W(n,2*x-1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 25

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.000000000000000D+00, &
     1.000000000000000D+00, &
     2.400000000000000D+00, &
     2.360000000000000D+00, &
     0.904000000000000D+00, &
    -1.094400000000000D+00, &
    -2.436160000000000D+00, &
    -2.316224000000000D+00, &
    -0.806553600000000D+00, &
     1.187048960000000D+00, &
     2.468422144000000D+00, &
     2.268742041600000D+00, &
     0.707816714240000D+00, &
    -1.277798641664000D+00, &
    -1.000000000000000D+00, &
    -0.119769600000000D+00, &
    -0.875276800000000D+00, &
     0.890508800000000D+00, &
     0.855897600000000D+00, &
    -1.000000000000000D+00, &
    -1.183705600000000D+00, &
     1.217779200000000D+00, &
     1.391244800000000D+00, &
    -3.141798400000000D+00, &
     15.00000000000000D+00 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12,  7,  7, &
     7,  7,  7, &
     7,  7,  7, &
     7,  7,  7 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.85D+00, &
    0.00D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine w_polynomial_ab ( a, b, m, n, xab, v )

!*****************************************************************************80
!
!! W_POLYNOMIAL_AB: evaluates Chebyshev polynomials WAB(n,x) in [A,B].
!
!  Discussion:
!
!    WAB(n,x) = W(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB(M), the evaluation points.
!    It must be the case that A <= XAB(*) <= B.
!
!    Output, real ( kind = rk ) V(M,N+1), the values.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) v(1:m,0:n)
  real ( kind = rk ) x(1:m)
  real ( kind = rk ) xab(1:m)

  x(1:m) = ( 2.0D+00 * xab(1:m) - a - b ) / ( b - a )

  call w_polynomial ( m, n, x, v )

  return
end
function w_polynomial_ab_value ( a, b, n, xab )

!*****************************************************************************80
!
!! W_POLYNOMIAL_AB_VALUE: evaluates Chebyshev polynomials WAB(n,x) in [A,B].
!
!  Discussion:
!
!    WAB(n,x) = W(n,(2*x-a-b)/(b-a))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the domain of definition.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) XAB, the evaluation point.
!    It must be the case that A <= XAB <= B.
!
!    Output, real ( kind = rk ) W_POLYNOMIAL_AB_VALUE, the value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a
  real ( kind = rk ) b
  real ( kind = rk ) value
  real ( kind = rk ) w_polynomial_ab_value
  real ( kind = rk ) w_polynomial_value
  real ( kind = rk ) x
  real ( kind = rk ) xab

  x = ( 2.0D+00 * xab - a - b ) / ( b - a )

  value = w_polynomial_value ( n, x )

  w_polynomial_ab_value = value

  return
end
subroutine w_polynomial_coefficients ( n, c )

!*****************************************************************************80
!
!! W_POLYNOMIAL_COEFFICIENTS: coefficients of Chebyshev polynomials W(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      1     2
!     2     -1     2      4
!     3     -1    -4      4      8
!     4      1    -4    -12      8      16
!     5      1     6    -12    -32     +16    32
!     6     -1     6     24    -32     -80    32     64
!     7     -1    -8    +24    +80     -80  -192     64   128
!
!  Recursion:
!
!    W(0,X) = 1,
!    W(1,X) = 2 * X + 1,
!    W(N,X) = 2 * X * W(N-1,X) - W(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
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
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) c(0:n,0:n)
  integer i

  if ( n < 0 ) then
    return
  end if

  c(0:n,0:n) = 0.0D+00

  c(0,0) = 1.0D+00

  if ( n == 0 ) then
    return
  end if

  c(1,0) = + 1.0D+00
  c(1,1) = + 2.0D+00

  do i = 2, n
    c(i,0)     =                        - c(i-2,0)
    c(i,1:i-2) = 2.0D+00 * c(i-1,0:i-3) - c(i-2,1:i-2)
    c(i,  i-1) = 2.0D+00 * c(i-1,  i-2)
    c(i,  i  ) = 2.0D+00 * c(i-1,  i-1)
  end do

  return
end
subroutine w_polynomial_plot ( n_num, n_val, output_filename )

!*****************************************************************************80
!
!! W_POLYNOMIAL_PLOT plots Chebyshev polynomials W(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NUM, the number of polynomials to be plotted.
!
!    Input, integer N_VAL(N_NUM), the degrees of 1 or more
!    Chebyshev polynomials to be plotted together.
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the name into which the
!    graphics information is to be stored.  Note that the PNG format will
!    be used.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: m = 501
  integer n_num

  real ( kind = rk ) a
  real ( kind = rk ) b
  integer column
  character ( len = 255 ) command_filename
  integer command_unit
  character ( len = 255 ) data_filename
  integer data_unit
  integer i
  integer i4vec_max
  integer j
  integer n
  integer n_max
  integer n_val(n_num)
  character ( len = * ) output_filename
  real ( kind = rk ), allocatable :: v(:,:)
  real ( kind = rk ) x(m)

  a = -1.0D+00
  b = +1.0D+00

  call r8vec_linspace ( m, a, b, x )
!
!  Compute all the data.
!
  n_max = i4vec_max ( n_num, n_val )
  allocate ( v(m,0:n_max) )
  call w_polynomial ( m, n_max, x, v )
!
!  Create the data file.
!
  data_filename = 'w_polynomial_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, m
    write ( data_unit, '(2x,g14.6)', advance = 'no' ) x(i)
    do j = 1, n_num
      n = n_val(j)
      write ( data_unit, '(2x,g14.6)', advance = 'no' ) v(i,n)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Plot the selected data.
!
  command_filename = 'w_polynomial_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set nokey'
  write ( command_unit, '(a)' ) &
    'set output "' // trim ( output_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---T(n,x)--->"'
  write ( command_unit, '(a)' ) &
    'set title "Chebyshev Polynomials W(n,x)"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  do j = 1, n_num
    column = n_val(j) + 1
    if ( j == 1 ) then
      write ( command_unit, '(a)', advance = 'no' ) 'plot '
    else
      write ( command_unit, '(a)', advance = 'no' ) '     '
    end if
    write ( command_unit, '(a,i2,a)', advance = 'no' ) &
      '"' // trim ( data_filename ) // &
      '" using 1:', column, ' lw 3 linecolor rgb "red"'
    if ( j < n_num ) then
      write ( command_unit, '(a)' ) ', \'
    else
      write ( command_unit, '(a)' ) ''
    end if
  end do

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

  deallocate ( v )

  return
end
function w_polynomial_value ( n, x )

!*****************************************************************************80
!
!! W_POLYNOMIAL_VALUE: returns the single value W(n,x).
!
!  Discussion:
!
!    In cases where calling W_POLYNOMIAL is inconvenient, because it returns
!    a vector of values for multiple arguments X, this simpler interface
!    may be appropriate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Input, real ( kind = rk ) X, the argument of the polynomial.
!
!    Output, real ( kind = rk ) W_POLYNOMIAL_VALUE, the value of T(n,x).
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n
  real ( kind = rk ) value
  real ( kind = rk ), allocatable :: vec(:)
  real ( kind = rk ) w_polynomial_value
  real ( kind = rk ) x
  real ( kind = rk ) x_vec(1)

  if ( n < 0 ) then

    value = 0.0D+00

  else

    m = 1
    allocate ( vec(0:n) )

    x_vec(1) = x
    call w_polynomial ( m, n, x_vec, vec )

    value = vec(n)
    deallocate ( vec )

  end if

  w_polynomial_value = value

  return
end
subroutine w_polynomial_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! W_POLYNOMIAL_VALUES returns values of Chebyshev polynomials W(n,x).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      u = Sqrt[(x+1)/2],
!      ChebyshevU[2*n,u]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2015
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer, parameter :: n_max = 14

  real ( kind = rk ) fx
  real ( kind = rk ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.000000000000000D+00, &
     1.000000000000000D+00, &
     2.600000000000000D+00, &
     3.160000000000000D+00, &
     2.456000000000000D+00, &
     0.769600000000000D+00, &
    -1.224640000000000D+00, &
    -2.729024000000000D+00, &
    -3.141798400000000D+00, &
    -2.297853440000000D+00, &
    -0.534767104000000D+00, &
     1.442226073600000D+00, &
     2.842328821760000D+00, &
     3.105500041216000D+00 /)
  integer n
  integer n_data
  integer, save, dimension ( n_max ) :: n_vec = (/ &
    -1, &
     0,  1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10, 11, &
    12 /)
  real ( kind = rk ) x
  real ( kind = rk ), save, dimension ( n_max ) :: x_vec = (/ &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00, &
    0.8D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine w_polynomial_zeros ( n, z )

!*****************************************************************************80
!
!! W_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials W(n,x).
!
!  Discussion:
!
!    The I-th zero of U(N,X) is cos(I*PI/(N+1/2)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) Z(N), the zeroes.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) angle
  integer i
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) z(n)

  do i = 1, n
    angle = real ( 2 * ( n - i + 1 ), kind = rk ) * r8_pi &
      / real ( 2 * n + 1, kind = rk )
    z(i) = cos ( angle )
  end do

  return
end
subroutine w_quadrature_rule ( n, t, w )

!*****************************************************************************80
!
!! W_QUADRATURE_RULE: quadrature rule for W(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the rule.
!
!    Output, real ( kind = rk ) T(N), W(N), the points and weights of the rule.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) bj(n)
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) t(n)
  real ( kind = rk ) w(n)

  t(1:n) = 0.0D+00
  t(1) = - 0.5D+00

  bj(1:n) = 0.5D+00

  w(1) = sqrt ( r8_pi )
  w(2:n) = 0.0D+00

  call imtqlx ( n, t, bj, w )

  w(1:n) = w(1:n) ** 2

  return
end
function ww_product_integral ( i, j )

!*****************************************************************************80
!
!! WW_PRODUCT_INTEGRAL: int (-1<x<1) W(i,x)*W(j,x)*sqrt(1-x)/sqrt(1+x) dx
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the polynomial indices.
!    0 <= I, J.
!
!    Output, real ( kind = rk ) WW_PRODUCT_INTEGRAL, the integral.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i
  integer j
  real ( kind = rk ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = rk ) value
  real ( kind = rk ) ww_product_integral

  if ( i < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WW_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= I is required.'
    stop 1
  end if

  if ( j < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WW_PRODUCT_INTEGRAL - Fatal error!'
    write ( *, '(a)' ) '  0 <= J is required.'
    stop 1
  end if

  if ( i /= j ) then
    value = 0.0D+00
  else
    value = r8_pi
  end if

  ww_product_integral = value

  return
end
