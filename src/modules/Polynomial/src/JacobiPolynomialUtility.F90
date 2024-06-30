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
! date: 3 Aug 2022
! summary: Utility related to Jacobi Polynomials is defined.
!
!{!pages/JacobiPolynomialUtility.md!}

MODULE JacobiPolynomialUtility
USE GlobalData
USE BaseType, ONLY: iface_1DFunction
IMPLICIT NONE
PRIVATE
PUBLIC :: GetJacobiRecurrenceCoeff
PUBLIC :: GetJacobiRecurrenceCoeff2
PUBLIC :: JacobiAlpha
PUBLIC :: JacobiBeta
PUBLIC :: JacobiLeadingCoeff
PUBLIC :: JacobiLeadingCoeffRatio
PUBLIC :: JacobiNormSQR
PUBLIC :: JacobiNormSQR2
PUBLIC :: JacobiNormSQRRatio
PUBLIC :: JacobiJacobiMatrix
PUBLIC :: JacobiGaussQuadrature
PUBLIC :: JacobiJacobiRadauMatrix
PUBLIC :: JacobiGaussRadauQuadrature
PUBLIC :: JacobiJacobiLobattoMatrix
PUBLIC :: JacobiGaussLobattoQuadrature
PUBLIC :: JacobiZeros
PUBLIC :: JacobiQuadrature
PUBLIC :: JacobiEvalAll
PUBLIC :: JacobiEvalAll_
PUBLIC :: JacobiEval
PUBLIC :: JacobiEvalSum
PUBLIC :: JacobiGradientEval
PUBLIC :: JacobiGradientEvalAll
PUBLIC :: JacobiGradientEvalAll_
PUBLIC :: JacobiGradientEvalSum
PUBLIC :: JacobiTransform
PUBLIC :: JacobiInvTransform
PUBLIC :: JacobiGradientCoeff
PUBLIC :: JacobiDMatrix

!----------------------------------------------------------------------------
!                                                  GetJacobiRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order monic polynomial
!
!# Introduction
!
! These recurrence coefficients are for monic jacobi polynomials.

INTERFACE
  MODULE PURE SUBROUTINE GetJacobiRecurrenceCoeff(n, alpha, beta, &
    & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetJacobiRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetJacobiRecurrenceCoeff2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order polynomial
!
!# Introduction
!
! These recurrence coefficients are for non-monic jacobi polynomials.
!
!$$
! P_{n+1}^{(\alpha,\beta)}=\left(a_{n}x+b_{n}\right)P_{n}^{(\alpha,\beta)}
! -c_{n}P_{n-1}^{(\alpha,\beta)},\quad n=1,2,\cdots
!$$

INTERFACE
  MODULE PURE SUBROUTINE GetJacobiRecurrenceCoeff2(n, alpha, beta, &
    & A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
  END SUBROUTINE GetJacobiRecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               JacobiAlpha
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns reccurence coeff alpha

INTERFACE
  MODULE ELEMENTAL PURE FUNCTION JacobiAlpha(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                                JacobiBeta
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns reccurence coeff beta

INTERFACE
  MODULE ELEMENTAL PURE FUNCTION JacobiBeta(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobiLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Leading coefficient of Jacobi polynomial

INTERFACE
  MODULE PURE FUNCTION JacobiLeadingCoeff(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                    JacobiLeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Leading coefficient ratio of Jacobi polynomial, n+1/n

INTERFACE
  MODULE PURE FUNCTION JacobiLeadingCoeffRatio(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiLeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobiNormSQR
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Jacobi polynomial
!
!# Introduction
!
! This function returns the following
!
!$$
!\Vert P_{n}^{\alpha,\beta}\Vert_{d\lambda}^{2}=\int_{-1}^{+1}P_{n}^
!{\alpha,\beta}P_{n}^{\alpha,\beta}(1-x)^{\alpha}(1+x)^{\beta}dx
!$$

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQR(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans
  END FUNCTION JacobiNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                            JacobiNormSQR2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Jacobi polynomial
!
!# Introduction
!
! This function returns the following
!
!$$
!\Vert P_{n}^{\alpha,\beta}\Vert_{d\lambda}^{2}=\int_{-1}^{+1}P_{n}^
!{\alpha,\beta}P_{n}^{\alpha,\beta}(1-x)^{\alpha}(1+x)^{\beta}dx
!$$

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQR2(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans(0:n)
  END FUNCTION JacobiNormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobiNormSQRRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Ratio of Square norm of Jacobi polynomial n+1/n

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQRRatio(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans
  END FUNCTION JacobiNormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                      JacobiJacobiMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiMatrix(n, alpha, beta, D, E, &
      & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of jacobi poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE JacobiJacobiMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                     JacobiGaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:         Returns the Gauss quadrature points for Jacobi Polynomial
!
!# Introduction
!
! This routine computes the n Gauss-Quadrature points. Which,
! are n zeros of a jacobi polynomial defined with respect to the
! weight $(1-x)^{\alpha} (1+x)^{\beta}$.
!
! All Gauss-Quadrature points are inside $(-1, 1)$

INTERFACE
  MODULE SUBROUTINE JacobiGaussQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE JacobiGaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                   JacobiJacobiRadauMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiRadauMatrix(a, n, alpha, beta, D, &
    & E, alphaCoeff, betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of jacobi poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE JacobiJacobiRadauMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                JacobiGaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Radau quadrature points for Jacobi Polynomial
!
!# Introduction
!
! This routine returns the $n+1$ Quadrature points and weights.
!
! The Gauss-Radau quadrature points consists one of the end points denoted
! by $a$. So $a$ can be $\pm 1$. The remaining $n$ points are internal to
! to $(-1, +1)$, and they are n-zeros of Jacobi  polynomial of order n with
! respect to the following weight.
!
!- $(1-x)^{\alpha} (1+x)^{\beta} (x+1)$ if $a=-1$.
!- $(1-x)^{\alpha} (1+x)^{\beta} (1-x)$ if $a=+1$.
!
! Here n is the order of Jacobi polynomial.
!
! If $a=1$ then n+1 quadrature point will be +1
! If $a=-1$ then 1st quadrature point will be -1

INTERFACE
  MODULE SUBROUTINE JacobiGaussRadauQuadrature(a, n, alpha, beta, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! the value of one of the end points
    !! it should be either -1 or +1
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE JacobiGaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                 JacobiJacobiLobattoMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiLobattoMatrix(n, alpha, beta, D, &
    & E, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of jacobi poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE JacobiJacobiLobattoMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                              JacobiGaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Lobatto quadrature points for Jacobi Polynomial
!
!# Introduction
!
! This routine returns the $n+2$ Quadrature points and weights.
!
! The Gauss-Lobatto quadrature points consists both $\pm 1$ as
! quadrature points.
!
!- The first quadrature point is $-1$
!- The second quadrature point is $+1$
!
! The remaining $n$ points are internal to
! to $(-1, +1)$, and they are n-zeros of Jacobi  polynomial of order n with
! respect to the following weight.
!
!$$(1-x)^{\alpha} (1+x)^{\beta} (x+1)(1-x)$$
!
! Here n is the order of Jacobi polynomial.

INTERFACE
  MODULE SUBROUTINE JacobiGaussLobattoQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomials
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE JacobiGaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                              JacobiZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary:         Returns zeros of Jacobi polynomials

INTERFACE
  MODULE FUNCTION JacobiZeros(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans(n)
  END FUNCTION JacobiZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                          JacobiQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: This routine can return Jacobi-Gauss, Jacobi-Radau, Jacobi-Lobatto
!
!# Introduction
!
! This routine returns the Quadrature point of Jacobi polynomial
!
!@note
! Here n is the number of quadrature points. Please note it is not
! the order of jacobi polynomial. The order is decided internally
! depending upon the quadType
!@endnote
!
!@note
! pt and wt should be allocated outside, and length should be n.
!@endnote
!

INTERFACE
  MODULE SUBROUTINE JacobiQuadrature(n, alpha, beta, pt, wt, quadType)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Jacobi polynomial
    !! for quadType = GaussRadauLeft or GaussRadauRight n is order+1
    !! for quadType = GaussLobatto, n = order+2
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial
    REAL(DFP), INTENT(OUT) :: pt(n)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(n)
    !! n+1 weights from 1 to n+1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss
    !! GaussRadauLeft
    !! GaussRadauRight
    !! GaussLobatto
  END SUBROUTINE JacobiQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobiEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials from order = 0 to n at single points
!
!# Introduction
!
! Evaluate Jacobi polynomials from order = 0 to n at single points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(1:N+1), the values of the first N+1 Jacobi polynomials at x

INTERFACE JacobiEvalAll
  MODULE PURE FUNCTION JacobiEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll1
END INTERFACE JacobiEvalAll

!----------------------------------------------------------------------------
!                                                       JacobiEvalAll
!----------------------------------------------------------------------------

INTERFACE JacobiEvalAll_
  MODULE PURE SUBROUTINE JacobiEvalAll1_(n, alpha, beta, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE JacobiEvalAll1_
END INTERFACE JacobiEvalAll_

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomial of order = 0 to n at several points
!
!# Introduction
!
! Evaluate Jacobi polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Jacobi polynomials at the point
! X.

INTERFACE JacobiEvalAll
  MODULE PURE FUNCTION JacobiEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll2
END INTERFACE JacobiEvalAll

!----------------------------------------------------------------------------
!                                                              JacobiEvalAll
!----------------------------------------------------------------------------

INTERFACE JacobiEvalAll_
  MODULE PURE SUBROUTINE JacobiEvalAll2_(n, alpha, beta, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE JacobiEvalAll2_
END INTERFACE JacobiEvalAll_

!----------------------------------------------------------------------------
!                                                             JacobiEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials of order n at single points.
!
!# Introduction
!
! Evaluate Jacobi polynomials of order n at single points.
!
!- N, the order of polynomial to compute.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.

INTERFACE JacobiEval
  MODULE PURE FUNCTION JacobiEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval1
END INTERFACE JacobiEval

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials of order n at several points
!
!# Introduction
!
! Evaluate Jacobi polynomials of order n at several points
!
!- N, the order of polynomial to compute.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.

INTERFACE JacobiEval
  MODULE PURE FUNCTION JacobiEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval2
END INTERFACE JacobiEval

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Jacobi polynomials at point x

INTERFACE JacobiEvalSum
  MODULE PURE FUNCTION JacobiEvalSum1(n, alpha, beta, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEvalSum1
END INTERFACE JacobiEvalSum

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Jacobi polynomials at several x

INTERFACE JacobiEvalSum
  MODULE PURE FUNCTION JacobiEvalSum2(n, alpha, beta, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEvalSum2
END INTERFACE JacobiEvalSum

!----------------------------------------------------------------------------
!                                                         JacobiGradientEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Oct 2022
! summary: Evaluate Gradient of Jacobi polynomial

INTERFACE JacobiGradientEval
  MODULE PURE FUNCTION JacobiGradientEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEval1
END INTERFACE JacobiGradientEval

!----------------------------------------------------------------------------
!                                                        JacobiGradientEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Oct 2022
! summary: Evaluate Gradient of Jacobi polynomial

INTERFACE JacobiGradientEval
  MODULE PURE FUNCTION JacobiGradientEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiGradientEval2
END INTERFACE JacobiGradientEval

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Oct 2022
! summary: Evaluate Gradient of Jacobi polynomial

INTERFACE JacobiGradientEvalAll
  MODULE PURE FUNCTION JacobiGradientEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalAll1
END INTERFACE JacobiGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE JacobiGradientEvalAll_
  MODULE PURE SUBROUTINE JacobiGradientEvalAll1_(n, alpha, beta, x, &
                                                 ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE JacobiGradientEvalAll1_
END INTERFACE JacobiGradientEvalAll_

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Oct 2022
! summary: Evaluate Gradient of Jacobi polynomial

INTERFACE JacobiGradientEvalAll
  MODULE PURE FUNCTION JacobiGradientEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiGradientEvalAll2
END INTERFACE JacobiGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE JacobiGradientEvalAll_
  MODULE PURE SUBROUTINE JacobiGradientEvalAll2_(n, alpha, beta, x, &
                                                 ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE JacobiGradientEvalAll2_
END INTERFACE JacobiGradientEvalAll_

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Jacobi polynomials at
! point x

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum1(n, alpha, beta, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum1
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Jacobi polynomials at
! several x

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum2(n, alpha, beta, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum2
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth derivative of finite sum of Jacobi polynomials at
! point x

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum3(n, alpha, beta, x, coeff, k) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum3
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth gradient of finite sum of Jacobi polynomials at
! several x

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum4(n, alpha, beta, x, coeff, k) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum4
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                           JacobiTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Discrete Jacobi Transform

INTERFACE JacobiTransform
  MODULE PURE FUNCTION JacobiTransform1(n, alpha, beta, coeff, x, w, &
    &  quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! nodal value (at quad points)
    REAL(DFP), INTENT(IN) :: x(0:n)
    !! quadrature points
    REAL(DFP), INTENT(IN) :: w(0:n)
    !! weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION JacobiTransform1
END INTERFACE JacobiTransform

!----------------------------------------------------------------------------
!                                                           JacobiTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Columnwise Discrete Jacobi Transform

INTERFACE JacobiTransform
  MODULE PURE FUNCTION JacobiTransform2(n, alpha, beta, coeff, x, w, &
    & quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:, 1:)
    !! nodal value (at quad points)
    REAL(DFP), INTENT(IN) :: x(0:n)
    !! quadrature points
    REAL(DFP), INTENT(IN) :: w(0:n)
    !! weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n, 1:SIZE(coeff, 2))
    !! modal values  or coefficients for each column of val
  END FUNCTION JacobiTransform2
END INTERFACE JacobiTransform

!----------------------------------------------------------------------------
!                                                           JacobiTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Jacobi Transform of a function on [-1,1]
!
!# Introduction
!
! This function performs the jacobi transformation of a function defined
! on -1 to 1. The interface of the function is give below:
!
!```fortran
! ABSTRACT INTERFACE
!   ELEMENTAL FUNCTION iface_1DFunction(x) RESULT(ans)
!     IMPORT :: DFP
!     REAL(DFP), INTENT(IN) :: x
!     REAL(DFP) :: ans
!   END FUNCTION iface_1DFunction
! END INTERFACE
!```
!
!@note
! This routine is not pure, because this subroutine calls `JacobiQuadrature`
! which is not pure due to Lapack call.
!@endnote

INTERFACE JacobiTransform
  MODULE FUNCTION JacobiTransform3(n, alpha, beta, f, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION JacobiTransform3
END INTERFACE JacobiTransform

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Discrete Jacobi Transform

INTERFACE JacobiInvTransform
  MODULE PURE FUNCTION JacobiInvTransform1(n, alpha, beta, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION JacobiInvTransform1
END INTERFACE JacobiInvTransform

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Discrete Jacobi Transform

INTERFACE JacobiInvTransform
  MODULE PURE FUNCTION JacobiInvTransform2(n, alpha, beta, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION JacobiInvTransform2
END INTERFACE JacobiInvTransform

!----------------------------------------------------------------------------
!                                                      JacobiGradientCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Returns coefficient for gradient of Jacobi expansion
!
!# Introduction
!
! This routine returns the coefficients of gradient of Jacobi expansion.
! Input is cofficients of Jacobipolynomials (modal values).
!

INTERFACE JacobiGradientCoeff
  MODULE PURE FUNCTION JacobiGradientCoeff1(n, alpha, beta, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from JacobiTransform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION JacobiGradientCoeff1
END INTERFACE JacobiGradientCoeff

!----------------------------------------------------------------------------
!                                                              JacobiDMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Returns coefficient for gradient of Jacobi expansion
!
!# Introduction
!
! This routine returns the coefficients of gradient of Jacobi expansion.
! Input is cofficients of Jacobipolynomials (modal values).
!

INTERFACE JacobiDMatrix
  MODULE PURE FUNCTION JacobiDMatrix1(n, alpha, beta, x, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION JacobiDMatrix1
END INTERFACE JacobiDMatrix

END MODULE JacobiPolynomialUtility
