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
! summary: Utility related to Legendre Polynomials is defined.
!
!{!pages/LegendrePolynomialUtility.md!}

MODULE LegendrePolynomialUtility
USE GlobalData
USE BaseType, ONLY: iface_1DFunction
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                             LegendreAlpha
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, alpha, of Legendre polynomial

INTERFACE
  MODULE PURE FUNCTION LegendreAlpha(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreAlpha
END INTERFACE

PUBLIC :: LegendreAlpha

!----------------------------------------------------------------------------
!                                                              LegendreBeta
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, beta, of Legendre polynomial

INTERFACE
  MODULE PURE FUNCTION LegendreBeta(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreBeta
END INTERFACE

PUBLIC :: LegendreBeta

!----------------------------------------------------------------------------
!                                                  GetLegendreRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Return the recurrence coefficient for monic Legendre polynomial
!
!# Introduction
!
! These recurrence coefficients are for monic Legendre polynomials.
!
!$$
! \pi_{n+1}=\left(x-\alpha_{n}\right)\pi_{n}-\beta_{n}\pi_{n-1},\quad n=0,1,2
!$$
!
!$$
! \alpha_{n}=0,n\ge0
!$$
!
!$$
! \beta_{0}=2
!$$
!
!$$
! \beta_{n\ge1}=\frac{n^{2}}{4n^{2}-1}
!$$

INTERFACE
  MODULE PURE SUBROUTINE GetLegendreRecurrenceCoeff(n, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial, it should be greater than 1
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetLegendreRecurrenceCoeff
END INTERFACE

PUBLIC :: GetLegendreRecurrenceCoeff

!----------------------------------------------------------------------------
!                                              GetLegendreRecurrenceCoeff2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Return the recurrence coefficient for Legendre polynomial

INTERFACE
  MODULE PURE SUBROUTINE GetLegendreRecurrenceCoeff2(n, A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial, it should be greater than 1
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    !! size is n
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    !! this coefficient is zero
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
    !! size is n
  END SUBROUTINE GetLegendreRecurrenceCoeff2
END INTERFACE

PUBLIC :: GetLegendreRecurrenceCoeff2

!----------------------------------------------------------------------------
!                                                         LegendreLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Leading coefficient of Legendre polynomial
!
!# Introduction
!
! Leading coefficient of legendre polynomial
!
!$$
! k_{n}=\frac{\left(2n\right)!}{2^{n}\left(n!\right)^{2}}
!$$
!

INTERFACE
  MODULE PURE FUNCTION LegendreLeadingCoeff(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreLeadingCoeff
END INTERFACE

PUBLIC :: LegendreLeadingCoeff

!----------------------------------------------------------------------------
!                                                 LegendreLeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Ration of Leading coefficient of Legendre polynomial n+1/n

INTERFACE
  MODULE PURE FUNCTION LegendreLeadingCoeffRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreLeadingCoeffRatio
END INTERFACE

PUBLIC :: LegendreLeadingCoeffRatio

!----------------------------------------------------------------------------
!                                                            LegendreNormSQR
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of Legendre polynomial
!
!# Introduction
!
! This function returns the square norm of legendre polynomial
!
!$$
! \Vert P_{n}\Vert^{2}=:h_{n}=\frac{2}{2n+1}
!$$

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION LegendreNormSQR
END INTERFACE

PUBLIC :: LegendreNormSQR

!----------------------------------------------------------------------------
!                                                            LegendreNormSQR2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of Legendre polynomial
!
!# Introduction
!
! This function returns the square norm of legendre polynomial
!
!$$
! \Vert P_{n}\Vert^{2}=:h_{n}=\frac{2}{2n+1}
!$$

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQR2(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n)
  END FUNCTION LegendreNormSQR2
END INTERFACE

PUBLIC :: LegendreNormSQR2

!----------------------------------------------------------------------------
!                                                      LegendreNormSQRRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Ratio of Square norm of Legendre polynomial n+1/n

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQRRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION LegendreNormSQRRatio
END INTERFACE

PUBLIC :: LegendreNormSqrRatio

!----------------------------------------------------------------------------
!                                                      LegendreJacobiMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Return the Jacobi matrix for Legendre polynomial

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiMatrix(n, D, E, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    !! recurrence coefficient of monic legendre polynomial, from 0 to n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
    !! recurrence coefficient of monic legendre polynomial, from 0 to n-1
  END SUBROUTINE LegendreJacobiMatrix
END INTERFACE

PUBLIC :: LegendreJacobiMatrix

!----------------------------------------------------------------------------
!                                                     LegendreGaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss quadrature points for Legendre Polynomial
!
!# Introduction
!
! This routine computes the n Gauss-Quadrature points. Which,
! are n zeros of a Legendre polynomial defined with respect to the
! weight $(1-x)^{\alpha} (1+x)^{\beta}$.
!
! All Gauss-Quadrature points are inside $(-1, 1)$

INTERFACE
  MODULE SUBROUTINE LegendreGaussQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE LegendreGaussQuadrature
END INTERFACE

PUBLIC :: LegendreGaussQuadrature

!----------------------------------------------------------------------------
!                                               LegendreJacobiRadauMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiRadauMatrix(a, n, D, E, alphaCoeff, &
    & betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreJacobiRadauMatrix
END INTERFACE

PUBLIC :: LegendreJacobiRadauMatrix

!----------------------------------------------------------------------------
!                                                LegendreGaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Radau quadrature points for Legendre Polynomial
!
!# Introduction
!
! This routine returns the $n+1$ Quadrature points and weights.
!
! The Gauss-Radau quadrature points consists one of the end points denoted
! by $a$. So $a$ can be $\pm 1$. The remaining $n$ points are internal to
! to $(-1, +1)$, and they are n-zeros of Legendre  polynomial of order n with
! respect to the following weight.
!
!- $(1-x)^{\alpha} (1+x)^{\beta} (x+1)$ if $a=-1$.
!- $(1-x)^{\alpha} (1+x)^{\beta} (1-x)$ if $a=+1$.
!
! Here n is the order of Legendre polynomial.
!
! If $a=1$ then n+1 quadrature point will be +1
! If $a=-1$ then 1st quadrature point will be -1

INTERFACE
  MODULE SUBROUTINE LegendreGaussRadauQuadrature(a, n, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! the value of one of the end points
    !! it should be either -1 or +1
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE LegendreGaussRadauQuadrature
END INTERFACE

PUBLIC :: LegendreGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                             LegendreLegendreLobattoMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiLobattoMatrix(n, D, E, alphaCoeff, &
    & betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreJacobiLobattoMatrix
END INTERFACE

PUBLIC :: LegendreJacobiLobattoMatrix

!----------------------------------------------------------------------------
!                                              LegendreGaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Lobatto quadrature points for Legendre Polynomial
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
! to $(-1, +1)$, and they are n-zeros of Legendre  polynomial of order n with
! respect to the following weight.
!
!$$(1-x)^{\alpha} (1+x)^{\beta} (x+1)(1-x)$$
!
! Here n is the order of Legendre polynomial.

INTERFACE
  MODULE SUBROUTINE LegendreGaussLobattoQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomials
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE LegendreGaussLobattoQuadrature
END INTERFACE

PUBLIC :: LegendreGaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                              LegendreZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary:         Returns zeros of Legendre polynomials

INTERFACE
  MODULE FUNCTION LegendreZeros(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans(n)
  END FUNCTION LegendreZeros
END INTERFACE

PUBLIC :: LegendreZeros

!----------------------------------------------------------------------------
!                                                          LegendreQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: This routine can return Legendre-Gauss, Legendre-Radau,
! Legendre-Lobatto
!
!# Introduction
!
! This routine returns the Quadrature point of Legendre polynomial
!
!@note
! Here n is the number of quadrature points. Please note it is not
! the order of Legendre polynomial. The order is decided internally
! depending upon the quadType
!@endnote
!
!@note
! pt and wt should be allocated outside, and length should be n.
!@endnote
!

INTERFACE
  MODULE SUBROUTINE LegendreQuadrature(n, pt, wt, quadType, onlyInside)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Legendre polynomial
    !! for quadType = GaussRadauLeft or GaussRadauRight n is order+1
    !! for quadType = GaussLobatto, n = order+2
    REAL(DFP), INTENT(OUT) :: pt(n)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(n)
    !! n+1 weights from 1 to n+1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss
    !! GaussRadauLeft
    !! GaussRadauRight
    !! GaussLobatto
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyInside
    !! only inside
  END SUBROUTINE LegendreQuadrature
END INTERFACE

PUBLIC :: LegendreQuadrature

!----------------------------------------------------------------------------
!                                                             LegendreEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Legendre polynomial of order n at single points
!
!# Introduction
!
! Evaluate Legendre polynomial of order n at single points

INTERFACE
  MODULE PURE FUNCTION LegendreEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation, it should be between -1 and 1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEval1
END INTERFACE

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval1
END INTERFACE LegendreEval

PUBLIC :: LegendreEval

!----------------------------------------------------------------------------
!                                                          LegendreEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Legendre polynomials of order n at several points
!
!# Introduction
!
! Evaluate Legendre polynomials of order n at several points

INTERFACE
  MODULE PURE FUNCTION LegendreEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! several points of evaluation
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at points x
  END FUNCTION LegendreEval2
END INTERFACE

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval2
END INTERFACE LegendreEval

!----------------------------------------------------------------------------
!                                                            LegendreEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Legendre polynomials from order = 0 to n at single point
!
!# Introduction
!
! Evaluate Legendre polynomials from order = 0 to n at single points
!
!- x: the point at which the polynomials are to be evaluated.

INTERFACE
  MODULE PURE FUNCTION LegendreEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x
    !! Point of evaluation, $x \in [-1, 1]$
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Legendre polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LegendreEvalAll1
END INTERFACE

INTERFACE LegendreEvalAll
  MODULE PROCEDURE LegendreEvalAll1
END INTERFACE LegendreEvalAll

PUBLIC :: LegendreEvalAll

!----------------------------------------------------------------------------
!                                                           LegendreEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Legendre polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate Legendre polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Legendre polynomials at the point
! X.
!
!- the ith row of ans denotes the values of all polynomials at
! ith point. In this case shape of ans is (M,1:N+1), where M is number of
! points, N+1 number of polynomials. So ans(j, :) denotes value of all
! polynomials at jth point, and ans(:, n) denotes value of Pn at all nodes

INTERFACE
  MODULE PURE FUNCTION LegendreEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x(:)
    !! number of points, SIZE(x)=M
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! shape (M,N+1)
  END FUNCTION LegendreEvalAll2
END INTERFACE

INTERFACE LegendreEvalAll
  MODULE PROCEDURE LegendreEvalAll2
END INTERFACE LegendreEvalAll

!----------------------------------------------------------------------------
!                                             LegendreMonomialExpansionAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Returns the monomial expansion of all legendre polynomials
!
!# Introduction
!
! Returns all the monomial expansion of all legendre polynomials
!
!- n : is the order of the polynomial
!- ans(:,i) contains the coefficient of monomials for polynomial order=i-1
!
! for example, n=5, we have following structure of ans
!
! | P0 | P1 | P2   | P3   | P4    | P5    |
! |----|----|------|------|-------|-------|
! | 1  | 0  | -0.5 | -0   | 0.375 | 0     |
! | 0  | 1  | 0    | -1.5 | -0    | 1.875 |
! | 0  | 0  | 1.5  | 0    | -3.75 | -0    |
! | 0  | 0  | 0    | 2.5  | 0     | -8.75 |
! | 0  | 0  | 0    | 0    | 4.375 | 0     |
! | 0  | 0  | 0    | 0    | 0     | 7.875 |

INTERFACE
  MODULE PURE FUNCTION LegendreMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION LegendreMonomialExpansionAll
END INTERFACE

PUBLIC :: LegendreMonomialExpansionAll

!----------------------------------------------------------------------------
!                                             LegendreMonomialExpansion
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Returns the monomial expansion of a legendre polynomials
!
!# Introduction
!
! Returns all the monomial expansion of a legendre polynomials
!
!- n : is the order of the polynomial
!- ans(:) contains the coefficient of monomials for polynomial order=n
!

INTERFACE
  MODULE PURE FUNCTION LegendreMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LegendreMonomialExpansion
END INTERFACE

PUBLIC :: LegendreMonomialExpansion

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of legendre polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of legendre polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LegendreGradientEvalAll1
END INTERFACE
!!

INTERFACE LegendreGradientEvalAll
  MODULE PROCEDURE LegendreGradientEvalAll1
END INTERFACE LegendreGradientEvalAll

PUBLIC :: LegendreGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of legendre polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of legendre polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LegendreGradientEvalAll2
END INTERFACE
!!

INTERFACE LegendreGradientEvalAll
  MODULE PROCEDURE LegendreGradientEvalAll2
END INTERFACE LegendreGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of legendre polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of legendre polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LegendreGradientEval1
END INTERFACE
!!

INTERFACE LegendreGradientEval
  MODULE PROCEDURE LegendreGradientEval1
END INTERFACE LegendreGradientEval

PUBLIC :: LegendreGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of legendre polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of legendre polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LegendreGradientEval2
END INTERFACE
!!

INTERFACE LegendreGradientEval
  MODULE PROCEDURE LegendreGradientEval2
END INTERFACE LegendreGradientEval

!----------------------------------------------------------------------------
!                                                     LegendreEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Legendre polynomials at point x

INTERFACE
  MODULE PURE FUNCTION LegendreEvalSum1(n, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEvalSum1
END INTERFACE

INTERFACE LegendreEvalSum
  MODULE PROCEDURE LegendreEvalSum1
END INTERFACE LegendreEvalSum

PUBLIC :: LegendreEvalSum

!----------------------------------------------------------------------------
!                                                          LegendreEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Legendre polynomials at several x

INTERFACE
  MODULE PURE FUNCTION LegendreEvalSum2(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEvalSum2
END INTERFACE

INTERFACE LegendreEvalSum
  MODULE PROCEDURE LegendreEvalSum2
END INTERFACE LegendreEvalSum

!----------------------------------------------------------------------------
!                                             LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Legendre polynomials
! at point x

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalSum1(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum1
END INTERFACE

INTERFACE LegendreGradientEvalSum
  MODULE PROCEDURE LegendreGradientEvalSum1
END INTERFACE LegendreGradientEvalSum

PUBLIC :: LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                              LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Legendre polynomials
! at several x

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalSum2(n, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum2
END INTERFACE

INTERFACE LegendreGradientEvalSum
  MODULE PROCEDURE LegendreGradientEvalSum2
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                              LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth derivative of finite sum of Legendre
! polynomials at point x

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalSum3(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum3
END INTERFACE

INTERFACE LegendreGradientEvalSum
  MODULE PROCEDURE LegendreGradientEvalSum3
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                             LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth gradient of finite sum of Legendre
!  polynomials at several x

INTERFACE
  MODULE PURE FUNCTION LegendreGradientEvalSum4(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum4
END INTERFACE

INTERFACE LegendreGradientEvalSum
  MODULE PROCEDURE LegendreGradientEvalSum4
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                                         LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Legendre Transform

INTERFACE
  MODULE PURE FUNCTION LegendreTransform1(n, coeff, x, w, &
    &  quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
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
  END FUNCTION LegendreTransform1
END INTERFACE

INTERFACE LegendreTransform
  MODULE PROCEDURE LegendreTransform1
END INTERFACE LegendreTransform

PUBLIC :: LegendreTransform

!----------------------------------------------------------------------------
!                                                   LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Columnwise Discrete Legendre Transform

INTERFACE
  MODULE PURE FUNCTION LegendreTransform2(n, coeff, x, w, &
    & quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
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
  END FUNCTION LegendreTransform2
END INTERFACE

INTERFACE LegendreTransform
  MODULE PROCEDURE LegendreTransform2
END INTERFACE LegendreTransform

!----------------------------------------------------------------------------
!                                                   LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Legendre Transform of a function on [-1,1]
!
!# Introduction
!
! This function performs the Legendre transformation of f defined
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
! This routine is not pure, because this subroutine calls
!  `LegendreQuadrature` which is not pure due to Lapack call.
!@endnote

INTERFACE
  MODULE FUNCTION LegendreTransform3(n, f, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION LegendreTransform3
END INTERFACE

INTERFACE LegendreTransform
  MODULE PROCEDURE LegendreTransform3
END INTERFACE LegendreTransform

!----------------------------------------------------------------------------
!                                                 LegendreInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Inverse Legendre Transform

INTERFACE
  MODULE PURE FUNCTION LegendreInvTransform1(n, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION LegendreInvTransform1
END INTERFACE

INTERFACE LegendreInvTransform
  MODULE PROCEDURE LegendreInvTransform1
END INTERFACE LegendreInvTransform

PUBLIC :: LegendreInvTransform

!----------------------------------------------------------------------------
!                                                 LegendreInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Inverse Legendre Transform

INTERFACE
  MODULE PURE FUNCTION LegendreInvTransform2(n, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION LegendreInvTransform2
END INTERFACE

INTERFACE LegendreInvTransform
  MODULE PROCEDURE LegendreInvTransform2
END INTERFACE LegendreInvTransform

!----------------------------------------------------------------------------
!                                               LegendreGradientCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficients for gradient of Legendre expansion
!
!# Introduction
!
!- This routine returns the coefficients of gradient of Jacobi expansion.
!- Input is coefficient of Legendre expansion (modal values)
!- Output is coefficient of derivative of legendre expansion (modal values)

INTERFACE
  MODULE PURE FUNCTION LegendreGradientCoeff1(n, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from LegendreTransform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION LegendreGradientCoeff1
END INTERFACE

INTERFACE LegendreGradientCoeff
  MODULE PROCEDURE LegendreGradientCoeff1
END INTERFACE LegendreGradientCoeff

PUBLIC :: LegendreGradientCoeff

!----------------------------------------------------------------------------
!                                                           LegendreDMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 Oct 2022
! summary: Returns differentiation matrix for Legendre expansion

INTERFACE
  MODULE PURE FUNCTION LegendreDMatrix1(n, x, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION LegendreDMatrix1
END INTERFACE

INTERFACE LegendreDMatrix
  MODULE PROCEDURE LegendreDMatrix1
END INTERFACE LegendreDMatrix

PUBLIC :: LegendreDMatrix

!----------------------------------------------------------------------------
!                                                 LegendreDMatEvenOdd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 Oct 2022
! summary: Performs even and odd decomposition of Differential matrix

INTERFACE
  MODULE PURE SUBROUTINE LegendreDMatEvenOdd1(n, D, e, o)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: D(0:n, 0:n)
      !! n+1 by n+1
    REAL(DFP), INTENT(OUT) :: e(0:, 0:)
      !! even Decomposition, 0:n/2, 0:n/2
    REAL(DFP), INTENT(OUT) :: o(0:, 0:)
      !! odd decomposition, 0:n/2, 0:n/2
  END SUBROUTINE LegendreDMatEvenOdd1
END INTERFACE

INTERFACE LegendreDMatEvenOdd
  MODULE PROCEDURE LegendreDMatEvenOdd1
END INTERFACE LegendreDMatEvenOdd

PUBLIC :: LegendreDMatEvenOdd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LegendrePolynomialUtility
