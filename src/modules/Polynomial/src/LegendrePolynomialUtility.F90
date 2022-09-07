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
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                  GetLegendreRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order polynomial
!
!# Introduction
!
! These recurrence coefficients are for monic Legendre polynomials.

INTERFACE
  MODULE PURE SUBROUTINE GetLegendreRecurrenceCoeff(n, alpha, beta, &
    & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetLegendreRecurrenceCoeff
END INTERFACE

PUBLIC :: GetLegendreRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                         LegendreLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Leading coefficient of Legendre polynomial

INTERFACE
  MODULE PURE FUNCTION LegendreLeadingCoeff(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Legendre poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Legendre poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreLeadingCoeff
END INTERFACE

PUBLIC :: LegendreLeadingCoeff

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Legendre polynomial
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
  MODULE PURE FUNCTION LegendreNormSQR(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans
  END FUNCTION LegendreNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                      LegendreLegendreMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LegendreLegendreMatrix(n, alpha, beta, D, E, &
      & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Legendre poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreLegendreMatrix
END INTERFACE

PUBLIC :: LegendreLegendreMatrix

!----------------------------------------------------------------------------
!                                                     LegendreGaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:         Returns the Gauss quadrature points for Legendre Polynomial
!
!# Introduction
!
! This routine computes the n Gauss-Quadrature points. Which,
! are n zeros of a Legendre polynomial defined with respect to the
! weight $(1-x)^{\alpha} (1+x)^{\beta}$.
!
! All Gauss-Quadrature points are inside $(-1, 1)$

INTERFACE
  MODULE SUBROUTINE LegendreGaussQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE LegendreGaussQuadrature
END INTERFACE

PUBLIC :: LegendreGaussQuadrature

!----------------------------------------------------------------------------
!                                               LegendreLegendreRadauMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LegendreLegendreRadauMatrix(a, n, alpha, beta, D, &
    & E, alphaCoeff, betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Legendre poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreLegendreRadauMatrix
END INTERFACE

PUBLIC :: LegendreLegendreRadauMatrix

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
  MODULE SUBROUTINE LegendreGaussRadauQuadrature(a, n, alpha, beta, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! the value of one of the end points
    !! it should be either -1 or +1
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Legendre polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE LegendreGaussRadauQuadrature
END INTERFACE

PUBLIC :: LegendreGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                 LegendreLegendreLobattoMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE LegendreLegendreLobattoMatrix(n, alpha, beta, D, &
    & E, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of jacobu poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Legendre poly
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreLegendreLobattoMatrix
END INTERFACE

PUBLIC :: LegendreLegendreLobattoMatrix

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
  MODULE SUBROUTINE LegendreGaussLobattoQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomials
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), INTENT(OUT) :: wt(:)
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
  MODULE FUNCTION LegendreZeros(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans(n)
  END FUNCTION LegendreZeros
END INTERFACE

PUBLIC :: LegendreZeros

!----------------------------------------------------------------------------
!                                                          LegendreQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: This routine can return Legendre-Gauss, Legendre-Radau, Legendre-Lobatto
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
  MODULE SUBROUTINE LegendreQuadrature(n, alpha, beta, pt, wt, quadType)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Legendre polynomial
    !! for quadType = GaussRadauLeft or GaussRadauRight n is order+1
    !! for quadType = GaussLobatto, n = order+2
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Legendre polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(n)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), INTENT(OUT) :: wt(n)
    !! n+1 weights from 1 to n+1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss
    !! GaussRadauLeft
    !! GaussRadauRight
    !! GaussLobatto
  END SUBROUTINE LegendreQuadrature
END INTERFACE

PUBLIC :: LegendreQuadrature

!----------------------------------------------------------------------------
!                                                             LegendreEvalAll
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

INTERFACE
  MODULE PURE FUNCTION LegendreEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
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
!                                                             LegendreEvalUpto
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

INTERFACE
  MODULE PURE FUNCTION LegendreEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Legendre polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LegendreEvalAll2
END INTERFACE

INTERFACE LegendreEvalAll
  MODULE PROCEDURE LegendreEvalAll2
END INTERFACE LegendreEvalAll

!----------------------------------------------------------------------------
!                                                             LegendreEval
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

INTERFACE
  MODULE PURE FUNCTION LegendreEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEval1
END INTERFACE

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval1
END INTERFACE LegendreEval

PUBLIC :: LegendreEval

!----------------------------------------------------------------------------
!                                                             LegendreEvalUpto
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

INTERFACE
  MODULE PURE FUNCTION LegendreEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEval2
END INTERFACE

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval2
END INTERFACE LegendreEval

END MODULE LegendrePolynomialUtility
