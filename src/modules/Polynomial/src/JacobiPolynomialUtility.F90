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
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                  GetJacobiRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order polynomial
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

PUBLIC :: GetJacobiRecurrenceCoeff

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

PUBLIC :: JacobiLeadingCoeff

!----------------------------------------------------------------------------
!
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

PUBLIC :: JacobiJacobiMatrix

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
    REAL(DFP), INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE JacobiGaussQuadrature
END INTERFACE

PUBLIC :: JacobiGaussQuadrature

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

PUBLIC :: JacobiJacobiRadauMatrix

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
    REAL(DFP), INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE JacobiGaussRadauQuadrature
END INTERFACE

PUBLIC :: JacobiGaussRadauQuadrature

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

PUBLIC :: JacobiJacobiLobattoMatrix

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
    REAL(DFP), INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE JacobiGaussLobattoQuadrature
END INTERFACE

PUBLIC :: JacobiGaussLobattoQuadrature

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

PUBLIC :: JacobiZeros

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
    REAL(DFP), INTENT(OUT) :: wt(n)
    !! n+1 weights from 1 to n+1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss
    !! GaussRadauLeft
    !! GaussRadauRight
    !! GaussLobatto
  END SUBROUTINE JacobiQuadrature
END INTERFACE

PUBLIC :: JacobiQuadrature

!----------------------------------------------------------------------------
!                                                             JacobiEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials from order = 0 to n at several points
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

INTERFACE
  MODULE PURE FUNCTION JacobiEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll1
END INTERFACE

INTERFACE JacobiEvalAll
  MODULE PROCEDURE JacobiEvalAll1
END INTERFACE JacobiEvalAll

PUBLIC :: JacobiEvalAll

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials from order = 0 to n at several points
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

INTERFACE
  MODULE PURE FUNCTION JacobiEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll2
END INTERFACE

INTERFACE JacobiEvalAll
  MODULE PROCEDURE JacobiEvalAll2
END INTERFACE JacobiEvalAll

!----------------------------------------------------------------------------
!                                                             JacobiEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials from order = 0 to n at several points
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

INTERFACE
  MODULE PURE FUNCTION JacobiEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval1
END INTERFACE

INTERFACE JacobiEval
  MODULE PROCEDURE JacobiEval1
END INTERFACE JacobiEval

PUBLIC :: JacobiEval

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Jacobi polynomials from order = 0 to n at several points
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

INTERFACE
  MODULE PURE FUNCTION JacobiEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval2
END INTERFACE

INTERFACE JacobiEval
  MODULE PROCEDURE JacobiEval2
END INTERFACE JacobiEval

END MODULE JacobiPolynomialUtility
