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
! summary: Utility related to Ultraspherical Polynomials is defined.
!
!{!pages/UltrasphericalPolynomialUtility.md!}

MODULE UltrasphericalPolynomialUtility
USE GlobalData
USE BaseType, ONLY: iface_1DFunction
IMPLICIT NONE
PRIVATE
PUBLIC :: UltrasphericalAlpha
PUBLIC :: UltrasphericalBeta
PUBLIC :: GetUltrasphericalRecurrenceCoeff
PUBLIC :: GetUltrasphericalRecurrenceCoeff2
PUBLIC :: UltrasphericalLeadingCoeff
PUBLIC :: UltrasphericalLeadingCoeffRatio
PUBLIC :: UltrasphericalNormSQR
PUBLIC :: UltrasphericalNormSQR2
PUBLIC :: UltrasphericalNormSQRRatio
PUBLIC :: UltrasphericalJacobiMatrix
PUBLIC :: UltrasphericalGaussQuadrature
PUBLIC :: UltrasphericalJacobiRadauMatrix
PUBLIC :: UltrasphericalGaussRadauQuadrature
PUBLIC :: UltrasphericalJacobiLobattoMatrix
PUBLIC :: UltrasphericalGaussLobattoQuadrature
PUBLIC :: UltrasphericalZeros
PUBLIC :: UltrasphericalQuadrature
PUBLIC :: UltrasphericalEval
PUBLIC :: UltrasphericalEvalAll
PUBLIC :: UltrasphericalEvalAll_
PUBLIC :: UltrasphericalGradientEvalAll
PUBLIC :: UltrasphericalGradientEvalAll_
PUBLIC :: UltrasphericalGradientEval
PUBLIC :: UltrasphericalEvalSum
PUBLIC :: UltrasphericalGradientEvalSum
PUBLIC :: UltrasphericalTransform
PUBLIC :: UltrasphericalInvTransform
PUBLIC :: UltrasphericalGradientCoeff
PUBLIC :: UltrasphericalDMatrix
PUBLIC :: UltrasphericalDMatEvenOdd

!----------------------------------------------------------------------------
!                                                        UltrasphericalAlpha
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, alpha , of Ultraspherical polynomial

INTERFACE
  MODULE PURE FUNCTION UltrasphericalAlpha(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
    !! answer
  END FUNCTION UltrasphericalAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                        UltrasphericalBeta
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, beta, of Ultraspherical polynomial

INTERFACE
  MODULE PURE FUNCTION UltrasphericalBeta(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
    !! answer
  END FUNCTION UltrasphericalBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetUltrasphericalRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Return the recurrence coefficient for nth order polynomial (monic)

INTERFACE
  MODULE PURE SUBROUTINE GetUltrasphericalRecurrenceCoeff(n, &
    & lambda, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    !! lambda should not be zero
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetUltrasphericalRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetUltrasphericalRecurrenceCoeff2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Return the recurrence coefficient for nth order polynomial (monic)

INTERFACE
  MODULE PURE SUBROUTINE GetUltrasphericalRecurrenceCoeff2(n, lambda, &
    & A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    !! lambda should not be 0.0
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    !! size is n
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    !! this coefficient is zero
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
    !! size is n
  END SUBROUTINE GetUltrasphericalRecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 UltrasphericalLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Leading coefficient of Ultraspherical polynomial

INTERFACE
  MODULE PURE FUNCTION UltrasphericalLeadingCoeff(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
    !! answer
  END FUNCTION UltrasphericalLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                           UltrasphericalLeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary: Ratio of leading coefficients, kn+1/kn

INTERFACE
  MODULE PURE FUNCTION UltrasphericalLeadingCoeffRatio(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
    !! answer
  END FUNCTION UltrasphericalLeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                     UltrasphericalNormSQR
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of Ultraspherical polynomial

INTERFACE
  MODULE PURE FUNCTION UltrasphericalNormSQR(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
  END FUNCTION UltrasphericalNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                     UltrasphericalNormSQR2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of Ultraspherical polynomial

INTERFACE
  MODULE PURE FUNCTION UltrasphericalNormSQR2(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans(0:n)
  END FUNCTION UltrasphericalNormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm ration of Ultraspherical polynomial, n+1/n

INTERFACE
  MODULE PURE FUNCTION UltrasphericalNormSQRRatio(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans
  END FUNCTION UltrasphericalNormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                 UltrasphericalJacobiMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Return the Jacobi matrix for Ultraspherical polynomial

INTERFACE
  MODULE PURE SUBROUTINE UltrasphericalJacobiMatrix(n, lambda, D, E, &
    &  alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    !! recurrence coefficient of monic Ultraspherical polynomial, from 0 to n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
    !! recurrence coefficient of monic Ultraspherical polynomial, from 0 to n-1
  END SUBROUTINE UltrasphericalJacobiMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                             UltrasphericalGaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss quadrature points for Ultraspherical Polynomial

INTERFACE
  MODULE SUBROUTINE UltrasphericalGaussQuadrature(n, lambda, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE UltrasphericalGaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                            UltrasphericalJacobiRadauMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE UltrasphericalJacobiRadauMatrix(a, n, lambda, D, E, &
    & alphaCoeff, betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE UltrasphericalJacobiRadauMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                         UltrasphericalGaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Radau quadrature points for Ultraspherical
! Polynomial

INTERFACE
  MODULE SUBROUTINE UltrasphericalGaussRadauQuadrature(a, n, lambda, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! the value of one of the end points
    !! it should be either -1 or +1
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE UltrasphericalGaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                  UltrasphericalUltrasphericalLobattoMatrix
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE UltrasphericalJacobiLobattoMatrix(n, lambda, D, E, &
    & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE UltrasphericalJacobiLobattoMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                      UltrasphericalGaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the Gauss-Lobatto quadrature points for Ultraspherical
! Polynomial

INTERFACE
  MODULE SUBROUTINE UltrasphericalGaussLobattoQuadrature(n, lambda, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomials
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE UltrasphericalGaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                        UltrasphericalZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary:         Returns zeros of Ultraspherical polynomials

INTERFACE
  MODULE FUNCTION UltrasphericalZeros(n, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP) :: ans(n)
  END FUNCTION UltrasphericalZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                  UltrasphericalQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: This routine can return Ultraspherical-Gauss, Ultraspherical-Radau,
! Ultraspherical-Lobatto
!
!# Introduction
!
! This routine returns the Quadrature point of Ultraspherical polynomial
!
!@note
! Here n is the number of quadrature points. Please note it is not
! the order of Ultraspherical polynomial. The order is decided internally
! depending upon the quadType
!@endnote
!
!@note
! pt and wt should be allocated outside, and length should be n.
!@endnote
!

INTERFACE
  MODULE SUBROUTINE UltrasphericalQuadrature(n, lambda, pt, wt, &
    & quadType, onlyInside)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Ultraspherical polynomial
    !! for quadType = GaussRadauLeft or GaussRadauRight n is order+1
    !! for quadType = GaussLobatto, n = order+2
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
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
  END SUBROUTINE UltrasphericalQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                        UltrasphericalEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE
  MODULE PURE FUNCTION UltrasphericalEval1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEval1
END INTERFACE

INTERFACE UltrasphericalEval
  MODULE PROCEDURE UltrasphericalEval1
END INTERFACE UltrasphericalEval

!----------------------------------------------------------------------------
!                                                          UltrasphericalEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE
  MODULE PURE FUNCTION UltrasphericalEval2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEval2
END INTERFACE

INTERFACE UltrasphericalEval
  MODULE PROCEDURE UltrasphericalEval2
END INTERFACE UltrasphericalEval

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE UltrasphericalEvalAll
  MODULE PURE FUNCTION UltrasphericalEvalAll1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Ultraspherical polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION UltrasphericalEvalAll1
END INTERFACE UltrasphericalEvalAll

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE UltrasphericalEvalAll_
  MODULE PURE SUBROUTINE UltrasphericalEvalAll1_(n, lambda, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    ! REAL(DFP) :: ans(n + 1)
    !! Evaluate Ultraspherical polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE UltrasphericalEvalAll1_
END INTERFACE UltrasphericalEvalAll_

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE UltrasphericalEvalAll
  MODULE PURE FUNCTION UltrasphericalEvalAll2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Ultraspherical polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION UltrasphericalEvalAll2
END INTERFACE UltrasphericalEvalAll

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Ultraspherical polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Ultraspherical polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Ultraspherical polynomials at
! the point
! X.

INTERFACE UltrasphericalEvalAll_
 MODULE PURE SUBROUTINE UltrasphericalEvalAll2_(n, lambda, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Ultraspherical polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow, ncol
  END SUBROUTINE UltrasphericalEvalAll2_
END INTERFACE UltrasphericalEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE UltrasphericalGradientEvalAll
  MODULE PURE FUNCTION UltrasphericalGradientEvalAll1(n, lambda, x) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UltrasphericalGradientEvalAll1
END INTERFACE UltrasphericalGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE UltrasphericalGradientEvalAll_
  MODULE PURE SUBROUTINE UltrasphericalGradientEvalAll1_(n, lambda, x, ans, &
                                                         tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! 1:n+1
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE UltrasphericalGradientEvalAll1_
END INTERFACE UltrasphericalGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE UltrasphericalGradientEvalAll
  MODULE PURE FUNCTION UltrasphericalGradientEvalAll2(n, lambda, x) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION UltrasphericalGradientEvalAll2
END INTERFACE UltrasphericalGradientEvalAll

!----------------------------------------------------------------------------
!                                             UltraSphericalGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE UltrasphericalGradientEvalAll_
  MODULE PURE SUBROUTINE UltrasphericalGradientEvalAll2_(n, lambda, x, ans, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(1:SIZE(x), 1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow
    INTEGER(I4B), INTENT(OUT) :: ncol
  END SUBROUTINE UltrasphericalGradientEvalAll2_
END INTERFACE UltrasphericalGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEval1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UltrasphericalGradientEval1
END INTERFACE
!!

INTERFACE UltrasphericalGradientEval
  MODULE PROCEDURE UltrasphericalGradientEval1
END INTERFACE UltrasphericalGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Ultraspherical polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Ultraspherical polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEval2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UltrasphericalGradientEval2
END INTERFACE

INTERFACE UltrasphericalGradientEval
  MODULE PROCEDURE UltrasphericalGradientEval2
END INTERFACE UltrasphericalGradientEval

!----------------------------------------------------------------------------
!                                                     UltrasphericalEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Ultraspherical polynomials at point x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalEvalSum1(n, lambda, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! alpha of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEvalSum1
END INTERFACE

INTERFACE UltrasphericalEvalSum
  MODULE PROCEDURE UltrasphericalEvalSum1
END INTERFACE UltrasphericalEvalSum

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Ultraspherical polynomials at several x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalEvalSum2(n, lambda, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! alpha of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEvalSum2
END INTERFACE

INTERFACE UltrasphericalEvalSum
  MODULE PROCEDURE UltrasphericalEvalSum2
END INTERFACE UltrasphericalEvalSum

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Ultraspherical polynomials
! at point x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEvalSum1(n, lambda, x, &
    &  coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalGradientEvalSum1
END INTERFACE

INTERFACE UltrasphericalGradientEvalSum
  MODULE PROCEDURE UltrasphericalGradientEvalSum1
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                              UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Ultraspherical polynomials
! at several x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEvalSum2(n, lambda, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalGradientEvalSum2
END INTERFACE

INTERFACE UltrasphericalGradientEvalSum
  MODULE PROCEDURE UltrasphericalGradientEvalSum2
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                              UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth derivative of finite sum of Ultraspherical
! polynomials at point x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEvalSum3(n, lambda, x, &
    & coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalGradientEvalSum3
END INTERFACE

INTERFACE UltrasphericalGradientEvalSum
  MODULE PROCEDURE UltrasphericalGradientEvalSum3
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth gradient of finite sum of Ultraspherical
!  polynomials at several x

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientEvalSum4(n, lambda, x, &
    &  coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalGradientEvalSum4
END INTERFACE

INTERFACE UltrasphericalGradientEvalSum
  MODULE PROCEDURE UltrasphericalGradientEvalSum4
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                                  UltrasphericalTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Transform

INTERFACE
  MODULE PURE FUNCTION UltrasphericalTransform1(n, lambda, coeff, x, w, &
    &  quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
  END FUNCTION UltrasphericalTransform1
END INTERFACE

INTERFACE UltrasphericalTransform
  MODULE PROCEDURE UltrasphericalTransform1
END INTERFACE UltrasphericalTransform

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Columnwise Discrete Ultraspherical Transform

INTERFACE
  MODULE PURE FUNCTION UltrasphericalTransform2(n, lambda, coeff, x, w, &
    & quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
  END FUNCTION UltrasphericalTransform2
END INTERFACE

INTERFACE UltrasphericalTransform
  MODULE PROCEDURE UltrasphericalTransform2
END INTERFACE UltrasphericalTransform

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Ultraspherical Transform of a function on [-1,1]
!
!# Introduction
!
! This function performs the Ultraspherical transformation of f defined
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
!  `UltrasphericalQuadrature` which is not pure due to Lapack call.
!@endnote

INTERFACE
  MODULE FUNCTION UltrasphericalTransform3(n, lambda, f, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION UltrasphericalTransform3
END INTERFACE

INTERFACE UltrasphericalTransform
  MODULE PROCEDURE UltrasphericalTransform3
END INTERFACE UltrasphericalTransform

!----------------------------------------------------------------------------
!                                                 UltrasphericalInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Discrete Ultraspherical Transform

INTERFACE
  MODULE PURE FUNCTION UltrasphericalInvTransform1(n, lambda, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION UltrasphericalInvTransform1
END INTERFACE

INTERFACE UltrasphericalInvTransform
  MODULE PROCEDURE UltrasphericalInvTransform1
END INTERFACE UltrasphericalInvTransform

!----------------------------------------------------------------------------
!                                                 UltrasphericalInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary:         Discrete Ultraspherical Transform

INTERFACE
  MODULE PURE FUNCTION UltrasphericalInvTransform2(n, lambda, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION UltrasphericalInvTransform2
END INTERFACE

INTERFACE UltrasphericalInvTransform
  MODULE PROCEDURE UltrasphericalInvTransform2
END INTERFACE UltrasphericalInvTransform

!----------------------------------------------------------------------------
!                                               UltrasphericalGradientCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficient for gradient of Ultraspherical expansion
!
!# Introduction
!
! This routine returns the coefficients of gradient of Jacobi expansion.
! Input is cofficients of Jacobipolynomials (modal values).

INTERFACE
  MODULE PURE FUNCTION UltrasphericalGradientCoeff1(n, lambda, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from UltrasphericalTransform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION UltrasphericalGradientCoeff1
END INTERFACE

INTERFACE UltrasphericalGradientCoeff
  MODULE PROCEDURE UltrasphericalGradientCoeff1
END INTERFACE UltrasphericalGradientCoeff

!----------------------------------------------------------------------------
!                                                     UltrasphericalDMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficient for gradient of Ultraspherical expansion
!
!# Introduction
!
! This routine returns the coefficients of gradient of Ultraspherical
! expansion.
! Input is cofficients of Ultrasphericalpolynomials (modal values).

INTERFACE
  MODULE PURE FUNCTION UltrasphericalDMatrix1(n, lambda, x, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION UltrasphericalDMatrix1
END INTERFACE

INTERFACE UltrasphericalDMatrix
  MODULE PROCEDURE UltrasphericalDMatrix1
END INTERFACE UltrasphericalDMatrix

!----------------------------------------------------------------------------
!                                                 UltrasphericalDMatEvenOdd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficient for gradient of Ultraspherical expansion
!
!# Introduction
!
! This routine returns the coefficients of gradient of Ultraspherical
! expansion.
! Input is cofficients of Ultrasphericalpolynomials (modal values).
!

INTERFACE
  MODULE PURE SUBROUTINE UltrasphericalDMatEvenOdd1(n, D, e, o)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Ultraspherical polynomial
    REAL(DFP), INTENT(IN) :: D(0:n, 0:n)
      !! n+1 by n+1
    REAL(DFP), INTENT(OUT) :: e(0:, 0:)
      !! even Decomposition
    REAL(DFP), INTENT(OUT) :: o(0:, 0:)
      !! odd decomposition
  END SUBROUTINE UltrasphericalDMatEvenOdd1
END INTERFACE

INTERFACE UltrasphericalDMatEvenOdd
  MODULE PROCEDURE UltrasphericalDMatEvenOdd1
END INTERFACE UltrasphericalDMatEvenOdd

END MODULE UltrasphericalPolynomialUtility
