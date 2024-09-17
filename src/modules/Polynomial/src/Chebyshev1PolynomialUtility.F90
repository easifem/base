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

MODULE Chebyshev1PolynomialUtility
USE GlobalData, ONLY: DFP, I4B, LGT

USE BaseType, ONLY: iface_1DFunction

IMPLICIT NONE

PUBLIC :: Chebyshev1Alpha
PUBLIC :: Chebyshev1Beta
PUBLIC :: GetChebyshev1RecurrenceCoeff
PUBLIC :: GetChebyshev1RecurrenceCoeff2
PUBLIC :: Chebyshev1LeadingCoeff
PUBLIC :: Chebyshev1LeadingCoeffRatio
PUBLIC :: Chebyshev1NormSQR
PUBLIC :: Chebyshev1NormSQR2
PUBLIC :: Chebyshev1NormSQRRatio
PUBLIC :: Chebyshev1JacobiMatrix
PUBLIC :: Chebyshev1GaussQuadrature
PUBLIC :: Chebyshev1JacobiRadauMatrix
PUBLIC :: Chebyshev1GaussRadauQuadrature
PUBLIC :: Chebyshev1JacobiLobattoMatrix
PUBLIC :: Chebyshev1GaussLobattoQuadrature
PUBLIC :: Chebyshev1Zeros
PUBLIC :: Chebyshev1Quadrature
PUBLIC :: Chebyshev1Eval
PUBLIC :: Chebyshev1EvalAll
PUBLIC :: Chebyshev1MonomialExpansionAll
PUBLIC :: Chebyshev1MonomialExpansion
PUBLIC :: Chebyshev1GradientEvalAll
PUBLIC :: Chebyshev1GradientEvalAll_
PUBLIC :: Chebyshev1GradientEval
PUBLIC :: Chebyshev1EvalSum
PUBLIC :: Chebyshev1GradientEvalSum
PUBLIC :: Chebyshev1Transform
PUBLIC :: Chebyshev1Transform_
PUBLIC :: Chebyshev1InvTransform
PUBLIC :: Chebyshev1GradientCoeff
PUBLIC :: Chebyshev1DMatrix
PUBLIC :: Chebyshev1DMatEvenOdd

!----------------------------------------------------------------------------
!                                                        Chebyshev1Alpha
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, beta, of Chebyshev1 polynomial

INTERFACE
  MODULE PURE FUNCTION Chebyshev1Alpha(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev1 polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION Chebyshev1Alpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Chebyshev1Beta
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Recurrence coefficient, beta, of Chebyshev1 polynomial

INTERFACE
  MODULE PURE FUNCTION Chebyshev1Beta(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev1 polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION Chebyshev1Beta
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetChebyshev1RecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order Chebyshev1
! polynomial
!
!
!# Introduction
!
! These recurrence coefficients are for monic jacobi polynomials.

INTERFACE
  MODULE PURE SUBROUTINE GetChebyshev1RecurrenceCoeff(n, alphaCoeff, &
    & betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetChebyshev1RecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetChebyshev1RecurrenceCoeff2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order Chebyshev1
! polynomial
!
!
!# Introduction
!
! These recurrence coefficients are for monic jacobi polynomials.

INTERFACE
  MODULE PURE SUBROUTINE GetChebyshev1RecurrenceCoeff2(n, A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    !! size is n
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    !! this coefficient is zero
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
    !! size is n
  END SUBROUTINE GetChebyshev1RecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Chebyshev1LeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Leading coefficient of Chebyshev1 polynomial

INTERFACE
  MODULE PURE FUNCTION Chebyshev1LeadingCoeff(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev1 polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION Chebyshev1LeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                               Chebyshev1LeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary:         Ratio of leading coefficients, kn+1/kn

INTERFACE
  MODULE PURE FUNCTION Chebyshev1LeadingCoeffRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev1 polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION Chebyshev1LeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Chebyshev1NormSQR
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Chebyshev1 polynomial

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1NormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Chebyshev1NormSQR2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Chebyshev1 polynomial

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQR2(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n)
  END FUNCTION Chebyshev1NormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Chebyshev1NormSQRRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Ratio of Square norm of Chebyshev1 polynomial, n+1/n

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQRRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1NormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Chebyshev1JacobiMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary: Return the Jacobi matrix for Chebyshev polynomial

INTERFACE
  MODULE PURE SUBROUTINE Chebyshev1JacobiMatrix(n, D, E, &
    & alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    !! recurrence coefficient of monic Chebyshev polynomial, from 0 to n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
    !! recurrence coefficient of monic Chebyshev polynomial, from 0 to n-1
  END SUBROUTINE Chebyshev1JacobiMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Chebyshev1GaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary: Return the Jacobi matrix for Chebyshev polynomial

INTERFACE
  MODULE SUBROUTINE Chebyshev1GaussQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev polynomial.
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE Chebyshev1GaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                               Chebyshev1JacobiRadauMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary: Return the Jacobi-Radau matrix for Chebyshev polynomial

INTERFACE
  MODULE PURE SUBROUTINE Chebyshev1JacobiRadauMatrix(a, n, D, E, alphaCoeff, &
    & betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial.
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE Chebyshev1JacobiRadauMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                            Chebyshev1GaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Returns the GaussRadau quadrature points for Chebyshev1 Polynomial

INTERFACE
  MODULE SUBROUTINE Chebyshev1GaussRadauQuadrature(a, n, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! +1.0 or -1.0
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n+1
  END SUBROUTINE Chebyshev1GaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                             Chebyshev1JacobiLobattoMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2022
! summary: Return the Jacobi-Lobatto matrix for Chebyshev polynomial

INTERFACE
  MODULE PURE SUBROUTINE Chebyshev1JacobiLobattoMatrix(n, D, E, alphaCoeff, &
    & betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE Chebyshev1JacobiLobattoMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                            Chebyshev1GaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary:Returns the GaussLobatto quadrature points for Chebyshev1 Polynomial

INTERFACE
  MODULE SUBROUTINE Chebyshev1GaussLobattoQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n+2
  END SUBROUTINE Chebyshev1GaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Chebyshev1Zeros
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION Chebyshev1Zeros(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Chebyshev polynomial
    REAL(DFP) :: ans(n)
  END FUNCTION Chebyshev1Zeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Chebyshev1Quadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: This routine can return Chebyshev-Gauss, Chebyshev-Radau,
! Chebyshev-Lobatto
!
!# Introduction
!
! This routine returns the Quadrature point of Chebyshev polynomial
!
!@note
! Here n is the number of quadrature points. Please note it is not
! the order of Chebyshev polynomial. The order is decided internally
! depending upon the quadType
!@endnote
!
!@note
! pt and wt should be allocated outside, and length should be n.
!@endnote
!

INTERFACE
  MODULE SUBROUTINE Chebyshev1Quadrature(n, pt, wt, quadType, onlyInside)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Chebyshev polynomial
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
  END SUBROUTINE Chebyshev1Quadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Chebyshev1Eval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Chebyshev1 polynomials of order = n at single x

INTERFACE Chebyshev1Eval
  MODULE PURE FUNCTION Chebyshev1Eval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1Eval1
END INTERFACE Chebyshev1Eval

!----------------------------------------------------------------------------
!                                                          Chebyshev1Eval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Chebyshev1 polynomials of order n at several points

INTERFACE Chebyshev1Eval
  MODULE PURE FUNCTION Chebyshev1Eval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! several points of evaluation
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1Eval2
END INTERFACE Chebyshev1Eval

!----------------------------------------------------------------------------
!                                                         Chebyshev1EvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Chebyshev1 polynomials from order = 0 to n at single point
!
!# Introduction
!
! Evaluate Chebyshev1 polynomials from order = 0 to n at single point
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- x: the point at which the polynomials are to be evaluated.
!- ans(1:N+1), the values of the first N+1 Chebyshev1 polynomials at the
! point

INTERFACE Chebyshev1EvalAll
  MODULE PURE FUNCTION Chebyshev1EvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Chebyshev1 polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION Chebyshev1EvalAll1
END INTERFACE Chebyshev1EvalAll

!----------------------------------------------------------------------------
!                                                       Chebyshev1EvalAll
!----------------------------------------------------------------------------

INTERFACE Chebyshev1EvalAll_
  MODULE PURE SUBROUTINE Chebyshev1EvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:)
    ! ans(n + 1)
    !! Evaluate Chebyshev1 polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE Chebyshev1EvalAll1_
END INTERFACE Chebyshev1EvalAll_

!----------------------------------------------------------------------------
!                                                           Chebyshev1EvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Chebyshev1 polynomials from order = 0 to n at several
! points
!
!# Introduction
!
! Evaluate Chebyshev1 polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- x: the points at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Chebyshev1 polynomials at the
! points x(1:m)

INTERFACE Chebyshev1EvalAll
  MODULE PURE FUNCTION Chebyshev1EvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! several points of evaluation
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Chebyshev1 polynomial of order = 0 to n (total n+1)
    !! at points x
  END FUNCTION Chebyshev1EvalAll2
END INTERFACE Chebyshev1EvalAll

!----------------------------------------------------------------------------
!                                                         ChebyshevEvalAll2_
!----------------------------------------------------------------------------

INTERFACE Chebyshev1EvalAll_
  MODULE PURE SUBROUTINE Chebyshev1EvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! several points of evaluation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Evaluate Chebyshev1 polynomial of order = 0 to n (total n+1)
    !! at points x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Chebyshev1EvalAll2_
END INTERFACE Chebyshev1EvalAll_

!----------------------------------------------------------------------------
!                                             Chebyshev1MonomialExpansionAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Returns the monomial expansion of all Chebyshev1 polynomials
!
!# Introduction
!
! Returns all the monomial expansion of all Chebyshev1 polynomials
!
!- n : is the order of the polynomial
!- ans(:,i) contains the coefficient of monomials for polynomial order=i-1
!
! for example, n=5, we have following structure of ans
!
! | P0 | P1 | P2 | P3 | P4 | P5  |
! |----|----|----|----|----|-----|
! | 1  | 0  | -1 | -0 | 1  | 0   |
! | 0  | 1  | 0  | -3 | -0 | 5   |
! | 0  | 0  | 2  | 0  | -8 | -0  |
! | 0  | 0  | 0  | 4  | 0  | -20 |
! | 0  | 0  | 0  | 0  | 8  | 0   |
! | 0  | 0  | 0  | 0  | 0  | 16  |

INTERFACE
  MODULE PURE FUNCTION Chebyshev1MonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION Chebyshev1MonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                             Chebyshev1MonomialExpansion
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Returns the monomial expansion of a Chebyshev1 polynomials
!
!# Introduction
!
! Returns all the monomial expansion of a Chebyshev1 polynomials
!
!- n : is the order of the polynomial
!- ans(:) contains the coefficient of monomials for polynomial order=n
!

INTERFACE
  MODULE PURE FUNCTION Chebyshev1MonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION Chebyshev1MonomialExpansion
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Chebyshev1 polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Chebyshev1 polynomial of order upto n.

INTERFACE Chebyshev1GradientEvalAll
  MODULE PURE FUNCTION Chebyshev1GradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION Chebyshev1GradientEvalAll1
END INTERFACE Chebyshev1GradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Chebyshev1GradientEvalAll_
  MODULE PURE SUBROUTINE Chebyshev1GradientEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! ans(1:n + 1)
  END SUBROUTINE Chebyshev1GradientEvalAll1_
END INTERFACE Chebyshev1GradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Chebyshev1 polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Chebyshev1 polynomial of order upto n.

INTERFACE Chebyshev1GradientEvalAll
  MODULE PURE FUNCTION Chebyshev1GradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION Chebyshev1GradientEvalAll2
END INTERFACE Chebyshev1GradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Chebyshev1GradientEvalAll_
  MODULE PURE SUBROUTINE Chebyshev1GradientEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(1:SIZE(x), 1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Chebyshev1GradientEvalAll2_
END INTERFACE Chebyshev1GradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Chebyshev1 polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Chebyshev1 polynomial of order upto n.

INTERFACE Chebyshev1GradientEval
  MODULE PURE FUNCTION Chebyshev1GradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1GradientEval1
END INTERFACE Chebyshev1GradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of Chebyshev1 polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Chebyshev1 polynomial of order upto n.

INTERFACE Chebyshev1GradientEval
  MODULE PURE FUNCTION Chebyshev1GradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION Chebyshev1GradientEval2
END INTERFACE Chebyshev1GradientEval

!----------------------------------------------------------------------------
!                                                         Chebyshev1EvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Chebyshev1 polynomials at point x

INTERFACE Chebyshev1EvalSum
  MODULE PURE FUNCTION Chebyshev1EvalSum1(n, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1EvalSum1
END INTERFACE Chebyshev1EvalSum

!----------------------------------------------------------------------------
!                                                         Chebyshev1EvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate finite sum of Chebyshev1 polynomials at several x

INTERFACE Chebyshev1EvalSum
  MODULE PURE FUNCTION Chebyshev1EvalSum2(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1EvalSum2
END INTERFACE Chebyshev1EvalSum

!----------------------------------------------------------------------------
!                                                   Chebyshev1GradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Chebyshev1 polynomials
! at point x

INTERFACE Chebyshev1GradientEvalSum
  MODULE PURE FUNCTION Chebyshev1GradientEvalSum1(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1GradientEvalSum1
END INTERFACE Chebyshev1GradientEvalSum

!----------------------------------------------------------------------------
!                                                  Chebyshev1GradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the gradient of finite sum of Chebyshev1 polynomials
! at several x

INTERFACE Chebyshev1GradientEvalSum
  MODULE PURE FUNCTION Chebyshev1GradientEvalSum2(n, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1GradientEvalSum2
END INTERFACE Chebyshev1GradientEvalSum

!----------------------------------------------------------------------------
!                                                   Chebyshev1GradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth derivative of finite sum of Chebyshev1
! polynomials at point x

INTERFACE Chebyshev1GradientEvalSum
  MODULE PURE FUNCTION Chebyshev1GradientEvalSum3(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1GradientEvalSum3
END INTERFACE Chebyshev1GradientEvalSum

!----------------------------------------------------------------------------
!                                                   Chebyshev1GradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate the kth gradient of finite sum of Chebyshev1
!  polynomials at several x

INTERFACE Chebyshev1GradientEvalSum
  MODULE PURE FUNCTION Chebyshev1GradientEvalSum4(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Chebyshev1 polynomial of order n at point x
  END FUNCTION Chebyshev1GradientEvalSum4
END INTERFACE Chebyshev1GradientEvalSum

!----------------------------------------------------------------------------
!                                                         Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Chebyshev1 Transform

INTERFACE Chebyshev1Transform
  MODULE PURE FUNCTION Chebyshev1Transform1(n, coeff, x, w, &
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
  END FUNCTION Chebyshev1Transform1
END INTERFACE Chebyshev1Transform

!----------------------------------------------------------------------------
!                                                     Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-19
! summary:  Discrete Chebyshev1 Transform

INTERFACE Chebyshev1Transform_
  MODULE PURE SUBROUTINE Chebyshev1Transform1_(n, coeff, x, w, &
                                               quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    REAL(DFP), INTENT(IN) :: x(0:)
    !! quadrature points
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! ans(0:n)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! tsize = n+1
  END SUBROUTINE Chebyshev1Transform1_
END INTERFACE Chebyshev1Transform_

!----------------------------------------------------------------------------
!                                                     Chebyshev1Transform
!----------------------------------------------------------------------------

INTERFACE Chebyshev1Transform_
  MODULE PURE SUBROUTINE Chebyshev1Transform4_(n, coeff, PP, w, &
                                               quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    REAL(DFP), INTENT(IN) :: PP(0:, 0:)
    !! quadrature points
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! ans(0:n)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! tsize = n+1
  END SUBROUTINE Chebyshev1Transform4_
END INTERFACE Chebyshev1Transform_

!----------------------------------------------------------------------------
!                                                   Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Chebyshev1 Transform of a function on [-1,1]
!
!# Introduction
!
! This function performs the Chebyshev1 transformation of f defined
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
!  `Chebyshev1Quadrature` which is not pure due to Lapack call.
!@endnote

INTERFACE Chebyshev1Transform
  MODULE FUNCTION Chebyshev1Transform3(n, f, quadType, x1, x2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! x1, x2 are the end points of the interval
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION Chebyshev1Transform3
END INTERFACE Chebyshev1Transform

!----------------------------------------------------------------------------
!                                                       Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-08-19
! summary:  Chebyshev1 Transform of a function on [-1,1]

INTERFACE Chebyshev1Transform_
  MODULE SUBROUTINE Chebyshev1Transform3_(n, f, quadType, x1, x2, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! x1, x2 are the end points of the interval
    REAL(DFP) :: ans(0:)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! tsize = n+1
  END SUBROUTINE Chebyshev1Transform3_
END INTERFACE Chebyshev1Transform_

!----------------------------------------------------------------------------
!                                                         Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Discrete Chebyshev1 Transform
!
!# Introduction
! Discrete Chebyshev transform. We calculate weights and quadrature points
! internally.

INTERFACE Chebyshev1Transform
  MODULE PURE FUNCTION Chebyshev1Transform2(n, coeff, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    !! size if quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION Chebyshev1Transform2
END INTERFACE Chebyshev1Transform

!----------------------------------------------------------------------------
!                                                       Chebyshev1Transform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  Discrete Chebyshev1 Transform

INTERFACE Chebyshev1Transform_
  MODULE PURE SUBROUTINE Chebyshev1Transform2_(n, coeff, quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    !! size is quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! tsize = n+1
  END SUBROUTINE Chebyshev1Transform2_
END INTERFACE Chebyshev1Transform_

!----------------------------------------------------------------------------
!                                                 Chebyshev1InvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Inverse Chebyshev1 Transform

INTERFACE Chebyshev1InvTransform
  MODULE PURE FUNCTION Chebyshev1InvTransform1(n, coeff, x) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION Chebyshev1InvTransform1
END INTERFACE Chebyshev1InvTransform

!----------------------------------------------------------------------------
!                                                 Chebyshev1InvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Inverse Chebyshev1 Transform

INTERFACE Chebyshev1InvTransform
  MODULE PURE FUNCTION Chebyshev1InvTransform2(n, coeff, x) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION Chebyshev1InvTransform2
END INTERFACE Chebyshev1InvTransform

!----------------------------------------------------------------------------
!                                               Chebyshev1GradientCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 13 Oct 2022
! summary: Returns coefficients for gradient of Chebyshev1 expansion
!
!# Introduction
!
!- This routine returns the coefficients of gradient of Jacobi expansion.
!- Input is coefficient of Chebyshev1 expansion (modal values)
!- Output is coefficient of derivative of Chebyshev1 expansion (modal values)

INTERFACE Chebyshev1GradientCoeff
  MODULE PURE FUNCTION Chebyshev1GradientCoeff1(n, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from Chebyshev1Transform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION Chebyshev1GradientCoeff1
END INTERFACE Chebyshev1GradientCoeff

!----------------------------------------------------------------------------
!                                                           Chebyshev1DMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 Oct 2022
! summary: Returns differentiation matrix for Chebyshev1 expansion

INTERFACE Chebyshev1DMatrix
  MODULE PURE FUNCTION Chebyshev1DMatrix1(n, x, quadType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Chebyshev1 polynomial
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION Chebyshev1DMatrix1
END INTERFACE Chebyshev1DMatrix

!----------------------------------------------------------------------------
!                                                 Chebyshev1DMatEvenOdd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 Oct 2022
! summary: Performs even and odd decomposition of Differential matrix

INTERFACE Chebyshev1DMatEvenOdd
  MODULE PURE SUBROUTINE Chebyshev1DMatEvenOdd1(n, D, e, o)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Chebyshev1 polynomial
    REAL(DFP), INTENT(IN) :: D(0:n, 0:n)
      !! n+1 by n+1
    REAL(DFP), INTENT(OUT) :: e(0:, 0:)
      !! even Decomposition, 0:n/2, 0:n/2
    REAL(DFP), INTENT(OUT) :: o(0:, 0:)
      !! odd decomposition, 0:n/2, 0:n/2
  END SUBROUTINE Chebyshev1DMatEvenOdd1
END INTERFACE Chebyshev1DMatEvenOdd

END MODULE Chebyshev1PolynomialUtility
