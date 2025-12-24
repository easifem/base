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
! summary: Utility related to Lobatto Polynomials is defined.
!
!{!pages/LobattoPolynomialUtility.md!}

MODULE LobattoPolynomialUtility
USE GlobalData, ONLY: I4B, DFP, LGT

USE BaseType, ONLY: iface_1DFunction

IMPLICIT NONE

PRIVATE

PUBLIC :: LobattoLeadingCoeff
PUBLIC :: LobattoZeros
PUBLIC :: LobattoEval
PUBLIC :: LobattoEvalAll
PUBLIC :: LobattoEvalAll_
PUBLIC :: LobattoKernelEvalAll
PUBLIC :: LobattoKernelEvalAll_
PUBLIC :: LobattoKernelGradientEvalAll
PUBLIC :: LobattoKernelGradientEvalAll_
PUBLIC :: LobattoMonomialExpansionAll
PUBLIC :: LobattoMonomialExpansion

PUBLIC :: LobattoGradientEvalAll
PUBLIC :: LobattoGradientEvalAll_

PUBLIC :: LobattoGradientEval
PUBLIC :: LobattoMassMatrix
PUBLIC :: LobattoStiffnessMatrix

PUBLIC :: LobattoTransform_

PUBLIC :: Lobatto0, Lobatto1, Lobatto2, Lobatto3, Lobatto4, Lobatto5

PUBLIC :: Lobatto6, Lobatto7, Lobatto8, Lobatto9, Lobatto10

!----------------------------------------------------------------------------
!                                                         LobattoTransform_
!----------------------------------------------------------------------------

INTERFACE LobattoTransform_
  MODULE SUBROUTINE LobattoTransform1_(n, coeff, PP, w, quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! Order of Legendre polynomials
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! Value of function at quadrature points
    !! size of coeff is number of quadrature points
    REAL(DFP), INTENT(IN) :: PP(0:, 0:)
    !! Value of lobatto polynomials
    !! PP(:, jj) value of Pjj at quadrature points
    !! PP(ii, :) value of all lobatto polynomials at point ii
    !! number of rows in PP is number of quadrature points
    !! number of columns in PP is n+1
    REAL(DFP), INTENT(IN) :: w(0:)
    !! Weights for each quadrature points
    !! size of w is number of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type
    !! Gauss, GaussLobatto, GaussRadau, GaussRadauLeft GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients of Legendre polynomial
    !! ans(0) is coefficient of P0
    !! ans(1) is coefficient of P1
    !! and so on
    ! REAL(DFP) :: ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE LobattoTransform1_
END INTERFACE LobattoTransform_

!----------------------------------------------------------------------------
!                                                          LobattoTransform_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-20
! summary:  LobattoTransform

INTERFACE LobattoTransform_
  MODULE SUBROUTINE LobattoTransform2_(n, coeff, x, w, quadType, ans, &
                                       tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! Order of Lobatto polynomials
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! Value of function at quadrature points
    REAL(DFP), INTENT(IN) :: x(0:)
    !! Quadrature points
    !! These quadrature points are used in LobattoEvalAll method
    REAL(DFP), INTENT(IN) :: w(0:)
    !! Weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients of Lobatto polynomial
    !! ans(0) is coefficient of P0
    !! ans(1) is coefficient of P1
    !! and so on
    ! REAL(DFP) :: ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE LobattoTransform2_
END INTERFACE LobattoTransform_

!----------------------------------------------------------------------------
!                                                       LobattoTransform_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-08-20
! summary:  LobattoTransform of function

INTERFACE LobattoTransform_
  MODULE SUBROUTINE LobattoTransform3_(n, f, quadType, x1, x2, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type
    !! Gauss, GaussLobatto, GaussRadau, GaussRadauLeft GaussRadauRight
    !! We will use Legendre quadrature points
    REAL(DFP), INTENT(IN) :: x1, x2
    !! domain of function f
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients
    !! ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! n+1
  END SUBROUTINE LobattoTransform3_
END INTERFACE LobattoTransform_

!----------------------------------------------------------------------------
!                                                         LobattoLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Leading coefficient of Lobatto polynomial

INTERFACE
  MODULE PURE FUNCTION LobattoLeadingCoeff(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Lobatto polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LobattoLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of Lobatto polynomial
!

INTERFACE
  MODULE PURE FUNCTION LobattoNormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION LobattoNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                              LobattoZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary:         Returns zeros of Lobatto polynomials

INTERFACE
  MODULE FUNCTION LobattoZeros(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Lobatto polynomial, should be greater than equal to 2
    REAL(DFP) :: ans(n)
    !!
  END FUNCTION LobattoZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                             LobattoEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Lobatto polynomials at the point
! X.

INTERFACE LobattoEval
  MODULE PURE FUNCTION LobattoEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Lobatto polynomial of order n at point x
  END FUNCTION LobattoEval1
END INTERFACE LobattoEval

!----------------------------------------------------------------------------
!                                                          LobattoEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Lobatto polynomials at the point
! X.

INTERFACE LobattoEval
  MODULE PURE FUNCTION LobattoEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Lobatto polynomial of order n at point x
  END FUNCTION LobattoEval2
END INTERFACE LobattoEval

!----------------------------------------------------------------------------
!                                                             LobattoEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Lobatto polynomials at the point
! X.

INTERFACE LobattoEvalAll
  MODULE PURE FUNCTION LobattoEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LobattoEvalAll1
END INTERFACE LobattoEvalAll

!----------------------------------------------------------------------------
!                                                            LobattoEvalAll_
!----------------------------------------------------------------------------

INTERFACE LobattoEvalAll_
  MODULE PURE SUBROUTINE LobattoEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LobattoEvalAll1_
END INTERFACE LobattoEvalAll_

!----------------------------------------------------------------------------
!                                                           LobattoEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 Lobatto polynomials at the point
! X.

INTERFACE LobattoEvalAll
  MODULE PURE FUNCTION LobattoEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LobattoEvalAll2
END INTERFACE LobattoEvalAll

!----------------------------------------------------------------------------
!                                                            LobattoEvalAll_
!----------------------------------------------------------------------------

INTERFACE LobattoEvalAll_
  MODULE PURE SUBROUTINE LobattoEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x), n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LobattoEvalAll2_
END INTERFACE LobattoEvalAll_

!----------------------------------------------------------------------------
!                                                      LobattoKernelEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto bubble functions order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto bubble polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute.
!- x: the point at which the polynomials are to be evaluated.

INTERFACE LobattoKernelEvalAll
  MODULE PURE FUNCTION LobattoKernelEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 0:n)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LobattoKernelEvalAll1
END INTERFACE LobattoKernelEvalAll

!----------------------------------------------------------------------------
!                                                      LobattoKernelEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate Lobatto bubble functions order = 0 to n at several points
!
!# Introduction
!
! Evaluate Lobatto bubble polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute.
!- x: the point at which the polynomials are to be evaluated.

INTERFACE LobattoKernelEvalAll_
  MODULE PURE SUBROUTINE LobattoKernelEvalAll1_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(1:, 0:)
    !! ans(1:SIZE(x), 0:n)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LobattoKernelEvalAll1_
END INTERFACE LobattoKernelEvalAll_

!----------------------------------------------------------------------------
!                                               LobattoKernelGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Gradient of Lobatto bubbles of order = 0 to n

INTERFACE LobattoKernelGradientEvalAll
  MODULE PURE FUNCTION LobattoKernelGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 0:n)
    !! Gradient of Lobatto bubbles of order 0 to n
  END FUNCTION LobattoKernelGradientEvalAll1
END INTERFACE LobattoKernelGradientEvalAll

!----------------------------------------------------------------------------
!                                               LobattoKernelGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Gradient of Lobatto bubbles of order = 0 to n

INTERFACE LobattoKernelGradientEvalAll_
  MODULE PURE SUBROUTINE LobattoKernelGradientEvalAll1_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(1:, 0:)
    ! ans(1:SIZE(x), 0:n)
    !! Gradient of Lobatto bubbles of order 0 to n
    INTEGER(I4B), INTENT(OUT) :: nrow
    INTEGER(I4B), INTENT(OUT) :: ncol
  END SUBROUTINE LobattoKernelGradientEvalAll1_
END INTERFACE LobattoKernelGradientEvalAll_

!----------------------------------------------------------------------------
!                                             LobattoMonomialExpansionAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Returns the monomial expansion of all Lobatto polynomials
!
!# Introduction
!
! Returns all the monomial expansion of all Lobatto polynomials
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
  MODULE PURE FUNCTION LobattoMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION LobattoMonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                             LobattoMonomialExpansion
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Returns the monomial expansion of a Lobatto polynomials
!
!# Introduction
!
! Returns all the monomial expansion of a Lobatto polynomials
!
!- n : is the order of the polynomial
!- ans(:) contains the coefficient of monomials for polynomial order=n
!

INTERFACE
  MODULE PURE FUNCTION LobattoMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LobattoMonomialExpansion
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Lobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Lobatto polynomial of order upto n.

INTERFACE LobattoGradientEvalAll
  MODULE PURE FUNCTION LobattoGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LobattoGradientEvalAll1
END INTERFACE LobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LobattoGradientEvalAll_
  MODULE PURE SUBROUTINE LobattoGradientEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LobattoGradientEvalAll1_
END INTERFACE LobattoGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Lobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Lobatto polynomial of order upto n.

INTERFACE LobattoGradientEvalAll
  MODULE PURE FUNCTION LobattoGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LobattoGradientEvalAll2
END INTERFACE LobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE LobattoGradientEvalAll_
  MODULE PURE SUBROUTINE LobattoGradientEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    ! ans(1:SIZE(x), 1:n + 1)
  END SUBROUTINE LobattoGradientEvalAll2_
END INTERFACE LobattoGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Lobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Lobatto polynomial of order upto n.

INTERFACE LobattoGradientEval
  MODULE PURE FUNCTION LobattoGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LobattoGradientEval1
END INTERFACE LobattoGradientEval
!!

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:         Evaluate gradient of Lobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of Lobatto polynomial of order upto n.

INTERFACE LobattoGradientEval
  MODULE PURE FUNCTION LobattoGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LobattoGradientEval2
END INTERFACE LobattoGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Lobatto mass matrix

INTERFACE
  MODULE PURE FUNCTION LobattoMassMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION LobattoMassMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Lobatto mass matrix

INTERFACE
  MODULE PURE FUNCTION LobattoStiffnessMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION LobattoStiffnessMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Lobatto0
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto0(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto1
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto1(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto2
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto2(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto3
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto3(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto3
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto4
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto4(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto4
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto5
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto5(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto5
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto6
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto6(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto6
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto7
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto7(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto7
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto8
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto8(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto8
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto9
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto9(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto9
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto10
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Lobatto10(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto10
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LobattoPolynomialUtility
