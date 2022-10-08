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
USE GlobalData
IMPLICIT NONE
PRIVATE

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

PUBLIC :: LobattoLeadingCoeff

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

PUBLIC :: LobattoZeros

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

INTERFACE
  MODULE PURE FUNCTION LobattoEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Lobatto polynomial of order n at point x
  END FUNCTION LobattoEval1
END INTERFACE

INTERFACE LobattoEval
  MODULE PROCEDURE LobattoEval1
END INTERFACE LobattoEval

PUBLIC :: LobattoEval

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

INTERFACE
  MODULE PURE FUNCTION LobattoEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Lobatto polynomial of order n at point x
  END FUNCTION LobattoEval2
END INTERFACE

INTERFACE LobattoEval
  MODULE PROCEDURE LobattoEval2
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

INTERFACE
  MODULE PURE FUNCTION LobattoEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LobattoEvalAll1
END INTERFACE

INTERFACE LobattoEvalAll
  MODULE PROCEDURE LobattoEvalAll1
END INTERFACE LobattoEvalAll

PUBLIC :: LobattoEvalAll

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

INTERFACE
  MODULE PURE FUNCTION LobattoEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Lobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LobattoEvalAll2
END INTERFACE

INTERFACE LobattoEvalAll
  MODULE PROCEDURE LobattoEvalAll2
END INTERFACE LobattoEvalAll

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

PUBLIC :: LobattoMonomialExpansionAll

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

PUBLIC :: LobattoMonomialExpansion

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

INTERFACE
  MODULE PURE FUNCTION LobattoGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LobattoGradientEvalAll1
END INTERFACE
!!

INTERFACE LobattoGradientEvalAll
  MODULE PROCEDURE LobattoGradientEvalAll1
END INTERFACE LobattoGradientEvalAll

PUBLIC :: LobattoGradientEvalAll

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

INTERFACE
  MODULE PURE FUNCTION LobattoGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LobattoGradientEvalAll2
END INTERFACE
!!

INTERFACE LobattoGradientEvalAll
  MODULE PROCEDURE LobattoGradientEvalAll2
END INTERFACE LobattoGradientEvalAll

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

INTERFACE
  MODULE PURE FUNCTION LobattoGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LobattoGradientEval1
END INTERFACE
!!

INTERFACE LobattoGradientEval
  MODULE PROCEDURE LobattoGradientEval1
END INTERFACE LobattoGradientEval

PUBLIC :: LobattoGradientEval

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

INTERFACE
  MODULE PURE FUNCTION LobattoGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LobattoGradientEval2
END INTERFACE

INTERFACE LobattoGradientEval
  MODULE PROCEDURE LobattoGradientEval2
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

PUBLIC :: LobattoMassMatrix

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

PUBLIC :: LobattoStiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LobattoPolynomialUtility
