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
! summary: Utility related to UnscaledLobatto Polynomials is defined.
!
!{!pages/UnscaledLobattoPolynomialUtility.md!}

MODULE UnscaledLobattoPolynomialUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: UnscaledLobattoLeadingCoeff
PUBLIC :: UnscaledLobattoZeros
PUBLIC :: UnscaledLobattoEval
PUBLIC :: UnscaledLobattoEvalAll
PUBLIC :: UnscaledLobattoEvalAll_
PUBLIC :: UnscaledLobattoMonomialExpansionAll
PUBLIC :: UnscaledLobattoMonomialExpansion
PUBLIC :: UnscaledLobattoGradientEvalAll
PUBLIC :: UnscaledLobattoGradientEval
PUBLIC :: UnscaledLobattoMassMatrix
PUBLIC :: UnscaledLobattoStiffnessMatrix

!----------------------------------------------------------------------------
!                                               UnscaledLobattoLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Leading coefficient of UnscaledLobatto polynomial

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoLeadingCoeff(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of UnscaledLobatto polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION UnscaledLobattoLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Square norm of UnscaledLobatto polynomial
!

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoNormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION UnscaledLobattoNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                       UnscaledLobattoZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Returns zeros of UnscaledLobatto polynomials

INTERFACE
  MODULE FUNCTION UnscaledLobattoZeros(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of UnscaledLobatto polynomial, should be greater than equal to 2
    REAL(DFP) :: ans(n)
    !!
  END FUNCTION UnscaledLobattoZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                        UnscaledLobattoEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate UnscaledLobatto polynomials from order = 0 to n at several points
!
!# Introduction
!
! Evaluate UnscaledLobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 UnscaledLobatto
! polynomials at the point X.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate UnscaledLobatto polynomial of order n at point x
  END FUNCTION UnscaledLobattoEval1
END INTERFACE

INTERFACE UnscaledLobattoEval
  MODULE PROCEDURE UnscaledLobattoEval1
END INTERFACE UnscaledLobattoEval

!----------------------------------------------------------------------------
!                                                          UnscaledLobattoEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate UnscaledLobatto polynomials from order = 0 to n at
! several points
!
!# Introduction
!
! Evaluate UnscaledLobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 UnscaledLobatto polynomials at
! the point X.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate UnscaledLobatto polynomial of order n at point x
  END FUNCTION UnscaledLobattoEval2
END INTERFACE

INTERFACE UnscaledLobattoEval
  MODULE PROCEDURE UnscaledLobattoEval2
END INTERFACE UnscaledLobattoEval

!----------------------------------------------------------------------------
!                                                    UnscaledLobattoEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate UnscaledLobatto polynomials from order = 0 to n at
! several points
!
!# Introduction
!
! Evaluate UnscaledLobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 UnscaledLobatto polynomials at
! the point X.

INTERFACE UnscaledLobattoEvalAll
  MODULE PURE FUNCTION UnscaledLobattoEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate UnscaledLobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION UnscaledLobattoEvalAll1
END INTERFACE UnscaledLobattoEvalAll

!----------------------------------------------------------------------------
!                                                   UnscaledLobattoEvalAll_
!----------------------------------------------------------------------------

INTERFACE UnscaledLobattoEvalAll_
  MODULE PURE SUBROUTINE UnscaledLobattoEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Evaluate UnscaledLobatto polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE UnscaledLobattoEvalAll1_
END INTERFACE UnscaledLobattoEvalAll_

!----------------------------------------------------------------------------
!                                                     UnscaledLobattoEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 Sept 2022
! summary: Evaluate UnscaledLobatto polynomials from order = 0 to n at
! several points
!
!# Introduction
!
! Evaluate UnscaledLobatto polynomials from order = 0 to n at several points
!
!- N, the highest order polynomial to compute. Note that polynomials 0
! through N will be computed.
!- alpha, beta are parameters
!- x: the point at which the polynomials are to be evaluated.
!- ans(M,1:N+1), the values of the first N+1 UnscaledLobatto polynomials at
! the point X.

INTERFACE UnscaledLobattoEvalAll

  MODULE PURE FUNCTION UnscaledLobattoEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate UnscaledLobatto polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION UnscaledLobattoEvalAll2
END INTERFACE UnscaledLobattoEvalAll

!----------------------------------------------------------------------------
!                                                 UnscaledLobattoEvalAll_
!----------------------------------------------------------------------------

INTERFACE UnscaledLobattoEvalAll_

  MODULE PURE SUBROUTINE UnscaledLobattoEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Evaluate UnscaledLobatto polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE UnscaledLobattoEvalAll2_
END INTERFACE UnscaledLobattoEvalAll_

!----------------------------------------------------------------------------
!                                       UnscaledLobattoMonomialExpansionAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary:Returns the monomial expansion of all UnscaledLobatto polynomials
!
!# Introduction
!
! Returns all the monomial expansion of all UnscaledLobatto polynomials
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
  MODULE PURE FUNCTION UnscaledLobattoMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION UnscaledLobattoMonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                           UnscaledLobattoMonomialExpansion
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Returns the monomial expansion of a UnscaledLobatto polynomials
!
!# Introduction
!
! Returns all the monomial expansion of a UnscaledLobatto polynomials
!
!- n : is the order of the polynomial
!- ans(:) contains the coefficient of monomials for polynomial order=n
!

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UnscaledLobattoMonomialExpansion
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of UnscaledLobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of UnscaledLobatto polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UnscaledLobattoGradientEvalAll1
END INTERFACE
!!

INTERFACE UnscaledLobattoGradientEvalAll
  MODULE PROCEDURE UnscaledLobattoGradientEvalAll1
END INTERFACE UnscaledLobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of UnscaledLobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of UnscaledLobatto polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION UnscaledLobattoGradientEvalAll2
END INTERFACE
!!

INTERFACE UnscaledLobattoGradientEvalAll
  MODULE PROCEDURE UnscaledLobattoGradientEvalAll2
END INTERFACE UnscaledLobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of UnscaledLobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of UnscaledLobatto polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UnscaledLobattoGradientEval1
END INTERFACE
!!

INTERFACE UnscaledLobattoGradientEval
  MODULE PROCEDURE UnscaledLobattoGradientEval1
END INTERFACE UnscaledLobattoGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: Evaluate gradient of UnscaledLobatto polynomial of order upto n
!
!# Introduction
!
! Evaluate gradient of UnscaledLobatto polynomial of order upto n.

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UnscaledLobattoGradientEval2
END INTERFACE

INTERFACE UnscaledLobattoGradientEval
  MODULE PROCEDURE UnscaledLobattoGradientEval2
END INTERFACE UnscaledLobattoGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: UnscaledLobatto mass matrix

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoMassMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION UnscaledLobattoMassMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 Sept 2022
! summary: UnscaledLobatto mass matrix

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoStiffnessMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION UnscaledLobattoStiffnessMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UnscaledLobattoPolynomialUtility
