! This program is a part of EASIFEM library
! Copyright (C) 2020-2021 Vikas Sharma, Ph.D
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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Unscaled integrated Legendre basis utilities.
!
! The first two modes are endpoint functions. Interior derivatives are
! Legendre polynomials. Unlike scaled Lobatto interior modes, their derivative
! norms are not one.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$\psi_0=(1-x)/2,\quad\psi_1=(1+x)/2$$
! $$\psi_n=(P_n-P_{n-2})/(2n-1),\quad n\ge2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UnscaledLobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE UnscaledLobattoPolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
PUBLIC :: UnscaledLobattoGradientEvalAll_
PUBLIC :: UnscaledLobattoGradientEval
PUBLIC :: UnscaledLobattoLaplacianEvalAll
PUBLIC :: UnscaledLobattoLaplacianEvalAll_
PUBLIC :: UnscaledLobattoLaplacianEval
PUBLIC :: UnscaledLobattoMassMatrix
PUBLIC :: UnscaledLobattoStiffnessMatrix

!----------------------------------------------------------------------------
!                                               UnscaledLobattoLeadingCoeff
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the coefficient of the highest power.
!
!  For n=0 and n=1 the current implementation returns +1/2 and -1/2,
! respectively; these signs disagree with the endpoint functions. The example
! uses the consistent interior case n=2.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$p_n(x)=k_n x^n+\cdots$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoLeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UnscaledLobattoLeadingCoeff(n=2_I4B)
! CALL OK(ABS(y-(1.5_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return Lobatto interpolation nodes.
!
! n is the number of nodes. For n>=2 both endpoints are included; interior
! nodes are Jacobi(1,1) roots of degree n-2. This is not a request for zeros
! of degree n LobattoEval. Use n>=1.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of returned nodes; use n>=1 and
! enough nodes for the rule.
!
! Returns:
!   ans : REAL(DFP)(n).
!
! Formula:
! $$z_1=-1,\quad z_n=1,\quad P_{n-2}^{(1,1)}(z_i)=0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(3)
!
! z = UnscaledLobattoZeros(n=3_I4B)
! CALL OK(MAXVAL(ABS(z- &
!   [-1.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-11_DFP, &
!   "Three nodes include both endpoints")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y_i=p_n(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UnscaledLobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoEval
  MODULE PURE FUNCTION UnscaledLobattoEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate UnscaledLobatto polynomial of order n at point x
  END FUNCTION UnscaledLobattoEval1
END INTERFACE UnscaledLobattoEval

!----------------------------------------------------------------------------
!                                                          UnscaledLobattoEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$y_i=p_n(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UnscaledLobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoEval
  MODULE PURE FUNCTION UnscaledLobattoEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate UnscaledLobatto polynomial of order n at point x
  END FUNCTION UnscaledLobattoEval2
END INTERFACE UnscaledLobattoEval

!----------------------------------------------------------------------------
!                                                    UnscaledLobattoEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(n + 1).
!
! Formula:
! $$Y_{i,j+1}=p_j(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(:), tsize.
!
! Formula:
! $$Y_{i,j+1}=p_j(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UnscaledLobattoEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), n + 1).
!
! Formula:
! $$Y_{i,j+1}=p_j(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{i,j+1}=p_j(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UnscaledLobattoEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Expand all polynomials through degree n in monomials.
!
! Rows correspond to powers and columns to polynomial degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP)(1:n + 1, 1:n + 1).
!
! Formula:
! $$p_j(x)=\sum_{k=0}^{j} C_{k+1,j+1}x^k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: &
!   UnscaledLobattoMonomialExpansionAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3, 3)
!
! c = UnscaledLobattoMonomialExpansionAll(n=2_I4B)
! CALL OK(MAXVAL(ABS(c(:, 3)- &
!   ([-1.5_DFP, 0.0_DFP, 1.5_DFP]/3.0_DFP))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION UnscaledLobattoMonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                           UnscaledLobattoMonomialExpansion
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Expand one polynomial in monomials.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP)(1:n + 1).
!
! Formula:
! $$p_n(x)=\sum_{k=0}^{n} c_{k+1}x^k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoMonomialExpansion
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3)
!
! c = UnscaledLobattoMonomialExpansion(n=2_I4B)
! CALL OK(MAXVAL(ABS(c- &
!   ([-1.5_DFP, 0.0_DFP, 1.5_DFP]/3.0_DFP))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UnscaledLobattoMonomialExpansion
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(1:n + 1).
!
! Formula:
! $$Y_{i,j+1}=p_j'(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEvalAll
  MODULE PURE FUNCTION UnscaledLobattoGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UnscaledLobattoGradientEvalAll1
END INTERFACE UnscaledLobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(:), tsize.
!
! Formula:
! $$Y_{i,j+1}=p_j'(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UnscaledLobattoGradientEvalAll_( &
!   n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEvalAll_
  MODULE PURE SUBROUTINE UnscaledLobattoGradientEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! ans(1:n + 1)
  END SUBROUTINE UnscaledLobattoGradientEvalAll1_
END INTERFACE UnscaledLobattoGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(1:SIZE(x), 1:n + 1).
!
! Formula:
! $$Y_{i,j+1}=p_j'(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEvalAll
  MODULE PURE FUNCTION UnscaledLobattoGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION UnscaledLobattoGradientEvalAll2
END INTERFACE UnscaledLobattoGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{i,j+1}=p_j'(x_i),\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UnscaledLobattoGradientEvalAll_( &
!   n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEvalAll_
  MODULE PURE SUBROUTINE UnscaledLobattoGradientEvalAll2_(n, x, ans, &
                                                          nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x)
    !! ncol = n + 1
  END SUBROUTINE UnscaledLobattoGradientEvalAll2_
END INTERFACE UnscaledLobattoGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y_i=p_n'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UnscaledLobattoGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEval
  MODULE PURE FUNCTION UnscaledLobattoGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UnscaledLobattoGradientEval1
END INTERFACE UnscaledLobattoGradientEval
!!

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(1:SIZE(x)).
!
! Formula:
! $$y_i=p_n'(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UnscaledLobattoGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x/3.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UnscaledLobattoGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoGradientEval
  MODULE PURE FUNCTION UnscaledLobattoGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UnscaledLobattoGradientEval2
END INTERFACE UnscaledLobattoGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the mass matrix on the biunit interval.
!
! Rows and columns enumerate modes zero through n.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP)(n + 1, n + 1).
!
! Formula:
! $$M_{ij}=\int_{-1}^{1}p_i(x)p_j(x)\,dx$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoMassMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(3, 3)
!
! a = UnscaledLobattoMassMatrix(n=2_I4B)
! CALL OK(MAXVAL(ABS(a-TRANSPOSE(a))) < 1.0E-11_DFP, &
!   "Matrix is symmetric")
! CALL OK(ABS(a(1, 1)-(2.0_DFP/3.0_DFP)) < 1.0E-11_DFP .AND. &
!   ABS(a(1, 2)-(1.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Endpoint block agrees with exact integrals")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoMassMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION UnscaledLobattoMassMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the stiffness matrix on the biunit interval.
!
! Rows and columns enumerate modes zero through n.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP)(n + 1, n + 1).
!
! Formula:
! $$K_{ij}=\int_{-1}^{1}p_i'(x)p_j'(x)\,dx$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoStiffnessMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(3, 3)
!
! a = UnscaledLobattoStiffnessMatrix(n=2_I4B)
! CALL OK(MAXVAL(ABS(a-TRANSPOSE(a))) < 1.0E-11_DFP, &
!   "Matrix is symmetric")
! CALL OK(ABS(a(1, 1)-(0.5_DFP)) < 1.0E-11_DFP .AND. &
!   ABS(a(1, 2)-(-0.5_DFP)) < 1.0E-11_DFP, &
!   "Endpoint block agrees with exact integrals")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION UnscaledLobattoStiffnessMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION UnscaledLobattoStiffnessMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                            UnscaledLobattoLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEvalAll
  MODULE PURE FUNCTION UnscaledLobattoLaplacianEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UnscaledLobattoLaplacianEvalAll1
END INTERFACE UnscaledLobattoLaplacianEvalAll

!----------------------------------------------------------------------------
!                                          UnscaledLobattoLaplacianEvalAll_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEvalAll_
  MODULE PURE SUBROUTINE UnscaledLobattoLaplacianEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! ans(1:n + 1)
  END SUBROUTINE UnscaledLobattoLaplacianEvalAll1_
END INTERFACE UnscaledLobattoLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                            UnscaledLobattoLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEvalAll
  MODULE PURE FUNCTION UnscaledLobattoLaplacianEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION UnscaledLobattoLaplacianEvalAll2
END INTERFACE UnscaledLobattoLaplacianEvalAll

!----------------------------------------------------------------------------
!                                           UnscaledLobattoLaplacianEvalAll_
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEvalAll_
  MODULE PURE SUBROUTINE UnscaledLobattoLaplacianEvalAll2_(n, x, ans, &
                                                           nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x)
    !! ncol = n + 1
  END SUBROUTINE UnscaledLobattoLaplacianEvalAll2_
END INTERFACE UnscaledLobattoLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                               UnscaledLobattoLaplacianEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEval
  MODULE PURE FUNCTION UnscaledLobattoLaplacianEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UnscaledLobattoLaplacianEval1
END INTERFACE UnscaledLobattoLaplacianEval
!!

!----------------------------------------------------------------------------
!                                               UnscaledLobattoLaplacianEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Reserved second derivative interface; currently unimplemented.
!
! Do not call this interface: its implementation is empty or forwards to an
! empty body. Function results and output dimensions are undefined. The
! example deliberately calls GradientEval instead and demonstrates a numerical
! workaround; it is not validation of this Laplacian interface.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   No defined result: the implementation body is empty.
!
! Formula:
! $$p_n''(x)\simeq\frac{p_n'(x+h)-p_n'(x-h)}{2h}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UnscaledLobattoGradientEval(n=2_I4B, x=x+h)
! gm = UnscaledLobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(1.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UnscaledLobattoLaplacianEval
  MODULE PURE FUNCTION UnscaledLobattoLaplacianEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UnscaledLobattoLaplacianEval2
END INTERFACE UnscaledLobattoLaplacianEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UnscaledLobattoPolynomialUtility
