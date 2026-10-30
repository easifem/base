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
! summary: First kind Chebyshev polynomial utilities.
!
! Quadrature weights include the singular Chebyshev weight. Use
! Chebyshev1EvalSum for full modal coefficients; ChebClenshaw halves its
! constant coefficient.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$T_n(\cos\theta)=\cos(n\theta),\quad w(x)=(1-x^2)^{-1/2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Eval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1Eval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(Chebyshev1Eval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE Chebyshev1PolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: iface_1DFunction
IMPLICIT NONE
PRIVATE

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
PUBLIC :: Chebyshev1EvalAll_
PUBLIC :: Chebyshev1MonomialExpansionAll
PUBLIC :: Chebyshev1MonomialExpansion
PUBLIC :: Chebyshev1GradientEvalAll
PUBLIC :: Chebyshev1GradientEvalAll_
PUBLIC :: Chebyshev1GradientEval
PUBLIC :: Chebyshev1LaplacianEvalAll
PUBLIC :: Chebyshev1LaplacianEvalAll_
PUBLIC :: Chebyshev1LaplacianEval
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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return a monic recurrence diagonal coefficient.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$\pi_{n+1}=(x-\alpha_n)\pi_n-\beta_n\pi_{n-1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Alpha
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1Alpha(n=2_I4B)
! CALL OK(ABS(y-(0.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return a monic recurrence beta coefficient.
!
! For beta, n=0 denotes the weight integral; the displayed norm ratio uses
! monic squared norms.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$\beta_0=\int_{-1}^{1}w(x)\,dx,\quad \beta_n=\widehat h_n/\widehat
! h_{n-1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Beta
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1Beta(n=2_I4B)
! CALL OK(ABS(y-(0.25_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return monic three term recurrence coefficients.
!
! Arrays are indexed from zero and contain n coefficients. Use n>=1.
! betaCoeff(0) stores the weight integral.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Recurrence length or matrix size; use n>=1.
!   alphaCoeff : REAL(DFP), INTENT(OUT)(0:n - 1). Monic recurrence diagonal
! coefficients, indexed from zero.
!   betaCoeff : REAL(DFP), INTENT(OUT)(0:n - 1). Monic recurrence beta values;
! index zero is the weight integral.
!
! Returns:
!   Writes alphaCoeff(0:n - 1), betaCoeff(0:n - 1).
!
! Formula:
! $$\pi_{j+1}=(x-\alpha_j)\pi_j-\beta_j\pi_{j-1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: GetChebyshev1RecurrenceCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2)
!
! CALL GetChebyshev1RecurrenceCoeff( &
!   n=3_I4B, alphaCoeff=a, betaCoeff=b)
! CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
!   "Symmetric family has zero diagonal")
! CALL OK(ABS(b(2)-0.25_DFP) &
!   < 1.0E-11_DFP, "Monic beta coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return standard three term recurrence coefficients.
!
! Arrays are indexed from zero and contain n coefficients. Use n>=1.
! Initialize the family specific degree zero and degree one polynomials before
! advancing the recurrence.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Recurrence length or matrix size; use n>=1.
!   A : REAL(DFP), INTENT(OUT)(0:n - 1). Multiplier of x in the standard three
! term recurrence.
!   B : REAL(DFP), INTENT(OUT)(0:n - 1). Constant term in the standard three
! term recurrence.
!   C : REAL(DFP), INTENT(OUT)(0:n - 1). Multiplier of the previous
! polynomial, subtracted in the recurrence.
!
! Returns:
!   Writes A(0:n - 1), B(0:n - 1), C(0:n - 1).
!
! Formula:
! $$p_{j+1}=(A_j x+B_j)p_j-C_j p_{j-1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: GetChebyshev1RecurrenceCoeff2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2), c(0:2)
!
! CALL GetChebyshev1RecurrenceCoeff2(n=3_I4B, A=a, B=b, C=c)
! CALL OK(MAXVAL(ABS(b)) < 1.0E-11_DFP, &
!   "Symmetric family has no constant recurrence term")
! CALL OK(ABS(a(1)-2.0_DFP) < 1.0E-11_DFP, &
!   "Degree two recurrence multiplier")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the coefficient of the highest power.
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1LeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1LeadingCoeff(n=2_I4B)
! CALL OK(ABS(y-(2.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive leading coefficients.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$r_n=k_{n+1}/k_n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1LeadingCoeffRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1LeadingCoeffRatio(n=2_I4B)
! CALL OK(ABS(y-(2.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the weighted squared norm.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$h_n=\int_{-1}^{1}p_n(x)^2w(x)\,dx$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQR
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1NormSQR(n=2_I4B)
! CALL OK(ABS(y-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1NormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Chebyshev1NormSQR2
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return weighted squared norms from degree zero through n.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP)(0:n).
!
! Formula:
! $$h_j=\int_{-1}^{1}p_j(x)^2w(x)\,dx,\quad 0\le j\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQR2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(0:2)
!
! y = Chebyshev1NormSQR2(n=2_I4B)
! CALL OK(ABS(y(2)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-11_DFP, &
!   "Norm array includes degree two")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQR2(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n)
  END FUNCTION Chebyshev1NormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Chebyshev1NormSQRRatio
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.
!
!  The implementation returns 1 for every n. This is the correct norm ratio
! for n>=1; at n=0 the mathematical ratio is 1/2. Use NormSQR or NormSQR2 when
! that distinction matters.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$r_n=h_{n+1}/h_n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQRRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Chebyshev1NormSQRRatio(n=2_I4B)
! CALL OK(ABS(y-(1.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Chebyshev1NormSQRRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1NormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Chebyshev1JacobiMatrix
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Gauss Jacobi matrix.
!
! Allocate D with n entries and E with one fewer. Optional recurrence outputs
! use zero based indexing.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Recurrence length or matrix size; use n>=1.
!   D : REAL(DFP), INTENT(OUT)(:). Jacobi matrix diagonal, or nodal derivative
! matrix in the block splitter.
!   E : REAL(DFP), INTENT(OUT)(:). Jacobi matrix off diagonal, or even block
! in the block splitter.
!   alphaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence
! diagonal coefficients, indexed from zero.
!   betaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence beta
! values; index zero is the weight integral.
!
! Returns:
!   Writes D(:), E(:), alphaCoeff(0:), betaCoeff(0:).
!
! Formula:
! $$J_{i,i}=\alpha_{i-1},\quad J_{i,i+1}=\sqrt{\beta_i}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1JacobiMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(2), e(1)
!
! CALL Chebyshev1JacobiMatrix(n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute Gauss quadrature nodes and weights.
!
! This low level routine returns n nodes.  Weights include the family weight.
! Nodes and weights must be paired in the returned order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of returned nodes; use n>=1 and
! enough nodes for the rule.
!   pt : REAL(DFP), INTENT(OUT)(:). Quadrature nodes; allocate the point count
! specified above.
!   wt : REAL(DFP), OPTIONAL, INTENT(OUT)(:). Quadrature weights paired with
! pt; includes the family weight.
!
! Returns:
!   Writes pt(:), wt(:).
!
! Formula:
! $$\int_{-1}^{1} f(x)w(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2), w(2)
!
! CALL Chebyshev1GaussQuadrature(n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(ACOS(-1.0_DFP))) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Radau Jacobi matrix.
!
! Allocate D with n+1 entries and E with one fewer. The final recurrence
! entries are modified to include the prescribed endpoint(s).
!
! Arguments:
!   a : REAL(DFP), INTENT(IN). Recurrence multiplier array, or prescribed
! endpoint -1/+1 for Radau.
!   n : INTEGER(I4B), INTENT(IN). Number of free nodes; output has n+1 points
! or matrix rows.
!   D : REAL(DFP), INTENT(OUT)(:). Jacobi matrix diagonal, or nodal derivative
! matrix in the block splitter.
!   E : REAL(DFP), INTENT(OUT)(:). Jacobi matrix off diagonal, or even block
! in the block splitter.
!   alphaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence
! diagonal coefficients, indexed from zero.
!   betaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence beta
! values; index zero is the weight integral.
!
! Returns:
!   Writes D(:), E(:), alphaCoeff(0:), betaCoeff(0:).
!
! Formula:
! $$J_{i,i}=\alpha_{i-1},\quad J_{i,i+1}=\sqrt{\beta_i}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1JacobiRadauMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(3), e(2)
!
! CALL Chebyshev1JacobiRadauMatrix(a=-1.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute Gauss Radau quadrature nodes and weights.
!
! This low level routine returns n+1 nodes. Set a to -1 or +1. Weights include
! the family weight. Nodes and weights must be paired in the returned order.
!
! Arguments:
!   a : REAL(DFP), INTENT(IN). Recurrence multiplier array, or prescribed
! endpoint -1/+1 for Radau.
!   n : INTEGER(I4B), INTENT(IN). Number of free nodes; output has n+1 points
! or matrix rows.
!   pt : REAL(DFP), INTENT(OUT)(:). Quadrature nodes; allocate the point count
! specified above.
!   wt : REAL(DFP), OPTIONAL, INTENT(OUT)(:). Quadrature weights paired with
! pt; includes the family weight.
!
! Returns:
!   Writes pt(:), wt(:).
!
! Formula:
! $$\int_{-1}^{1} f(x)w(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussRadauQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3)
!
! CALL Chebyshev1GaussRadauQuadrature(a=-1.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(ACOS(-1.0_DFP))) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(MINVAL(ABS(x+1.0_DFP)) < 1.0E-11_DFP, &
!   "Prescribed endpoint is present")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Lobatto Jacobi matrix.
!
! Allocate D with n+2 entries and E with one fewer. The final recurrence
! entries are modified to include the prescribed endpoint(s).
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of free nodes; output has n+2 points
! or matrix rows.
!   D : REAL(DFP), INTENT(OUT)(:). Jacobi matrix diagonal, or nodal derivative
! matrix in the block splitter.
!   E : REAL(DFP), INTENT(OUT)(:). Jacobi matrix off diagonal, or even block
! in the block splitter.
!   alphaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence
! diagonal coefficients, indexed from zero.
!   betaCoeff : REAL(DFP), OPTIONAL, INTENT(OUT)(0:). Monic recurrence beta
! values; index zero is the weight integral.
!
! Returns:
!   Writes D(:), E(:), alphaCoeff(0:), betaCoeff(0:).
!
! Formula:
! $$J_{i,i}=\alpha_{i-1},\quad J_{i,i+1}=\sqrt{\beta_i}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1JacobiLobattoMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(4), e(3)
!
! CALL Chebyshev1JacobiLobattoMatrix(n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute Gauss Lobatto quadrature nodes and weights.
!
! This low level routine returns n+2 nodes.  Weights include the family
! weight. Nodes and weights must be paired in the returned order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of free nodes; output has n+2 points
! or matrix rows.
!   pt : REAL(DFP), INTENT(OUT)(:). Quadrature nodes; allocate the point count
! specified above.
!   wt : REAL(DFP), OPTIONAL, INTENT(OUT)(:). Quadrature weights paired with
! pt; includes the family weight.
!
! Returns:
!   Writes pt(:), wt(:).
!
! Formula:
! $$\int_{-1}^{1} f(x)w(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussLobattoQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL Chebyshev1GaussLobattoQuadrature(n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(ACOS(-1.0_DFP))) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the n roots of a degree n polynomial.
!
! Use n>=1. Root order is family dependent; do not assume increasing order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of returned nodes; use n>=1 and
! enough nodes for the rule.
!
! Returns:
!   ans : REAL(DFP)(n).
!
! Formula:
! $$p_n(z_i)=0,\quad 1\le i\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Zeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(2)
!
! z = Chebyshev1Zeros(n=2_I4B)
! CALL OK(MAXVAL(ABS(z*z-0.5_DFP)) &
!   < 1.0E-11_DFP, "Roots satisfy the quadratic")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute a selected quadrature nodes and weights.
!
! n is the total number of requested points. Select Gauss, GaussRadauLeft,
! GaussRadauRight, or GaussLobatto from GlobalData. Weights include the family
! weight. Nodes and weights must be paired in the returned order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of returned nodes; use n>=1 and
! enough nodes for the rule.
!   pt : REAL(DFP), INTENT(OUT)(n). Quadrature nodes; allocate the point count
! specified above.
!   wt : REAL(DFP), OPTIONAL, INTENT(OUT)(n). Quadrature weights paired with
! pt; includes the family weight.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   onlyInside : LOGICAL(LGT), OPTIONAL, INTENT(IN). Optional endpoint
! exclusion flag for the quadrature dispatcher.
!
! Returns:
!   Writes pt(n), wt(n).
!
! Formula:
! $$\int_{-1}^{1} f(x)w(x)\,dx\simeq\sum_i w_i f(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: GaussLobatto
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL Chebyshev1Quadrature( &
!   n=4_I4B, pt=x, wt=w, quadType=GaussLobatto)
! CALL OK(ABS(SUM(w)-(ACOS(-1.0_DFP))) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Eval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1Eval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(Chebyshev1Eval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Eval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1Eval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(Chebyshev1Eval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1EvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL Chebyshev1EvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1EvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL Chebyshev1EvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(2.0_DFP*x*x-1.0_DFP))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1MonomialExpansionAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3, 3)
!
! c = Chebyshev1MonomialExpansionAll(n=2_I4B)
! CALL OK(MAXVAL(ABS(c(:, 3)- &
!   ([-1.0_DFP, 0.0_DFP, 2.0_DFP]))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Chebyshev1MonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION Chebyshev1MonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                             Chebyshev1MonomialExpansion
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1MonomialExpansion
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3)
!
! c = Chebyshev1MonomialExpansion(n=2_I4B)
! CALL OK(MAXVAL(ABS(c- &
!   ([-1.0_DFP, 0.0_DFP, 2.0_DFP]))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Chebyshev1MonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION Chebyshev1MonomialExpansion
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1GradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(4.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL Chebyshev1GradientEvalAll_( &
!   n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(4.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1GradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(4.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL Chebyshev1GradientEvalAll_( &
!   n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(4.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1GradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(Chebyshev1GradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = Chebyshev1GradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(Chebyshev1GradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1EvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1EvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the derivative of a polynomial expansion.
!
! Coefficients are modal values indexed from zero. Overloads with k request
! the kth derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j'(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1GradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the derivative of a polynomial expansion.
!
! Coefficients are modal values indexed from zero. Overloads with k request
! the kth derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j'(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1GradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the derivative of a polynomial expansion.
!
! Coefficients are modal values indexed from zero. Overloads with k request
! the kth derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!   k : INTEGER(I4B), INTENT(IN). Derivative order.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j'(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1GradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the derivative of a polynomial expansion.
!
! Coefficients are modal values indexed from zero. Overloads with k request
! the kth derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!   k : INTEGER(I4B), INTENT(IN). Derivative order.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j'(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1GradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(4.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:n). Sampled function values at quadrature
! nodes.
!   x : REAL(DFP), INTENT(IN)(0:n). Evaluation coordinate(s).
!   w : REAL(DFP), INTENT(IN)(0:n). Quadrature weights paired with x or PP
! rows.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!
! Returns:
!   ans : REAL(DFP)(0:n).
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! c = Chebyshev1Transform(n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:). Sampled function values at quadrature
! nodes.
!   x : REAL(DFP), INTENT(IN)(0:). Evaluation coordinate(s).
!   w : REAL(DFP), INTENT(IN)(0:). Quadrature weights paired with x or PP
! rows.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   ans : REAL(DFP), INTENT(INOUT)(0:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(0:), tsize.
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform_
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! CALL Chebyshev1Transform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:). Sampled function values at quadrature
! nodes.
!   PP : REAL(DFP), INTENT(IN)(0:, 0:). Precomputed polynomial values, with
! point rows and degree columns.
!   w : REAL(DFP), INTENT(IN)(0:). Quadrature weights paired with x or PP
! rows.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   ans : REAL(DFP), INTENT(INOUT)(0:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(0:), tsize.
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform_
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! CALL Chebyshev1Transform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   f : PROCEDURE(iface_1DFunction), POINTER, INTENT(IN). Procedure pointer to
! an iface_1DFunction callback.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   x1 : REAL(DFP), INTENT(IN). Left endpoint of the callback evaluation
! interval.
!   x2 : REAL(DFP), INTENT(IN). Right endpoint of the callback evaluation
! interval.
!
! Returns:
!   ans : REAL(DFP)(0:n).
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! c = Chebyshev1Transform(n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   f : PROCEDURE(iface_1DFunction), POINTER, INTENT(IN). Procedure pointer to
! an iface_1DFunction callback.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   x1 : REAL(DFP), INTENT(IN). Left endpoint of the callback evaluation
! interval.
!   x2 : REAL(DFP), INTENT(IN). Right endpoint of the callback evaluation
! interval.
!   ans : REAL(DFP)(0:). Result storage; allocate enough space for all active
! dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes tsize.
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform_
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! CALL Chebyshev1Transform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:). Sampled function values at quadrature
! nodes.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!
! Returns:
!   ans : REAL(DFP)(0:n).
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! c = Chebyshev1Transform(n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.
!
! coeff contains sampled function values on input, not modal coefficients.
! Output entries are modal coefficients. Callback overloads map reference
! nodes to [x1,x2]. Precomputed PP uses rows for points and columns for
! degrees.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:). Sampled function values at quadrature
! nodes.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   ans : REAL(DFP), INTENT(INOUT)(0:). Result storage; allocate enough space
! for all active dimensions.
!   tsize : INTEGER(I4B), INTENT(OUT). Number of active vector output entries.
!
! Returns:
!   Writes ans(0:), tsize.
!
! Formula:
! $$M c=b,\quad M_{ij}=\int p_i p_j w\,dx,\quad b_j=\sum_k w_k f_k p_j(x_k)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Transform_
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL Chebyshev1GaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 2.0_DFP*x*x-1.0_DFP
! CALL Chebyshev1Transform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1InvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1InvTransform(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$y(x)=\sum_{j=0}^{n}c_j p_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1InvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Chebyshev1InvTransform(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(2.0_DFP*x*x-1.0_DFP))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Differentiate modal coefficients in the same family.
!
! The output retains n+1 entries; the final coefficient is zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   coeff : REAL(DFP), INTENT(IN)(0:n). Modal expansion coefficients, indexed
! from zero.
!
! Returns:
!   ans : REAL(DFP)(0:n).
!
! Formula:
! $$\sum_{j=0}^{n}d_j p_j(x)=\sum_{j=0}^{n}c_j p_j'(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(0:2), dc(0:2)
!
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! dc = Chebyshev1GradientCoeff(n=2_I4B, coeff=c)
! CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 4.0_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Derivative coefficients in the same basis")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the nodal first derivative matrix.
!
! n is the interpolation degree; pass n+1 nodes from the selected quadrature
! rule in their returned order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Interpolation degree; x contains n+1 nodes.
!   x : REAL(DFP), INTENT(IN)(0:n). Evaluation coordinate(s).
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!
! Returns:
!   ans : REAL(DFP)(0:n, 0:n).
!
! Formula:
! $$f'(x_i)=\sum_{j=0}^{n}D_{ij} f(x_j)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Gauss
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1DMatrix
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)
!
! CALL Chebyshev1Quadrature(n=3_I4B, pt=x, wt=w, quadType=Gauss)
! d = Chebyshev1DMatrix(n=2_I4B, x=x, quadType=Gauss)
! CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
!   < 1.0E-10_DFP, "Differentiate a nodal quadratic")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Form even and odd blocks by pairing mirrored columns.
!
! The example uses odd n, where both blocks are fully assigned. The even n
! branch does not assign every output entry; initialize buffers and inspect
! that branch before use.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   D : REAL(DFP), INTENT(IN)(0:n, 0:n). Jacobi matrix diagonal, or nodal
! derivative matrix in the block splitter.
!   e : REAL(DFP), INTENT(OUT)(0:, 0:). Jacobi matrix off diagonal, or even
! block in the block splitter.
!   o : REAL(DFP), INTENT(OUT)(0:, 0:). Odd block formed by mirrored column
! subtraction.
!
! Returns:
!   Writes e(0:, 0:), o(0:, 0:).
!
! Formula:
! $$E_{ij}=D_{ij}+D_{i,n-j},\quad O_{ij}=D_{ij}-D_{i,n-j}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1DMatEvenOdd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(0:3, 0:3), e(0:1, 0:1), o(0:1, 0:1)
! INTEGER(I4B) :: j
!
! DO j = 0, 3
!   d(:, j) = REAL(j, DFP)
! END DO
! CALL Chebyshev1DMatEvenOdd(n=3_I4B, D=d, e=e, o=o)
! CALL OK(MAXVAL(ABS(e-3.0_DFP)) < 1.0E-11_DFP, &
!   "Even block combines mirrored columns")
! CALL OK(MAXVAL(ABS(o(:, 0)+3.0_DFP)) < 1.0E-11_DFP, &
!   "Odd block subtracts mirrored columns")
! END PROGRAM main
!```

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

!----------------------------------------------------------------------------
!                                                 Chebyshev1LaplacianEvalAll
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEvalAll
  MODULE PURE FUNCTION Chebyshev1LaplacianEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION Chebyshev1LaplacianEvalAll1
END INTERFACE Chebyshev1LaplacianEvalAll

!----------------------------------------------------------------------------
!                                                 Chebyshev1LaplacianEvalAll_
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEvalAll_
  MODULE PURE SUBROUTINE Chebyshev1LaplacianEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! ans(1:n + 1)
  END SUBROUTINE Chebyshev1LaplacianEvalAll1_
END INTERFACE Chebyshev1LaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                 Chebyshev1LaplacianEvalAll
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEvalAll
  MODULE PURE FUNCTION Chebyshev1LaplacianEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION Chebyshev1LaplacianEvalAll2
END INTERFACE Chebyshev1LaplacianEvalAll

!----------------------------------------------------------------------------
!                                                Chebyshev1LaplacianEvalAll_
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEvalAll_
  MODULE PURE SUBROUTINE Chebyshev1LaplacianEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(1:SIZE(x), 1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE Chebyshev1LaplacianEvalAll2_
END INTERFACE Chebyshev1LaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                    Chebyshev1LaplacianEval
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEval
  MODULE PURE FUNCTION Chebyshev1LaplacianEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Chebyshev1LaplacianEval1
END INTERFACE Chebyshev1LaplacianEval

!----------------------------------------------------------------------------
!                                                     Chebyshev1LaplacianEval
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
! USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = Chebyshev1GradientEval(n=2_I4B, x=x+h)
! gm = Chebyshev1GradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(4.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE Chebyshev1LaplacianEval
  MODULE PURE FUNCTION Chebyshev1LaplacianEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION Chebyshev1LaplacianEval2
END INTERFACE Chebyshev1LaplacianEval

END MODULE Chebyshev1PolynomialUtility
