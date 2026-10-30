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
! summary: Gegenbauer polynomial evaluation, quadrature and transforms.
!
! Use lambda>0 for the documented examples and nonsingular normalization
! formulas. lambda=1/2 gives Legendre polynomials; lambda=0 is a singular
! limit in several routines and should not be substituted for the Chebyshev
! module.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$C_0^\lambda=1,\quad C_1^\lambda=2\lambda x,\quad
! w(x)=(1-x^2)^{\lambda-1/2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE UltrasphericalPolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
PUBLIC :: UltrasphericalLaplacianEvalAll
PUBLIC :: UltrasphericalLaplacianEvalAll_
PUBLIC :: UltrasphericalLaplacianEval
PUBLIC :: UltrasphericalEvalSum
PUBLIC :: UltrasphericalGradientEvalSum
PUBLIC :: UltrasphericalTransform
PUBLIC :: UltrasphericalTransform_
PUBLIC :: UltrasphericalInvTransform
PUBLIC :: UltrasphericalGradientCoeff
PUBLIC :: UltrasphericalDMatrix
PUBLIC :: UltrasphericalDMatEvenOdd

!----------------------------------------------------------------------------
!                                                        UltrasphericalAlpha
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return a monic recurrence diagonal coefficient.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalAlpha
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalAlpha(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(0.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return a monic recurrence beta coefficient.
!
! For beta, n=0 denotes the weight integral; the displayed norm ratio uses
! monic squared norms.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalBeta
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalBeta(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(4.0_DFP/15.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return monic three term recurrence coefficients.
!
! Arrays are indexed from zero and contain n coefficients. Use n>=1.
! betaCoeff(0) stores the weight integral.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Recurrence length or matrix size; use n>=1.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: GetUltrasphericalRecurrenceCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2)
!
! CALL GetUltrasphericalRecurrenceCoeff( &
!   lambda=0.5_DFP, n=3_I4B, alphaCoeff=a, betaCoeff=b)
! CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
!   "Symmetric family has zero diagonal")
! CALL OK(ABS(b(2)-4.0_DFP/15.0_DFP) &
!   < 1.0E-11_DFP, "Monic beta coefficient")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: GetUltrasphericalRecurrenceCoeff2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2), c(0:2)
!
! CALL GetUltrasphericalRecurrenceCoeff2( &
!   lambda=0.5_DFP, n=3_I4B, A=a, B=b, C=c)
! CALL OK(MAXVAL(ABS(b)) < 1.0E-11_DFP, &
!   "Symmetric family has no constant recurrence term")
! CALL OK(ABS(a(1)-1.5_DFP) < 1.0E-11_DFP, &
!   "Degree two recurrence multiplier")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the coefficient of the highest power.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalLeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalLeadingCoeff(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(1.5_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive leading coefficients.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalLeadingCoeffRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalLeadingCoeffRatio(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the weighted squared norm.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalNormSQR
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalNormSQR(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return weighted squared norms from degree zero through n.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalNormSQR2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(0:2)
!
! y = UltrasphericalNormSQR2(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y(2)-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Norm array includes degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalNormSQRRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = UltrasphericalNormSQRRatio(lambda=0.5_DFP, n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/7.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Gauss Jacobi matrix.
!
! Allocate D with n entries and E with one fewer. Optional recurrence outputs
! use zero based indexing.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Recurrence length or matrix size; use n>=1.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalJacobiMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(2), e(1)
!
! CALL UltrasphericalJacobiMatrix(lambda=0.5_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

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
    !! recurrence coefficient of monic Ultraspherical polynomial,
    !! from 0 to n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
    !! recurrence coefficient of monic Ultraspherical polynomial,
    !! from 0 to n-1
  END SUBROUTINE UltrasphericalJacobiMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                             UltrasphericalGaussQuadrature
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2), w(2)
!
! CALL UltrasphericalGaussQuadrature( &
!   lambda=0.5_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalJacobiRadauMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(3), e(2)
!
! CALL UltrasphericalJacobiRadauMatrix( &
!   lambda=0.5_DFP, a=-1.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: &
!   UltrasphericalGaussRadauQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3)
!
! CALL UltrasphericalGaussRadauQuadrature( &
!   lambda=0.5_DFP, a=-1.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(MINVAL(ABS(x+1.0_DFP)) < 1.0E-11_DFP, &
!   "Prescribed endpoint is present")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalJacobiLobattoMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(4), e(3)
!
! CALL UltrasphericalJacobiLobattoMatrix( &
!   lambda=0.5_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: &
!   UltrasphericalGaussLobattoQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL UltrasphericalGaussLobattoQuadrature( &
!   lambda=0.5_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the n roots of a degree n polynomial.
!
! Use n>=1. Root order is family dependent; do not assume increasing order.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Number of returned nodes; use n>=1 and
! enough nodes for the rule.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(2)
!
! z = UltrasphericalZeros(lambda=0.5_DFP, n=2_I4B)
! CALL OK(MAXVAL(ABS(z*z-1.0_DFP/3.0_DFP)) &
!   < 1.0E-11_DFP, "Roots satisfy the quadratic")
! END PROGRAM main
!```

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL UltrasphericalQuadrature( &
!   lambda=0.5_DFP, n=4_I4B, pt=x, wt=w, quadType=GaussLobatto)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UltrasphericalEval
  MODULE PURE FUNCTION UltrasphericalEval1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEval1
END INTERFACE UltrasphericalEval

!----------------------------------------------------------------------------
!                                                          UltrasphericalEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UltrasphericalEval(lambda=0.5_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UltrasphericalEval
  MODULE PURE FUNCTION UltrasphericalEval2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Ultraspherical polynomial of order n at point x
  END FUNCTION UltrasphericalEval2
END INTERFACE UltrasphericalEval

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalEvalAll(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UltrasphericalEvalAll_( &
!   lambda=0.5_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalEvalAll(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output
! columns.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UltrasphericalEvalAll_( &
!   lambda=0.5_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalEvalAll_
 MODULE PURE SUBROUTINE UltrasphericalEvalAll2_(n, lambda, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    !! Independent sample coordinates.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Values in (1:SIZE(x), 1:n+1); capacity must cover that range.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow, ncol
    !! Active row count SIZE(x), and active column count n+1.
  END SUBROUTINE UltrasphericalEvalAll2_
END INTERFACE UltrasphericalEvalAll_

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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalGradientEvalAll(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UltrasphericalGradientEvalAll_( &
!   lambda=0.5_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalGradientEvalAll(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points. Supply enough
! storage for the active output; trailing entries are not part of the result.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL UltrasphericalGradientEvalAll_( &
!   lambda=0.5_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEval
  MODULE PURE FUNCTION UltrasphericalGradientEval1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UltrasphericalGradientEval1
END INTERFACE UltrasphericalGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEval
  MODULE PURE FUNCTION UltrasphericalGradientEval2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UltrasphericalGradientEval2
END INTERFACE UltrasphericalGradientEval

!----------------------------------------------------------------------------
!                                                     UltrasphericalEvalSum
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalEvalSum(lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalEvalSum
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
END INTERFACE UltrasphericalEvalSum

!----------------------------------------------------------------------------
!                                                      UltrasphericalEvalSum
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalEvalSum(lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalEvalSum
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
END INTERFACE UltrasphericalEvalSum

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalGradientEvalSum( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEvalSum
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
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                              UltrasphericalGradientEvalSum
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalGradientEvalSum( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEvalSum
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
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                              UltrasphericalGradientEvalSum
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalGradientEvalSum( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEvalSum
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
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                             UltrasphericalGradientEvalSum
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalGradientEvalSum( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientEvalSum
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
END INTERFACE UltrasphericalGradientEvalSum

!----------------------------------------------------------------------------
!                                                  UltrasphericalTransform
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = UltrasphericalTransform( &
!   lambda=0.5_DFP, n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE UltrasphericalTransform
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
END INTERFACE UltrasphericalTransform

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform_
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL UltrasphericalTransform_( &
!   lambda=0.5_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE UltrasphericalTransform_
  MODULE PURE SUBROUTINE UltrasphericalTransform1_(n, lambda, coeff, x, w, &
                                                   quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
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
    !! size of ans
    !! n + 1
  END SUBROUTINE UltrasphericalTransform1_
END INTERFACE UltrasphericalTransform_

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform_
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL UltrasphericalTransform_( &
!   lambda=0.5_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE UltrasphericalTransform_
  MODULE PURE SUBROUTINE UltrasphericalTransform4_(n, lambda, coeff, PP, w, &
                                                   quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    !! size is number of quadrature points
    REAL(DFP), INTENT(IN) :: PP(0:, 0:)
    !! quadrature points
    !! number of rows is number of quadrature points
    !! number of columns is n+1
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    !! size of number of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! ans(0:n)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of ans
    !! n + 1
  END SUBROUTINE UltrasphericalTransform4_
END INTERFACE UltrasphericalTransform_

!----------------------------------------------------------------------------
!                                                   UltrasphericalTransform
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = UltrasphericalTransform( &
!   lambda=0.5_DFP, n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE UltrasphericalTransform
  MODULE FUNCTION UltrasphericalTransform3(n, lambda, f, quadType, x1, x2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION UltrasphericalTransform3
END INTERFACE UltrasphericalTransform

!----------------------------------------------------------------------------
!                                                    UltrasphericalTransform
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
!   f : PROCEDURE(iface_1DFunction), POINTER, INTENT(IN). Procedure pointer to
! an iface_1DFunction callback.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
!   x1 : REAL(DFP), INTENT(IN). Left endpoint of the callback evaluation
! interval.
!   x2 : REAL(DFP), INTENT(IN). Right endpoint of the callback evaluation
! interval.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform_
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL UltrasphericalTransform_( &
!   lambda=0.5_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE UltrasphericalTransform_
  MODULE SUBROUTINE UltrasphericalTransform3_(n, lambda, f, quadType, &
                                              x1, x2, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! $\lambda > -0.5, \lambda \ne 0.0$
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! domain of function f
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! ans(0:n)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE UltrasphericalTransform3_
END INTERFACE UltrasphericalTransform_

!----------------------------------------------------------------------------
!                                                 UltrasphericalInvTransform
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalInvTransform( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION UltrasphericalInvTransform1(n, lambda, coeff, x) &
    RESULT(ans)
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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalInvTransform( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalInvTransform
  MODULE PROCEDURE UltrasphericalInvTransform1
END INTERFACE UltrasphericalInvTransform

!----------------------------------------------------------------------------
!                                                 UltrasphericalInvTransform
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalInvTransform( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = UltrasphericalInvTransform( &
!   lambda=0.5_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE UltrasphericalInvTransform
  MODULE PROCEDURE UltrasphericalInvTransform2
END INTERFACE UltrasphericalInvTransform

!----------------------------------------------------------------------------
!                                               UltrasphericalGradientCoeff
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Differentiate modal coefficients in the same family.
!
! The output retains n+1 entries; the final coefficient is zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(0:2), dc(0:2)
!
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! dc = UltrasphericalGradientCoeff(lambda=0.5_DFP, n=2_I4B, coeff=c)
! CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 3.0_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Derivative coefficients in the same basis")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Differentiate modal coefficients in the same family.
!
! The output retains n+1 entries; the final coefficient is zero.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(0:2), dc(0:2)
!
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! dc = UltrasphericalGradientCoeff(lambda=0.5_DFP, n=2_I4B, coeff=c)
! CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 3.0_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Derivative coefficients in the same basis")
! END PROGRAM main
!```

INTERFACE UltrasphericalGradientCoeff
  MODULE PROCEDURE UltrasphericalGradientCoeff1
END INTERFACE UltrasphericalGradientCoeff

!----------------------------------------------------------------------------
!                                                     UltrasphericalDMatrix
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalDMatrix
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)
!
! CALL UltrasphericalQuadrature( &
!   lambda=0.5_DFP, n=3_I4B, pt=x, wt=w, quadType=Gauss)
! d = UltrasphericalDMatrix( &
!   lambda=0.5_DFP, n=2_I4B, x=x, quadType=Gauss)
! CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
!   < 1.0E-10_DFP, "Differentiate a nodal quadratic")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Build the nodal first derivative matrix.
!
! n is the interpolation degree; pass n+1 nodes from the selected quadrature
! rule in their returned order.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalDMatrix
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)
!
! CALL UltrasphericalQuadrature( &
!   lambda=0.5_DFP, n=3_I4B, pt=x, wt=w, quadType=Gauss)
! d = UltrasphericalDMatrix( &
!   lambda=0.5_DFP, n=2_I4B, x=x, quadType=Gauss)
! CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
!   < 1.0E-10_DFP, "Differentiate a nodal quadratic")
! END PROGRAM main
!```

INTERFACE UltrasphericalDMatrix
  MODULE PROCEDURE UltrasphericalDMatrix1
END INTERFACE UltrasphericalDMatrix

!----------------------------------------------------------------------------
!                                                 UltrasphericalDMatEvenOdd
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalDMatEvenOdd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(0:3, 0:3), e(0:1, 0:1), o(0:1, 0:1)
! INTEGER(I4B) :: j
!
! DO j = 0, 3
!   d(:, j) = REAL(j, DFP)
! END DO
! CALL UltrasphericalDMatEvenOdd(n=3_I4B, D=d, e=e, o=o)
! CALL OK(MAXVAL(ABS(e-3.0_DFP)) < 1.0E-11_DFP, &
!   "Even block combines mirrored columns")
! CALL OK(MAXVAL(ABS(o(:, 0)+3.0_DFP)) < 1.0E-11_DFP, &
!   "Odd block subtracts mirrored columns")
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Form even and odd blocks by pairing mirrored columns.
!
! The example uses odd n, where both blocks are fully assigned. The even n
! branch does not assign every output entry; initialize buffers and inspect
! that branch before use.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalDMatEvenOdd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(0:3, 0:3), e(0:1, 0:1), o(0:1, 0:1)
! INTEGER(I4B) :: j
!
! DO j = 0, 3
!   d(:, j) = REAL(j, DFP)
! END DO
! CALL UltrasphericalDMatEvenOdd(n=3_I4B, D=d, e=e, o=o)
! CALL OK(MAXVAL(ABS(e-3.0_DFP)) < 1.0E-11_DFP, &
!   "Even block combines mirrored columns")
! CALL OK(MAXVAL(ABS(o(:, 0)+3.0_DFP)) < 1.0E-11_DFP, &
!   "Odd block subtracts mirrored columns")
! END PROGRAM main
!```

INTERFACE UltrasphericalDMatEvenOdd
  MODULE PROCEDURE UltrasphericalDMatEvenOdd1
END INTERFACE UltrasphericalDMatEvenOdd

!----------------------------------------------------------------------------
!                                             UltrasphericalLaplacianEvalAll
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEvalAll
  MODULE PURE FUNCTION UltrasphericalLaplacianEvalAll1(n, lambda, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION UltrasphericalLaplacianEvalAll1
END INTERFACE UltrasphericalLaplacianEvalAll

!----------------------------------------------------------------------------
!                                           UltrashphericalLaplacianEvalAll_
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEvalAll_
  MODULE PURE SUBROUTINE UltrasphericalLaplacianEvalAll1_(n, lambda, x, ans, &
                                                          tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! 1:n+1
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE UltrasphericalLaplacianEvalAll1_
END INTERFACE UltrasphericalLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                             UltrasphericalLaplacianEvalAll
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEvalAll
  MODULE PURE FUNCTION UltrasphericalLaplacianEvalAll2(n, lambda, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION UltrasphericalLaplacianEvalAll2
END INTERFACE UltrasphericalLaplacianEvalAll

!----------------------------------------------------------------------------
!                                             UltraSphericalLaplacianEvalAll_
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEvalAll_
  MODULE PURE SUBROUTINE UltrasphericalLaplacianEvalAll2_(n, lambda, x, ans, &
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
  END SUBROUTINE UltrasphericalLaplacianEvalAll2_
END INTERFACE UltrasphericalLaplacianEvalAll_

!----------------------------------------------------------------------------
!
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEval
  MODULE PURE FUNCTION UltrasphericalLaplacianEval1(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION UltrasphericalLaplacianEval1
END INTERFACE UltrasphericalLaplacianEval

!----------------------------------------------------------------------------
!
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
!   lambda : REAL(DFP), INTENT(IN). Gegenbauer family parameter.
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
! USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x+h)
! gm = UltrasphericalGradientEval(lambda=0.5_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE UltrasphericalLaplacianEval
  MODULE PURE FUNCTION UltrasphericalLaplacianEval2(n, lambda, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: lambda
    !! lambda should be greater than -0.5
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION UltrasphericalLaplacianEval2
END INTERFACE UltrasphericalLaplacianEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE UltrasphericalPolynomialUtility
