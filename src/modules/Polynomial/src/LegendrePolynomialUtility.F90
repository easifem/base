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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Legendre evaluation, recurrence, quadrature and transforms.
!
! Standard Legendre normalization has P_n(1)=1. Evaluation degree n and
! quadrature point count n are distinct concepts.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$P_0=1,\quad P_1=x,\quad w(x)=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LegendrePolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE LegendrePolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: iface_1DFunction
IMPLICIT NONE
PRIVATE

PUBLIC :: LegendreAlpha
PUBLIC :: LegendreBeta
PUBLIC :: GetLegendreRecurrenceCoeff
PUBLIC :: GetLegendreRecurrenceCoeff2
PUBLIC :: LegendreLeadingCoeff
PUBLIC :: LegendreLeadingCoeffRatio
PUBLIC :: LegendreNormSQR
PUBLIC :: LegendreNormSQR2
PUBLIC :: LegendreNormSqrRatio
PUBLIC :: LegendreJacobiMatrix
PUBLIC :: LegendreGaussQuadrature
PUBLIC :: LegendreJacobiRadauMatrix
PUBLIC :: LegendreGaussRadauQuadrature
PUBLIC :: LegendreJacobiLobattoMatrix
PUBLIC :: LegendreGaussLobattoQuadrature
PUBLIC :: LegendreZeros
PUBLIC :: LegendreQuadrature
PUBLIC :: LegendreEval
PUBLIC :: LegendreEvalAll
PUBLIC :: LegendreEvalAll_
PUBLIC :: LegendreMonomialExpansionAll
PUBLIC :: LegendreMonomialExpansion
PUBLIC :: LegendreGradientEvalAll
PUBLIC :: LegendreGradientEvalAll_
PUBLIC :: LegendreGradientEval
PUBLIC :: LegendreLaplacianEvalAll
PUBLIC :: LegendreLaplacianEvalAll_
PUBLIC :: LegendreLaplacianEval
PUBLIC :: LegendreEvalSum
PUBLIC :: LegendreGradientEvalSum
PUBLIC :: LegendreTransform
PUBLIC :: LegendreTransform_
PUBLIC :: LegendreInvTransform
PUBLIC :: LegendreGradientCoeff
PUBLIC :: LegendreDMatrix
PUBLIC :: LegendreDMatEvenOdd

!----------------------------------------------------------------------------
!                                                             LegendreAlpha
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreAlpha
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreAlpha(n=2_I4B)
! CALL OK(ABS(y-(0.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreAlpha(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                              LegendreBeta
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreBeta
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreBeta(n=2_I4B)
! CALL OK(ABS(y-(4.0_DFP/15.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreBeta(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetLegendreRecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: GetLegendreRecurrenceCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2)
!
! CALL GetLegendreRecurrenceCoeff(n=3_I4B, alphaCoeff=a, betaCoeff=b)
! CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
!   "Symmetric family has zero diagonal")
! CALL OK(ABS(b(2)-4.0_DFP/15.0_DFP) &
!   < 1.0E-11_DFP, "Monic beta coefficient")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetLegendreRecurrenceCoeff(n, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial, it should be greater than 1
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetLegendreRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLegendreRecurrenceCoeff2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: GetLegendreRecurrenceCoeff2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2), c(0:2)
!
! CALL GetLegendreRecurrenceCoeff2(n=3_I4B, A=a, B=b, C=c)
! CALL OK(MAXVAL(ABS(b)) < 1.0E-11_DFP, &
!   "Symmetric family has no constant recurrence term")
! CALL OK(ABS(a(1)-1.5_DFP) < 1.0E-11_DFP, &
!   "Degree two recurrence multiplier")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetLegendreRecurrenceCoeff2(n, A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial, it should be greater than 1
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    !! size is n
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    !! this coefficient is zero
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
    !! size is n
  END SUBROUTINE GetLegendreRecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         LegendreLeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreLeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreLeadingCoeff(n=2_I4B)
! CALL OK(ABS(y-(1.5_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreLeadingCoeff(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreLeadingCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                 LegendreLeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreLeadingCoeffRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreLeadingCoeffRatio(n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreLeadingCoeffRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans
    !! answer
  END FUNCTION LegendreLeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                            LegendreNormSQR
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreNormSQR
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreNormSQR(n=2_I4B)
! CALL OK(ABS(y-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQR(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION LegendreNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                            LegendreNormSQR2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreNormSQR2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(0:2)
!
! y = LegendreNormSQR2(n=2_I4B)
! CALL OK(ABS(y(2)-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Norm array includes degree two")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQR2(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(0:n)
  END FUNCTION LegendreNormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      LegendreNormSQRRatio
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.
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
! USE LegendrePolynomialUtility, ONLY: LegendreNormSqrRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreNormSqrRatio(n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/7.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreNormSQRRatio(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans
  END FUNCTION LegendreNormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                      LegendreJacobiMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreJacobiMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(2), e(1)
!
! CALL LegendreJacobiMatrix(n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiMatrix(n, D, E, alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    !! recurrence coefficient of monic legendre polynomial, from 0 to n-1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
    !! recurrence coefficient of monic legendre polynomial, from 0 to n-1
  END SUBROUTINE LegendreJacobiMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LegendreGaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2), w(2)
!
! CALL LegendreGaussQuadrature(n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE LegendreGaussQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE LegendreGaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                               LegendreJacobiRadauMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreJacobiRadauMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(3), e(2)
!
! CALL LegendreJacobiRadauMatrix(a=-1.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiRadauMatrix(a, n, D, E, alphaCoeff, &
    & betaCoeff)
    REAL(DFP), INTENT(IN) :: a
    !! one of the end of the domain
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+1
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreJacobiRadauMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                LegendreGaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGaussRadauQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3)
!
! CALL LegendreGaussRadauQuadrature(a=-1.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(MINVAL(ABS(x+1.0_DFP)) < 1.0E-11_DFP, &
!   "Prescribed endpoint is present")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE LegendreGaussRadauQuadrature(a, n, pt, wt)
    REAL(DFP), INTENT(IN) :: a
    !! the value of one of the end points
    !! it should be either -1 or +1
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+1 quadrature points from 1 to n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE LegendreGaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                             LegendreLegendreLobattoMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreJacobiLobattoMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(4), e(3)
!
! CALL LegendreJacobiLobattoMatrix(n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE LegendreJacobiLobattoMatrix(n, D, E, alphaCoeff, &
    & betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! n should be greater than or equal to 1
    REAL(DFP), INTENT(OUT) :: D(:)
    !! the size should be 1:n+2
    REAL(DFP), INTENT(OUT) :: E(:)
    !! the size should be 1:n+1
    REAL(DFP), OPTIONAL, INTENT(OUT) :: alphaCoeff(0:)
    REAL(DFP), OPTIONAL, INTENT(OUT) :: betaCoeff(0:)
  END SUBROUTINE LegendreJacobiLobattoMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                              LegendreGaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGaussLobattoQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL LegendreGaussLobattoQuadrature(n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE LegendreGaussLobattoQuadrature(n, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomials
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE LegendreGaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                              LegendreZeros
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(2)
!
! z = LegendreZeros(n=2_I4B)
! CALL OK(MAXVAL(ABS(z*z-1.0_DFP/3.0_DFP)) &
!   < 1.0E-11_DFP, "Roots satisfy the quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION LegendreZeros(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomial
    REAL(DFP) :: ans(n)
  END FUNCTION LegendreZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                          LegendreQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL LegendreQuadrature(n=4_I4B, pt=x, wt=w, quadType=GaussLobatto)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE LegendreQuadrature(n, pt, wt, quadType, onlyInside)
    INTEGER(I4B), INTENT(IN) :: n
    !! number of quadrature points, the order will be computed as follows
    !! for quadType = Gauss, n is same as order of Legendre polynomial
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
  END SUBROUTINE LegendreQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                             LegendreEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation, it should be between -1 and 1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEval1
END INTERFACE

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval1
END INTERFACE LegendreEval

!----------------------------------------------------------------------------
!                                                          LegendreEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! several points of evaluation
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at points x
  END FUNCTION LegendreEval2
END INTERFACE

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE LegendreEval
  MODULE PROCEDURE LegendreEval2
END INTERFACE LegendreEval

!----------------------------------------------------------------------------
!                                                            LegendreEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalAll
  MODULE PURE FUNCTION LegendreEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x
    !! Point of evaluation, $x \in [-1, 1]$
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Legendre polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION LegendreEvalAll1
END INTERFACE LegendreEvalAll

!----------------------------------------------------------------------------
!                                                         LegendreEvalAll_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LegendreEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalAll_
  MODULE PURE SUBROUTINE LegendreEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x
    !! Point of evaluation, $x \in [-1, 1]$
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Evaluate Legendre polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LegendreEvalAll1_
END INTERFACE LegendreEvalAll_

!----------------------------------------------------------------------------
!                                                           LegendreEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalAll
  MODULE PURE FUNCTION LegendreEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x(:)
    !! number of points, SIZE(x)=M
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! shape (M,N+1)
  END FUNCTION LegendreEvalAll2
END INTERFACE LegendreEvalAll

!----------------------------------------------------------------------------
!                                                          LegendreEvalAll_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LegendreEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalAll_
  MODULE PURE SUBROUTINE LegendreEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! Highest order of polynomial.
    !! Polynomials from 0 to n will be computed.
    REAL(DFP), INTENT(IN) :: x(:)
    !! number of points, SIZE(x)=M
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! shape (M,N+1)
    !! ans(:, jj) denotes value of Pjj at x
    !! ans(ii, :) denotes value of all polynomials at x(ii)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LegendreEvalAll2_
END INTERFACE LegendreEvalAll_

!----------------------------------------------------------------------------
!                                             LegendreMonomialExpansionAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreMonomialExpansionAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3, 3)
!
! c = LegendreMonomialExpansionAll(n=2_I4B)
! CALL OK(MAXVAL(ABS(c(:, 3)- &
!   ([-0.5_DFP, 0.0_DFP, 1.5_DFP]))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION LegendreMonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                             LegendreMonomialExpansion
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreMonomialExpansion
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3)
!
! c = LegendreMonomialExpansion(n=2_I4B)
! CALL OK(MAXVAL(ABS(c- &
!   ([-0.5_DFP, 0.0_DFP, 1.5_DFP]))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LegendreMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LegendreMonomialExpansion
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalAll
  MODULE PURE FUNCTION LegendreGradientEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LegendreGradientEvalAll1
END INTERFACE LegendreGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LegendreGradientEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalAll_
  MODULE PURE SUBROUTINE LegendreGradientEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size
  END SUBROUTINE LegendreGradientEvalAll1_
END INTERFACE LegendreGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalAll
  MODULE PURE FUNCTION LegendreGradientEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LegendreGradientEvalAll2
END INTERFACE LegendreGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LegendreGradientEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalAll_
  MODULE PURE SUBROUTINE LegendreGradientEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(1:SIZE(x), 1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x)
    !! ncol = n + 1
  END SUBROUTINE LegendreGradientEvalAll2_
END INTERFACE LegendreGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE LegendreGradientEval
  MODULE PURE FUNCTION LegendreGradientEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LegendreGradientEval1
END INTERFACE LegendreGradientEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LegendreGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LegendreGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE LegendreGradientEval
  MODULE PURE FUNCTION LegendreGradientEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LegendreGradientEval2
END INTERFACE LegendreGradientEval

!----------------------------------------------------------------------------
!                                                     LegendreEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalSum
  MODULE PURE FUNCTION LegendreEvalSum1(n, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEvalSum1
END INTERFACE LegendreEvalSum

!----------------------------------------------------------------------------
!                                                          LegendreEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreEvalSum
  MODULE PURE FUNCTION LegendreEvalSum2(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreEvalSum2
END INTERFACE LegendreEvalSum

!----------------------------------------------------------------------------
!                                             LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreGradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalSum
  MODULE PURE FUNCTION LegendreGradientEvalSum1(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum1
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                              LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreGradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalSum
  MODULE PURE FUNCTION LegendreGradientEvalSum2(n, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum2
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                              LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreGradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalSum
  MODULE PURE FUNCTION LegendreGradientEvalSum3(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum3
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                             LegendreGradientEvalSum
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreGradientEvalSum(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreGradientEvalSum
  MODULE PURE FUNCTION LegendreGradientEvalSum4(n, x, coeff, k) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Legendre polynomial of order n at point x
  END FUNCTION LegendreGradientEvalSum4
END INTERFACE LegendreGradientEvalSum

!----------------------------------------------------------------------------
!                                                         LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreTransform
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL LegendreGaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = LegendreTransform(n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE LegendreTransform
  MODULE PURE FUNCTION LegendreTransform1(n, coeff, x, w, &
                                          quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Legendre polynomials
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! value of function at quadrature points
    !! size if number of quadrature points
    !! number of quadrature points should be at least n+1
    REAL(DFP), INTENT(IN) :: x(0:)
    !! quadrature points
    !! These quadrature points are used in LegendreEvalAll method
    !! size is number of quadrature points
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    !! size is number of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients of Legendre polynomial
    !! ans(0) is coefficient of P0
    !! ans(1) is coefficient of P1
    !! and so on
  END FUNCTION LegendreTransform1
END INTERFACE LegendreTransform

!----------------------------------------------------------------------------
!                                                 LegendreTransform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL LegendreTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE LegendreTransform_
  MODULE PURE SUBROUTINE LegendreTransform1_(n, coeff, x, w, quadType, ans, &
                                             tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! Order of Legendre polynomials
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! Value of function at quadrature points
    REAL(DFP), INTENT(IN) :: x(0:)
    !! Quadrature points
    !! These quadrature points are used in LegendreEvalAll method
    REAL(DFP), INTENT(IN) :: w(0:)
    !! Weights
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients of Legendre polynomial
    !! ans(0) is coefficient of P0
    !! ans(1) is coefficient of P1
    !! and so on
    ! REAL(DFP) :: ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE LegendreTransform1_
END INTERFACE LegendreTransform_

!----------------------------------------------------------------------------
!                                                          LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL LegendreTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE LegendreTransform_
  MODULE PURE SUBROUTINE LegendreTransform4_(n, coeff, PP, w, quadType, ans, &
                                             tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! Order of Legendre polynomials
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! Value of function at quadrature points
    !! size is number of quadrature points
    REAL(DFP), INTENT(IN) :: PP(0:, 0:)
    !! Quadrature points
    !! These quadrature points are used in LegendreEvalAll method
    !! number of rows in PP is number of quadrature points
    !! number of columns in PP is n+1
    REAL(DFP), INTENT(IN) :: w(0:)
    !! Weights
    !! soze of w is number of quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients of Legendre polynomial
    !! ans(0) is coefficient of P0
    !! ans(1) is coefficient of P1
    !! and so on
    ! REAL(DFP) :: ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of ans
  END SUBROUTINE LegendreTransform4_
END INTERFACE LegendreTransform_

!----------------------------------------------------------------------------
!                                                          LegendreTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreTransform
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL LegendreGaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = LegendreTransform(n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE LegendreTransform
  MODULE FUNCTION LegendreTransform3(n, f, quadType, x1, x2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! domain of function f
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION LegendreTransform3
END INTERFACE LegendreTransform

!----------------------------------------------------------------------------
!                                                 LegendreTransform@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL LegendreTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE LegendreTransform_
  MODULE SUBROUTINE LegendreTransform3_(n, f, quadType, x1, x2, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! domain of function f
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients
    !! ans(0:n)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! n+1
  END SUBROUTINE LegendreTransform3_
END INTERFACE LegendreTransform_

!----------------------------------------------------------------------------
!                                                       LegendreInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreInvTransform(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreInvTransform
  MODULE PURE FUNCTION LegendreInvTransform1(n, coeff, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION LegendreInvTransform1
END INTERFACE LegendreInvTransform

!----------------------------------------------------------------------------
!                                                      LegendreInvTransform
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = LegendreInvTransform(n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE LegendreInvTransform
  MODULE PURE FUNCTION LegendreInvTransform2(n, coeff, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION LegendreInvTransform2
END INTERFACE LegendreInvTransform

!----------------------------------------------------------------------------
!                                                     LegendreGradientCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(0:2), dc(0:2)
!
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! dc = LegendreGradientCoeff(n=2_I4B, coeff=c)
! CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 3.0_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Derivative coefficients in the same basis")
! END PROGRAM main
!```

INTERFACE LegendreGradientCoeff
  MODULE PURE FUNCTION LegendreGradientCoeff1(n, coeff) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from LegendreTransform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION LegendreGradientCoeff1
END INTERFACE LegendreGradientCoeff

!----------------------------------------------------------------------------
!                                                           LegendreDMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreDMatrix
! USE LegendrePolynomialUtility, ONLY: LegendreQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)
!
! CALL LegendreQuadrature(n=3_I4B, pt=x, wt=w, quadType=Gauss)
! d = LegendreDMatrix(n=2_I4B, x=x, quadType=Gauss)
! CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
!   < 1.0E-10_DFP, "Differentiate a nodal quadratic")
! END PROGRAM main
!```

INTERFACE LegendreDMatrix
  MODULE PURE FUNCTION LegendreDMatrix1(n, x, quadType) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION LegendreDMatrix1
END INTERFACE LegendreDMatrix

!----------------------------------------------------------------------------
!                                                       LegendreDMatEvenOdd
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreDMatEvenOdd
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(0:3, 0:3), e(0:1, 0:1), o(0:1, 0:1)
! INTEGER(I4B) :: j
!
! DO j = 0, 3
!   d(:, j) = REAL(j, DFP)
! END DO
! CALL LegendreDMatEvenOdd(n=3_I4B, D=d, e=e, o=o)
! CALL OK(MAXVAL(ABS(e-3.0_DFP)) < 1.0E-11_DFP, &
!   "Even block combines mirrored columns")
! CALL OK(MAXVAL(ABS(o(:, 0)+3.0_DFP)) < 1.0E-11_DFP, &
!   "Odd block subtracts mirrored columns")
! END PROGRAM main
!```

INTERFACE LegendreDMatEvenOdd
  MODULE PURE SUBROUTINE LegendreDMatEvenOdd1(n, D, e, o)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Legendre polynomial
    REAL(DFP), INTENT(IN) :: D(0:n, 0:n)
      !! n+1 by n+1
    REAL(DFP), INTENT(OUT) :: e(0:, 0:)
      !! even Decomposition, 0:n/2, 0:n/2
    REAL(DFP), INTENT(OUT) :: o(0:, 0:)
      !! odd decomposition, 0:n/2, 0:n/2
  END SUBROUTINE LegendreDMatEvenOdd1
END INTERFACE LegendreDMatEvenOdd

!----------------------------------------------------------------------------
!                                                    LegendreLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEvalAll
  MODULE PURE FUNCTION LegendreLaplacianEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LegendreLaplacianEvalAll1
END INTERFACE LegendreLaplacianEvalAll

!----------------------------------------------------------------------------
!                                                   LegendreLaplacianEvalAll_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEvalAll_
  MODULE PURE SUBROUTINE LegendreLaplacianEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size
  END SUBROUTINE LegendreLaplacianEvalAll1_
END INTERFACE LegendreLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                    LegendreLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEvalAll
  MODULE PURE FUNCTION LegendreLaplacianEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LegendreLaplacianEvalAll2
END INTERFACE LegendreLaplacianEvalAll

!----------------------------------------------------------------------------
!                                                   LegendreLaplacianEvalAll_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEvalAll_
  MODULE PURE SUBROUTINE LegendreLaplacianEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(1:SIZE(x), 1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x)
    !! ncol = n + 1
  END SUBROUTINE LegendreLaplacianEvalAll2_
END INTERFACE LegendreLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                      LegendreLaplacianEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEval
  MODULE PURE FUNCTION LegendreLaplacianEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LegendreLaplacianEval1
END INTERFACE LegendreLaplacianEval

!----------------------------------------------------------------------------
!                                                      LegendreLaplacianEval
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LegendrePolynomialUtility, ONLY: LegendreGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LegendreGradientEval(n=2_I4B, x=x+h)
! gm = LegendreGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LegendreLaplacianEval
  MODULE PURE FUNCTION LegendreLaplacianEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LegendreLaplacianEval2
END INTERFACE LegendreLaplacianEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LegendrePolynomialUtility
