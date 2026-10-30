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
! summary: Jacobi polynomial evaluation, quadrature and transforms.
!
! The standard Jacobi normalization is used, not unit norm or monic
! normalization. alpha and beta are family parameters; alphaCoeff and
! betaCoeff are monic recurrence arrays.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$w(x)=(1-x)^\alpha(1+x)^\beta,\quad \alpha,\beta>-1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE JacobiPolynomialUtility, ONLY: JacobiEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE JacobiPolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: iface_1DFunction

IMPLICIT NONE

PRIVATE

PUBLIC :: GetJacobiRecurrenceCoeff
PUBLIC :: GetJacobiRecurrenceCoeff2
PUBLIC :: JacobiAlpha
PUBLIC :: JacobiBeta
PUBLIC :: JacobiLeadingCoeff
PUBLIC :: JacobiLeadingCoeffRatio
PUBLIC :: JacobiNormSQR
PUBLIC :: JacobiNormSQR2
PUBLIC :: JacobiNormSQRRatio
PUBLIC :: JacobiJacobiMatrix
PUBLIC :: JacobiGaussQuadrature
PUBLIC :: JacobiJacobiRadauMatrix
PUBLIC :: JacobiGaussRadauQuadrature
PUBLIC :: JacobiJacobiLobattoMatrix
PUBLIC :: JacobiGaussLobattoQuadrature
PUBLIC :: JacobiZeros
PUBLIC :: JacobiQuadrature
PUBLIC :: JacobiEvalAll
PUBLIC :: JacobiEvalAll_
PUBLIC :: JacobiEval
PUBLIC :: JacobiEvalSum
PUBLIC :: JacobiGradientEval
PUBLIC :: JacobiGradientEvalAll
PUBLIC :: JacobiGradientEvalAll_
PUBLIC :: JacobiGradientEvalSum
PUBLIC :: JacobiLaplacianEval
PUBLIC :: JacobiLaplacianEvalAll
PUBLIC :: JacobiLaplacianEvalAll_
PUBLIC :: JacobiTransform
PUBLIC :: JacobiTransform_
PUBLIC :: JacobiInvTransform
PUBLIC :: JacobiGradientCoeff
PUBLIC :: JacobiDMatrix

!----------------------------------------------------------------------------
!                                                  GetJacobiRecurrenceCoeff
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: GetJacobiRecurrenceCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2)
!
! CALL GetJacobiRecurrenceCoeff( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, alphaCoeff=a, betaCoeff=b)
! CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
!   "Symmetric family has zero diagonal")
! CALL OK(ABS(b(2)-4.0_DFP/15.0_DFP) &
!   < 1.0E-11_DFP, "Monic beta coefficient")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetJacobiRecurrenceCoeff(n, alpha, beta, &
                                                  alphaCoeff, betaCoeff)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: alphaCoeff(0:n - 1)
    REAL(DFP), INTENT(OUT) :: betaCoeff(0:n - 1)
  END SUBROUTINE GetJacobiRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetJacobiRecurrenceCoeff2
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: GetJacobiRecurrenceCoeff2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:2), b(0:2), c(0:2)
!
! CALL GetJacobiRecurrenceCoeff2( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, A=a, B=b, C=c)
! CALL OK(MAXVAL(ABS(b)) < 1.0E-11_DFP, &
!   "Symmetric family has no constant recurrence term")
! CALL OK(ABS(a(1)-1.5_DFP) < 1.0E-11_DFP, &
!   "Degree two recurrence multiplier")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetJacobiRecurrenceCoeff2(n, alpha, beta, &
                                                   A, B, C)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial, it should be greater than 1
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: A(0:n - 1)
    REAL(DFP), INTENT(OUT) :: B(0:n - 1)
    REAL(DFP), INTENT(OUT) :: C(0:n - 1)
  END SUBROUTINE GetJacobiRecurrenceCoeff2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               JacobiAlpha
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return a monic recurrence diagonal coefficient.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiAlpha
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiAlpha(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(0.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE ELEMENTAL PURE FUNCTION JacobiAlpha(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiAlpha
END INTERFACE

!----------------------------------------------------------------------------
!                                                                JacobiBeta
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiBeta
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiBeta(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(4.0_DFP/15.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE ELEMENTAL PURE FUNCTION JacobiBeta(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiBeta
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobiLeadingCoeff
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the coefficient of the highest power.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiLeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiLeadingCoeff(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(1.5_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!----------------------------------------------------------------------------
!                                                    JacobiLeadingCoeffRatio
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive leading coefficients.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiLeadingCoeffRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiLeadingCoeffRatio(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobiLeadingCoeffRatio(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha in Jacobi poly
    REAL(DFP), INTENT(IN) :: beta
    !! beta in Jacobi poly
    REAL(DFP) :: ans
    !! answer
  END FUNCTION JacobiLeadingCoeffRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobiNormSQR
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the weighted squared norm.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiNormSQR
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiNormSQR(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQR(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans
  END FUNCTION JacobiNormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                            JacobiNormSQR2
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return weighted squared norms from degree zero through n.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiNormSQR2
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y(0:2)
!
! y = JacobiNormSQR2(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y(2)-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
!   "Norm array includes degree two")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQR2(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans(0:n)
  END FUNCTION JacobiNormSQR2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobiNormSQRRatio
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiNormSQRRatio
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = JacobiNormSQRRatio(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(ABS(y-(5.0_DFP/7.0_DFP)) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobiNormSQRRatio(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans
  END FUNCTION JacobiNormSQRRatio
END INTERFACE

!----------------------------------------------------------------------------
!                                                      JacobiJacobiMatrix
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiJacobiMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(2), e(1)
!
! CALL JacobiJacobiMatrix( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiMatrix(n, alpha, beta, D, E, &
                                            alphaCoeff, betaCoeff)
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

!----------------------------------------------------------------------------
!                                                     JacobiGaussQuadrature
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(2), w(2)
!
! CALL JacobiGaussQuadrature( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE JacobiGaussQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! It represents the order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! the size is 1 to n
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! the size is 1 to n
  END SUBROUTINE JacobiGaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                   JacobiJacobiRadauMatrix
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiJacobiRadauMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(3), e(2)
!
! CALL JacobiJacobiRadauMatrix( &
!   alpha=0.0_DFP, beta=0.0_DFP, a=-1.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiRadauMatrix(a, n, alpha, beta, D, &
                                                 E, alphaCoeff, betaCoeff)
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

!----------------------------------------------------------------------------
!                                                JacobiGaussRadauQuadrature
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGaussRadauQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3)
!
! CALL JacobiGaussRadauQuadrature( &
!   alpha=0.0_DFP, beta=0.0_DFP, a=-1.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(MINVAL(ABS(x+1.0_DFP)) < 1.0E-11_DFP, &
!   "Prescribed endpoint is present")
! END PROGRAM main
!```

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
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+1 weights from 1 to n+1
  END SUBROUTINE JacobiGaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                 JacobiJacobiLobattoMatrix
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiJacobiLobattoMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: d(4), e(3)
!
! CALL JacobiJacobiLobattoMatrix( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, D=d, E=e)
! CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
!   "Symmetric weight has a zero diagonal")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE JacobiJacobiLobattoMatrix(n, alpha, beta, D, &
                                                   E, alphaCoeff, betaCoeff)
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

!----------------------------------------------------------------------------
!                                              JacobiGaussLobattoQuadrature
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGaussLobattoQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL JacobiGaussLobattoQuadrature( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, pt=x, wt=w)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE JacobiGaussLobattoQuadrature(n, alpha, beta, pt, wt)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomials
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(OUT) :: pt(:)
    !! n+2 quad points indexed from 1 to n+2
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(:)
    !! n+2 weights, index from 1 to n+2
  END SUBROUTINE JacobiGaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                              JacobiZeros
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(2)
!
! z = JacobiZeros(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
! CALL OK(MAXVAL(ABS(z*z-1.0_DFP/3.0_DFP)) &
!   < 1.0E-11_DFP, "Roots satisfy the quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION JacobiZeros(n, alpha, beta) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP) :: ans(n)
  END FUNCTION JacobiZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                          JacobiQuadrature
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
!   pt : REAL(DFP), INTENT(OUT)(n). Quadrature nodes; allocate the point count
! specified above.
!   wt : REAL(DFP), OPTIONAL, INTENT(OUT)(n). Quadrature weights paired with
! pt; includes the family weight.
!   quadType : INTEGER(I4B), INTENT(IN). Quadrature selector from GlobalData;
! must match the supplied nodes.
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
! USE JacobiPolynomialUtility, ONLY: JacobiQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4)
!
! CALL JacobiQuadrature( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=4_I4B, pt=x, wt=w, &
!   quadType=GaussLobatto)
! CALL OK(ABS(SUM(w)-(2.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate a constant")
! CALL OK(ABS(SUM(w*x*x)-(2.0_DFP/3.0_DFP)) < 1.0E-10_DFP, &
!   "Weights integrate the weighted second moment")
! CALL OK(ABS(MINVAL(x)+1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(MAXVAL(x)-1.0_DFP) < 1.0E-11_DFP, "Both endpoints")
! END PROGRAM main
!```

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
    REAL(DFP), OPTIONAL, INTENT(OUT) :: wt(n)
    !! n+1 weights from 1 to n+1
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Gauss
    !! GaussRadauLeft
    !! GaussRadauRight
    !! GaussLobatto
  END SUBROUTINE JacobiQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobiEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiEvalAll(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalAll
  MODULE PURE FUNCTION JacobiEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll1
END INTERFACE JacobiEvalAll

!----------------------------------------------------------------------------
!                                                       JacobiEvalAll
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL JacobiEvalAll_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalAll_
  MODULE PURE SUBROUTINE JacobiEvalAll1_(n, alpha, beta, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE JacobiEvalAll1_
END INTERFACE JacobiEvalAll_

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate polynomials through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiEvalAll(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalAll
  MODULE PURE FUNCTION JacobiEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
  END FUNCTION JacobiEvalAll2
END INTERFACE JacobiEvalAll

!----------------------------------------------------------------------------
!                                                              JacobiEvalAll
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL JacobiEvalAll_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalAll_
  MODULE PURE SUBROUTINE JacobiEvalAll2_(n, alpha, beta, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Evaluate Jacobi polynomial of order = 0 to n (total n+1)
    !! at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE JacobiEvalAll2_
END INTERFACE JacobiEvalAll_

!----------------------------------------------------------------------------
!                                                             JacobiEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE JacobiEval
  MODULE PURE FUNCTION JacobiEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval1
END INTERFACE JacobiEval

!----------------------------------------------------------------------------
!                                                             JacobiEvalUpto
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate one polynomial.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(JacobiEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE JacobiEval
  MODULE PURE FUNCTION JacobiEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEval2
END INTERFACE JacobiEval

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalSum
  MODULE PURE FUNCTION JacobiEvalSum1(n, alpha, beta, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEvalSum1
END INTERFACE JacobiEvalSum

!----------------------------------------------------------------------------
!                                                             JacobiEvalSum
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiEvalSum
  MODULE PURE FUNCTION JacobiEvalSum2(n, alpha, beta, x, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiEvalSum2
END INTERFACE JacobiEvalSum

!----------------------------------------------------------------------------
!                                                         JacobiGradientEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(JacobiGradientEval( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE JacobiGradientEval
  MODULE PURE FUNCTION JacobiGradientEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEval1
END INTERFACE JacobiGradientEval

!----------------------------------------------------------------------------
!                                                        JacobiGradientEval
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate the first derivative.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(JacobiGradientEval( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

INTERFACE JacobiGradientEval
  MODULE PURE FUNCTION JacobiGradientEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiGradientEval2
END INTERFACE JacobiGradientEval

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(n + 1).
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiGradientEvalAll( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalAll
  MODULE PURE FUNCTION JacobiGradientEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalAll1
END INTERFACE JacobiGradientEvalAll

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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL JacobiGradientEvalAll_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalAll_
  MODULE PURE SUBROUTINE JacobiGradientEvalAll1_(n, alpha, beta, x, &
                                                 ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE JacobiGradientEvalAll1_
END INTERFACE JacobiGradientEvalAll_

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalAll
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate first derivatives through degree n.
!
! Scalar x returns n+1 entries; vector x uses rows for points.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), n + 1).
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = JacobiGradientEvalAll( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalAll
  MODULE PURE FUNCTION JacobiGradientEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiGradientEvalAll2
END INTERFACE JacobiGradientEvalAll

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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL JacobiGradientEvalAll_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalAll_
  MODULE PURE SUBROUTINE JacobiGradientEvalAll2_(n, alpha, beta, x, &
                                                 ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE JacobiGradientEvalAll2_
END INTERFACE JacobiGradientEvalAll_

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalSum
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiGradientEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum1(n, alpha, beta, x, coeff) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum1
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiGradientEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum2(n, alpha, beta, x, coeff) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum2
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                      JacobiGradientEvalSum
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiGradientEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum3(n, alpha, beta, x, coeff, k) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! order of derivative
    REAL(DFP) :: ans
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum3
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                     JacobiGradientEvalSum
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalSum
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiGradientEvalSum( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiGradientEvalSum
  MODULE PURE FUNCTION JacobiGradientEvalSum4(n, alpha, beta, x, coeff, k) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi Polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! point
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! Coefficient of finite sum, size = n+1
    INTEGER(I4B), INTENT(IN) :: k
    !! kth order derivative
    REAL(DFP) :: ans(SIZE(x))
    !! Evaluate Jacobi polynomial of order n at point x
  END FUNCTION JacobiGradientEvalSum4
END INTERFACE JacobiGradientEvalSum

!----------------------------------------------------------------------------
!                                                           JacobiTransform
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiTransform
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL JacobiGaussQuadrature(alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = JacobiTransform( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiTransform
  MODULE PURE FUNCTION JacobiTransform1(n, alpha, beta, coeff, x, w, &
                                        quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
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
  END FUNCTION JacobiTransform1
END INTERFACE JacobiTransform

!----------------------------------------------------------------------------
!                                                         JacobiTransform_
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiTransform_
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL JacobiGaussQuadrature(alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL JacobiTransform_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiTransform_
  MODULE PURE SUBROUTINE JacobiTransform1_(n, alpha, beta, coeff, x, w, &
                                           quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    !! size is number of quadrature points
    REAL(DFP), INTENT(IN) :: x(0:)
    !! quadrature points
    !! size is quadrature points
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    !! size is quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! n+1
  END SUBROUTINE JacobiTransform1_
END INTERFACE JacobiTransform_

!----------------------------------------------------------------------------
!                                                           JacobiTransform
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiTransform_
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL JacobiGaussQuadrature(alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL JacobiTransform_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiTransform_
  MODULE PURE SUBROUTINE JacobiTransform4_(n, alpha, beta, coeff, PP, w, &
                                           quadType, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:)
    !! nodal value (at quad points)
    !! size is number of quadrature points
    REAL(DFP), INTENT(IN) :: PP(0:, 0:)
    !! quadrature points
    !! number of rows in number of quadrature points
    !! number of columns is n+1
    REAL(DFP), INTENT(IN) :: w(0:)
    !! weights
    !! size is quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(INOUT) :: ans(0:)
    !! modal values  or coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! n+1
  END SUBROUTINE JacobiTransform4_
END INTERFACE JacobiTransform_

!----------------------------------------------------------------------------
!                                                           JacobiTransform
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiTransform
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
!
! CALL JacobiGaussQuadrature(alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! c = JacobiTransform( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiTransform
  MODULE FUNCTION JacobiTransform3(n, alpha, beta, f, quadType, x1, x2) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    PROCEDURE(iface_1DFunction), POINTER, INTENT(IN) :: f
    !! 1D space function
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature type, Gauss, GaussLobatto, GaussRadau, GaussRadauLeft
    !! GaussRadauRight
    REAL(DFP), INTENT(IN) :: x1, x2
    !! domain of function f
    REAL(DFP) :: ans(0:n)
    !! modal values  or coefficients
  END FUNCTION JacobiTransform3
END INTERFACE JacobiTransform

!----------------------------------------------------------------------------
!                                                         JacobiTransform_
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiTransform_
! USE JacobiPolynomialUtility, ONLY: JacobiGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), w(3), f(3), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL JacobiGaussQuadrature(alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w)
! f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
! CALL JacobiTransform_( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, &
!   coeff=f, &
!   x=x, w=w, &
!   quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiTransform_
  MODULE SUBROUTINE JacobiTransform3_(n, alpha, beta, f, quadType, x1, x2, &
                                      ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
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
    !! n+1
  END SUBROUTINE JacobiTransform3_
END INTERFACE JacobiTransform_

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiInvTransform( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiInvTransform
  MODULE PURE FUNCTION JacobiInvTransform1(n, alpha, beta, coeff, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x
    !! x point in physical space
    REAL(DFP) :: ans
    !! value in physical space
  END FUNCTION JacobiInvTransform1
END INTERFACE JacobiInvTransform

!----------------------------------------------------------------------------
!                                                        JacobiInvTransform
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate a polynomial expansion.
!
! Coefficients are modal values indexed from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiInvTransform
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3), c(0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = JacobiInvTransform( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, coeff=c)
! CALL OK(MAXVAL(ABS(y-(0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, &
!   "A single modal coefficient recovers degree two")
! END PROGRAM main
!```

INTERFACE JacobiInvTransform
  MODULE PURE FUNCTION JacobiInvTransform2(n, alpha, beta, coeff, x) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: beta
    !! beta of Jacobi polynomial > -1.0_DFP
    REAL(DFP), INTENT(IN) :: coeff(0:n)
    !! n+1  coefficient (modal values)
    REAL(DFP), INTENT(IN) :: x(:)
    !! x point in physical space
    REAL(DFP) :: ans(SIZE(x))
    !! value in physical space
  END FUNCTION JacobiInvTransform2
END INTERFACE JacobiInvTransform

!----------------------------------------------------------------------------
!                                                      JacobiGradientCoeff
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Differentiate modal coefficients in the same family.
!
! The output retains n+1 entries; the final coefficient is zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(0:2), dc(0:2)
!
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! dc = JacobiGradientCoeff( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, coeff=c)
! CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 3.0_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Derivative coefficients in the same basis")
! END PROGRAM main
!```

INTERFACE JacobiGradientCoeff
  MODULE PURE FUNCTION JacobiGradientCoeff1(n, alpha, beta, coeff) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
    REAL(DFP), INTENT(IN) :: coeff(0:n)
      !! coefficients $\tilde{u}_{n}$ obtained from JacobiTransform
    REAL(DFP) :: ans(0:n)
      !! coefficient of gradient
  END FUNCTION JacobiGradientCoeff1
END INTERFACE JacobiGradientCoeff

!----------------------------------------------------------------------------
!                                                              JacobiDMatrix
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiDMatrix
! USE JacobiPolynomialUtility, ONLY: JacobiQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(0:2), w(0:2), d(0:2, 0:2)
!
! CALL JacobiQuadrature( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, pt=x, wt=w, quadType=Gauss)
! d = JacobiDMatrix( &
!   alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x, quadType=Gauss)
! CALL OK(MAXVAL(ABS(MATMUL(d, x*x)-2.0_DFP*x)) &
!   < 1.0E-10_DFP, "Differentiate a nodal quadratic")
! END PROGRAM main
!```

INTERFACE JacobiDMatrix
  MODULE PURE FUNCTION JacobiDMatrix1(n, alpha, beta, x, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
      !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
      !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
      !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x(0:n)
      !! quadrature points
    INTEGER(I4B), INTENT(IN) :: quadType
      !! Gauss and GaussLobatto
    REAL(DFP) :: ans(0:n, 0:n)
      !! D matrix
  END FUNCTION JacobiDMatrix1
END INTERFACE JacobiDMatrix

!----------------------------------------------------------------------------
!                                                         JacobiLaplacianEval
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEval
  MODULE PURE FUNCTION JacobiLaplacianEval1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiLaplacianEval1
END INTERFACE JacobiLaplacianEval

!----------------------------------------------------------------------------
!                                                        JacobiLaplacianEval
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEval
  MODULE PURE FUNCTION JacobiLaplacianEval2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x))
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiLaplacianEval2
END INTERFACE JacobiLaplacianEval

!----------------------------------------------------------------------------
!                                                      JacobiLaplacianEvalAll
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEvalAll
  MODULE PURE FUNCTION JacobiLaplacianEvalAll1(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP) :: ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
  END FUNCTION JacobiLaplacianEvalAll1
END INTERFACE JacobiLaplacianEvalAll

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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEvalAll_
  MODULE PURE SUBROUTINE JacobiLaplacianEvalAll1_(n, alpha, beta, x, &
                                                  ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of Jacobi polynomial
    REAL(DFP), INTENT(IN) :: alpha
    !! alpha > -1.0
    REAL(DFP), INTENT(IN) :: beta
    !! beta > -1.0
    REAL(DFP), INTENT(IN) :: x
    !! point
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(n + 1)
    !! Derivative of Jacobi polynomial of order n at point x
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE JacobiLaplacianEvalAll1_
END INTERFACE JacobiLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                     JacobiLaplacianEvalAll
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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEvalAll
  MODULE PURE FUNCTION JacobiLaplacianEvalAll2(n, alpha, beta, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
  END FUNCTION JacobiLaplacianEvalAll2
END INTERFACE JacobiLaplacianEvalAll

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
!   alpha : REAL(DFP), INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN). Jacobi weight exponent at -1.
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
! USE JacobiPolynomialUtility, ONLY: JacobiGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x+h)
! gm = JacobiGradientEval(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE JacobiLaplacianEvalAll_
  MODULE PURE SUBROUTINE JacobiLaplacianEvalAll2_(n, alpha, beta, x, &
                                                  ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: alpha
    REAL(DFP), INTENT(IN) :: beta
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    !! Derivative of Jacobi polynomial of order n at x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE JacobiLaplacianEvalAll2_
END INTERFACE JacobiLaplacianEvalAll_

END MODULE JacobiPolynomialUtility
