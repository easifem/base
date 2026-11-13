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
! summary: Scaled integrated Legendre basis and Lobatto kernel utilities.
!
! The first two modes are endpoint functions. Interior modes vanish at both
! endpoints and have unit derivative norm. These modes are not an orthogonal
! mass basis; projection solves a mass matrix system.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$\phi_0=(1-x)/2,\quad\phi_1=(1+x)/2$$
! $$\phi_n=(P_n-P_{n-2})/\sqrt{2(2n-1)},\quad n\ge2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

MODULE LobattoPolynomialUtility
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
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

PUBLIC :: LobattoLaplacianEvalAll
PUBLIC :: LobattoLaplacianEvalAll_
PUBLIC :: LobattoLaplacianEval

PUBLIC :: LobattoMassMatrix
PUBLIC :: LobattoStiffnessMatrix

PUBLIC :: LobattoTransform_

PUBLIC :: Lobatto0, Lobatto1, Lobatto2, Lobatto3, Lobatto4, Lobatto5

PUBLIC :: Lobatto6, Lobatto7, Lobatto8, Lobatto9, Lobatto10

!----------------------------------------------------------------------------
!                                                         LobattoTransform_
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
! USE LobattoPolynomialUtility, ONLY: LobattoTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4), f(4), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=4_I4B, pt=x, wt=w)
! f = 1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)
! CALL LobattoTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4), f(4), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=4_I4B, pt=x, wt=w)
! f = 1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)
! CALL LobattoTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoTransform_
! USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(4), w(4), f(4), c(0:2)
! INTEGER(I4B) :: ns
!
! CALL LegendreGaussQuadrature(n=4_I4B, pt=x, wt=w)
! f = 1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)
! CALL LobattoTransform_( &
!   n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
! CALL OK(MAXVAL(ABS(c- &
!   [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
!   "Recover the single degree two modal coefficient")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
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
! USE LobattoPolynomialUtility, ONLY: LobattoLeadingCoeff
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LobattoLeadingCoeff(n=2_I4B)
! CALL OK(ABS(y-(1.5_DFP/SQRT(6.0_DFP))) < 1.0E-11_DFP, &
!   "Degree two coefficient or norm")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
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
! USE LobattoPolynomialUtility, ONLY: LobattoZeros
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: z(3)
!
! z = LobattoZeros(n=3_I4B)
! CALL OK(MAXVAL(ABS(z- &
!   [-1.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-11_DFP, &
!   "Three nodes include both endpoints")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LobattoEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Lobatto kernel modes.
!
! The second index enumerates kernel modes from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(1:SIZE(x), 0:n).
!
! Formula:
! $$K_j(x)=-\frac{\sqrt{8(2j+3)}}{(j+1)(j+2)}C_j^{3/2}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: LobattoKernelEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoKernelEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 0)-(-SQRT(6.0_DFP)))) &
!   < 1.0E-11_DFP, "Constant kernel mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate Lobatto kernel modes.
!
! The second index enumerates kernel modes from zero.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(1:, 0:). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(1:, 0:), nrow, ncol.
!
! Formula:
! $$K_j(x)=-\frac{\sqrt{8(2j+3)}}{(j+1)(j+2)}C_j^{3/2}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: LobattoKernelEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 0:2)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoKernelEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 0)-(-SQRT(6.0_DFP)))) &
!   < 1.0E-11_DFP, "Constant kernel mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate derivatives of Lobatto kernel modes.
!
! The second index enumerates kernel modes from zero. The returned values are
! derivatives of the displayed kernel formula.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP)(1:SIZE(x), 0:n).
!
! Formula:
! $$K_j(x)=-\frac{\sqrt{8(2j+3)}}{(j+1)(j+2)}C_j^{3/2}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: LobattoKernelGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 0:2)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoKernelGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 0)-(0.0_DFP))) &
!   < 1.0E-11_DFP, "Constant kernel mode")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate derivatives of Lobatto kernel modes.
!
! The second index enumerates kernel modes from zero. The returned values are
! derivatives of the displayed kernel formula.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   ans : REAL(DFP), INTENT(INOUT)(1:, 0:). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(1:, 0:), nrow, ncol.
!
! Formula:
! $$K_j(x)=-\frac{\sqrt{8(2j+3)}}{(j+1)(j+2)}C_j^{3/2}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: LobattoKernelGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 0:2)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoKernelGradientEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 0)-(0.0_DFP))) &
!   < 1.0E-11_DFP, "Constant kernel mode")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoMonomialExpansionAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3, 3)
!
! c = LobattoMonomialExpansionAll(n=2_I4B)
! CALL OK(MAXVAL(ABS(c(:, 3)- &
!   ([-1.5_DFP, 0.0_DFP, 1.5_DFP]/SQRT(6.0_DFP)))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LobattoMonomialExpansionAll(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1, 1:n + 1)
  END FUNCTION LobattoMonomialExpansionAll
END INTERFACE

!----------------------------------------------------------------------------
!                                             LobattoMonomialExpansion
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
! USE LobattoPolynomialUtility, ONLY: LobattoMonomialExpansion
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: c(3)
!
! c = LobattoMonomialExpansion(n=2_I4B)
! CALL OK(MAXVAL(ABS(c- &
!   ([-1.5_DFP, 0.0_DFP, 1.5_DFP]/SQRT(6.0_DFP)))) < 1.0E-11_DFP, &
!   "Coefficients are ordered by increasing power")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LobattoMonomialExpansion(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LobattoMonomialExpansion
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoGradientEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoGradientEvalAll(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3, 3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LobattoGradientEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
! CALL OK(nr == 3 .AND. nc == 3, "Active dimensions")
! CALL OK(MAXVAL(ABS(y(:, 3)-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Third column contains degree two")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LobattoGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), y(3)
! REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LobattoGradientEval(n=2_I4B, x=x)
! CALL OK(MAXVAL(ABS(y-(3.0_DFP*x/SQRT(6.0_DFP)))) < tol, &
!   "Degree two agrees with its analytic formula")
! CALL OK(ABS(LobattoGradientEval(n=2_I4B, x=0.0_DFP) &
!   -y(2)) < tol, "Scalar and vector calls agree")
! END PROGRAM main
!```

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

!> author: Vikas Sharma, Ph.D.
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
! USE LobattoPolynomialUtility, ONLY: LobattoMassMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(3, 3)
!
! a = LobattoMassMatrix(n=2_I4B)
! CALL OK(MAXVAL(ABS(a-TRANSPOSE(a))) < 1.0E-11_DFP, &
!   "Matrix is symmetric")
! CALL OK(ABS(a(1, 1)-(2.0_DFP/3.0_DFP)) < 1.0E-11_DFP .AND. &
!   ABS(a(1, 2)-(1.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Endpoint block agrees with exact integrals")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LobattoMassMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION LobattoMassMatrix
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
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
! USE LobattoPolynomialUtility, ONLY: LobattoStiffnessMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(3, 3)
!
! a = LobattoStiffnessMatrix(n=2_I4B)
! CALL OK(MAXVAL(ABS(a-TRANSPOSE(a))) < 1.0E-11_DFP, &
!   "Matrix is symmetric")
! CALL OK(ABS(a(1, 1)-(0.5_DFP)) < 1.0E-11_DFP .AND. &
!   ABS(a(1, 2)-(-0.5_DFP)) < 1.0E-11_DFP, &
!   "Endpoint block agrees with exact integrals")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LobattoStiffnessMatrix(n) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: ans(n + 1, n + 1)
  END FUNCTION LobattoStiffnessMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Lobatto0
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 0 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{0}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto0
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto0(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=0_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto0(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 1 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{1}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto1
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto1(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=1_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto1(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 2 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{2}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto2
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto2(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=2_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto2(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto2
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto3
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 3 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{3}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto3
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto3(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=3_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto3(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto3
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto4
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 4 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{4}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto4
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto4(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=4_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto4(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto4
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto5
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 5 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{5}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto5
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto5(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=5_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto5(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto5
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto6
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 6 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{6}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto6
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto6(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=6_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto6(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto6
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto7
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 7 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{7}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto7
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto7(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=7_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto7(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto7
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto8
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 8 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{8}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto8
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto8(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=8_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto8(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto8
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto9
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 9 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{9}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto9
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto9(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=9_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto9(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto9
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Lobatto10
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate the fixed degree 10 Lobatto polynomial.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$y=\phi_{10}(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE LobattoPolynomialUtility, ONLY: Lobatto10
! USE LobattoPolynomialUtility, ONLY: LobattoEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = Lobatto10(x=0.25_DFP)
! CALL OK(ABS(y-LobattoEval(n=10_I4B, x=0.25_DFP)) &
!   < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Lobatto10(x) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION Lobatto10
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LobattoLaplacianEvalAll
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEvalAll
  MODULE PURE FUNCTION LobattoLaplacianEvalAll1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans(1:n + 1)
  END FUNCTION LobattoLaplacianEvalAll1
END INTERFACE LobattoLaplacianEvalAll

!----------------------------------------------------------------------------
!
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEvalAll_
  MODULE PURE SUBROUTINE LobattoLaplacianEvalAll1_(n, x, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(1:n + 1)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LobattoLaplacianEvalAll1_
END INTERFACE LobattoLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                    LobattoLaplacianEvalAll
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEvalAll
  MODULE PURE FUNCTION LobattoLaplacianEvalAll2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x), 1:n + 1)
  END FUNCTION LobattoLaplacianEvalAll2
END INTERFACE LobattoLaplacianEvalAll

!----------------------------------------------------------------------------
!                                                    LobattoLaplacianEvalAll_
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEvalAll_
  MODULE PURE SUBROUTINE LobattoLaplacianEvalAll2_(n, x, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    ! ans(1:SIZE(x), 1:n + 1)
  END SUBROUTINE LobattoLaplacianEvalAll2_
END INTERFACE LobattoLaplacianEvalAll_

!----------------------------------------------------------------------------
!                                                       LobattoLaplacianEval
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEval
  MODULE PURE FUNCTION LobattoLaplacianEval1(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP) :: ans
  END FUNCTION LobattoLaplacianEval1
END INTERFACE LobattoLaplacianEval
!!

!----------------------------------------------------------------------------
!                                                       LobattoLaplacianEval
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
! USE LobattoPolynomialUtility, ONLY: LobattoGradientEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), gp(3), gm(3), d2(3)
! REAL(DFP), PARAMETER :: h = 1.0E-5_DFP
!
! ! The declared Laplacian routine has no implementation.
! ! Approximate the second derivative using the working gradient API.
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! gp = LobattoGradientEval(n=2_I4B, x=x+h)
! gm = LobattoGradientEval(n=2_I4B, x=x-h)
! d2 = (gp-gm)/(2.0_DFP*h)
! CALL OK(MAXVAL(ABS(d2-(3.0_DFP/SQRT(6.0_DFP)))) < 1.0E-8_DFP, &
!   "Gradient based second derivative workaround")
! END PROGRAM main
!```

INTERFACE LobattoLaplacianEval
  MODULE PURE FUNCTION LobattoLaplacianEval2(n, x) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(1:SIZE(x))
  END FUNCTION LobattoLaplacianEval2
END INTERFACE LobattoLaplacianEval

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LobattoPolynomialUtility
