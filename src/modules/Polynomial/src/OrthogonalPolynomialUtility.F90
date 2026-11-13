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
! summary: Clenshaw series evaluation and polynomial family dispatch.
!
! Family dispatch includes monomials and integrated Lobatto modes as well as
! orthogonal polynomials. Reference element Laplacian dispatch currently
! supports Line only.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$u(x)=\sum_j c_j\phi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: EvalAllOrthopol
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = EvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Legendre)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

MODULE OrthogonalPolynomialUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: Clenshaw
PUBLIC :: ChebClenshaw
PUBLIC :: JacobiMatrix
PUBLIC :: EvalAllOrthopol
PUBLIC :: EvalAllOrthopol_
PUBLIC :: GradientEvalAllOrthopol
PUBLIC :: GradientEvalAllOrthopol_

PUBLIC :: OrthogonalEvalAll_
PUBLIC :: OrthogonalEvalAll
PUBLIC :: OrthogonalGradientEvalAll_
PUBLIC :: OrthogonalGradientEvalAll

PUBLIC :: LaplacianEvalAllOrthopol
PUBLIC :: LaplacianEvalAllOrthopol_
PUBLIC :: OrthogonalLaplacianEvalAll_
PUBLIC :: OrthogonalLaplacianEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a series using backward Clenshaw recurrence.
!
! alpha, beta and c contain n+1 entries starting at zero. Defaults are y0=1
! and ym1=0. The two argument Clenshaw overload uses the Chebyshev convention
! described by ChebClenshaw.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   alpha : REAL(DFP), INTENT(IN)(0:). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN)(0:). Jacobi weight exponent at -1.
!   y0 : REAL(DFP), OPTIONAL, INTENT(IN). Initial degree zero recurrence
! value; default 1.
!   ym1 : REAL(DFP), OPTIONAL, INTENT(IN). Initial degree minus one recurrence
! value; default 0.
!   c : REAL(DFP), INTENT(IN)(0:). Recurrence multiplier array, or modal
! series coefficients.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$s(x)=\sum_{j=0}^{n}c_j\pi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: Clenshaw
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), a(0:2), b(0:2), c(0:2), y(3)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! a = 0.0_DFP
! b = [2.0_DFP, 1.0_DFP/3.0_DFP, 4.0_DFP/15.0_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Clenshaw(x=x, alpha=a, beta=b, c=c)
! CALL OK(MAXVAL(ABS(y-(x*x-1.0_DFP/3.0_DFP))) &
!   < 1.0E-11_DFP, "Monic Legendre degree two")
! END PROGRAM main
!```

INTERFACE Clenshaw
  MODULE PURE FUNCTION Clenshaw_1(x, alpha, beta, y0, ym1, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: alpha(0:)
    REAL(DFP), INTENT(IN) :: beta(0:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y0
    !! if y0 is absent then y0 = 1.0
    REAL(DFP), OPTIONAL, INTENT(IN) :: ym1
    !! if ym1 is absent then ym1 = 0.0
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans
  END FUNCTION Clenshaw_1
END INTERFACE Clenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a series using backward Clenshaw recurrence.
!
! alpha, beta and c contain n+1 entries starting at zero. Defaults are y0=1
! and ym1=0. The two argument Clenshaw overload uses the Chebyshev convention
! described by ChebClenshaw.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   alpha : REAL(DFP), INTENT(IN)(0:). Jacobi weight exponent at +1.
!   beta : REAL(DFP), INTENT(IN)(0:). Jacobi weight exponent at -1.
!   y0 : REAL(DFP), OPTIONAL, INTENT(IN). Initial degree zero recurrence
! value; default 1.
!   ym1 : REAL(DFP), OPTIONAL, INTENT(IN). Initial degree minus one recurrence
! value; default 0.
!   c : REAL(DFP), INTENT(IN)(0:). Recurrence multiplier array, or modal
! series coefficients.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$s(x)=\sum_{j=0}^{n}c_j\pi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: Clenshaw
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), a(0:2), b(0:2), c(0:2), y(3)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! a = 0.0_DFP
! b = [2.0_DFP, 1.0_DFP/3.0_DFP, 4.0_DFP/15.0_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Clenshaw(x=x, alpha=a, beta=b, c=c)
! CALL OK(MAXVAL(ABS(y-(x*x-1.0_DFP/3.0_DFP))) &
!   < 1.0E-11_DFP, "Monic Legendre degree two")
! END PROGRAM main
!```

INTERFACE Clenshaw
  MODULE PURE FUNCTION Clenshaw_2(x, alpha, beta, y0, ym1, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: alpha(0:)
    REAL(DFP), INTENT(IN) :: beta(0:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: y0
    !! if y0 is absent then y0 = 1.0
    REAL(DFP), OPTIONAL, INTENT(IN) :: ym1
    !! if ym1 is absent then ym1 = 0.0
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION Clenshaw_2
END INTERFACE Clenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a series using backward Clenshaw recurrence.
!
! alpha, beta and c contain n+1 entries starting at zero. Defaults are y0=1
! and ym1=0. The two argument Clenshaw overload uses the Chebyshev convention
! described by ChebClenshaw.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN). Evaluation coordinate(s).
!   c : REAL(DFP), INTENT(IN)(0:). Recurrence multiplier array, or modal
! series coefficients.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$s(x)=\sum_{j=0}^{n}c_j\pi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: Clenshaw
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), a(0:2), b(0:2), c(0:2), y(3)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! a = 0.0_DFP
! b = [2.0_DFP, 1.0_DFP/3.0_DFP, 4.0_DFP/15.0_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Clenshaw(x=x, alpha=a, beta=b, c=c)
! CALL OK(MAXVAL(ABS(y-(x*x-1.0_DFP/3.0_DFP))) &
!   < 1.0E-11_DFP, "Monic Legendre degree two")
! END PROGRAM main
!```

INTERFACE Clenshaw
  MODULE PURE FUNCTION ChebClenshaw_1(x, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans
  END FUNCTION ChebClenshaw_1
END INTERFACE Clenshaw

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a series using backward Clenshaw recurrence.
!
! alpha, beta and c contain n+1 entries starting at zero. Defaults are y0=1
! and ym1=0. The two argument Clenshaw overload uses the Chebyshev convention
! described by ChebClenshaw.
!
! Arguments:
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   c : REAL(DFP), INTENT(IN)(0:). Recurrence multiplier array, or modal
! series coefficients.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x)).
!
! Formula:
! $$s(x)=\sum_{j=0}^{n}c_j\pi_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: Clenshaw
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), a(0:2), b(0:2), c(0:2), y(3)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! a = 0.0_DFP
! b = [2.0_DFP, 1.0_DFP/3.0_DFP, 4.0_DFP/15.0_DFP]
! c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! y = Clenshaw(x=x, alpha=a, beta=b, c=c)
! CALL OK(MAXVAL(ABS(y-(x*x-1.0_DFP/3.0_DFP))) &
!   < 1.0E-11_DFP, "Monic Legendre degree two")
! END PROGRAM main
!```

INTERFACE Clenshaw
  MODULE PURE FUNCTION ChebClenshaw_2(x, c) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP), INTENT(IN) :: c(0:)
    REAL(DFP) :: ans(SIZE(x))
  END FUNCTION ChebClenshaw_2
END INTERFACE Clenshaw

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate a Chebyshev series with a halved constant term.
!
! ChebClenshaw exposes the vector overload. Clenshaw(x,c) also accepts scalar
! x. This constant coefficient convention differs from Chebyshev1EvalSum.
!
! Arguments:
!   See the specific overload declarations for arguments.
!
! Returns:
!   The selected overload returns or writes the documented result.
!
! Formula:
! $$s(x)=c_0/2+\sum_{j=1}^{n}c_j T_j(x)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: ChebClenshaw
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3), c(0:2), y(3)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! c = [2.0_DFP, 0.0_DFP, 1.0_DFP]
! y = ChebClenshaw(x=x, c=c)
! CALL OK(MAXVAL(ABS(y-2.0_DFP*x*x)) < 1.0E-11_DFP, &
!   "Chebyshev series uses half the constant coefficient")
! END PROGRAM main
!```

INTERFACE ChebClenshaw
  MODULE PROCEDURE ChebClenshaw_2
END INTERFACE ChebClenshaw

!----------------------------------------------------------------------------
!                                                             JacobiMatrix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build a Jacobi matrix from monic recurrence coefficients.
!
! For n recurrence coefficients, allocate D(n) and E(n-1). betaCoeff(0) is not
! placed on the off diagonal.
!
! Arguments:
!   alphaCoeff : REAL(DFP), INTENT(IN)(0:). Monic recurrence diagonal
! coefficients, indexed from zero.
!   betaCoeff : REAL(DFP), INTENT(IN)(0:). Monic recurrence beta values; index
! zero is the weight integral.
!   D : REAL(DFP), INTENT(OUT)(:). Jacobi matrix diagonal, or nodal derivative
! matrix in the block splitter.
!   E : REAL(DFP), INTENT(OUT)(:). Jacobi matrix off diagonal, or even block
! in the block splitter.
!
! Returns:
!   Writes D(:), E(:).
!
! Formula:
! $$D_i=\alpha_{i-1},\quad E_i=\sqrt{\beta_i}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE OrthogonalPolynomialUtility, ONLY: JacobiMatrix
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: a(0:1), b(0:1), d(2), e(1)
!
! a = 0.0_DFP
! b = [2.0_DFP, 1.0_DFP/3.0_DFP]
! CALL JacobiMatrix(alphaCoeff=a, betaCoeff=b, D=d, E=e)
! CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, "Diagonal coefficients")
! CALL OK(ABS(e(1)-SQRT(1.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
!   "Off diagonal coefficient")
! END PROGRAM main
!```

INTERFACE JacobiMatrix
  MODULE PURE SUBROUTINE JacobiMatrix_1(alphaCoeff, betaCoeff, D, E)
    REAL(DFP), INTENT(IN) :: alphaCoeff(0:)
  !! size n, from 0 to n-1
    REAL(DFP), INTENT(IN) :: betaCoeff(0:)
  !! size n, from 0 to n-1
    REAL(DFP), INTENT(OUT) :: D(:)
  !! entry from 1 to n are filled
    REAL(DFP), INTENT(OUT) :: E(:)
  !! entry from 1 to n-1 are filled
  END SUBROUTINE JacobiMatrix_1
END INTERFACE JacobiMatrix

!----------------------------------------------------------------------------
!                                                           EvalAllOrthopol
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis values by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), n + 1).
!
! Formula:
! $$Y_{ia}=\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: EvalAllOrthopol
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = EvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Legendre)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION EvalAllOrthopol(n, x, orthopol, alpha, beta, &
                                       lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! orthogonal polynomial family
    !! Legendre, Jacobi, Lobatto, Chebyshev, Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
  END FUNCTION EvalAllOrthopol
END INTERFACE

!----------------------------------------------------------------------------
!                                                           EvalAllOrthopol_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis values by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{ia}=\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: EvalAllOrthopol_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL EvalAllOrthopol_(n=2_I4B, x=x, orthopol=poly%Legendre, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE EvalAllOrthopol_(n, x, orthopol, alpha, beta, &
                                          lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! orthogonal polynomial family
    !! Legendre Jacobi ! Lobatto ! Chebyshev ! Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE EvalAllOrthopol_
END INTERFACE

!----------------------------------------------------------------------------
!                                                           EvalAllOrthopol
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis gradients by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), n + 1).
!
! Formula:
! $$G_{iak}=\partial_k\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = GradientEvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Legendre)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION GradientEvalAllOrthopol(n, x, orthopol, alpha, &
                                               beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! Orthogonal polynomial family
    !! Legendre Jacobi Lobatto Chebyshev Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
  END FUNCTION GradientEvalAllOrthopol
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis gradients by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$G_{iak}=\partial_k\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: GradientEvalAllOrthopol_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL GradientEvalAllOrthopol_(n=2_I4B, x=x, orthopol=poly%Legendre, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GradientEvalAllOrthopol_(n, x, orthopol, ans, &
                                              nrow, ncol, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! Orthogonal polynomial family
    !! Legendre Jacobi Lobatto Chebyshev Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
  END SUBROUTINE GradientEvalAllOrthopol_
END INTERFACE

!----------------------------------------------------------------------------
!                                                          OrthogonalEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis values on a reference element.
!
! Element dispatch supports Line, Triangle, Quadrangle, Tetrahedron and
! Hexahedron. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$Y_{ia}=\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! y = OrthogonalEvalAll(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Legendre)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION OrthogonalEvalAll(order, elemType, xij, domainName, &
                                   basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Value of n+1 Orthogonal polynomials at point x
  END FUNCTION OrthogonalEvalAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                 OrthogonalGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis values on a reference element.
!
! Element dispatch supports Line, Triangle, Quadrangle, Tetrahedron and
! Hexahedron. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$Y_{ia}=\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! CALL OrthogonalEvalAll_(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Legendre, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (0.5_DFP*(3.0_DFP*x*x-1.0_DFP)))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE OrthogonalEvalAll_(order, elemType, xij, domainName, &
                              basisType, ans, nrow, ncol, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1 Orthogonal polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols in ans
  END SUBROUTINE OrthogonalEvalAll_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   OrthogonalGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis gradients on a reference element.
!
! Element dispatch supports Line, Triangle, Quadrangle, Tetrahedron and
! Hexahedron. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :, :).
!
! Formula:
! $$G_{iak}=\partial_k\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalGradientEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! y = OrthogonalGradientEvalAll(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Legendre)
! CALL OK(MAXVAL(ABS(y(:, 3, 1)- &
!   (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
 MODULE FUNCTION OrthogonalGradientEvalAll(order, elemType, xij, domainName, &
                                   basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), ALLOCATABLE :: ans(:, :, :)
    !! Value of n+1 Orthogonal polynomials at point x
  END FUNCTION OrthogonalGradientEvalAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                 OrthogonalGradientEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis gradients on a reference element.
!
! Element dispatch supports Line, Triangle, Quadrangle, Tetrahedron and
! Hexahedron. Point rows and basis columns are preserved in outputs.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!   ans : REAL(DFP), INTENT(INOUT)(:, :, :). Result storage; allocate enough
! space for all active dimensions.
!   dim1 : INTEGER(I4B), INTENT(OUT). Active first dimension (evaluation
! points).
!   dim2 : INTEGER(I4B), INTENT(OUT). Active second dimension (basis
! functions).
!   dim3 : INTEGER(I4B), INTENT(OUT). Active third dimension (coordinate
! directions).
!
! Returns:
!   Writes ans(:, :, :), dim1, dim2, dim3.
!
! Formula:
! $$G_{iak}=\partial_k\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalGradientEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP) :: y(3, 3, 1)
! INTEGER(I4B) :: nr, nc, nd
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! CALL OrthogonalGradientEvalAll_(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Legendre, &
!   ans=y, dim1=nr, dim2=nc, dim3=nd)
! CALL OK(MAXVAL(ABS(y(:, 3, 1)- &
!   (3.0_DFP*x))) < 1.0E-11_DFP, "Legendre quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE OrthogonalGradientEvalAll_(order, elemType, xij, &
            domainName, basisType, ans, dim1, dim2, dim3, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Value of n+1 Orthogonal polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! number of rows and cols in ans
  END SUBROUTINE OrthogonalGradientEvalAll_
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LaplacianEvalAllOrthopol
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs. Only the
! Monomial family has an implemented Laplacian here; all six named polynomial
! family Laplacians currently forward to empty bodies. Use BIUNIT for the line
! example; UNIT second derivative scaling in the element backend requires
! review.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP)(SIZE(x), n + 1).
!
! Formula:
! $$L_{ia}=\sum_k\partial_k^2\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: LaplacianEvalAllOrthopol
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! y = LaplacianEvalAllOrthopol(n=2_I4B, x=x, orthopol=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION LaplacianEvalAllOrthopol(n, x, orthopol, alpha, &
                                                beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! Orthogonal polynomial family
    !! Legendre Jacobi Lobatto Chebyshev Ultraspherical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x), n + 1)
    !! The number of rows in ans is equal to the num of points evaluation.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
  END FUNCTION LaplacianEvalAllOrthopol
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians by polynomial family.
!
! Select a family using poly from GlobalData. Jacobi requires alpha and beta;
! Ultraspherical requires lambda. Monomial and Lobatto families are also
! available. Point rows and basis columns are preserved in outputs. Only the
! Monomial family has an implemented Laplacian here; all six named polynomial
! family Laplacians currently forward to empty bodies. Use BIUNIT for the line
! example; UNIT second derivative scaling in the element backend requires
! review.
!
! Arguments:
!   n : INTEGER(I4B), INTENT(IN). Maximum polynomial degree; use n>=0.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation coordinate(s).
!   orthopol : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$L_{ia}=\sum_k\partial_k^2\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE OrthogonalPolynomialUtility, ONLY: LaplacianEvalAllOrthopol_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! CALL LaplacianEvalAllOrthopol_(n=2_I4B, x=x, orthopol=poly%Monomial, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE LaplacianEvalAllOrthopol_( &
    n, x, orthopol, ans, nrow, ncol, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: n
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: x(:)
    !! points of evaluation
    INTEGER(I4B), INTENT(IN) :: orthopol
    !! Orthogonal polynomial family
    !! Legendre Jacobi Lobatto Chebyshev Ultraspherical
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(x), n + 1)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! The number of rows in ans is equal to the number of points.
    !! The number of columns are equal to the orthogonal
    !! polynomials from order  = 0 to n
    !! Therefore, jth column is denotes the value of jth polynomial
    !! at all the points.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
  END SUBROUTINE LaplacianEvalAllOrthopol_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 OrthogonalLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians on a reference element.
!
! Laplacian element dispatch currently supports Line only. Point rows and
! basis columns are preserved in outputs. Only the Monomial family has an
! implemented Laplacian here; all six named polynomial family Laplacians
! currently forward to empty bodies. Use BIUNIT for the line example; UNIT
! second derivative scaling in the element backend requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!
! Returns:
!   ans : REAL(DFP), ALLOCATABLE(:, :).
!
! Formula:
! $$L_{ia}=\sum_k\partial_k^2\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalLaplacianEvalAll
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP), ALLOCATABLE :: y(:, :)
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! y = OrthogonalLaplacianEvalAll(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Monomial)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION OrthogonalLaplacianEvalAll( &
    order, elemType, xij, domainName, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Value of n+1 Orthogonal polynomials at point x
  END FUNCTION OrthogonalLaplacianEvalAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                 OrthogonalLaplacianEvalAll
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians on a reference element.
!
! Laplacian element dispatch currently supports Line only. Point rows and
! basis columns are preserved in outputs. Only the Monomial family has an
! implemented Laplacian here; all six named polynomial family Laplacians
! currently forward to empty bodies. Use BIUNIT for the line example; UNIT
! second derivative scaling in the element backend requires review.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Interpolation degree; use a nonnegative
! value unless noted above.
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData.
!   xij : REAL(DFP), INTENT(IN)(:, :). Coordinate array with points in
! columns; its role is specified by the interface.
!   domainName : CHARACTER(*), INTENT(IN). Reference domain name supported by
! the selected element.
!   basisType : INTEGER(I4B), INTENT(IN). Polynomial family selector from
! GlobalData (poly).
!   alpha : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at +1.
!   beta : REAL(DFP), OPTIONAL, INTENT(IN). Jacobi weight exponent at -1.
!   lambda : REAL(DFP), OPTIONAL, INTENT(IN). Gegenbauer family parameter.
!   ans : REAL(DFP), INTENT(INOUT)(:, :). Result storage; allocate enough
! space for all active dimensions.
!   nrow : INTEGER(I4B), INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), INTENT(OUT). Number of active output columns.
!
! Returns:
!   Writes ans(:, :), nrow, ncol.
!
! Formula:
! $$L_{ia}=\sum_k\partial_k^2\phi_a(x_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: poly
! USE GlobalData, ONLY: Line
! USE OrthogonalPolynomialUtility, ONLY: OrthogonalLaplacianEvalAll_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3)
! REAL(DFP) :: points(1, 3)
! REAL(DFP) :: y(3, 3)
! INTEGER(I4B) :: nr, nc
!
! x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
! points(1, :) = x
! CALL OrthogonalLaplacianEvalAll_(order=2_I4B, elemType=Line, xij=points, &
!   domainName="BIUNIT", basisType=poly%Monomial, &
!   ans=y, nrow=nr, ncol=nc)
! CALL OK(MAXVAL(ABS(y(:, 3)- &
!   (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE OrthogonalLaplacianEvalAll_( &
    order, elemType, xij, domainName, basisType, ans, nrow, ncol, &
    alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT ! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis type
    !! used for line, quadrangle, and hexahedron element
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! alpha needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! beta is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! lambda is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1 Orthogonal polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols in ans
  END SUBROUTINE OrthogonalLaplacianEvalAll_
END INTERFACE

END MODULE OrthogonalPolynomialUtility
