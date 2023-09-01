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

SUBMODULE(OrthogonalPolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Clenshaw_1
REAL(DFP), DIMENSION(0:SIZE(c)) :: u
INTEGER(I4B) :: ii, n
REAL(DFP) :: y00, ym10

y00 = INPUT(default=1.0_DFP, option=y0)
ym10 = INPUT(default=0.0_DFP, option=ym1)

!! The size of c, alpha, beta should be same n+1: 0 to n
!! The size of u is n+2, 0 to n+1
n = SIZE(c) - 1
u(n) = c(n)
u(n + 1) = 0.0_DFP
DO ii = n - 1, 0, -1
  u(ii) = (x - alpha(ii)) * u(ii + 1) - beta(ii + 1) * u(ii + 2) + c(ii)
END DO
ans = u(0) * y00 - beta(0) * u(1) * ym10
END PROCEDURE Clenshaw_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Clenshaw_2
REAL(DFP), DIMENSION(1:SIZE(x), 0:SIZE(c)) :: u
INTEGER(I4B) :: ii, n
REAL(DFP) :: y00, ym10
y00 = INPUT(default=1.0_DFP, option=y0)
ym10 = INPUT(default=0.0_DFP, option=ym1)
!! The size of c, alpha, beta should be same n+1: 0 to n
!! The size of u is n+2, 0 to n+1
n = SIZE(c) - 1
u(:, n) = c(n)
u(:, n + 1) = 0.0_DFP
DO ii = n - 1, 0, -1
  u(:, ii) = (x - alpha(ii)) * u(:, ii + 1) &
    & - beta(ii + 1) * u(:, ii + 2) + c(ii)
END DO
ans = u(:, 0) * y00 - beta(0) * u(:, 1) * ym10
END PROCEDURE Clenshaw_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebClenshaw_1
REAL(DFP), DIMENSION(0:SIZE(c) + 2) :: u
INTEGER(I4B) :: ii, n
!! The size of c is n+1: 0 to n
!! The size of u is n+3, 0 to n+2
n = SIZE(c) - 1
u(n) = c(n)
u(n + 1) = 0.0_DFP
u(n + 2) = 0.0_DFP
DO ii = n - 1, 0, -1
  u(ii) = 2.0_DFP * x * u(ii + 1) - u(ii + 2) + c(ii)
END DO
ans = 0.5_DFP * (u(0) - u(2))
END PROCEDURE ChebClenshaw_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE ChebClenshaw_2
REAL(DFP), DIMENSION(1:SIZE(x), 0:SIZE(c) + 2) :: u
INTEGER(I4B) :: ii, n
!! The size of c is n+1: 0 to n
!! The size of u is n+3, 0 to n+2
n = SIZE(c) - 1
u(:, n) = c(n)
u(:, n + 1) = 0.0_DFP
u(:, n + 2) = 0.0_DFP
DO ii = n - 1, 0, -1
  u(:, ii) = 2.0_DFP * x * u(:, ii + 1) - u(:, ii + 2) + c(ii)
END DO
ans = 0.5_DFP * (u(:, 0) - u(:, 2))
END PROCEDURE ChebClenshaw_2

!----------------------------------------------------------------------------
!                                                              JacobiMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiMatrix_1
INTEGER(I4B) :: n
n = SIZE(alphaCoeff)
D(1:n) = alphaCoeff(0:n - 1)
E(1:n - 1) = SQRT(betaCoeff(1:n - 1))
END PROCEDURE JacobiMatrix_1

!----------------------------------------------------------------------------
!                                                            EvalAllOrthopol
!----------------------------------------------------------------------------

MODULE PROCEDURE EvalAllOrthopol
SELECT CASE (orthopol)
CASE (Jacobi)
  ans = JacobiEvalAll(n=n, alpha=alpha, beta=beta, x=x)
CASE (Ultraspherical)
  ans = UltraSphericalEvalAll(n=n, lambda=lambda, x=x)
CASE (Legendre)
  ans = LegendreEvalAll(n=n, x=x)
CASE (Chebyshev)
  ans = Chebyshev1EvalAll(n=n, x=x)
CASE (Lobatto)
  ans = LobattoEvalAll(n=n, x=x)
CASE (UnscaledLobatto)
  ans = UnscaledLobattoEvalAll(n=n, x=x)
END SELECT
END PROCEDURE EvalAllOrthopol

!----------------------------------------------------------------------------
!                                                   GradientEvalAllOrthopol
!----------------------------------------------------------------------------

MODULE PROCEDURE GradientEvalAllOrthopol
SELECT CASE (orthopol)
CASE (Jacobi)
  ans = JacobiGradientEvalAll(n=n, alpha=alpha, beta=beta, x=x)
CASE (Ultraspherical)
  ans = UltraSphericalGradientEvalAll(n=n, lambda=lambda, x=x)
CASE (Legendre)
  ans = LegendreGradientEvalAll(n=n, x=x)
CASE (Chebyshev)
  ans = Chebyshev1GradientEvalAll(n=n, x=x)
CASE (Lobatto)
  ans = LobattoGradientEvalAll(n=n, x=x)
CASE (UnscaledLobatto)
  ans = UnscaledLobattoGradientEvalAll(n=n, x=x)
END SELECT
END PROCEDURE GradientEvalAllOrthopol

END SUBMODULE Methods
