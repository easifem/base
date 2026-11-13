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
USE GlobalData, ONLY: stderr
USE ReferenceElement_Method, ONLY: XiDimension
USE InputUtility, ONLY: Input
USE ErrorHandling, ONLY: ErrorMsg
USE BaseType, ONLY: poly => TypePolynomialOpt
USE BaseType, ONLY: elem => TypeElemNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE JacobiPolynomialUtility, ONLY: JacobiEvalAll_
USE JacobiPolynomialUtility, ONLY: JacobiGradientEvalAll_
USE JacobiPolynomialUtility, ONLY: JacobiLaplacianEvalAll_
USE UltrasphericalPolynomialUtility, ONLY: UltraSphericalEvalAll_
USE UltrasphericalPolynomialUtility, ONLY: UltraSphericalGradientEvalAll_
USE UltrasphericalPolynomialUtility, ONLY: UltraSphericalLaplacianEvalAll_
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1EvalAll_
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1GradientEvalAll_
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1LaplacianEvalAll_
USE LegendrePolynomialUtility, ONLY: LegendreEvalAll_
USE LegendrePolynomialUtility, ONLY: LegendreGradientEvalAll_
USE LegendrePolynomialUtility, ONLY: LegendreLaplacianEvalAll_
USE LobattoPolynomialUtility, ONLY: LobattoEvalAll_
USE LobattoPolynomialUtility, ONLY: LobattoGradientEvalAll_
USE LobattoPolynomialUtility, ONLY: LobattoLaplacianEvalAll_
USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoEvalAll_
USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoGradientEvalAll_
USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoLaplacianEvalAll_
USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_
USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line_
USE LineInterpolationUtility, ONLY: OrthogonalBasisLaplacian_Line_
USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle_
USE TriangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Triangle_
USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle_
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron_
USE TetrahedronInterpolationUtility, ONLY: &
  OrthogonalBasisGradient_Tetrahedron_
USE HexahedronInterpolationUtility, ONLY: OrthogonalBasis_Hexahedron_
USE HexahedronInterpolationUtility, ONLY: OrthogonalBasisGradient_Hexahedron_
! USE PrismInterpolationUtility, ONLY: OrthogonalBasis_Prism_
! USE PyramidInterpolationUtility, ONLY: OrthogonalBasis_Pyramid_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "OrthogonalPolynomialUtility@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Clenshaw_1
REAL(DFP), DIMENSION(0:SIZE(c)) :: u
INTEGER(I4B) :: ii, n
REAL(DFP) :: y00, ym10

y00 = Input(default=1.0_DFP, option=y0)
ym10 = Input(default=0.0_DFP, option=ym1)

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
y00 = Input(default=1.0_DFP, option=y0)
ym10 = Input(default=0.0_DFP, option=ym1)
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
INTEGER(I4B) :: nrow, ncol
CALL EvalAllOrthopol_(n=n, x=x, orthopol=orthopol, alpha=alpha, beta=beta, &
                      lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE EvalAllOrthopol

!----------------------------------------------------------------------------
!                                                            EvalAllOrthopol_
!----------------------------------------------------------------------------

MODULE PROCEDURE EvalAllOrthopol_
INTEGER(I4B) :: ii

SELECT CASE (orthopol)
CASE (poly%Jacobi)
  CALL JacobiEvalAll_(n=n, alpha=alpha, beta=beta, x=x, ans=ans, nrow=nrow, &
                      ncol=ncol)
CASE (poly%Ultraspherical)
  CALL UltraSphericalEvalAll_(n=n, lambda=lambda, x=x, ans=ans, nrow=nrow, &
                              ncol=ncol)
CASE (poly%Legendre)
  CALL LegendreEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Chebyshev)
  CALL Chebyshev1EvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Lobatto)
  CALL LobattoEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%UnscaledLobatto)
  CALL UnscaledLobattoEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Monomial)

  nrow = SIZE(x)
  !! Number of points of evaluation
  ncol = n + 1
  !! Number of basis functions

  ans(1:nrow, 1) = 1.0_DFP
  DO ii = 1, n
    ans(1:nrow, ii + 1) = ans(1:nrow, ii) * x(1:nrow)
  END DO

CASE DEFAULT
END SELECT
END PROCEDURE EvalAllOrthopol_

!----------------------------------------------------------------------------
!                                                   GradientEvalAllOrthopol
!----------------------------------------------------------------------------

MODULE PROCEDURE GradientEvalAllOrthopol
INTEGER(I4B) :: nrow, ncol
CALL GradientEvalAllOrthopol_(n=n, x=x, orthopol=orthopol, ans=ans, &
                              nrow=nrow, ncol=ncol, alpha=alpha, &
                              beta=beta, lambda=lambda)
END PROCEDURE GradientEvalAllOrthopol

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GradientEvalAllOrthopol_
INTEGER(I4B) :: indx, ii, jj
REAL(DFP) :: areal

SELECT CASE (orthopol)
CASE (poly%Jacobi)
  ! ans(1:nrow, 1:ncol) = JacobiGradientEvalAll(n=n, &
  ! alpha=alpha, beta=beta, x=x)
  CALL JacobiGradientEvalAll_(n=n, alpha=alpha, beta=beta, x=x, ans=ans, &
                              nrow=nrow, ncol=ncol)

CASE (poly%Ultraspherical)
  ! ans(1:nrow, 1:ncol) = UltraSphericalGradientEvalAll(n=n, &
  ! lambda=lambda, x=x)
  CALL UltraSphericalGradientEvalAll_(n=n, lambda=lambda, x=x, ans=ans, &
                                      nrow=nrow, ncol=ncol)

CASE (poly%Legendre)
  ! ans(1:nrow, 1:ncol) = LegendreGradientEvalAll(n=n, x=x)
  CALL LegendreGradientEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Chebyshev)
  ! ans(1:nrow, 1:ncol) = Chebyshev1GradientEvalAll(n=n, x=x)
  CALL Chebyshev1GradientEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Lobatto)
  ! ans(1:nrow, 1:ncol) = LobattoGradientEvalAll(n=n, x=x)
  CALL LobattoGradientEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%UnscaledLobatto)
  ! ans(1:nrow, 1:ncol) = UnscaledLobattoGradientEvalAll(n=n, x=x)
  CALL UnscaledLobattoGradientEvalAll_(n=n, x=x, ans=ans, &
                                       nrow=nrow, ncol=ncol)

CASE (poly%Monomial)
  nrow = SIZE(x)
  !! Number of points of evaluation
  ncol = n + 1
  !! Number of basis functions

  DO jj = 0, n
    indx = MAX(jj - 1_I4B, 0_I4B)
    areal = REAL(jj, kind=DFP)
    DO ii = 1, nrow
      ans(ii, jj + 1) = areal * (x(ii)**(indx))
    END DO
  END DO

CASE DEFAULT
END SELECT
END PROCEDURE GradientEvalAllOrthopol_

!----------------------------------------------------------------------------
!                                                         OrthogonalEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalEvalAll
INTEGER(I4B) :: nrow, ncol
nrow = SIZE(xij, 2)
ncol = LagrangeDOF(order=order, elemType=elemType)
ALLOCATE (ans(nrow, ncol))
CALL OrthogonalEvalAll_(order=order, elemType=elemType, xij=xij, &
             domainName=domainName, basisType=basisType, ans=ans, nrow=nrow, &
                        ncol=ncol, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE OrthogonalEvalAll

!----------------------------------------------------------------------------
!                                                         OrthogonalEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalEvalAll_
SELECT CASE (elemType)

CASE (elem%Line)

  CALL OrthogonalBasis_Line_(order=order, xij=xij, &
            refLine=domainName, basisType=basisType, alpha=alpha, beta=beta, &
                             lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)

CASE (elem%Triangle)

  CALL OrthogonalBasis_Triangle_(order=order, xij=xij, &
                        reftriangle=domainName, ans=ans, nrow=nrow, ncol=ncol)

CASE (elem%Quadrangle)

  CALL OrthogonalBasis_Quadrangle_(p=order, q=order, xij=xij, &
                        ans=ans, nrow=nrow, ncol=ncol, basisType1=basisType, &
             basisType2=basisType, alpha1=alpha, beta1=beta, lambda1=lambda, &
                                   alpha2=alpha, beta2=beta, lambda2=lambda)

CASE (elem%Tetrahedron)

  CALL OrthogonalBasis_Tetrahedron_(order=order, xij=xij, &
                     refTetrahedron=domainName, ans=ans, nrow=nrow, ncol=ncol)

CASE (elem%Hexahedron)

  CALL OrthogonalBasis_Hexahedron_(p=order, q=order, r=order, xij=xij, &
                                   ans=ans, nrow=nrow, ncol=ncol, &
             basisType1=basisType, alpha1=alpha, beta1=beta, lambda1=lambda, &
             basisType2=basisType, alpha2=alpha, beta2=beta, lambda2=lambda, &
               basisType3=basisType, alpha3=alpha, beta3=beta, lambda3=lambda)

CASE DEFAULT

  CALL ErrorMsg(msg="No case found for topology", &
                routine='OrthogonalEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN

END SELECT

END PROCEDURE OrthogonalEvalAll_

!----------------------------------------------------------------------------
!                                                 OrthogonalGradientEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalGradientEvalAll
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(xij, 2)
dim2 = LagrangeDOF(order=order, elemType=elemType)
dim3 = XiDimension(elemType)
ALLOCATE (ans(dim1, dim2, dim3))

CALL OrthogonalGradientEvalAll_(order, elemType, xij, domainName, basisType, &
                                ans, dim1, dim2, dim3, alpha, beta, lambda)

END PROCEDURE OrthogonalGradientEvalAll

!----------------------------------------------------------------------------
!                                                 OrthogonalGradientEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalGradientEvalAll_

SELECT CASE (elemType)

CASE (elem%Line)

  CALL OrthogonalBasisGradient_Line_(order=order, xij=xij, &
            refLine=domainName, basisType=basisType, alpha=alpha, beta=beta, &
                      lambda=lambda, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (elem%Triangle)

  CALL OrthogonalBasisGradient_Triangle_(order=order, xij=xij, &
       reftriangle=domainName, ans=ans, tsize1=dim1, tsize2=dim2, tsize3=dim3)

CASE (elem%Quadrangle)

  CALL OrthogonalBasisGradient_Quadrangle_(p=order, q=order, xij=xij, &
             ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, basisType1=basisType, &
             basisType2=basisType, alpha1=alpha, beta1=beta, lambda1=lambda, &
                                     alpha2=alpha, beta2=beta, lambda2=lambda)

CASE (elem%Tetrahedron)

  CALL OrthogonalBasisGradient_Tetrahedron_(order=order, xij=xij, &
          refTetrahedron=domainName, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (elem%Hexahedron)

  CALL OrthogonalBasisGradient_Hexahedron_(p=order, q=order, r=order, &
                          xij=xij, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
             basisType1=basisType, alpha1=alpha, beta1=beta, lambda1=lambda, &
             basisType2=basisType, alpha2=alpha, beta2=beta, lambda2=lambda, &
               basisType3=basisType, alpha3=alpha, beta3=beta, lambda3=lambda)

CASE DEFAULT

  CALL ErrorMsg(msg="No case found for topology", &
                routine='OrthogonalGradientEvalAll_()', &
                file=__FILE__, line=__LINE__, unitno=stderr)

  RETURN

END SELECT
END PROCEDURE OrthogonalGradientEvalAll_

!----------------------------------------------------------------------------
!                                                   LaplacianEvalAllOrthopol
!----------------------------------------------------------------------------

MODULE PROCEDURE LaplacianEvalAllOrthopol
INTEGER(I4B) :: nrow, ncol
CALL LaplacianEvalAllOrthopol_(n=n, x=x, orthopol=orthopol, ans=ans, &
                               nrow=nrow, ncol=ncol, alpha=alpha, beta=beta, &
                               lambda=lambda)
END PROCEDURE LaplacianEvalAllOrthopol

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE LaplacianEvalAllOrthopol_
INTEGER(I4B) :: ii, jj

SELECT CASE (orthopol)
CASE (poly%Jacobi)
  CALL JacobiLaplacianEvalAll_(n=n, alpha=alpha, beta=beta, x=x, ans=ans, &
                               nrow=nrow, ncol=ncol)

CASE (poly%Ultraspherical)
  CALL UltraSphericalLaplacianEvalAll_(n=n, lambda=lambda, x=x, ans=ans, &
                                       nrow=nrow, ncol=ncol)

CASE (poly%Legendre)
  CALL LegendreLaplacianEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Chebyshev)
  CALL Chebyshev1LaplacianEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%Lobatto)
  CALL LobattoLaplacianEvalAll_(n=n, x=x, ans=ans, nrow=nrow, ncol=ncol)

CASE (poly%UnscaledLobatto)
  CALL UnscaledLobattoLaplacianEvalAll_(n=n, x=x, ans=ans, &
                                        nrow=nrow, ncol=ncol)

CASE (poly%Monomial)
  nrow = SIZE(x)
  !! Number of points of evaluation
  ncol = n + 1
  !! Number of basis functions

  ans(1:nrow, 1:ncol) = math%zero

  DO jj = 2, n
    DO ii = 1, nrow
      ans(ii, jj + 1) = jj * (jj - 1) * (x(ii)**(jj - 2))
    END DO
  END DO

CASE DEFAULT

END SELECT
END PROCEDURE LaplacianEvalAllOrthopol_

!----------------------------------------------------------------------------
!                                                 OrthogonalLaplacianEvalAll
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalLaplacianEvalAll
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(xij, 2)
ncol = LagrangeDOF(order=order, elemType=elemType)
ALLOCATE (ans(nrow, ncol))
CALL OrthogonalLaplacianEvalAll_(order=order, elemType=elemType, xij=xij, &
                                 domainName=domainName, basisType=basisType, &
                                 ans=ans, nrow=nrow, ncol=ncol, alpha=alpha, &
                                 beta=beta, lambda=lambda)
END PROCEDURE OrthogonalLaplacianEvalAll

!----------------------------------------------------------------------------
!                                                 OrthogonalLaplacianEvalAll_
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalLaplacianEvalAll_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "OrthogonalLaplacianEvalAll_()"
#endif

SELECT CASE (elemType)
CASE (elem%Line)

  CALL OrthogonalBasisLaplacian_Line_( &
    order=order, xij=xij, refLine=domainName, basisType=basisType, &
    alpha=alpha, beta=beta, lambda=lambda, ans=ans, &
    nrow=nrow, ncol=ncol)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, modName, __LINE__, &
                    "no case found for elemType")
#endif
END SELECT
END PROCEDURE OrthogonalLaplacianEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
