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

SUBMODULE(LineInterpolationUtility) QuadratureMethods
USE BaseType, ONLY: ipopt => TypeInterpolationOpt, &
                    qpopt => TypeQuadratureOpt
USE MappingUtility, ONLY: FromBiunitLine2Segment_
USE LegendrePolynomialUtility, ONLY: LegendreQuadrature
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature
USE JacobiPolynomialUtility, ONLY: JacobiQuadrature
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@QuadratureMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                     QuadratureNumber_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadratureNumber_Line
SELECT CASE (quadType)
CASE (qpopt%GaussLegendre, qpopt%GaussChebyshev, &
      qpopt%GaussJacobi, qpopt%GaussUltraspherical)
  ans = 1_I4B + INT(order / 2, kind=I4B)
CASE (qpopt%GaussLegendreRadauRight, qpopt%GaussLegendreRadauLeft, &
      qpopt%GaussChebyshevRadauLeft, qpopt%GaussChebyshevRadauRight, &
      qpopt%GaussJacobiRadauLeft, qpopt%GaussJacobiRadauRight, &
      qpopt%GaussUltraSphericalRadauLeft, qpopt%GaussUltraSphericalRadauRight)
  ans = 2_I4B + INT((order - 1) / 2, kind=I4B)
CASE DEFAULT
  ans = 2_I4B + INT(order / 2, kind=I4B)
END SELECT
END PROCEDURE QuadratureNumber_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line1
INTEGER(I4B) :: nips(1), nrow, ncol
LOGICAL(LGT) :: isok

nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)

isok = PRESENT(xij)
nrow = 1
IF (isok) nrow = SIZE(xij, 1)

nrow = nrow + 1
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Line1_( &
  nips=nips, quadType=quadType, layout=layout, xij=xij, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE QuadraturePoint_Line1

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line2
INTEGER(I4B) :: nips(1), nrow, ncol
REAL(DFP) :: x12(1, 2)

nips(1) = QuadratureNumber_Line(order=order, quadType=quadType)
nrow = 2
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

x12(1, 1:2) = xij(1:2)

CALL QuadraturePoint_Line1_( &
  nips=nips, quadType=quadType, layout=layout, xij=x12, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE QuadraturePoint_Line2

!----------------------------------------------------------------------------
!                                                       QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line3
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = 1
isok = PRESENT(xij)
IF (isok) nrow = SIZE(xij, 1)

nrow = nrow + 1
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

CALL QuadraturePoint_Line1_( &
  nips=nips, quadType=quadType, layout=layout, xij=xij, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE QuadraturePoint_Line3

!----------------------------------------------------------------------------
!                                                        QuadraturePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line4
REAL(DFP) :: x12(1, 2)
INTEGER(I4B) :: nrow, ncol

nrow = 2
ncol = nips(1)

ALLOCATE (ans(nrow, ncol))

x12(1, 1:2) = xij(1:2)

CALL QuadraturePoint_Line1_( &
  nips=nips, quadType=quadType, layout=layout, xij=x12, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE QuadraturePoint_Line4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE QuadraturePoint_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "QuadraturePoint_Line1_()"
#endif

INTEGER(I4B) :: np, nsd, ii, jj
REAL(DFP) :: areal
LOGICAL(LGT) :: changeLayout, isok

nrow = 0
ncol = 0

#ifdef DEBUG_VER
SELECT CASE (quadType)
CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto, &
      ipopt%GaussJacobiRadauLeft, ipopt%GaussJacobiRadauRight)

  isok = PRESENT(alpha) .AND. PRESENT(beta)
  CALL AssertError1(isok, myName, modName, __LINE__, &
            "alpha and beta should be present for quadType=ipopt%GaussJacobi")

CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto, &
      ipopt%GaussUltraSphericalRadauLeft, ipopt%GaussUltraSphericalRadauRight)

  isok = PRESENT(lambda)
  CALL AssertError1(isok, myName, modName, __LINE__, &
            "lambda should be present for quadType=ipopt%GaussUltraspherical")
END SELECT
#endif

nsd = 1
isok = PRESENT(xij)
IF (isok) nsd = SIZE(xij, 1)

np = nips(1)
nrow = nsd + 1
ncol = nips(1)

isok = layout(1:1) .EQ. "V"
changeLayout = .FALSE.
IF (isok) changeLayout = .TRUE.

SELECT CASE (quadType)

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%Gauss)

CASE (ipopt%GaussLegendreRadauLeft)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussRadauLeft)

CASE (ipopt%GaussLegendreRadauRight)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussRadauRight)

CASE (ipopt%GaussLegendreLobatto)
  CALL LegendreQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                          quadType=ipopt%GaussLobatto)

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%Gauss)

CASE (ipopt%GaussChebyshevRadauLeft)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussRadauLeft)

CASE (ipopt%GaussChebyshevRadauRight)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussRadauRight)

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                            quadType=ipopt%GaussLobatto)

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%Gauss, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiRadauLeft)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%GaussRadauLeft, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiRadauRight)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                       quadType=ipopt%GaussRadauRight, alpha=alpha, beta=beta)

CASE (ipopt%GaussJacobiLobatto)
  CALL JacobiQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                        quadType=ipopt%GaussLobatto, alpha=alpha, beta=beta)

CASE (ipopt%GaussUltraspherical)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%Gauss, lambda=lambda)

CASE (ipopt%GaussUltrasphericalRadauLeft)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussRadauLeft, lambda=lambda)

CASE (ipopt%GaussUltrasphericalRadauRight)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussRadauRight, lambda=lambda)

CASE (ipopt%GaussUltrasphericalLobatto)
CALL UltrasphericalQuadrature(n=np, pt=ans(1, 1:ncol), wt=ans(nrow, 1:ncol), &
                                quadType=ipopt%GaussLobatto, lambda=lambda)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, modName, __LINE__, &
                    "Unknown iptype")
#endif
END SELECT

IF (changeLayout) THEN
  CALL ToVEFC_Line(ans(1, 1:ncol))
  CALL ToVEFC_Line(ans(nrow, 1:ncol))
END IF

IF (PRESENT(xij)) THEN
  CALL FromBiunitLine2Segment_(xin=ans(1, 1:ncol), x1=xij(:, 1), &
                               x2=xij(:, 2), ans=ans, nrow=ii, ncol=jj)

  areal = NORM2(xij(:, 2) - xij(:, 1)) / 2.0_DFP

  DO CONCURRENT(ii=1:ncol)
    ans(nrow, ii) = ans(nrow, ii) * areal
  END DO
END IF
END PROCEDURE QuadraturePoint_Line1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE QuadratureMethods
