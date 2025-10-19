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

SUBMODULE(LineInterpolationUtility) InterpolationMethods
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE MappingUtility, ONLY: FromBiunitLine2Segment_
USE LegendrePolynomialUtility, ONLY: LegendreQuadrature
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1Quadrature
USE JacobiPolynomialUtility, ONLY: JacobiQuadrature
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalQuadrature
USE SortUtility, ONLY: HeapSort

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "LineInterpolationUtility@InterpolationMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                ToVEFC_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE ToVEFC_Line
REAL(DFP) :: t1
INTEGER(I4B) :: np
LOGICAL(LGT) :: isok
np = SIZE(pt)
t1 = pt(np)
isok = np .GT. 2
IF (isok) THEN
  pt(3:np) = pt(2:np - 1)
  pt(2) = t1
END IF
END PROCEDURE ToVEFC_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: isok

isok = order .LE. 1_I4B
IF (isok) THEN
  ALLOCATE (ans(0))
  RETURN
END IF

tsize = LagrangeInDOF_Line(order=order)
ALLOCATE (ans(tsize))
CALL EquidistanceInPoint_Line1_(order=order, xij=xij, ans=ans, tsize=tsize)
END PROCEDURE EquidistanceInPoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line1_
INTEGER(I4B) :: ii
REAL(DFP) :: avar

tsize = 0
IF (order .LE. 1_I4B) RETURN

tsize = LagrangeInDOF_Line(order=order)

avar = (xij(2) - xij(1)) / order

DO ii = 1, tsize
  ans(ii) = xij(1) + REAL(ii, kind=dfp) * avar
END DO
END PROCEDURE EquidistanceInPoint_Line1_

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

isok = order .LE. 1_I4B
IF (isok) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF

isok = PRESENT(xij)
IF (isok) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1_I4B
END IF

ncol = LagrangeInDOF_Line(order=order)

ALLOCATE (ans(nrow, ncol))

CALL EquidistanceInPoint_Line2_(order=order, xij=xij, ans=ans, nrow=nrow, &
                                ncol=ncol)
END PROCEDURE EquidistanceInPoint_Line2

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Line2_
INTEGER(I4B) :: ii
REAL(DFP) :: x0(3, 3)

nrow = 0; ncol = 0
IF (order .LE. 1_I4B) RETURN

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
  x0(1:nrow, 1) = xij(1:nrow, 1)
  x0(1:nrow, 2) = xij(1:nrow, 2)
ELSE
  nrow = 1_I4B
  x0(1, 1) = -1.0
  x0(1, 2) = 1.0
END IF

ncol = LagrangeInDOF_Line(order=order)

x0(1:nrow, 3) = (x0(1:nrow, 2) - x0(1:nrow, 1)) / order

DO ii = 1, ncol
  ans(1:nrow, ii) = x0(1:nrow, 1) + ii * x0(1:nrow, 3)
END DO
END PROCEDURE EquidistanceInPoint_Line2_

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1
INTEGER(I4B) :: tsize

tsize = order + 1
ALLOCATE (ans(tsize))
CALL EquidistancePoint_Line1_(order=order, xij=xij, ans=ans, tsize=tsize)
END PROCEDURE EquidistancePoint_Line1

!----------------------------------------------------------------------------
!                                                   EquidistancePoint_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line1_
INTEGER(I4B) :: tempint

tsize = order + 1

SELECT CASE (order)
CASE (0)
  ans(1) = 0.5_DFP * (xij(1) + xij(2))

CASE (1)
  ans(1) = xij(1)
  ans(2) = xij(2)

CASE DEFAULT
  ans(1) = xij(1)
  ans(2) = xij(2)
  CALL EquidistanceInPoint_Line_(order=order, xij=xij, ans=ans(3:), &
                                 tsize=tempint)
END SELECT

END PROCEDURE EquidistancePoint_Line1_

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2
INTEGER(I4B) :: nrow, ncol

IF (PRESENT(xij)) THEN
  nrow = SIZE(xij, 1)
ELSE
  nrow = 1_I4B
END IF

ncol = order + 1
ALLOCATE (ans(nrow, ncol))

CALL EquidistancePoint_Line2_(order=order, xij=xij, ans=ans, nrow=nrow, &
                              ncol=ncol)
END PROCEDURE EquidistancePoint_Line2

!----------------------------------------------------------------------------
!                                                     EquidistancePoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Line2_
INTEGER(I4B) :: tempint

ncol = order + 1

SELECT CASE (order)

CASE (0)

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1) = 0.5_DFP * (xij(1:nrow, 1) + xij(1:nrow, 2))
    RETURN
  END IF

  nrow = 1_I4B
  ans(1, 1) = 0.0_DFP

CASE (1)

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1:2) = xij(1:nrow, 1:2)
    RETURN
  END IF

  nrow = 1
  ans(1, 1) = -1.0_DFP
  ans(1, 2) = 1.0_DFP

CASE DEFAULT

  IF (PRESENT(xij)) THEN
    nrow = SIZE(xij, 1)
    ans(1:nrow, 1:2) = xij(1:nrow, 1:2)
  ELSE
    nrow = 1
    ans(1, 1) = -1.0_DFP
    ans(1, 2) = 1.0_DFP
  END IF

  CALL EquidistanceInPoint_Line2_(order=order, xij=xij, ans=ans(:, 3:), &
                                  nrow=nrow, ncol=tempint)

END SELECT

END PROCEDURE EquidistancePoint_Line2_

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line1
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok

nrow = 1
isok = PRESENT(xij)
IF (isok) nrow = SIZE(xij, 1)
ncol = order + 1

ALLOCATE (ans(nrow, ncol))

CALL InterpolationPoint_Line1_( &
  order=order, ipType=ipType, ans=ans, nrow=nrow, ncol=ncol, layout=layout, &
  xij=xij, alpha=alpha, beta=beta, lambda=lambda)
END PROCEDURE InterpolationPoint_Line1

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line2
INTEGER(I4B) :: tsize
tsize = order + 1
ALLOCATE (ans(tsize))
CALL InterpolationPoint_Line2_( &
  order=order, ipType=ipType, xij=xij, layout=layout, alpha=alpha, &
  beta=beta, lambda=lambda, ans=ans, tsize=tsize)
END PROCEDURE InterpolationPoint_Line2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Line1_()"
#endif

REAL(DFP) :: temp(64)

IF (order .EQ. 0_I4B) THEN
  CALL EquidistancePoint_Line_(xij=xij, order=order, ans=ans, nrow=nrow, &
                               ncol=ncol)
  RETURN
END IF

CALL handle_error
!! handle_error is defined in this routine, see below

ncol = order + 1

SELECT CASE (ipType)

CASE (ipopt%Equidistance)
  CALL EquidistancePoint_Line_(xij=xij, order=order, nrow=nrow, ncol=ncol, &
                               ans=ans)
  CALL handle_increasing

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussLegendreLobatto)
  CALL LegendreQuadrature(n=ncol, pt=temp(1:ncol), &
                          quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=ncol, pt=temp(1:ncol), &
                            quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=ncol, pt=temp(1:ncol), quadType=ipopt%Gauss, &
                        alpha=alpha, beta=beta)
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobiLobatto)
  CALL JacobiQuadrature(n=ncol, pt=temp(1:ncol), &
                        quadType=ipopt%GaussLobatto, alpha=alpha, beta=beta)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussUltraspherical)
  CALL UltrasphericalQuadrature(n=ncol, pt=temp(1:ncol), &
                                quadType=ipopt%Gauss, lambda=lambda)
  CALL handle_non_equidistance

CASE (ipopt%GaussUltrasphericalLobatto)
  CALL UltrasphericalQuadrature(n=ncol, pt=temp(1:ncol), &
                                quadType=ipopt%GaussLobatto, lambda=lambda)

  CALL handle_vefc
  CALL handle_non_equidistance

#ifdef DEBUG_VER
CASE DEFAULT
  ! AssertError1(a, myName, modName, lineNo, msg)
  CALL AssertError1(.FALSE., myName, modName, __LINE__, &
                    "Unknown iptype")
#endif

END SELECT

CONTAINS

SUBROUTINE handle_vefc
  REAL(DFP) :: t1
  !! layout VEFC
  IF (layout(1:1) .EQ. "V") THEN
    t1 = temp(order + 1)
    IF (order .GE. 2) THEN
      temp(3:order + 1) = temp(2:order)
    END IF
    temp(2) = t1
  END IF
END SUBROUTINE handle_vefc

SUBROUTINE handle_increasing
  INTEGER(I4B) :: ii
  !! layout INCREASING
  IF (layout(1:1) .EQ. "I") THEN
    DO ii = 1, nrow
      CALL HeapSort(ans(ii, :))
    END DO
  END IF
END SUBROUTINE

SUBROUTINE handle_non_equidistance
  IF (PRESENT(xij)) THEN
    CALL FromBiunitLine2Segment_(xin=temp(1:ncol), x1=xij(:, 1), &
                                 x2=xij(:, 2), ans=ans, nrow=nrow, ncol=ncol)
  ELSE
    nrow = 1
    ans(1, 1:ncol) = temp(1:ncol)
  END IF
END SUBROUTINE handle_non_equidistance

SUBROUTINE handle_error
#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok

  SELECT CASE (ipType)
  CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto)
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                    "alpha and beta should be present for ipType=GaussJacobi")

  CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto)
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                    "lambda should be present for ipType=GaussUltraSpherical")
  END SELECT
#endif

END SUBROUTINE handle_error

END PROCEDURE InterpolationPoint_Line1_

!----------------------------------------------------------------------------
!                                                    InterpolationPoint_Line_
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Line2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "InterpolationPoint_Line2_()"
#endif

tsize = order + 1
IF (order .EQ. 0_I4B) THEN
  ans(1) = 0.5_DFP * (xij(1) + xij(2))
  RETURN
END IF

CALL handle_error

SELECT CASE (ipType)

CASE (ipopt%Equidistance)
  CALL EquidistancePoint_Line_(xij=xij, order=order, tsize=tsize, ans=ans)

  IF (layout(1:2) .EQ. "IN") CALL HeapSort(ans(1:tsize))

CASE (ipopt%GaussLegendre)
  CALL LegendreQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshev)
  CALL Chebyshev1Quadrature(n=tsize, pt=ans, quadType=ipopt%Gauss)
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobi)
  CALL JacobiQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss, alpha=alpha, &
                        beta=beta)
  CALL handle_non_equidistance

CASE (ipopt%GaussUltraspherical)
  CALL UltrasphericalQuadrature(n=tsize, pt=ans, quadType=ipopt%Gauss, &
                                lambda=lambda)
  CALL handle_non_equidistance

CASE (ipopt%GaussLegendreLobatto)
  CALL LegendreQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussChebyshevLobatto)
  CALL Chebyshev1Quadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussJacobiLobatto)
  CALL JacobiQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto, alpha=alpha, &
                        beta=beta)
  CALL handle_vefc
  CALL handle_non_equidistance

CASE (ipopt%GaussUltrasphericalLobatto)
 CALL UltrasphericalQuadrature(n=tsize, pt=ans, quadType=ipopt%GaussLobatto, &
                                lambda=lambda)
  CALL handle_vefc
  CALL handle_non_equidistance

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, modName, __LINE__, "Unknown ipType")
#endif

END SELECT

CONTAINS

SUBROUTINE handle_vefc
  REAL(DFP) :: t1

  IF (layout(1:2) .EQ. "VE") THEN
    t1 = ans(order + 1)
    IF (order .GE. 2) THEN
      ans(3:) = ans(2:order)
    END IF
    ans(2) = t1
  END IF

END SUBROUTINE handle_vefc

SUBROUTINE handle_non_equidistance
  CALL FromBiunitLine2Segment_(xin=ans, x1=xij(1), x2=xij(2), &
                               ans=ans, tsize=tsize)
END SUBROUTINE handle_non_equidistance

SUBROUTINE handle_error

#ifdef DEBUG_VER
  LOGICAL(LGT) :: isok

  SELECT CASE (ipType)
  CASE (ipopt%GaussJacobi, ipopt%GaussJacobiLobatto)
    isok = PRESENT(alpha) .AND. PRESENT(beta)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                    "alpha and beta should be present for ipType=GaussJacobi")

  CASE (ipopt%GaussUltraSpherical, ipopt%GaussUltraSphericalLobatto)
    isok = PRESENT(lambda)
    CALL AssertError1(isok, myName, modName, __LINE__, &
                    "lambda should be present for ipType=GaussUltraSpherical")
  END SELECT

#endif

END SUBROUTINE handle_error

END PROCEDURE InterpolationPoint_Line2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE InterpolationMethods
