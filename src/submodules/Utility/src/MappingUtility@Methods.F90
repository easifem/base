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

SUBMODULE(MappingUtility) Methods
USE BaseMethod, ONLY: UpperCase, SOFTLE, RefCoord_Hexahedron
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment1
ans = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin
END PROCEDURE FromBiunitLine2Segment1

!----------------------------------------------------------------------------
!                                                       FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin)
  ans(:, ii) = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin(ii)
END DO
END PROCEDURE FromBiunitLine2Segment2

!----------------------------------------------------------------------------
!                                                   FromBiUnitLine2UnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitLine2UnitLine
ans = 0.5_DFP * (1.0_DFP + xin)
END PROCEDURE FromBiUnitLine2UnitLine

!----------------------------------------------------------------------------
!                                                   FromUnitLine2BiUnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitLine2BiUnitLine
ans = 2.0_DFP * xin - 1.0_DFP
END PROCEDURE FromUnitLine2BiUnitLine

!----------------------------------------------------------------------------
!                                                  FromUnitTriangle2Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2Triangle1
INTEGER(I4B) :: ii
DO ii = 1, SIZE(ans, 2)
  ans(:, ii) = x1 + (x2 - x1) * xin(1, ii) + (x3 - x1) * xin(2, ii)
END DO
END PROCEDURE FromUnitTriangle2Triangle1

!----------------------------------------------------------------------------
!                                        FromBiUnitQuadrangle2UnitQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitQuadrangle2UnitQuadrangle1
ans = FromBiUnitQuadrangle2Quadrangle(&
  & xin=xin, &
  & x1=[0.0_DFP, 0.0_DFP],  &
  & x2=[1.0_DFP, 0.0_DFP],  &
  & x3=[1.0_DFP, 1.0_DFP],  &
  & x4=[0.0_DFP, 1.0_DFP])
END PROCEDURE FromBiUnitQuadrangle2UnitQuadrangle1

!----------------------------------------------------------------------------
!                                        FromBiUnitQuadrangle2UnitQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitQuadrangle2BiUnitQuadrangle1
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4

DO ii = 1, SIZE(ans, 2)
  xi = xin(1, ii)
  eta = xin(2, ii)
  p1 = (1.0 - xi) * (1.0 - eta)
  p2 = xi * (1.0 - eta)
  p3 = xi * eta
  p4 = (1.0 - xi) * eta
  ans(1:2, ii) =  &
    &   [-1.0_DFP, -1.0_DFP] * p1  &
    & + [1.0_DFP, -1.0_DFP] * p2  &
    & + [1.0_DFP, 1.0_DFP] * p3  &
    & + [-1.0_DFP, 1.0_DFP] * p4
END DO
END PROCEDURE FromUnitQuadrangle2BiUnitQuadrangle1

!----------------------------------------------------------------------------
!                                           FromBiUnitQuadrangle2Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitQuadrangle2Quadrangle1
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4
!!
DO ii = 1, SIZE(ans, 2)
  xi = xin(1, ii)
  eta = xin(2, ii)
  p1 = 0.25 * (1.0 - xi) * (1.0 - eta)
  p2 = 0.25 * (1.0 + xi) * (1.0 - eta)
  p3 = 0.25 * (1.0 + xi) * (1.0 + eta)
  p4 = 0.25 * (1.0 - xi) * (1.0 + eta)
  ans(:, ii) = x1 * p1 + x2 * p2 + x3 * p3 + x4 * p4
END DO
END PROCEDURE FromBiUnitQuadrangle2Quadrangle1

!----------------------------------------------------------------------------
!                                           FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2Hexahedron1
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4, p5, p6, p7, p8, zeta
REAL(DFP), PARAMETER :: one = 1.0_DFP, p125 = 0.125_DFP

DO ii = 1, SIZE(ans, 2)
  xi = xin(1, ii)
  eta = xin(2, ii)
  zeta = xin(3, ii)
  p1 = p125 * (one - xi) * (one - eta) * (one - zeta)
  p2 = p125 * (one + xi) * (one - eta) * (one - zeta)
  p3 = p125 * (one + xi) * (one + eta) * (one - zeta)
  p4 = p125 * (one - xi) * (one + eta) * (one - zeta)
  p5 = p125 * (one - xi) * (one - eta) * (one + zeta)
  p6 = p125 * (one + xi) * (one - eta) * (one + zeta)
  p7 = p125 * (one + xi) * (one + eta) * (one + zeta)
  p8 = p125 * (one - xi) * (one + eta) * (one + zeta)
  ans(:, ii) = x1 * p1 + x2 * p2 + x3 * p3 + x4 * p4 + &
    & x5 * p5 + x6 * p6 + x7 * p7 + x8 * p8
END DO
END PROCEDURE FromBiUnitHexahedron2Hexahedron1

!----------------------------------------------------------------------------
!                                       FromBiUnitHexahedron2UnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitHexahedron1
REAL(DFP) :: xij(3, 8)
xij = RefCoord_Hexahedron(refHexahedron="UNIT")
ans = FromBiUnitHexahedron2Hexahedron(&
  & xin=xin,  &
  & x1=xij(:, 1),  &
  & x2=xij(:, 2), &
  & x3=xij(:, 3), &
  & x4=xij(:, 4), &
  & x5=xij(:, 5), &
  & x6=xij(:, 6), &
  & x7=xij(:, 7), &
  & x8=xij(:, 8))
END PROCEDURE FromBiUnitHexahedron2UnitHexahedron1

!----------------------------------------------------------------------------
!                                           FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitHexahedron2BiUnitHexahedron1
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4, p5, p6, p7, p8, zeta
REAL(DFP), PARAMETER :: one = 1.0_DFP, p125 = 0.125_DFP
REAL(DFP) :: x(3, 8)

x = RefCoord_Hexahedron(refHexahedron="BIUNIT")

DO ii = 1, SIZE(ans, 2)
  xi = xin(1, ii)
  eta = xin(2, ii)
  zeta = xin(3, ii)
  p1 = (one - xi) * (one - eta) * (one - zeta)
  p2 = (xi) * (one - eta) * (one - zeta)
  p3 = (xi) * (eta) * (one - zeta)
  p4 = (one - xi) * (eta) * (one - zeta)
  p5 = (one - xi) * (one - eta) * (zeta)
  p6 = (xi) * (one - eta) * (zeta)
  p7 = (xi) * (eta) * (zeta)
  p8 = (one - xi) * (eta) * (zeta)
  ans(:, ii) = x(:, 1) * p1 + x(:, 2) * p2 + x(:, 3) * p3 + x(:, 4) * p4 + &
    & x(:, 5) * p5 + x(:, 6) * p6 + x(:, 7) * p7 + x(:, 8) * p8
END DO
END PROCEDURE FromUnitHexahedron2BiUnitHexahedron1

!----------------------------------------------------------------------------
!                                             FromBiUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2BiUnitSqr
! ans(1, :) = 2.0_DFP * (1.0_DFP + xin(1, :)) / (1.0_DFP - xin(2, :)) - 1.0_DFP
ans(1, :) = (1.0_DFP + zero + 2.0_DFP * xin(1, :) + xin(2, :)) &
  & / (1.0_DFP + zero - xin(2, :))
ans(2, :) = xin(2, :)
END PROCEDURE FromBiUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2BiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2BiUnitTriangle
ans(1, :) = 0.5_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :)) &
  & - 1.0_DFP
ans(2, :) = xin(2, :)
END PROCEDURE FromBiUnitSqr2BiUnitTriangle

!----------------------------------------------------------------------------
!                                                 FromUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitSqr
ans(1, :) = (2.0_DFP * xin(1, :) + xin(2, :) - 1.0_DFP + zero) &
  & / (1.0_DFP + zero - xin(2, :))
ans(2, :) = 2.0_DFP * xin(2, :) - 1.0_DFP
END PROCEDURE FromUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                                 FromBiUnitSqr2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2UnitTriangle
ans(1, :) = 0.25_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :))
ans(2, :) = 0.5_DFP * (xin(2, :) + 1.0_DFP)
END PROCEDURE FromBiUnitSqr2UnitTriangle

!----------------------------------------------------------------------------
!                                               BarycentricCoordUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTriangle
ans(1, :) = 1.0_DFP - xin(1, :) - xin(2, :)
ans(2, :) = xin(1, :)
ans(3, :) = xin(2, :)
END PROCEDURE BarycentricCoordUnitTriangle

!----------------------------------------------------------------------------
!                                             BarycentricCoordBiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTriangle
ans(1, :) = -0.5_DFP * (xin(1, :) + xin(2, :))
ans(2, :) = 0.5_DFP * (1.0_DFP + xin(1, :))
ans(3, :) = 0.5_DFP * (1.0_DFP + xin(2, :))
END PROCEDURE BarycentricCoordBiUnitTriangle

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTriangle
CHARACTER(20) :: layout
layout = TRIM(UpperCase(refTriangle))
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  ans = BarycentricCoordBiUnitTriangle(xin)
CASE ("UNIT")
  ans = BarycentricCoordUnitTriangle(xin)
END SELECT
END PROCEDURE BarycentricCoordTriangle

!----------------------------------------------------------------------------
!                                        FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2UnitTriangle
ans = 0.5_DFP * (1.0_DFP + xin)
END PROCEDURE FromBiUnitTriangle2UnitTriangle

!----------------------------------------------------------------------------
!                                        FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitTriangle
ans = -1.0_DFP + 2.0_DFP * xin
END PROCEDURE FromUnitTriangle2BiUnitTriangle

!----------------------------------------------------------------------------
!                                     FromBiUnitTetrahedron2UnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron
ans = 0.5_DFP * (1.0_DFP + xin)
END PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron
ans = -1.0_DFP + 2.0_DFP * xin
END PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron

!----------------------------------------------------------------------------
!                                         FromBiUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2Tetrahedron
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin, 2)
  ans(:, ii) = &
  & -0.5_DFP * (1.0_DFP + xin(1, :) + xin(2, :) + xin(3, :)) * x1(ii) &
  & + 0.5_DFP * (1.0_DFP + xin(1, :)) * x2(ii)  &
  & + 0.5_DFP * (1.0_DFP + xin(2, :)) * x3(ii)  &
  & + 0.5_DFP * (1.0_DFP + xin(3, :)) * x4(ii)
END DO
END PROCEDURE FromBiUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                            FromUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2Tetrahedron
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin, 2)
  ans(:, ii) = &
  &  (1.0_DFP - xin(1, :) - xin(2, :) - xin(3, :)) * x1(ii) &
  & + xin(1, :) * x2(ii)  &
  & + xin(2, :) * x3(ii)  &
  & + xin(3, :) * x4(ii)
END DO
END PROCEDURE FromUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                               BarycentricCoordUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTetrahedron
ans(1, :) = 1.0_DFP - xin(1, :) - xin(2, :) - xin(3, :)
ans(2, :) = xin(1, :)
ans(3, :) = xin(2, :)
ans(4, :) = xin(3, :)
END PROCEDURE BarycentricCoordUnitTetrahedron

!----------------------------------------------------------------------------
!                                             BarycentricCoordBiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTetrahedron
ans(1, :) = -0.5_DFP * (1.0_DFP + xin(1, :) + xin(2, :) + xin(3, :))
ans(2, :) = 0.5_DFP * (1.0_DFP + xin(1, :))
ans(3, :) = 0.5_DFP * (1.0_DFP + xin(2, :))
ans(4, :) = 0.5_DFP * (1.0_DFP + xin(3, :))
END PROCEDURE BarycentricCoordBiUnitTetrahedron

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTetrahedron
CHARACTER(20) :: layout
layout = TRIM(UpperCase(refTetrahedron))
SELECT CASE (TRIM(layout))
CASE ("BIUNIT")
  ans = BarycentricCoordBiUnitTetrahedron(xin)
CASE ("UNIT")
  ans = BarycentricCoordUnitTetrahedron(xin)
END SELECT
END PROCEDURE BarycentricCoordTetrahedron

!----------------------------------------------------------------------------
!                                     FromBiUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2BiUnitHexahedron
INTEGER(I4B) :: ii
REAL(DFP) :: tol, alpha, beta

tol = 1.0E-12

DO ii = 1, SIZE(xin, 2)
  alpha = xin(2, ii) + xin(3, ii)
  beta = 1.0_DFP - xin(3, ii)

  IF (SOFTLE(ABS(alpha), zero, tol)) THEN
    ans(1, ii) = -1.0_DFP
  ELSE
    ans(1, ii) = -(2.0_DFP + 2.0_DFP * xin(1, ii) + alpha) / alpha
  END IF

  IF (SOFTLE(ABS(beta), zero, tol)) THEN
    ans(2, ii) = -1.0_DFP
  ELSE
    ans(2, ii) = (1.0_DFP + 2.0_DFP * xin(2, ii) + xin(3, ii)) / beta
  END IF

  ans(3, ii) = xin(3, ii)
END DO

END PROCEDURE FromBiUnitTetrahedron2BiUnitHexahedron

!----------------------------------------------------------------------------
!                                     FromBiUnitHexahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron
ans(1, :) = 0.25_DFP  &
           & * (1.0_DFP + xin(1, :)) &
           & * (1.0_DFP - xin(2, :)) &
           & * (1.0_DFP - xin(3, :)) - 1.0_DFP

ans(2, :) = 0.5_DFP  &
           & * (1.0_DFP + xin(2, :)) &
           & * (1.0_DFP - xin(3, :)) - 1.0_DFP

ans(3, :) = xin(3, :)
END PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron

!----------------------------------------------------------------------------
!                                     FromUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2BiUnitHexahedron
ans = FromBiUnitTetrahedron2BiUnitHexahedron(&
  & FromUnitTetrahedron2BiUnitTetrahedron(xin))
END PROCEDURE FromUnitTetrahedron2BiUnitHexahedron

!----------------------------------------------------------------------------
!                                     FromBiUnitHexahedron2UnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitTetrahedron
ans = FromBiUnitTetrahedron2UnitTetrahedron( &
  & FromBiUnitHexahedron2BiUnitTetrahedron(xin))
END PROCEDURE FromBiUnitHexahedron2UnitTetrahedron

END SUBMODULE Methods
