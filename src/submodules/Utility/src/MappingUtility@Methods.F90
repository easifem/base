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
USE BaseMethod, ONLY: UpperCase, &
                      SOFTLE, &
                      RefCoord_Tetrahedron, &
                      RefCoord_Hexahedron, &
                      TriangleArea2D, &
                      TriangleArea3D, &
                      QuadrangleArea2D, &
                      QuadrangleArea3D, &
                      TetrahedronVolume3D, &
                      HexahedronVolume3D

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

MODULE PROCEDURE FromBiunitLine2Segment1_
tsize = SIZE(xin)
ans(1:tsize) = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin
END PROCEDURE FromBiunitLine2Segment1_

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
!                                                       FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment2_
INTEGER(I4B) :: ii
nrow = SIZE(x1)
ncol = SIZE(xin)
DO ii = 1, ncol
  ans(1:nrow, ii) = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin(ii)
END DO
END PROCEDURE FromBiunitLine2Segment2_

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
!                                                            FromLine2Line
!----------------------------------------------------------------------------

MODULE PROCEDURE FromLine2Line_
CHARACTER(2) :: acase
INTEGER(I4B) :: ii, n

acase = from(1:1)//to(1:1)
n = SIZE(xin)

SELECT CASE (acase)

CASE ("BU", "bu", "bU", "Bu")

  DO CONCURRENT(ii=1:n)
    ans(ii) = 0.5_DFP * (1.0_DFP + xin(ii))
  END DO

CASE ("BB", "UU", "bb", "uu")

  DO CONCURRENT(ii=1:n)
    ans(ii) = xin(ii)
  END DO

CASE ("UB", "ub", "uB", "Ub")

  DO CONCURRENT(ii=1:n)
    ans(ii) = 2.0_DFP * xin(ii) - 1.0_DFP
  END DO

END SELECT
END PROCEDURE FromLine2Line_

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
!                                                  FromUnitTriangle2Triangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2Triangle1_
INTEGER(I4B) :: ii, jj

nrow = SIZE(x1)
ncol = SIZE(xin, 2)

DO CONCURRENT(jj=1:ncol, ii=1:nrow)
  ans(ii, jj) = x1(ii) + (x2(ii) - x1(ii)) * xin(1, jj) &
                + (x3(ii) - x1(ii)) * xin(2, jj)
END DO
END PROCEDURE FromUnitTriangle2Triangle1_

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
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitQuadrangle2Quadrangle1_(xin=xin, ans=ans, x1=x1, x2=x2, &
                                       x3=x3, x4=x4, nrow=nrow, ncol=ncol)
END PROCEDURE FromBiUnitQuadrangle2Quadrangle1

!----------------------------------------------------------------------------
!                                           FromBiUnitQuadrangle2Quadrangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitQuadrangle2Quadrangle1_
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4

! ans(SIZE(x1), SIZE(xin, 2))
nrow = SIZE(x1)
ncol = SIZE(xin, 2)

DO ii = 1, ncol
  xi = xin(1, ii)
  eta = xin(2, ii)
  p1 = 0.25 * (1.0 - xi) * (1.0 - eta)
  p2 = 0.25 * (1.0 + xi) * (1.0 - eta)
  p3 = 0.25 * (1.0 + xi) * (1.0 + eta)
  p4 = 0.25 * (1.0 - xi) * (1.0 + eta)
  ans(1:nrow, ii) = x1 * p1 + x2 * p2 + x3 * p3 + x4 * p4
END DO
END PROCEDURE FromBiUnitQuadrangle2Quadrangle1_

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
!                                                     FromTriangle2Square_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromTriangle2Square_
CHARACTER(2) :: acase
acase = from(1:1)//to(1:1)

SELECT CASE (acase)

CASE ("BB", "bb")

  ans(1, :) = (1.0_DFP + zero + 2.0_DFP * xin(1, :) + xin(2, :)) &
              / (1.0_DFP + zero - xin(2, :))
  ans(2, :) = xin(2, :)

CASE ("UB", "ub")

  ans(1, :) = (2.0_DFP * xin(1, :) + xin(2, :) - 1.0_DFP + zero) &
              / (1.0_DFP + zero - xin(2, :))
  ans(2, :) = 2.0_DFP * xin(2, :) - 1.0_DFP

END SELECT
END PROCEDURE FromTriangle2Square_

!----------------------------------------------------------------------------
!                                             FromBiUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2BiUnitSqr
CALL FromTriangle2Square_(xin=xin, ans=ans, from="B", to="B")
END PROCEDURE FromBiUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                                 FromUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitSqr
CALL FromTriangle2Square_(xin=xin, ans=ans, from="U", to="B")
END PROCEDURE FromUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                                     FromSquare2Triangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromSquare2Triangle_
CHARACTER(2) :: acase
acase = from(1:1)//to(1:1)

SELECT CASE (acase)

CASE ("BB", "bb", "Bb", "bB")

  ans(1, :) = 0.5_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :)) &
              - 1.0_DFP
  ans(2, :) = xin(2, :)

CASE ("BU", "bu", "Bu", "bU")

  ans(1, :) = 0.25_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :))
  ans(2, :) = 0.5_DFP * (xin(2, :) + 1.0_DFP)

END SELECT
END PROCEDURE FromSquare2Triangle_

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2BiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2BiUnitTriangle
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="B", to="B")
END PROCEDURE FromBiUnitSqr2BiUnitTriangle

!----------------------------------------------------------------------------
!                                                 FromBiUnitSqr2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2UnitTriangle
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="B", to="U")
END PROCEDURE FromBiUnitSqr2UnitTriangle

!----------------------------------------------------------------------------
!                                               BarycentricCoordUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTriangle
CALL BaryCentricCoordTriangle_(xin, "U", ans)
END PROCEDURE BarycentricCoordUnitTriangle

!----------------------------------------------------------------------------
!                                             BarycentricCoordBiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTriangle
CALL BaryCentricCoordTriangle_(xin, "B", ans)
END PROCEDURE BarycentricCoordBiUnitTriangle

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTriangle
CALL BaryCentricCoordTriangle_(xin, refTriangle, ans)
END PROCEDURE BarycentricCoordTriangle

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTriangle_
SELECT CASE (refTriangle(1:1))
CASE ("B", "b")
  ans(1, :) = -0.5_DFP * (xin(1, :) + xin(2, :))
  ans(2, :) = 0.5_DFP * (1.0_DFP + xin(1, :))
  ans(3, :) = 0.5_DFP * (1.0_DFP + xin(2, :))

CASE ("U", "u")
  ans(1, :) = 1.0_DFP - xin(1, :) - xin(2, :)
  ans(2, :) = xin(1, :)
  ans(3, :) = xin(2, :)
END SELECT
END PROCEDURE BarycentricCoordTriangle_

!----------------------------------------------------------------------------
!                                                   FromTriangle2Triangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromTriangle2Triangle_
CHARACTER(2) :: acase
INTEGER(I4B) :: ii, n

acase = from(1:1)//to(1:1)

SELECT CASE (acase)

CASE ("BU", "bu", "bU", "Bu")

  ans = 0.5_DFP * (1.0_DFP + xin)

CASE ("UB", "ub", "Ub", "uB")

  ans = -1.0_DFP + 2.0_DFP * xin

CASE ("UT", "ut", "Ut", "uT")

  n = SIZE(xin, 2)
  DO CONCURRENT(ii=1:n)
    ans(:, ii) = x1 + (x2 - x1) * xin(1, ii) + (x3 - x1) * xin(2, ii)
  END DO

END SELECT
END PROCEDURE FromTriangle2Triangle_

!----------------------------------------------------------------------------
!                                            FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2UnitTriangle
CALL FromTriangle2Triangle_(xin=xin, ans=ans, from="B", to="U")
END PROCEDURE FromBiUnitTriangle2UnitTriangle

!----------------------------------------------------------------------------
!                                           FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitTriangle
CALL FromTriangle2Triangle_(xin=xin, ans=ans, from="U", to="B")
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
    -0.5_DFP * (1.0_DFP + xin(1, ii) + xin(2, ii) + xin(3, ii)) * x1(:) &
    + 0.5_DFP * (1.0_DFP + xin(1, ii)) * x2(:) &
    + 0.5_DFP * (1.0_DFP + xin(2, ii)) * x3(:) &
    + 0.5_DFP * (1.0_DFP + xin(3, ii)) * x4(:)
END DO
END PROCEDURE FromBiUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                            FromUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2Tetrahedron
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin, 2)
  ans(:, ii) = &
    (1.0_DFP - xin(1, ii) - xin(2, ii) - xin(3, ii)) * x1(:) &
    + xin(1, ii) * x2(:) &
    + xin(2, ii) * x3(:) &
    + xin(3, ii) * x4(:)
END DO
END PROCEDURE FromUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                            BarycentricCoordUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTetrahedron
ans(1, :) = 1.0_DFP - xin(1, :) - xin(2, :) - xin(3, :)
ans(2, :) = xin(1, :)
ans(3, :) = xin(2, :)
ans(4, :) = xin(3, :)
END PROCEDURE BarycentricCoordUnitTetrahedron

!----------------------------------------------------------------------------
!                                           BarycentricCoordBiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTetrahedron
ans(1, :) = -0.5_DFP * (1.0_DFP + xin(1, :) + xin(2, :) + xin(3, :))
ans(2, :) = 0.5_DFP * (1.0_DFP + xin(1, :))
ans(3, :) = 0.5_DFP * (1.0_DFP + xin(2, :))
ans(4, :) = 0.5_DFP * (1.0_DFP + xin(3, :))
END PROCEDURE BarycentricCoordBiUnitTetrahedron

!----------------------------------------------------------------------------
!                                                BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTetrahedron
SELECT CASE (refTetrahedron(1:1))
CASE ("B", "b")
  ans = BarycentricCoordBiUnitTetrahedron(xin)
CASE ("U", "u")
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
!                                       FromUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2BiUnitHexahedron
ans = FromBiUnitTetrahedron2BiUnitHexahedron(&
  & FromUnitTetrahedron2BiUnitTetrahedron(xin))
END PROCEDURE FromUnitTetrahedron2BiUnitHexahedron

!----------------------------------------------------------------------------
!                                       FromBiUnitHexahedron2UnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitTetrahedron
ans = FromBiUnitTetrahedron2UnitTetrahedron( &
  & FromBiUnitHexahedron2BiUnitTetrahedron(xin))
END PROCEDURE FromBiUnitHexahedron2UnitTetrahedron

!----------------------------------------------------------------------------
!                                                             JacobianLine
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianLine
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 1.0_DFP
  CASE ("UNIT")
    ans = 0.5_DFP
  CASE ("LINE")
    ans = NORM2(xij(:, 2) - xij(:, 1)) / 2.0_DFP
  END SELECT
CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 2.0_DFP
  CASE ("UNIT")
    ans = 1.0_DFP
  CASE ("LINE")
    ans = NORM2(xij(:, 2) - xij(:, 1))
  END SELECT
CASE ("LINE")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 2.0_DFP / NORM2(xij(:, 2) - xij(:, 1))
  CASE ("UNIT")
    ans = 1.0_DFP / NORM2(xij(:, 2) - xij(:, 1))
  CASE ("LINE")
    ans = 1.0_DFP
  END SELECT
END SELECT
END PROCEDURE JacobianLine

!----------------------------------------------------------------------------
!                                                         JacobianTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianTriangle
ans = 1.0_DFP
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 1.0_DFP
  CASE ("UNIT")
    ans = 0.25_DFP
  CASE ("TRIANGLE")
    IF (PRESENT(xij)) THEN

      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL TriangleArea2D(xij(1:2, 1:3), ans)
      ELSE
        CALL TriangleArea3D(xij(1:3, 1:3), ans)
      END IF

      ans = ans / 2.0_DFP

    END IF
  END SELECT
CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 4.0_DFP
  CASE ("UNIT")
    ans = 1.0_DFP

  CASE ("TRIANGLE")
    IF (PRESENT(xij)) THEN
      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL TriangleArea2D(xij(1:2, 1:3), ans)
      ELSE
        CALL TriangleArea3D(xij(1:3, 1:3), ans)
      END IF
      ans = ans / 0.5_DFP
    END IF
  END SELECT

CASE ("TRIANGLE")

  IF (PRESENT(xij)) THEN
    IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
      CALL TriangleArea2D(xij(1:2, 1:3), ans)
    ELSE IF (SIZE(xij, 1) .EQ. 3_I4B) THEN
      CALL TriangleArea3D(xij(1:3, 1:3), ans)
    END IF
  ELSE
    RETURN
  END IF

  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 2.0_DFP / ans
  CASE ("UNIT")
    ans = 0.5_DFP / ans
  END SELECT

END SELECT
END PROCEDURE JacobianTriangle

!----------------------------------------------------------------------------
!                                                         JacobianQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianQuadrangle
ans = 1.0_DFP
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 1.0_DFP
  CASE ("UNIT")
    ans = 0.25_DFP

  CASE ("QUADRANGLE")
    IF (PRESENT(xij)) THEN
      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL QuadrangleArea2D(xij(1:2, 1:4), ans)
      ELSE
        CALL QuadrangleArea3D(xij(1:3, 1:4), ans)
      END IF
      ans = ans / 4.0_DFP
    END IF
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 4.0_DFP
  CASE ("UNIT")
    ans = 1.0_DFP

  CASE ("QUADRANGLE")
    IF (PRESENT(xij)) THEN
      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL QuadrangleArea2D(xij(1:2, 1:4), ans)
      ELSE
        CALL QuadrangleArea3D(xij(1:3, 1:4), ans)
      END IF
    END IF
  END SELECT

CASE ("QUADRANGLE")

  IF (PRESENT(xij)) THEN
    IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
      CALL QuadrangleArea2D(xij(1:2, 1:4), ans)
    ELSE
      CALL QuadrangleArea3D(xij(1:3, 1:4), ans)
    END IF
  ELSE
    RETURN
  END IF

  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 4.0_DFP / ans
  CASE ("UNIT")
    ans = 1.0_DFP / ans
  END SELECT

END SELECT
END PROCEDURE JacobianQuadrangle

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianHexahedron
REAL(DFP) :: ans0
ans = 1.0_DFP
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 1.0_DFP
  CASE ("UNIT")
    ans = 0.125_DFP
  CASE ("HEXAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL HexahedronVolume3D(xij(1:3, 1:8), ans)
      CALL HexahedronVolume3D(RefCoord_Hexahedron(from), ans0)
      ans = ans / ans0
    END IF
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 8.0_DFP
  CASE ("UNIT")
    ans = 1.0_DFP
  CASE ("HEXAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL HexahedronVolume3D(xij(1:3, 1:8), ans)
      CALL HexahedronVolume3D(RefCoord_Hexahedron(from), ans0)
      ans = ans / ans0
    END IF
  END SELECT

CASE ("HEXAHEDRON")
  IF (PRESENT(xij)) THEN
    CALL HexahedronVolume3D(xij(1:3, 1:8), ans0)
  ELSE
    RETURN
  END IF

  SELECT CASE (TRIM(to))
  CASE ("BIUNIT", "UNIT")
    CALL HexahedronVolume3D(RefCoord_Hexahedron(to), ans)
    ans = ans / ans0
  END SELECT

END SELECT
END PROCEDURE JacobianHexahedron

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianTetrahedron
REAL(DFP) :: ans0
ans = 1.0_DFP
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 1.0_DFP
  CASE ("UNIT")
    ans = 0.125_DFP
  CASE ("TETRAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL TetrahedronVolume3D(xij(1:3, 1:4), ans)
      CALL TetrahedronVolume3D(RefCoord_Tetrahedron(from), ans0)
      ans = ans / ans0
    END IF
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 8.0_DFP
  CASE ("UNIT")
    ans = 1.0_DFP
  CASE ("TETRAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL TetrahedronVolume3D(xij(1:3, 1:4), ans)
      CALL TetrahedronVolume3D(RefCoord_Tetrahedron(from), ans0)
      ans = ans / ans0
    END IF
  END SELECT

CASE ("TETRAHEDRON")
  IF (PRESENT(xij)) THEN
    CALL TetrahedronVolume3D(xij(1:3, 1:4), ans0)
  ELSE
    RETURN
  END IF

  SELECT CASE (TRIM(to))
  CASE ("BIUNIT", "UNIT")
    CALL TetrahedronVolume3D(RefCoord_Tetrahedron(to), ans)
    ans = ans / ans0
  END SELECT

END SELECT
END PROCEDURE JacobianTetrahedron

END SUBMODULE Methods
