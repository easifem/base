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
USE StringUtility, ONLY: UpperCase
USE ApproxUtility, ONLY: SOFTLE
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TetrahedronVolume3D
USE ReferenceHexahedron_Method, ONLY: RefCoord_Hexahedron
USE ReferenceHexahedron_Method, ONLY: HexahedronVolume3D
USE ReferenceTriangle_Method, ONLY: TriangleArea2D
USE ReferenceTriangle_Method, ONLY: TriangleArea3D
USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea2D
USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea3D
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                     FromSegment2BiunitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromSegment2BiunitLine1
ans = (math%two * xin - (x1 + x2)) / (x2 - x1)
END PROCEDURE FromSegment2BiunitLine1

!----------------------------------------------------------------------------
!                                                      FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromSegment2BiunitLine1_
tsize = SIZE(xin)
ans(1:tsize) = (math%two * xin(1:tsize) - (x1 + x2)) / (x2 - x1)
END PROCEDURE FromSegment2BiunitLine1_

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment1
ans = math%half * (x1 + x2) + math%half * (x2 - x1) * xin
END PROCEDURE FromBiunitLine2Segment1

!----------------------------------------------------------------------------
!                                                      FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment1_
tsize = SIZE(xin)
ans(1:tsize) = math%half * (x1 + x2) + math%half * (x2 - x1) * xin
END PROCEDURE FromBiunitLine2Segment1_

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin)
  ans(:, ii) = math%half * (x1 + x2) + math%half * (x2 - x1) * xin(ii)
END DO
END PROCEDURE FromBiunitLine2Segment2

!----------------------------------------------------------------------------
!                                                      FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment2_
INTEGER(I4B) :: ii
nrow = SIZE(x1)
ncol = SIZE(xin)
DO ii = 1, ncol
  ans(1:nrow, ii) = math%half * (x1 + x2) + math%half * (x2 - x1) * xin(ii)
END DO
END PROCEDURE FromBiunitLine2Segment2_

!----------------------------------------------------------------------------
!                                                   FromBiUnitLine2UnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitLine2UnitLine
ans = math%half * (math%one + xin)
END PROCEDURE FromBiUnitLine2UnitLine

!----------------------------------------------------------------------------
!                                                   FromUnitLine2BiUnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitLine2BiUnitLine
INTEGER(I4B) :: tsize
CALL FromUnitLine2BiUnitLine_(xin=xin, ans=ans, tsize=tsize)
END PROCEDURE FromUnitLine2BiUnitLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitLine2BiUnitLine_
tsize = SIZE(xin)
ans(1:tsize) = math%two * xin(1:tsize) - math%one
END PROCEDURE FromUnitLine2BiUnitLine_

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
    ans(ii) = math%half * (math%one + xin(ii))
  END DO

CASE ("BB", "UU", "bb", "uu")

  DO CONCURRENT(ii=1:n)
    ans(ii) = xin(ii)
  END DO

CASE ("UB", "ub", "uB", "Ub")

  DO CONCURRENT(ii=1:n)
    ans(ii) = math%two * xin(ii) - math%one
  END DO

CASE DEFAULT

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
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitQuadrangle2UnitQuadrangle1_(xin=xin, ans=ans, nrow=nrow, &
                                           ncol=ncol)
END PROCEDURE FromBiUnitQuadrangle2UnitQuadrangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitQuadrangle2UnitQuadrangle1_
REAL(DFP), PARAMETER :: azero = 0.0_DFP, aone = math%one
REAL(DFP), PARAMETER :: x1(2) = [azero, azero], x2(2) = [aone, azero], &
                        x3(2) = [aone, aone], x4(2) = [azero, aone]
CALL FromBiUnitQuadrangle2Quadrangle_(xin=xin, x1=x1, x2=x2, x3=x3, x4=x4, &
                                      ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE FromBiUnitQuadrangle2UnitQuadrangle1_

!----------------------------------------------------------------------------
!                                        FromBiUnitQuadrangle2UnitQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitQuadrangle2BiUnitQuadrangle1
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4

DO ii = 1, SIZE(ans, 2)
  xi = xin(1, ii)
  eta = xin(2, ii)
  p1 = (math%one - xi) * (math%one - eta)
  p2 = xi * (math%one - eta)
  p3 = xi * eta
  p4 = (math%one - xi) * eta
  ans(1:2, ii) =  &
    &   [-math%one, -math%one] * p1  &
    & + [math%one, -math%one] * p2  &
    & + [math%one, math%one] * p3  &
    & + [-math%one, math%one] * p4
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
  p1 = 0.25_DFP * (math%one - xi) * (math%one - eta)
  p2 = 0.25_DFP * (math%one + xi) * (math%one - eta)
  p3 = 0.25_DFP * (math%one + xi) * (math%one + eta)
  p4 = 0.25_DFP * (math%one - xi) * (math%one + eta)
  ans(1:nrow, ii) = x1 * p1 + x2 * p2 + x3 * p3 + x4 * p4
END DO
END PROCEDURE FromBiUnitQuadrangle2Quadrangle1_

!----------------------------------------------------------------------------
!                                           FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2Hexahedron1
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitHexahedron2Hexahedron1_(xin, x1, x2, x3, x4, x5, x6, x7, x8, &
                                       ans, nrow, ncol)

END PROCEDURE FromBiUnitHexahedron2Hexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2Hexahedron1_
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4, p5, p6, p7, p8, zeta
REAL(DFP), PARAMETER :: one = math%one, p125 = 0.125_DFP

nrow = SIZE(x1)
ncol = SIZE(xin, 2)

DO ii = 1, ncol
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
  ans(1:nrow, ii) = x1 * p1 + x2 * p2 + x3 * p3 + x4 * p4 + &
                    x5 * p5 + x6 * p6 + x7 * p7 + x8 * p8
END DO
END PROCEDURE FromBiUnitHexahedron2Hexahedron1_

!----------------------------------------------------------------------------
!                                       FromBiUnitHexahedron2UnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitHexahedron1
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitHexahedron2UnitHexahedron1_(xin, ans, nrow, ncol)
END PROCEDURE FromBiUnitHexahedron2UnitHexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitHexahedron1_
INTEGER(I4B), PARAMETER :: three = 3, eight = 8
REAL(DFP) :: xij(three, eight)

xij = RefCoord_Hexahedron(refHexahedron="UNIT")

CALL FromBiUnitHexahedron2Hexahedron_( &
  xin=xin, x1=xij(:, 1), x2=xij(:, 2), &
  x3=xij(:, 3), x4=xij(:, 4), x5=xij(:, 5), x6=xij(:, 6), x7=xij(:, 7), &
  x8=xij(:, 8), ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE FromBiUnitHexahedron2UnitHexahedron1_

!----------------------------------------------------------------------------
!                                           FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitHexahedron2BiUnitHexahedron1
INTEGER(I4B) :: nrow, ncol
CALL FromUnitHexahedron2BiUnitHexahedron1_(xin, ans, nrow, ncol)
END PROCEDURE FromUnitHexahedron2BiUnitHexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitHexahedron2BiUnitHexahedron1_
INTEGER(I4B), PARAMETER :: three = 3, eight = 8
INTEGER(I4B) :: ii
REAL(DFP) :: xi, eta, p1, p2, p3, p4, p5, p6, p7, p8, zeta
REAL(DFP), PARAMETER :: one = math%one, p125 = 0.125_DFP
REAL(DFP) :: x(three, eight)

x = RefCoord_Hexahedron(refHexahedron="BIUNIT")

nrow = SIZE(xin, 1)
ncol = SIZE(xin, 2)

DO ii = 1, ncol
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
  ans(1:nrow, ii) = x(1:nrow, 1) * p1 &
                    + x(1:nrow, 2) * p2 &
                    + x(1:nrow, 3) * p3 &
                    + x(1:nrow, 4) * p4 &
                    + x(1:nrow, 5) * p5 &
                    + x(1:nrow, 6) * p6 &
                    + x(1:nrow, 7) * p7 &
                    + x(1:nrow, 8) * p8
END DO
END PROCEDURE FromUnitHexahedron2BiUnitHexahedron1_

!----------------------------------------------------------------------------
!                                                     FromTriangle2Square_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromTriangle2Square_
CHARACTER(2) :: acase
acase = from(1:1)//to(1:1)

SELECT CASE (acase)

CASE ("BB", "bb")

  ans(1, :) = (math%one + math%zero + math%two * xin(1, :) + xin(2, :)) &
              / (math%one + math%zero - xin(2, :))
  ans(2, :) = xin(2, :)

CASE ("UB", "ub")

  ans(1, :) = (math%two * xin(1, :) + xin(2, :) - math%one + math%zero) &
              / (math%one + math%zero - xin(2, :))
  ans(2, :) = math%two * xin(2, :) - math%one

CASE DEFAULT

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
REAL(DFP) :: rr(4)
INTEGER(I4B) :: ii
CHARACTER(2) :: acase

acase(1:1) = UpperCase(from(1:1))
acase(2:2) = UpperCase(to(1:1))

nrow = 2
ncol = SIZE(xin, 2)

SELECT CASE (acase)

CASE ("BB")

  DO ii = 1, ncol

    rr(1) = xin(2, ii)
    rr(2) = xin(1, ii)
    rr(3) = math%half * (math%one + rr(2))
    rr(4) = math%one - rr(1)
    rr(2) = rr(3) * rr(4) - math%one

    ans(1, ii) = rr(2)
    ans(2, ii) = rr(1)

  END DO

CASE ("BU")

  DO ii = 1, ncol
    rr(1) = xin(1, ii)
    rr(2) = xin(2, ii)
    rr(3) = 0.25_DFP * (math%one + rr(1))
    rr(4) = math%one - rr(2)
    rr(1) = rr(3) * rr(4)
    rr(3) = math%half * (rr(2) + math%one)

    ans(1, ii) = rr(1)
    ans(2, ii) = rr(3)
  END DO

CASE DEFAULT

END SELECT
END PROCEDURE FromSquare2Triangle_

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2BiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2BiUnitTriangle
INTEGER(I4B) :: nrow, ncol
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="B", to="B", nrow=nrow, &
                          ncol=ncol)
END PROCEDURE FromBiUnitSqr2BiUnitTriangle

!----------------------------------------------------------------------------
!                                                 FromBiUnitSqr2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2UnitTriangle
INTEGER(I4B) :: nrow, ncol
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="B", to="U", nrow=nrow, &
                          ncol=ncol)
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
  ans(1, :) = -math%half * (xin(1, :) + xin(2, :))
  ans(2, :) = math%half * (math%one + xin(1, :))
  ans(3, :) = math%half * (math%one + xin(2, :))

CASE ("U", "u")
  ans(1, :) = math%one - xin(1, :) - xin(2, :)
  ans(2, :) = xin(1, :)
  ans(3, :) = xin(2, :)

CASE DEFAULT
END SELECT
END PROCEDURE BarycentricCoordTriangle_

!----------------------------------------------------------------------------
!                                                   FromTriangle2Triangle_
!----------------------------------------------------------------------------

MODULE PROCEDURE FromTriangle2Triangle_
CHARACTER(2) :: acase
INTEGER(I4B) :: ii, jj
REAL(DFP) :: x21(3), x31(3)

ncol = SIZE(xin, 2)

acase(1:1) = Uppercase(from(1:1))
acase(2:2) = Uppercase(to(1:1))

SELECT CASE (acase)

CASE ("BU")

  nrow = SIZE(xin, 1)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = math%half * (math%one + xin(ii, jj))
  END DO

CASE ("UB")

  nrow = SIZE(xin, 1)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = -math%one + math%two * xin(ii, jj)
  END DO

CASE ("UT")

  nrow = SIZE(x1)

  x21(1:nrow) = x2(1:nrow) - x1(1:nrow)
  x31(1:nrow) = x3(1:nrow) - x1(1:nrow)

  DO CONCURRENT(ii=1:nrow, jj=1:ncol)
    ans(ii, jj) = x1(ii) + x21(ii) * xin(1, jj) + x31(ii) * xin(2, jj)
  END DO

CASE DEFAULT

END SELECT
END PROCEDURE FromTriangle2Triangle_

!----------------------------------------------------------------------------
!                                            FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2UnitTriangle
INTEGER(I4B) :: nrow, ncol
CALL FromTriangle2Triangle_(xin=xin, ans=ans, from="B", to="U", nrow=nrow, &
                            ncol=ncol)
END PROCEDURE FromBiUnitTriangle2UnitTriangle

!----------------------------------------------------------------------------
!                                           FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitTriangle
INTEGER(I4B) :: nrow, ncol
CALL FromTriangle2Triangle_(xin=xin, ans=ans, from="U", to="B", nrow=nrow, &
                            ncol=ncol)
END PROCEDURE FromUnitTriangle2BiUnitTriangle

!----------------------------------------------------------------------------
!                                     FromBiUnitTetrahedron2UnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitTetrahedron2UnitTetrahedron_(xin, ans, nrow, ncol)
END PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron_
INTEGER(I4B) :: ii, jj
REAL(DFP), PARAMETER :: half = math%half, one = math%one

nrow = SIZE(xin, 1)
ncol = SIZE(xin, 2)

DO CONCURRENT(ii=1:nrow, jj=1:ncol)
  ans(ii, jj) = half * (one + xin(ii, jj))
END DO

END PROCEDURE FromBiUnitTetrahedron2UnitTetrahedron_

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL FromUnitTetrahedron2BiUnitTetrahedron_(xin, ans, nrow, ncol)
END PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron_
REAL(DFP), PARAMETER :: minus_one = -math%one, two = math%two
INTEGER(I4B) :: ii, jj

nrow = SIZE(xin, 1)
ncol = SIZE(xin, 2)

DO CONCURRENT(ii=1:nrow, jj=1:ncol)
  ans(ii, jj) = minus_one + two * xin(ii, jj)
END DO

END PROCEDURE FromUnitTetrahedron2BiUnitTetrahedron_

!----------------------------------------------------------------------------
!                                         FromBiUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2Tetrahedron
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin, 2)
  ans(:, ii) = &
    -math%half * (math%one + xin(1, ii) + xin(2, ii) + xin(3, ii)) * x1(:) &
    + math%half * (math%one + xin(1, ii)) * x2(:) &
    + math%half * (math%one + xin(2, ii)) * x3(:) &
    + math%half * (math%one + xin(3, ii)) * x4(:)
END DO
END PROCEDURE FromBiUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                            FromUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2Tetrahedron
INTEGER(I4B) :: nrow, ncol
CALL FromUnitTetrahedron2Tetrahedron_(xin=xin, ans=ans, x1=x1, x2=x2, &
                                      x3=x3, x4=x4, nrow=nrow, ncol=ncol)
END PROCEDURE FromUnitTetrahedron2Tetrahedron

!----------------------------------------------------------------------------
!                                            FromUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTetrahedron2Tetrahedron_
INTEGER(I4B), PARAMETER :: ten = 10
INTEGER(I4B) :: ii
REAL(DFP), PARAMETER :: one = math%one
REAL(DFP) :: rr(ten)

nrow = SIZE(x1)
ncol = SIZE(xin, 2)

DO ii = 1, ncol

  rr(1:3) = xin(1:3, ii)
  rr(4) = one - rr(1) - rr(2) - rr(3)

  ans(1:nrow, ii) = rr(4) * x1(1:nrow) &
                    + rr(1) * x2(1:nrow) &
                    + rr(2) * x3(1:nrow) &
                    + rr(3) * x4(1:nrow)
END DO
END PROCEDURE FromUnitTetrahedron2Tetrahedron_

!----------------------------------------------------------------------------
!                                            BarycentricCoordUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL BarycentricCoordUnitTetrahedron_(xin=xin, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE BarycentricCoordUnitTetrahedron

!----------------------------------------------------------------------------
!                                            BarycentricCoordUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTetrahedron_
INTEGER(I4B) :: ii

nrow = 4
ncol = SIZE(xin, 2)

DO CONCURRENT(ii=1:ncol)
  ans(1, ii) = math%one - xin(1, ii) - xin(2, ii) - xin(3, ii)
  ans(2, ii) = xin(1, ii)
  ans(3, ii) = xin(2, ii)
  ans(4, ii) = xin(3, ii)
END DO
END PROCEDURE BarycentricCoordUnitTetrahedron_

!----------------------------------------------------------------------------
!                                           BarycentricCoordBiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL BarycentricCoordBiUnitTetrahedron_(xin=xin, ans=ans, nrow=nrow, &
                                        ncol=ncol)
END PROCEDURE BarycentricCoordBiUnitTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTetrahedron_
INTEGER(I4B) :: ii

nrow = 4
ncol = SIZE(xin, 2)

DO CONCURRENT(ii=1:ncol)
  ans(1, ii) = -math%half * (math%one + xin(1, ii) + xin(2, ii) + xin(3, ii))
  ans(2, ii) = math%half * (math%one + xin(1, ii))
  ans(3, ii) = math%half * (math%one + xin(2, ii))
  ans(4, ii) = math%half * (math%one + xin(3, ii))
END DO

END PROCEDURE BarycentricCoordBiUnitTetrahedron_

!----------------------------------------------------------------------------
!                                                BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL BarycentricCoordTetrahedron_(xin=xin, refTetrahedron=refTetrahedron, &
                                  ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE BarycentricCoordTetrahedron

!----------------------------------------------------------------------------
!                                                BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordTetrahedron_
SELECT CASE (refTetrahedron(1:1))
CASE ("B", "b")
  CALL BarycentricCoordBiUnitTetrahedron_(xin=xin, ans=ans, nrow=nrow, &
                                          ncol=ncol)
CASE ("U", "u")
  CALL BarycentricCoordUnitTetrahedron_(xin=xin, ans=ans, nrow=nrow, &
                                        ncol=ncol)

CASE DEFAULT
END SELECT
END PROCEDURE BarycentricCoordTetrahedron_

!----------------------------------------------------------------------------
!                                     FromBiUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTetrahedron2BiUnitHexahedron
INTEGER(I4B) :: ii
REAL(DFP) :: tol, alpha, beta

tol = 1.0E-12_DFP

DO ii = 1, SIZE(xin, 2)
  alpha = xin(2, ii) + xin(3, ii)
  beta = math%one - xin(3, ii)

  IF (SOFTLE(ABS(alpha), math%zero, tol)) THEN
    ans(1, ii) = -math%one
  ELSE
    ans(1, ii) = -(math%two + math%two * xin(1, ii) + alpha) / alpha
  END IF

  IF (SOFTLE(ABS(beta), math%zero, tol)) THEN
    ans(2, ii) = -math%one
  ELSE
    ans(2, ii) = (math%one + math%two * xin(2, ii) + xin(3, ii)) / beta
  END IF

  ans(3, ii) = xin(3, ii)
END DO

END PROCEDURE FromBiUnitTetrahedron2BiUnitHexahedron

!----------------------------------------------------------------------------
!                                     FromBiUnitHexahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitHexahedron2BiUnitTetrahedron_(xin, ans, nrow, ncol)
END PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron_
INTEGER(I4B), PARAMETER :: ten = 10
INTEGER(I4B) :: ii
REAL(DFP) :: rr(ten)
REAL(DFP), PARAMETER :: one = math%one

nrow = 3
ncol = SIZE(xin, 2)

DO ii = 1, ncol

  rr(1:3) = xin(1:3, ii)

  rr(4) = one + rr(1)
  rr(5) = one - rr(2)
  rr(6) = one - rr(3)
  rr(7) = 0.25_DFP * rr(4) * rr(5) * rr(6)
  rr(8) = one + rr(2)
  rr(9) = math%half * rr(8) * rr(6)

  ans(1, ii) = rr(7) - one
  ans(2, ii) = rr(9) - one
  ans(3, ii) = rr(3)

END DO

END PROCEDURE FromBiUnitHexahedron2BiUnitTetrahedron_

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
INTEGER(I4B) :: nrow, ncol
CALL FromBiUnitHexahedron2UnitTetrahedron_(xin, ans, nrow, ncol)
END PROCEDURE FromBiUnitHexahedron2UnitTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitHexahedron2UnitTetrahedron_

CALL FromBiUnitHexahedron2BiUnitTetrahedron_(xin=xin, ans=ans, &
                                             nrow=nrow, ncol=ncol)

CALL FromBiUnitTetrahedron2UnitTetrahedron_(xin=ans, ans=ans, nrow=nrow, &
                                            ncol=ncol)

END PROCEDURE FromBiUnitHexahedron2UnitTetrahedron_

!----------------------------------------------------------------------------
!                                                             JacobianLine
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianLine
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%one
  CASE ("UNIT")
    ans = math%half
  CASE ("LINE")
    ans = NORM2(xij(:, 2) - xij(:, 1)) / math%two
  CASE DEFAULT
  END SELECT
CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%two
  CASE ("UNIT")
    ans = math%one
  CASE ("LINE")
    ans = NORM2(xij(:, 2) - xij(:, 1))
  CASE DEFAULT
  END SELECT
CASE ("LINE")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%two / NORM2(xij(:, 2) - xij(:, 1))
  CASE ("UNIT")
    ans = math%one / NORM2(xij(:, 2) - xij(:, 1))
  CASE ("LINE")
    ans = math%one
  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE JacobianLine

!----------------------------------------------------------------------------
!                                                         JacobianTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianTriangle
ans = math%one
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%one
  CASE ("UNIT")
    ans = 0.25_DFP
  CASE ("TRIANGLE")
    IF (PRESENT(xij)) THEN

      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL TriangleArea2D(xij(1:2, 1:3), ans)
      ELSE
        CALL TriangleArea3D(xij(1:3, 1:3), ans)
      END IF

      ans = ans / math%two

    END IF
  CASE DEFAULT
  END SELECT
CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 4.0_DFP
  CASE ("UNIT")
    ans = math%one

  CASE ("TRIANGLE")
    IF (PRESENT(xij)) THEN
      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL TriangleArea2D(xij(1:2, 1:3), ans)
      ELSE
        CALL TriangleArea3D(xij(1:3, 1:3), ans)
      END IF
      ans = ans / math%half
    END IF
  CASE DEFAULT
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
    ans = math%two / ans
  CASE ("UNIT")
    ans = math%half / ans
  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE JacobianTriangle

!----------------------------------------------------------------------------
!                                                         JacobianQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianQuadrangle
ans = math%one
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%one
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
  CASE DEFAULT
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 4.0_DFP
  CASE ("UNIT")
    ans = math%one

  CASE ("QUADRANGLE")
    IF (PRESENT(xij)) THEN
      IF (SIZE(xij, 1) .EQ. 2_I4B) THEN
        CALL QuadrangleArea2D(xij(1:2, 1:4), ans)
      ELSE
        CALL QuadrangleArea3D(xij(1:3, 1:4), ans)
      END IF
    END IF
  CASE DEFAULT
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
    ans = math%one / ans
  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE JacobianQuadrangle

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianHexahedron
REAL(DFP) :: ans0
ans = math%one
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%one
  CASE ("UNIT")
    ans = 0.125_DFP
  CASE ("HEXAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL HexahedronVolume3D(xij(1:3, 1:8), ans)
      CALL HexahedronVolume3D(RefCoord_Hexahedron(from), ans0)
      ans = ans / ans0
    END IF
  CASE DEFAULT
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 8.0_DFP
  CASE ("UNIT")
    ans = math%one
  CASE ("HEXAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL HexahedronVolume3D(xij(1:3, 1:8), ans)
      CALL HexahedronVolume3D(RefCoord_Hexahedron(from), ans0)
      ans = ans / ans0
    END IF
  CASE DEFAULT
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
  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE JacobianHexahedron

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobianTetrahedron
REAL(DFP) :: ans0
ans = math%one
SELECT CASE (TRIM(from))
CASE ("BIUNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = math%one
  CASE ("UNIT")
    ans = 0.125_DFP
  CASE ("TETRAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL TetrahedronVolume3D(xij(1:3, 1:4), ans)
      CALL TetrahedronVolume3D(RefCoord_Tetrahedron(from), ans0)
      ans = ans / ans0
    END IF
  CASE DEFAULT
  END SELECT

CASE ("UNIT")
  SELECT CASE (TRIM(to))
  CASE ("BIUNIT")
    ans = 8.0_DFP
  CASE ("UNIT")
    ans = math%one
  CASE ("TETRAHEDRON")
    IF (PRESENT(xij)) THEN
      CALL TetrahedronVolume3D(xij(1:3, 1:4), ans)
      CALL TetrahedronVolume3D(RefCoord_Tetrahedron(from), ans0)
      ans = ans / ans0
    END IF
  CASE DEFAULT
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
  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE JacobianTetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Methods
