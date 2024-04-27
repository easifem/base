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

SUBMODULE(TriangleInterpolationUtility) OrthogonalBasisMethods
USE MappingUtility, ONLY: FromBiunitTriangle2BiunitSqr,  &
  & FromUnitTriangle2BiunitSqr, FromUnitLine2BiUnitLine
USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle,  &
  & DubinerGradient_Quadrangle
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle1
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
SELECT CASE (refTriangle(1:1))
CASE ("B", "b")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("U", "u")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT
ans = Dubiner_Quadrangle(order=order, xij=x)
END PROCEDURE Dubiner_Triangle1

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle2
REAL(DFP) :: x0(SIZE(x)), y0(SIZE(y))
SELECT CASE (reftriangle(1:1))
CASE ("B", "b")
  x0 = x
  y0 = y
CASE ("U", "u")
  x0 = FromUnitLine2BiUnitLine(xin=x)
  y0 = FromUnitLine2BiUnitLine(xin=y)
END SELECT
ans = Dubiner_Quadrangle(order=order, x=x0, y=y0)
END PROCEDURE Dubiner_Triangle2

!----------------------------------------------------------------------------
!                                           OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Triangle1
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
INTEGER(I4B) :: ii

SELECT CASE (reftriangle(1:1))
CASE ("B", "b")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("U", "u")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT

ans = DubinerGradient_Quadrangle(order=order, xij=x)

DO ii = 1, SIZE(ans, 2)
  ans(:, ii, 1) = ans(:, ii, 1) * 4.0_DFP / (1.0_DFP - x(2, :))
  ans(:, ii, 2) = ans(:, ii, 1) * (1.0_DFP + x(1, :)) * 0.5_DFP  &
    & + 2.0_DFP * ans(:, ii, 2)
END DO
END PROCEDURE OrthogonalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE OrthogonalBasisMethods
