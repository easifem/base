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
INTEGER(I4B) :: nrow, ncol
CALL Dubiner_Triangle1_(order=order, xij=xij, reftriangle=reftriangle, &
                        ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE Dubiner_Triangle1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle1_
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))

nrow = SIZE(xij, 2)
ncol = (order + 1) * (order + 2) / 2

SELECT CASE (reftriangle(1:1))
CASE ("B", "b")

  ! FIXME:
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)

CASE ("U", "u")

  ! FIXME:
  x = FromUnitTriangle2BiUnitSqr(xin=xij)

END SELECT

ans = Dubiner_Quadrangle(order=order, xij=x)
END PROCEDURE Dubiner_Triangle1_

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle2

END PROCEDURE Dubiner_Triangle2

!----------------------------------------------------------------------------
!                                                       Dubiner_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE Dubiner_Triangle2_
REAL(DFP) :: x0(SIZE(x)), y0(SIZE(y))

nrow = SIZE(x) * SIZE(y)
ncol = (order + 1) * (order + 2) / 2

SELECT CASE (reftriangle(1:1))

CASE ("B", "b")

  x0 = x
  y0 = y

CASE ("U", "u")

  ! FIXME:
  x0 = FromUnitLine2BiUnitLine(xin=x)

  ! FIXME:
  y0 = FromUnitLine2BiUnitLine(xin=y)

END SELECT

ans = Dubiner_Quadrangle(order=order, x=x0, y=y0)
END PROCEDURE Dubiner_Triangle2_

!----------------------------------------------------------------------------
!                                           OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Triangle1
INTEGER(I4B) :: s(3)
CALL OrthogonalBasisGradient_Triangle1_(order=order, xij=xij, &
      reftriangle=reftriangle, ans=ans, tsize1=s(1), tsize2=s(2), tsize3=s(3))
END PROCEDURE OrthogonalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                           OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE OrthogonalBasisGradient_Triangle1_
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
INTEGER(I4B) :: ii

tsize1 = SIZE(xij, 2)
tsize2 = (order + 1) * (order + 2) / 2
tsize3 = (order + 1) * (order + 2) / 2

SELECT CASE (reftriangle(1:1))

CASE ("B", "b")

  !FIXME:
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)

CASE ("U", "u")

  ! FIXME:
  x = FromUnitTriangle2BiUnitSqr(xin=xij)

END SELECT

! FIXME:
ans = DubinerGradient_Quadrangle(order=order, xij=x)

DO ii = 1, SIZE(ans, 2)
  ans(:, ii, 1) = ans(:, ii, 1) * 4.0_DFP / (1.0_DFP - x(2, :))
  ans(:, ii, 2) = ans(:, ii, 1) * (1.0_DFP + x(1, :)) * 0.5_DFP  &
    & + 2.0_DFP * ans(:, ii, 2)
END DO

END PROCEDURE OrthogonalBasisGradient_Triangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE OrthogonalBasisMethods
