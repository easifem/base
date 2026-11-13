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
! summary: Reexport polynomial and element interpolation utilities.
!
! This module also reexports element specific interpolation modules defined
! outside Polynomial. Prefer direct imports when only one family is needed.
!
! Arguments:
!   None at module scope; see the public interfaces.
!
! Returns:
!   Public polynomial and interpolation procedures.
!
! Formula:
! $$P_2(x)=(3x^2-1)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE PolynomialUtility, ONLY: LegendreEval
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: y
!
! y = LegendreEval(n=2_I4B, x=0.0_DFP)
! CALL OK(ABS(y+0.5_DFP) < 1.0E-12_DFP, "Umbrella reexports Legendre")
! END PROGRAM main
!```

MODULE PolynomialUtility
USE Chebyshev1PolynomialUtility
USE HexahedronInterpolationUtility
USE HierarchicalPolynomialUtility
USE InterpolationUtility
USE JacobiPolynomialUtility
USE LagrangePolynomialUtility
USE LegendrePolynomialUtility
USE LineInterpolationUtility
USE LobattoPolynomialUtility
USE OrthogonalPolynomialUtility
USE PrismInterpolationUtility
USE PyramidInterpolationUtility
USE QuadrangleInterpolationUtility
USE RecursiveNodesUtility
USE TetrahedronInterpolationUtility
USE TriangleInterpolationUtility
USE UltrasphericalPolynomialUtility
USE UnscaledLobattoPolynomialUtility
END MODULE PolynomialUtility
