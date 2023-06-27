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

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Methods for Lagrange polynomials are defined
!
!{!pages/LagrangePolynomialUtility_.md!}

MODULE LagrangePolynomialUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDOF
PUBLIC :: LagrangeInDOF
PUBLIC :: LagrangeDegree
PUBLIC :: LagrangeVandermonde
PUBLIC :: EquidistancePoint
PUBLIC :: InterpolationPoint
PUBLIC :: LagrangeCoeff

!----------------------------------------------------------------------------
!                                                   LagrangeDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the number of dof for lagrange polynomial

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the number of internal dof for lagrange polynomial

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeInDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                           LagrangeDegree
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the degrees of monomials for lagrange polynomial

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type: Line, Triangle, Quadrangle, Tetrahedron, ...
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the Vandermonde matrix

INTERFACE
  MODULE PURE FUNCTION LagrangeVandermonde(xij, order, elemType) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
  END FUNCTION LagrangeVandermonde
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary: Equidistance points on 1D/2D/3D elements

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint(order, elemType, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: elemType
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION EquidistancePoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Get the interpolation point

INTERFACE
  MODULE FUNCTION InterpolationPoint(order, elemType, ipType, &
    & xij, layout) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation, default values are given by
    !! line = [-1,1]
    !! triangle = (0,0), (0,1), (1,0)
    !! quadrangle = [-1,1]x[-1,1]
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" Vertex, Edge, Face, Cell
    !! "INCREASING" incresing order
    !! "DECREASING" decreasing order
    !! "XYZ" First X, then Y, then Z
    !! "YXZ" First Y, then X, then Z
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
  END FUNCTION InterpolationPoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:  Returns the coefficient of ith lagrange poly

INTERFACE
  MODULE FUNCTION LagrangeCoeff1(order, elemType, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff1
END INTERFACE

INTERFACE LagrangeCoeff
  MODULE PROCEDURE LagrangeCoeff1
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary: Returns the coefficient of all lagrange poly

INTERFACE
  MODULE FUNCTION LagrangeCoeff2(order, elemType, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff2
END INTERFACE

INTERFACE LagrangeCoeff
  MODULE PROCEDURE LagrangeCoeff2
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff3(order, elemType, i, v, &
    & isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff3
END INTERFACE

INTERFACE LagrangeCoeff
  MODULE PROCEDURE LagrangeCoeff3
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                              LagrangeCoeff
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff4(order, elemType, i, v, ipiv) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff4
END INTERFACE

INTERFACE LagrangeCoeff
  MODULE PROCEDURE LagrangeCoeff4
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangePolynomialUtility
