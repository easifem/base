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
!{!pages/LagrangePolynomialUtility.md!}

MODULE LagrangePolynomialUtility
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDOF
PUBLIC :: LagrangeInDOF
PUBLIC :: LagrangeDegree
PUBLIC :: LagrangeVandermonde
PUBLIC :: LagrangeVandermonde_
PUBLIC :: EquidistancePoint
PUBLIC :: InterpolationPoint
PUBLIC :: LagrangeCoeff
PUBLIC :: RefCoord
PUBLIC :: RefElemDomain
PUBLIC :: LagrangeEvalAll
PUBLIC :: LagrangeGradientEvalAll

!----------------------------------------------------------------------------
!                                                           RefElemDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain(elemType, baseContinuity, baseInterpol) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 RefCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE PURE FUNCTION RefCoord(elemType, refElem) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    CHARACTER(*), INTENT(IN) :: refElem
    !! "UNIT"
    !! "BIUNIT"
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  END FUNCTION RefCoord
END INTERFACE

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
    RESULT(ans)
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
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the Vandermonde matrix

INTERFACE
  MODULE PURE SUBROUTINE LagrangeVandermonde_(xij, order, elemType, ans, &
                                              nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeVandermonde_
END INTERFACE

!----------------------------------------------------------------------------
!                                                          EquidistancePoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary: Equidistance points on 1D/2D/3D elements

INTERFACE
  MODULE FUNCTION EquidistancePoint(order, elemType, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of element
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of linear elements
    !! Default values:
    !! Biunit line
    !! Unit triangle
    !! Biunit Quadrangle
    !! Unit Tetrahedron
    !! Biunit Hexahedron
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Equidistance points in xij format
    !! Number of rows = nsd
    !! Number of columns = Number of points
    !! The number of points depend upon the order and elemType
  END FUNCTION EquidistancePoint
END INTERFACE

!----------------------------------------------------------------------------
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Get the interpolation point

INTERFACE
  MODULE FUNCTION InterpolationPoint(order, elemType, ipType, xij, layout, &
                                     alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, following values are allowed.
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto,
    !! GaussUltraspherical, GaussUltrasphericalLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of linear elements.
    !! Domain of interpolation, default values are given by:
    !! Biunit line
    !! Unit triangle
    !! Biunit Quadrangle
    !! Unit Tetrahedron
    !! Biunit Hexahedron
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" Vertex, Edge, Face, Cell
    !! "INCREASING" incresing order
    !! "DECREASING" decreasing order
    !! "XYZ" First X, then Y, then Z
    !! "YXZ" First Y, then X, then Z
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi and Ultraspherical parameters
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

INTERFACE LagrangeCoeff
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
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary: Returns the coefficient of all lagrange poly

INTERFACE LagrangeCoeff
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
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                             LagrangeCoeff
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff3(order, elemType, i, v, &
                                 isVandermonde) RESULT(ans)
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
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                              LagrangeCoeff
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff
  MODULE FUNCTION LagrangeCoeff4(order, elemType, i, v, ipiv) RESULT(ans)
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
END INTERFACE LagrangeCoeff

!----------------------------------------------------------------------------
!                                                           LagrangeEvalAll
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll
  MODULE FUNCTION LagrangeEvalAll1(order, elemType, x, xij, domainName, &
                 coeff, firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll1
END INTERFACE LagrangeEvalAll

!----------------------------------------------------------------------------
!                                                           LagrangeEvalAll
!----------------------------------------------------------------------------

INTERFACE LagrangeGradientEvalAll
  MODULE FUNCTION LagrangeGradientEvalAll1(order, elemType, x, xij, &
     domainName, coeff, firstCall, basisType, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), SIZE(x, 1))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeGradientEvalAll1
END INTERFACE LagrangeGradientEvalAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangePolynomialUtility
