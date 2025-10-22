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

PUBLIC :: EquidistancePoint
PUBLIC :: EquidistancePoint_

PUBLIC :: LagrangeVandermonde
PUBLIC :: LagrangeVandermonde_

PUBLIC :: InterpolationPoint
PUBLIC :: InterpolationPoint_

PUBLIC :: LagrangeCoeff
PUBLIC :: LagrangeCoeff_

PUBLIC :: LagrangeEvalAll
PUBLIC :: LagrangeEvalAll_

PUBLIC :: LagrangeGradientEvalAll
PUBLIC :: LagrangeGradientEvalAll_

!----------------------------------------------------------------------------
!                                                   LagrangeDOF@BasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the number of dof for lagrange polynomial

INTERFACE LagrangeDOF
  MODULE PURE FUNCTION LagrangeDOF1(order, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeDOF1
END INTERFACE LagrangeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-11
! summary:  Get lagrange degree of freedom

INTERFACE LagrangeDOF
  MODULE PURE FUNCTION LagrangeDOF2(p, q, r, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p, q, r
    !! order in x, y, and z direction
    INTEGER(I4B), INTENT(IN) :: elemType
    !! for line, triangle, tetrahedron, prism , and pyramid only p is used
    !! for quadrangle and hexahedron, pq are used and pqr are used
    INTEGER(I4B) :: ans
    !! number of degree of freedom
  END FUNCTION LagrangeDOF2
END INTERFACE LagrangeDOF

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

INTERFACE LagrangeVandermonde_
  MODULE PURE SUBROUTINE LagrangeVandermonde1_(xij, order, elemType, ans, &
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
  END SUBROUTINE LagrangeVandermonde1_
END INTERFACE LagrangeVandermonde_

!----------------------------------------------------------------------------
!                                                       LagrangeVandermonde
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the Vandermonde matrix

INTERFACE LagrangeVandermonde_
  MODULE PURE SUBROUTINE LagrangeVandermonde2_(xij, degree, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !!  points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! degree of monomials
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! vandermonde matrix
    !! nrows := number of points
    !! ncols := number of dof
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xij, 2)
    !! ncol = SIZE(degree, 1)
  END SUBROUTINE LagrangeVandermonde2_
END INTERFACE LagrangeVandermonde_

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
    !! Biunit line ! Unit triangle ! Biunit Quadrangle ! Unit Tetrahedron
    !! Biunit Hexahedron
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Equidistance points in xij format
    !! Number of rows = nsd
    !! Number of columns = Number of points
    !! The number of points depend upon the order and elemType
  END FUNCTION EquidistancePoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE EquidistancePoint_(order, elemType, ans, nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of element
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Element type
    !! Point, Line, Triangle, Quadrangle, Tetrahedron
    !! Hexahedron, Prism, Pyramid
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Equidistance points in xij format
    !! Number of rows = nsd
    !! Number of columns = Number of points
    !! The number of points depend upon the order and elemType
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns in ans
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of linear elements
    !! Default values:
    !! Biunit line ! Unit triangle ! Biunit Quadrangle ! Unit Tetrahedron
    !! Biunit Hexahedron
  END SUBROUTINE EquidistancePoint_
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
!                                                         InterpolationPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Aug 2022
! summary: Get the interpolation point

INTERFACE
 MODULE SUBROUTINE InterpolationPoint_(order, elemType, ipType, xij, layout, &
                                        alpha, beta, lambda, ans, nrow, ncol)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points in xij format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! the number of rows and cols written in ans
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC" Vertex, Edge, Face, Cell
    !! "INCREASING" incresing order
    !! "DECREASING" decreasing order
    !! "XYZ" First X, then Y, then Z
    !! "YXZ" First Y, then X, then Z
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Nodal coordinates of linear elements.
    !! Domain of interpolation, default values are given by:
    !! Biunit line
    !! Unit triangle
    !! Biunit Quadrangle
    !! Unit Tetrahedron
    !! Biunit Hexahedron
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi and Ultraspherical parameters
  END SUBROUTINE InterpolationPoint_
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
!                                                           LagrangeCoeff_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:  Returns the coefficient of ith lagrange poly

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff1_(order, elemType, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff1_
END INTERFACE LagrangeCoeff_

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
!                                                           LagrangeCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary: Returns the coefficient of all lagrange poly

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff2_(order, elemType, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE LagrangeCoeff2_
END INTERFACE LagrangeCoeff_

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
!
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff3_(order, elemType, i, v, &
                                    isVandermonde, ans, tsize)
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff3_
END INTERFACE LagrangeCoeff_

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
!                                                              LagrangeCoeff
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_
  MODULE SUBROUTINE LagrangeCoeff4_(order, elemType, i, v, ipiv, ans, tsize)
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
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! ans(SIZE(v, 1))
    !! coefficients
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE LagrangeCoeff4_
END INTERFACE LagrangeCoeff_

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
!
!----------------------------------------------------------------------------

INTERFACE LagrangeEvalAll_
  MODULE SUBROUTINE LagrangeEvalAll1_(order, elemType, x, xij, ans, &
     nrow, ncol, domainName, coeff, firstCall, basisType, alpha, beta, lambda)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Value of n+1 Lagrange polynomials at point x
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x, 2)
    !! ncol = SIZE(xij, 2)
    CHARACTER(*), INTENT(IN) :: domainName
    !! domain of reference element
    !! UNIT
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
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
  END SUBROUTINE LagrangeEvalAll1_
END INTERFACE LagrangeEvalAll_

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

INTERFACE LagrangeGradientEvalAll_
  MODULE SUBROUTINE LagrangeGradientEvalAll1_(order, elemType, x, xij, ans, &
           dim1, dim2, dim3, domainName, coeff, firstCall, basisType, alpha, &
                                              beta, lambda)
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
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! Value of n+1 Lagrange polynomials at point x
    !! dim1 =  SIZE(x, 2)
    !! dim2 = SIZE(xij, 2)
    !! dim3 = SIZE(x, 1)
    !! ans(:, :, 1) denotes x gradient
    !! ans(:,:, 2) denotes y gradient
    !! ans(:,:, 3) denotes z gradient
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! data written in ans
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
  END SUBROUTINE LagrangeGradientEvalAll1_
END INTERFACE LagrangeGradientEvalAll_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE LagrangePolynomialUtility
