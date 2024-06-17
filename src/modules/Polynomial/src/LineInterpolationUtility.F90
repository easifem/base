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

MODULE LineInterpolationUtility
USE GlobalData
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDegree_Line
PUBLIC :: LagrangeDOF_Point
PUBLIC :: LagrangeDOF_Line
PUBLIC :: LagrangeInDOF_Line
PUBLIC :: GetTotalDOF_Line
PUBLIC :: GetTotalInDOF_Line
PUBLIC :: EquidistanceInPoint_Line
PUBLIC :: EquidistancePoint_Line
PUBLIC :: InterpolationPoint_Line
PUBLIC :: LagrangeCoeff_Line
PUBLIC :: LagrangeEvalAll_Line
PUBLIC :: LagrangeGradientEvalAll_Line
PUBLIC :: BasisEvalAll_Line
PUBLIC :: BasisGradientEvalAll_Line
PUBLIC :: QuadraturePoint_Line
PUBLIC :: ToVEFC_Line
PUBLIC :: QuadratureNumber_Line
PUBLIC :: RefElemDomain_Line
PUBLIC :: HeirarchicalBasis_Line
PUBLIC :: HeirarchicalGradientBasis_Line
PUBLIC :: OrthogonalBasis_Line
PUBLIC :: OrthogonalBasisGradient_Line

!----------------------------------------------------------------------------
!                                                       RefElemDomain_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain_Line(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      QuadratureNumber_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  REturns the number of quadrature points necessary for given order

INTERFACE
  MODULE PURE FUNCTION QuadratureNumber_Line(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: quadType
    INTEGER(I4B) :: ans
  END FUNCTION QuadratureNumber_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                            ToVEFC_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  Change layour of points on line

INTERFACE
  MODULE PURE SUBROUTINE ToVEFC_Line(pt)
    REAL(DFP), INTENT(INOUT) :: pt(:)
  END SUBROUTINE ToVEFC_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDegree_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary: Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                          LagrangeDOF_Point
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on a point of Line

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Point(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Point
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on Line

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!- These dof are strictly inside the line

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Line(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetTotalDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on Line

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Line(order, baseContinuity, &
                                        baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                        LagrangeInDOF_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial on an edge of a Line
!- These dof are strictly inside the line

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Line(order, baseContinuity, &
                                          baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    INTEGER(I4B) :: ans
  END FUNCTION GetTotalInDOF_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance internal points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge in 1D
!- All points are inside the interval
!- Points are in increasing order

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coordinates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION EquidistanceInPoint_Line1
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge in 1D, 2D, 3D
!- The end points are specified by `xij(1:nsd, 1)` and `xij(1:nsd, 2)`
!
!- All points are inside the interval
!- The number of space components in `ans` is nsd if xij present
!- Otherwise, the number of space components in `ans` is 1.

INTERFACE EquidistanceInPoint_Line
  MODULE PURE FUNCTION EquidistanceInPoint_Line2(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! Equidistnace points in $x_{iJ}$ format
  !! The number of rows is equal to the number of rows in xij
  !! (if xij present), otherwise, it is 1.
  END FUNCTION EquidistanceInPoint_Line2
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- Points are in "VEFC" format, which means `xij(1,1:2)` are end points

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coorindates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! equidistance points
  END FUNCTION EquidistancePoint_Line1
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                    EquidistancePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points on line
!
!# Introduction
!
!- This function returns the equidistance points on line
!- All points are inside the interval

INTERFACE EquidistancePoint_Line
  MODULE PURE FUNCTION EquidistancePoint_Line2(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coordinates of point 1 and point 2 in $x_{iJ}$ format
    !! number of rows = nsd
    !! number of cols = 2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! equidistance points in $x_{iJ}$ format
    !! If xij is not present, then number of rows in ans
    !! is 1. If `xij` is present then the number of rows in
    !! ans is  same as xij.
  END FUNCTION EquidistancePoint_Line2
END INTERFACE EquidistancePoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Returns the interpolation point
!
!# Introduction
!
!- This routine returns the interplation points on line
!- `xij` contains nodal coordinates of line in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=2
!- If xij is absent then [-1,1] is used
!- `ipType` is interpolation point type, it can take following values
!-  `Equidistance`, uniformly/evenly distributed points
!-  `GaussLegendre`, Zeros of Legendre polynomials, all nodes are strictly
! inside the domain.
!- `GaussLegendreLobatto` or `GaussLobatto` are zeros of Lobatto polynomials
! they always contains boundary points
!- `GaussChebyshev` Zeros of Chebyshev polynomials of first kind, all
! nodes are internal
!- `GaussChebyshevLobatto` they contains boundary points
!- `GaussJacobi` and `GaussJacobiLobatto`
!
!- `layout` specifies the arrangement of points. Following options are
! possible:
!
!- `layout=VEFC` vertex, edge, face, cell, in this case first two points are
! boundary points, remaining (from 3 to n) are internal points in
! increasing order.
!
!- `layout=INCREASING` points are arranged in increasing order

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line1(order, ipType, &
    & layout, xij, alpha, beta, lambda) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! size(ans,1) = 1
    !! size(ans,2) = order+1
  END FUNCTION InterpolationPoint_Line1
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Returns the interpolation point

INTERFACE InterpolationPoint_Line
  MODULE FUNCTION InterpolationPoint_Line2(order, ipType, xij, &
    & layout, alpha, beta, lambda) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    !! "DECREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! one dimensional interpolation point
  END FUNCTION InterpolationPoint_Line2
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line1
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line2(order, i, v, isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line2
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(order + 1)
    !! coefficients
  END FUNCTION LagrangeCoeff_Line3
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    REAL(DFP) :: ans(order + 1, order + 1)
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
  END FUNCTION LagrangeCoeff_Line4
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                       LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE LagrangeCoeff_Line
  MODULE FUNCTION LagrangeCoeff_Line5(order, xij, basisType, alpha, &
    & beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(xij,2)-1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2) = order+1
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
    !! jth column of ans corresponds to the coeff of lagrange polynomial
    !! at the jth point
  END FUNCTION LagrangeCoeff_Line5
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials of order n at single points

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line1(order, x, xij, coeff, firstCall, &
    & basisType, alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Line1
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials of n at several points

INTERFACE LagrangeEvalAll_Line
  MODULE FUNCTION LagrangeEvalAll_Line2( &
    & order, x, xij, coeff, firstCall, &
    & basisType, alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    !! size(xij, 1) = nsd
    !! size(xij, 2) = number of points
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION LagrangeEvalAll_Line2
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                               LagrangeGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials of n at several points

INTERFACE LagrangeGradientEvalAll_Line
  MODULE FUNCTION LagrangeGradientEvalAll_Line1( &
    & order, &
    & x, &
    & xij, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! point of evaluation in xij format
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation points
    !! xij should be present when firstCall is true.
    !! It is used for computing the coeff
    !! If coeff is absent then xij should be present
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 1)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Line1
END INTERFACE LagrangeGradientEvalAll_Line

!----------------------------------------------------------------------------
!                                                          BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate basis functions of order upto n

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line1( &
    & order, &
    & x, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1  polynomials at point x
  END FUNCTION BasisEvalAll_Line1
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate basis functions of order upto n

INTERFACE BasisEvalAll_Line
  MODULE FUNCTION BasisEvalAll_Line2( &
    & order, &
    & x, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT
    !! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION BasisEvalAll_Line2
END INTERFACE BasisEvalAll_Line

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate basis functions of order upto n

INTERFACE OrthogonalBasis_Line
  MODULE FUNCTION OrthogonalBasis_Line1( &
    & order, &
    & xij, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT
    !! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION OrthogonalBasis_Line1
END INTERFACE OrthogonalBasis_Line

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate basis functions of order upto n

INTERFACE OrthogonalBasisGradient_Line
  MODULE FUNCTION OrthogonalBasisGradient_Line1( &
    & order, &
    & xij, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    !! Number of rows in xij is 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT
    !! BIUNIT
    INTEGER(I4B), INTENT(IN) :: basisType
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION OrthogonalBasisGradient_Line1
END INTERFACE OrthogonalBasisGradient_Line

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Line

INTERFACE HeirarchicalBasis_Line
  MODULE FUNCTION HeirarchicalBasis_Line1(order, xij, refLine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1)
    !! Hierarchical basis
  END FUNCTION HeirarchicalBasis_Line1
END INTERFACE HeirarchicalBasis_Line

!----------------------------------------------------------------------------
!                                              HeirarchicalBasisGradient_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Eval gradient of all modal basis (heirarchical polynomial) on Line

INTERFACE HeirarchicalGradientBasis_Line
  MODULE FUNCTION HeirarchicalGradientBasis_Line1( &
    & order, &
    & xij, &
    & refLine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Polynomial order of interpolation
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Points of evaluation in xij format
    !! size(xij, 1) should be 1
    CHARACTER(*), INTENT(IN) :: refLine
    !! This parameter denotes the type of reference line.
    !! It can take following values:
    !! UNIT: in this case xij is in unit Line.
    !! BIUNIT: in this case xij is in biunit Line.
    REAL(DFP) :: ans(SIZE(xij, 2), order + 1, 1)
    !! Gradient of Hierarchical basis
  END FUNCTION HeirarchicalGradientBasis_Line1
END INTERFACE HeirarchicalGradientBasis_Line

!----------------------------------------------------------------------------
!                                                 BasisGradientEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate the gradient of basis functions of order upto n

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line1( &
    & order, &
    & x, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! Refline should be  BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1  polynomials at point x
  END FUNCTION BasisGradientEvalAll_Line1
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!                                                         BasisEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate gradient of basis functions of order upto n

INTERFACE BasisGradientEvalAll_Line
  MODULE FUNCTION BasisGradientEvalAll_Line2( &
    & order, &
    & x, &
    & refLine, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of  polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refLine
    !! UNIT
    !! BIUNIT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomial
    !! Jacobi
    !! Ultraspherical
    !! Legendre
    !! Chebyshev
    !! Lobatto
    !! UnscaledLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1  polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION BasisGradientEvalAll_Line2
END INTERFACE BasisGradientEvalAll_Line

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line1( &
    & order, &
    & quadType, &
    & layout, &
    & xij, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance,
    !! GaussLegendre,
    !! GaussLegendreLobatto,
    !! GaussChebyshev,
    !! GaussChebyshevLobatto,
    !! GaussJacobi,
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points
    !! If xij is present then the number of rows in ans
    !! is same as size(xij,1) + 1.
    !! If xij is not present then the number of rows in
    !! ans is 2
    !! The last row of ans contains the weights
    !! The first few rows contains the quadrature points
  END FUNCTION QuadraturePoint_Line1
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Returns the interpolation point

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line2( &
    & order, &
    & quadType, &
    & xij, &
    & layout, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! one dimensional interpolation point
  END FUNCTION QuadraturePoint_Line2
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                      QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-19
! summary:  Returns quadrature points

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line3( &
    & nips, &
    & quadType, &
    & layout, &
    & xij, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance,
    !! GaussLegendre,
    !! GaussLegendreLobatto,
    !! GaussChebyshev,
    !! GaussChebyshevLobatto,
    !! GaussJacobi,
    !! GaussJacobiLobatto
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points
    !! If xij is present then the number of rows in ans
    !! is same as size(xij,1) + 1.
    !! If xij is not present then the number of rows in
    !! ans is 2
    !! The last row of ans contains the weights
    !! The first few rows contains the quadrature points
  END FUNCTION QuadraturePoint_Line3
END INTERFACE QuadraturePoint_Line

!----------------------------------------------------------------------------
!                                                   QuadraturePoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Returns the interpolation point

INTERFACE QuadraturePoint_Line
  MODULE FUNCTION QuadraturePoint_Line4( &
    & nips, &
    & quadType, &
    & xij, &
    & layout, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: quadType
    !! Quadrature point type
    !! Equidistance
    !! GaussLegendre
    !! GaussLegendreLobatto
    !! GaussChebyshev,
    !! GaussChebyshevLobatto
    !! GaussJacobi
    !! GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! one dimensional interpolation point
  END FUNCTION QuadraturePoint_Line4
END INTERFACE QuadraturePoint_Line

END MODULE LineInterpolationUtility
