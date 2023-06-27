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
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDegree_Line
PUBLIC :: LagrangeDOF_Point
PUBLIC :: LagrangeDOF_Line
PUBLIC :: LagrangeInDOF_Line
PUBLIC :: EquidistanceInPoint_Line
PUBLIC :: EquidistancePoint_Line
PUBLIC :: InterpolationPoint_Line
PUBLIC :: LagrangeCoeff_Line
PUBLIC :: LagrangeEvalAll_Line

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
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance internal points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- All points are inside the interval
!- Points are in increasing order

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Line1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), INTENT(IN) :: xij(2)
  !! coordinates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
  END FUNCTION EquidistanceInPoint_Line1
END INTERFACE

INTERFACE EquidistanceInPoint_Line
  MODULE PROCEDURE EquidistanceInPoint_Line1
END INTERFACE EquidistanceInPoint_Line

!----------------------------------------------------------------------------
!                                                   EquidistanceInPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points on edge
!
!# Introduction
!
!- This function returns the equidistance points on edge
!- All points are inside the interval

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Line2(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 2
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Line2
END INTERFACE

INTERFACE EquidistanceInPoint_Line
  MODULE PROCEDURE EquidistanceInPoint_Line2
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

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Line1(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    REAL(DFP), INTENT(IN) :: xij(2)
    !! coorindates of point 1 and point 2
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! equidistance points
  END FUNCTION EquidistancePoint_Line1
END INTERFACE

INTERFACE EquidistancePoint_Line
  MODULE PROCEDURE EquidistancePoint_Line1
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

INTERFACE
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
  END FUNCTION EquidistancePoint_Line2
END INTERFACE

INTERFACE EquidistancePoint_Line
  MODULE PROCEDURE EquidistancePoint_Line2
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

INTERFACE
  MODULE FUNCTION InterpolationPoint_Line1(order, ipType, &
    & layout, xij) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! domain of interpolation
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points in xij format
    !! size(ans,1) = 1
    !! size(ans,2) = order+1
  END FUNCTION InterpolationPoint_Line1
END INTERFACE

INTERFACE InterpolationPoint_Line
  MODULE PROCEDURE InterpolationPoint_Line1
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                   InterpolationPoint_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Returns the interpolation point

INTERFACE
  MODULE FUNCTION InterpolationPoint_Line2(order, ipType, xij, &
    & layout) RESULT(ans)
    !!
    INTEGER(I4B), INTENT(IN) :: order
    !! order of interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Interpolation point type
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), INTENT(IN) :: xij(2)
    !! end points
    CHARACTER(*), INTENT(IN) :: layout
    !! "VEFC"
    !! "INCREASING"
    !! "DECREASING"
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! one dimensional interpolation point
  END FUNCTION InterpolationPoint_Line2
END INTERFACE

INTERFACE InterpolationPoint_Line
  MODULE PROCEDURE InterpolationPoint_Line2
END INTERFACE InterpolationPoint_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

INTERFACE LagrangeCoeff_Line
  MODULE PROCEDURE LagrangeCoeff_Line1
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

INTERFACE LagrangeCoeff_Line
  MODULE PROCEDURE LagrangeCoeff_Line2
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

INTERFACE LagrangeCoeff_Line
  MODULE PROCEDURE LagrangeCoeff_Line3
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                         LagrangeCoeff_Line
!----------------------------------------------------------------------------

INTERFACE
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
END INTERFACE

INTERFACE LagrangeCoeff_Line
  MODULE PROCEDURE LagrangeCoeff_Line4
END INTERFACE LagrangeCoeff_Line

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials from 0 to n at single points
!
!# Introduction
!
! Evaluate Lagrangepolynomials at single point
!
!- Two indicate the first call to subroutine set `firstCall` to True.
!
!- If `firstCall` is True, then
!   - If `V` is present and `ipiv` is absent, then on return V contains
!     vandermonde matrix
!   - If `V` is present and `ipiv` is present, then on return V contains
!     LU decomposition of vandermonde matrix and `ipiv` contains
!     inverse map of pivoting.
!
!- If `firstCall` is FALSE, then
!   - If `V` is present and `ipiv` is absent, then V denotes vandermonde
!     matrix, which will be used in the computations.
!   - If `V` is present and `ipiv` is present, then V denotes the
!     LU decomposition of vandermonde matrix and `ipiv` denotes the
!     inverse map of pivoting. These information will be used.

INTERFACE
  MODULE FUNCTION LagrangeEvalAll_Line1(order, x, xij, coeff, firstCall) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x
    !! point of evaluation
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: xij(1, order + 1)
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(order + 1, order + 1)
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP) :: ans(order + 1)
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Line1
END INTERFACE

INTERFACE LagrangeEvalAll_Line
  MODULE PROCEDURE LagrangeEvalAll_Line1
END INTERFACE LagrangeEvalAll_Line

!----------------------------------------------------------------------------
!                                                       LagrangeEvalAll_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-06-23
! summary: Evaluate Lagrange polynomials from 0 to n at several points

INTERFACE
  MODULE FUNCTION LagrangeEvalAll_Line2(order, x, xij, coeff, firstCall) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:)
    !! point of evaluation
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: xij(1, order + 1)
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(order + 1, order + 1)
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    REAL(DFP) :: ans(SIZE(x), order + 1)
    !! Value of n+1 Lagrange polynomials at point x
    !! ans(:, j) is the value of jth polynomial at x points
    !! ans(i, :) is the value of all polynomials at x(i) point
  END FUNCTION LagrangeEvalAll_Line2
END INTERFACE

INTERFACE LagrangeEvalAll_Line
  MODULE PROCEDURE LagrangeEvalAll_Line2
END INTERFACE LagrangeEvalAll_Line

END MODULE LineInterpolationUtility
