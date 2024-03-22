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

MODULE PyramidInterpolationUtility
USE GlobalData
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Pyramid
PUBLIC :: LagrangeDOF_Pyramid
PUBLIC :: LagrangeInDOF_Pyramid
PUBLIC :: EquidistanceInPoint_Pyramid
PUBLIC :: EquidistancePoint_Pyramid
PUBLIC :: InterpolationPoint_Pyramid
PUBLIC :: LagrangeCoeff_Pyramid
PUBLIC :: QuadraturePoint_Pyramid
PUBLIC :: TensorQuadraturePoint_Pyramid
PUBLIC :: RefElemDomain_Pyramid
PUBLIC :: LagrangeEvalAll_Pyramid
PUBLIC :: LagrangeGradientEvalAll_Pyramid
PUBLIC :: EdgeConnectivity_Pyramid
PUBLIC :: FacetConnectivity_Pyramid

INTEGER(I4B), PARAMETER :: CONST_tNODES = 5
INTEGER(I4B), PARAMETER :: CONST_tFACES = 5
INTEGER(I4B), PARAMETER :: CONST_tEDGES = 8
INTEGER(I4B), PARAMETER :: CONST_XIDIM = 3
INTEGER(I4B), PARAMETER :: CONST_MAX_NODES_FACE = 4
INTEGER(I4B), PARAMETER :: CONST_MIN_NODES_FACE = 3

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-07
! summary:  This function returns the edge connectivity of Pyramid

INTERFACE
  MODULE PURE FUNCTION EdgeConnectivity_Pyramid( &
    & baseInterpol,  &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2, CONST_tEDGES)
  END FUNCTION EdgeConnectivity_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-10
! summary:  This function returns the facet-connectivity of Pyramid

INTERFACE
  MODULE PURE FUNCTION FacetConnectivity_Pyramid( &
    & baseInterpol, &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    CHARACTER(*), INTENT(IN) :: baseContinuity
    INTEGER(I4B) :: ans(2 + CONST_MAX_NODES_FACE, CONST_tFACES)
    !! ans(1, iface) contains the total nodes in facet (iface)
    !! ans(2, iface) contains the integer name of facet element
    !! ans(3:2+ans(1,iface), iface ) contains the node numbers
  END FUNCTION FacetConnectivity_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                     RefElemDomain_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference element

INTERFACE
  MODULE FUNCTION RefElemDomain_Pyramid(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! Cointinuity (conformity) of basis functions
    !! "H1", "HDiv", "HCurl", "DG"
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! Basis function family for Interpolation
    !! Lagrange, Hierarchy, Serendipity, Hermit, Orthogonal
    TYPE(String) :: ans
  END FUNCTION RefElemDomain_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeDegree_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Pyramid(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Pyramid

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Pyramid(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Pyramid
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell of Pyramid
!- These dof are strictly inside the Pyramid

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Pyramid(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points in Pyramid
!
!# Introduction
!
!- This function returns the equidistance points in Pyramid
!- All points are inside the Pyramid

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Pyramid(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Pyramid element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Pyramid element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE
  MODULE PURE FUNCTION EquidistancePoint_Pyramid(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Interpolation point on Pyramid

INTERFACE
  MODULE PURE FUNCTION InterpolationPoint_Pyramid( &
    & order, &
    & ipType, &
    & layout, &
    & xij,  &
    & alpha, beta, lambda) RESULT(nodecoord)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation points
    CHARACTER(*), INTENT(IN) :: layout
    !! layout
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! coords of vertices in $x_{iJ}$ format
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Alpha, beta, and lambda
    REAL(DFP), ALLOCATABLE :: nodecoord(:, :)
    !! interpolation points in $x_{iJ}$ format
  END FUNCTION InterpolationPoint_Pyramid
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Pyramid1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Pyramid1
END INTERFACE

INTERFACE LagrangeCoeff_Pyramid
  MODULE PROCEDURE LagrangeCoeff_Pyramid1
END INTERFACE LagrangeCoeff_Pyramid

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Pyramid2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(v,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! coefficient for ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! vandermonde matrix size should be (order+1,order+1)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! This is just to resolve interface issue
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Pyramid2
END INTERFACE

INTERFACE LagrangeCoeff_Pyramid
  MODULE PROCEDURE LagrangeCoeff_Pyramid2
END INTERFACE LagrangeCoeff_Pyramid

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Pyramid3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial, it should be SIZE(x,2)-1
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Pyramid3
END INTERFACE

INTERFACE LagrangeCoeff_Pyramid
  MODULE PROCEDURE LagrangeCoeff_Pyramid3
END INTERFACE LagrangeCoeff_Pyramid

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Pyramid
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Pyramid4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Pyramid4
END INTERFACE

INTERFACE LagrangeCoeff_Pyramid
  MODULE PROCEDURE LagrangeCoeff_Pyramid4
END INTERFACE LagrangeCoeff_Pyramid

!----------------------------------------------------------------------------
!                                                    QuadraturePoints_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  quadrature points on Pyramid

INTERFACE QuadraturePoint_Pyramid
  MODULE FUNCTION QuadraturePoint_Pyramid1(&
    & order, &
    & quadType, &
    & refPyramid, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refPyramid
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be  3.
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Pyramid1
END INTERFACE QuadraturePoint_Pyramid

!----------------------------------------------------------------------------
!                                                     QuadraturePoints_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary:  quadrature points on Pyramid

INTERFACE QuadraturePoint_Pyramid
  MODULE FUNCTION QuadraturePoint_Pyramid2(&
    & nips, &
    & quadType, &
    & refPyramid, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! nips(1) .LE. 79, then we call
    !! economical quadrature rules.
    !! Otherwise, this routine will retport
    !! error
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type,
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refPyramid
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be 3
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION QuadraturePoint_Pyramid2
END INTERFACE QuadraturePoint_Pyramid

!----------------------------------------------------------------------------
!                                               TensorQuadraturePoints_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points on Pyramid

INTERFACE TensorQuadraturePoint_Pyramid
  MODULE FUNCTION TensorQuadraturePoint_Pyramid1(order, quadType, &
    & refPyramid, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refPyramid
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij can be 4.
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Pyramid1
END INTERFACE TensorQuadraturePoint_Pyramid

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor based quadrature points

INTERFACE TensorQuadraturePoint_Pyramid
  MODULE FUNCTION TensorQuadraturePoint_Pyramid2( &
    & nipsx, &
    & nipsy, &
    & nipsz, &
    & quadType, &
    & refPyramid, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type
    !! currently this variable is not used
    CHARACTER(*), INTENT(IN) :: refPyramid
    !! Reference triangle
    !! BIUNIT
    !! UNIT
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of triangle.
    !! The number of rows in xij should be 3
    !! The number of columns in xij should be 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! Quadrature points
  END FUNCTION TensorQuadraturePoint_Pyramid2
END INTERFACE TensorQuadraturePoint_Pyramid

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Pyramid
  MODULE FUNCTION LagrangeEvalAll_Pyramid1( &
    & order, &
    & x, &
    & xij, &
    & refPyramid, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(3)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    !! x(3) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    !! The number of rows in xij is 3
    !! The number of columns in xij should be equal to total
    !! degree of freedom
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refPyramid
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be computed and returned
    !! by this routine.
    !! If firstCall is False, then coeff should be given, which will be
    !! used.
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Pyramid1
END INTERFACE LagrangeEvalAll_Pyramid

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  Evaluate all Lagrange polynomials at several points

INTERFACE LagrangeEvalAll_Pyramid
  MODULE FUNCTION LagrangeEvalAll_Pyramid2( &
    & order, &
    & x, &
    & xij, &
    & refPyramid, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refPyramid
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Pyramid2
END INTERFACE LagrangeEvalAll_Pyramid

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Pyramid
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-23
! summary:  GradientEvaluate all Lagrange polynomials at several points

INTERFACE LagrangeGradientEvalAll_Pyramid
  MODULE FUNCTION LagrangeGradientEvalAll_Pyramid1( &
    & order, &
    & x, &
    & xij, &
    & refPyramid, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    !! x(3, :) is z coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refPyramid
    !! UNIT *default
    !! BIUNIT
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! Coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Legendre
    !! Lobatto
    !! Chebyshev
    !! Jacobi
    !! Ultraspherical
    !! Heirarchical
    !! Orthogonal
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 3)
    !! Value of gradient of nth order Lagrange polynomials at point x
    !! The first index denotes point of evaluation
    !! the second index denotes Lagrange polynomial number
    !! The third index denotes the spatial dimension in which gradient is
    !! computed
  END FUNCTION LagrangeGradientEvalAll_Pyramid1
END INTERFACE LagrangeGradientEvalAll_Pyramid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE PyramidInterpolationUtility
