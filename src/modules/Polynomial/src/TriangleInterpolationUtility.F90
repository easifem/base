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

MODULE TriangleInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Triangle
PUBLIC :: LagrangeDOF_Triangle
PUBLIC :: LagrangeInDOF_Triangle
PUBLIC :: EquidistanceInPoint_Triangle
PUBLIC :: EquidistancePoint_Triangle
PUBLIC :: InterpolationPoint_Triangle
PUBLIC :: LagrangeCoeff_Triangle
PUBLIC :: Dubiner_Triangle
PUBLIC :: BarycentricVertexBasis_Triangle
PUBLIC :: BarycentricEdgeBasis_Triangle
PUBLIC :: BarycentricHeirarchicalBasis_Triangle
PUBLIC :: VertexBasis_Triangle
PUBLIC :: EdgeBasis_Triangle
PUBLIC :: CellBasis_Triangle
PUBLIC :: HeirarchicalBasis_Triangle
PUBLIC :: RefTriangleCoord
PUBLIC :: LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!                                                           RefTriangleCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference triangle

INTERFACE
  MODULE PURE FUNCTION RefTriangleCoord(refTriangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTriangle
    REAL(DFP) :: ans(2, 3)
  END FUNCTION RefTriangleCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns the total number of degree of freedom for a
! lagrange polynomial on triangle

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of triangle
!- These dof are strictly inside the triangle

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary: Returns equidistance points in triangle
!
!# Introduction
!
!- This function returns the equidistance points in triangle
!- All points are inside the triangle

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Triangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order triangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! triangle element, the layout is always "VEFC"
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention, VEFC.

INTERFACE
  MODULE RECURSIVE PURE FUNCTION EquidistancePoint_Triangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary:         Blyth Pozrikidis nodes on triangle

INTERFACE
  MODULE FUNCTION BlythPozrikidis_Triangle(order, ipType, layout, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! xij coordinates
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! only layout = "VEFC" is allowed
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION BlythPozrikidis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary:         Isaac points on triangle

INTERFACE
  MODULE FUNCTION Isaac_Triangle(order, ipType, layout, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! Equidistance, GaussLegendre, GaussLegendreLobatto, GaussChebyshev,
    !! GaussChebyshevLobatto, GaussJacobi, GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! xij coordinates
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! only layout = "VEFC" is allowed
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION Isaac_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary: Interpolation points on triangle
!
!# Introduction
!
!- This routine returns the interplation points on triangle.
!- `xij` contains nodal coordinates of triangle in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=3
!- If xij is absent then unit triangle is assumed
!- `ipType` is interpolation point type, it can take following values
!-  `Equidistance`, uniformly/evenly distributed points
!- `GaussLegendreLobatto ---> IsaacLegendre
!- `GaussChebyshevLobatto ---> IsaacChebyshev
!- `ChenBabuska`
!- `Hesthaven`
!- `Feket`
!- `BlythPozChebyshev`
!- `BlythPozLegendre`
!- `IsaacChebyshev`
!- `IsaacLegendre`
!
!- `layout` specifies the arrangement of points. The nodes are always
! returned in VEFC format (vertex, edge, face, cell). 1:3 are are
! vertex points, then edge, and then internal nodes. The internal nodes
! also follow the same convention. Please read Gmsh manual  on this topic.

INTERFACE
  MODULE FUNCTION InterpolationPoint_Triangle(order, ipType, &
    & layout, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! Coord of domain in xij format
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout, always VEFC
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! xij coordinates
  END FUNCTION InterpolationPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Triangle1
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle2(order, i, v, isVandermonde) &
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
  END FUNCTION LagrangeCoeff_Triangle2
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle3(order, i, v, ipiv) RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Triangle3
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Returns the coefficients for ith lagrange polynomial

INTERFACE LagrangeCoeff_Triangle
 MODULE FUNCTION LagrangeCoeff_Triangle4(order, xij, basisType, refTriangle) &
        & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials
    !! Jacobi (Dubiner)
    !! Heirarchical
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTriangle
    !! UNIT
    !! BIUNIT
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Triangle4
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit.
!
! The shape of `ans` is (M,N), where M=SIZE(xij,2) (number of points)
! N = 0.5*(order+1)*(order+2).
!
! In this way, ans(j,:) denotes the values of all polynomial at jth point
!
! Polynomials are returned in following way:
!
!$$
! P_{0,0}, P_{0,1}, \cdots , P_{0,order} \\
! P_{1,0}, P_{1,1}, \cdots , P_{1,order-1} \\
! P_{2,0}, P_{2,1}, \cdots , P_{2,order-2} \\
! \cdots
! P_{order,0}
!$$
!
! For example for order=3, the polynomials are arranged as:
!
!$$
! P_{0,0}, P_{0,1}, P_{0,2}, P_{0,3} \\
! P_{1,0}, P_{1,1}, P_{1,2} \\
! P_{2,0}, P_{2,1} \\
! P_{3,0}
!$$

INTERFACE
  MODULE PURE FUNCTION Dubiner_Triangle1(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in reference triangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "unit"
    !! "biunit"
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Triangle1
END INTERFACE

INTERFACE Dubiner_Triangle
  MODULE PROCEDURE Dubiner_Triangle1
END INTERFACE Dubiner_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on triangle
!
!# Introduction
!
! Forms Dubiner basis on reference triangle domain. Reference triangle
! can be biunit or unit. Here x and y are coordinate on line.
! xij is given by outerproduct of x and y.

INTERFACE
  MODULE PURE FUNCTION Dubiner_Triangle2(order, x, y, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! x and y coordinates, total points = SIZE(x)*SIZE(y)
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "unit"
    !! "biunit"
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Triangle2
END INTERFACE

INTERFACE Dubiner_Triangle
  MODULE PROCEDURE Dubiner_Triangle2
END INTERFACE Dubiner_Triangle

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on reference Triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasis_Triangle(lambda) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentrix coords
    !! number of rows = 3
    !! number of columns = number of points
    REAL(DFP) :: ans(SIZE(lambda, 2), 3)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION BarycentricVertexBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on edge of triangle
!
!# Introduction
!
! Evaluate basis functions on edges of triangle
! pe1, pe2, pe3 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Triangle(pe1, pe2, pe3, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge (e1)
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge (e2)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge (e3)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! Number of rows in lambda is equal to three.
    REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3)
  END FUNCTION BarycentricEdgeBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Evaluate the edge basis on triangle using barycentric coordinate

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Triangle2(pe1, pe2, pe3, &
    & lambda, phi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge (e1)
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge (e2)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge (e3)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! size(lambda,1) = 3
    !! size(lambda,2) = number of points of evaluation
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3)
  END FUNCTION BarycentricEdgeBasis_Triangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                             BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of reference triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasis_Triangle2(order, lambda, phi) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
    REAL(DFP) :: ans(SIZE(lambda, 2), INT((order - 1) * (order - 2) / 2))
  END FUNCTION BarycentricCellBasis_Triangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                      BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Triangle1(order, &
    & pe1, pe2, pe3, lambda, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge e1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge e2
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 3
    !! number of cols = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, "BIUNIT", "UNIT"
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !!
  END FUNCTION BarycentricHeirarchicalBasis_Triangle1
END INTERFACE

INTERFACE BarycentricHeirarchicalBasis_Triangle
  MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle1
END INTERFACE BarycentricHeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                      BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Triangle2(order, lambda, &
    & refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation on triangle
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! Barycenteric coordinates
    !! number of rows = 3
    !! number of cols = number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, "BIUNIT", "UNIT"
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & INT((order + 1) * (order + 2) / 2))
    !!
  END FUNCTION BarycentricHeirarchicalBasis_Triangle2
END INTERFACE

INTERFACE BarycentricHeirarchicalBasis_Triangle
  MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle2
END INTERFACE BarycentricHeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit Triangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Triangle(xij, refTriangle) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTriangle
    REAL(DFP) :: ans(SIZE(xij, 2), 3)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on Triangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Triangle2(Lo1, Lo2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
    REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
    !! coordinates on biunit square
    REAL(DFP) :: ans(SIZE(Lo1, 1), 3)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Triangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        EdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit Triangle
!
! qe1 and qe2 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Triangle(pe1, pe2, pe3, xij, refTriangle) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on left vertical edge (e1), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on right vertical edge(e2), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on right vertical edge(e3), should be greater than 1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    REAL(DFP) :: ans(SIZE(xij, 2), pe1 + pe2 + pe3 - 3)
  END FUNCTION EdgeBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        EdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit Triangle
!
! qe1 and qe2 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Triangle2(pe1, pe2, pe3, L1, L2, Lo1, &
    & Lo2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on left vertical edge (e1), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on right vertical edge(e2), should be greater than 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on right vertical edge(e3), should be greater than 1
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
    !! L1 and L2 are jacobian polynomials
    REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
    !! coordinates on biunit square domain
    REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
    !! coordinates on biunit square domain
    REAL(DFP) :: ans(SIZE(L1, 1), pe1 + pe2 + pe3 - 3)
  END FUNCTION EdgeBasis_Triangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit Triangle

INTERFACE
  MODULE PURE FUNCTION CellBasis_Triangle(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the cell, order>2
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! point of evaluation
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    REAL(DFP) :: ans(SIZE(xij, 2), INT((order - 1) * (order - 2) / 2))
  END FUNCTION CellBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit Triangle
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit Triangle

INTERFACE
  MODULE PURE FUNCTION CellBasis_Triangle2(order, L1, eta_ij, &
    & Lo1, Lo2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the cell, order>2
    REAL(DFP), INTENT(IN) :: L1(1:, 0:)
    !! lobatto polynomials
    REAL(DFP), INTENT(IN) :: eta_ij(:, :)
    !! coordinates on biunit square
    REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
    !! coordinates on biunit square domain
    REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
    !! coordinates on biunit square domain
    REAL(DFP) :: ans(SIZE(L1, 1), INT((order - 1) * (order - 2) / 2))
  END FUNCTION CellBasis_Triangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on Triangle

INTERFACE HeirarchicalBasis_Triangle
  MODULE PURE FUNCTION HeirarchicalBasis_Triangle1(order, pe1, pe2, pe3,&
    & xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge e1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge e2
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !!
  END FUNCTION HeirarchicalBasis_Triangle1
END INTERFACE HeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomial of order n at single points

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle1(order, x, xij, refTriangle, &
    & coeff, firstCall, basisType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! x(1) is x coord
    !! x(2) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !!
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! interpolation points
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficient of Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! If firstCall is true, then coeff will be made
    !! If firstCall is False, then coeff will be used
    !! Default value of firstCall is True
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! Monomials *Default
    !! Jacobi=Dubiner
    !! Heirarchical
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Triangle1
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-04
! summary: Evaluate all Lagrange polynomials of order n at several points

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle2(order, x, xij, refTriangle, &
    & coeff, firstCall, basisType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! Order of Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! Point of evaluation
    !! x(1, :) is x coord
    !! x(2, :) is y coord
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! Interpolation points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Reference triangle
    !! Biunit
    !! Unit
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
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! Value of n+1 Lagrange polynomials at point x
  END FUNCTION LagrangeEvalAll_Triangle2
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!                                                                 Triangle
!----------------------------------------------------------------------------

END MODULE TriangleInterpolationUtility
