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

MODULE QuadrangleInterpolationUtility
USE GlobalData
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Quadrangle
PUBLIC :: LagrangeDOF_Quadrangle
PUBLIC :: LagrangeInDOF_Quadrangle
PUBLIC :: EquidistanceInPoint_Quadrangle
PUBLIC :: EquidistancePoint_Quadrangle
PUBLIC :: InterpolationPoint_Quadrangle
PUBLIC :: LagrangeCoeff_Quadrangle
PUBLIC :: Dubiner_Quadrangle
PUBLIC :: TensorProdOrthopol_Quadrangle
PUBLIC :: VertexBasis_Quadrangle
PUBLIC :: VerticalEdgeBasis_Quadrangle
PUBLIC :: HorizontalEdgeBasis_Quadrangle
PUBLIC :: CellBasis_Quadrangle
PUBLIC :: HeirarchicalBasis_Quadrangle
PUBLIC :: RefQuadrangleCoord
PUBLIC :: IJ2VEFC_Quadrangle_Clockwise
PUBLIC :: IJ2VEFC_Quadrangle_AntiClockwise

!----------------------------------------------------------------------------
!                                                   RefQuadrangleCoord
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION RefQuadrangleCoord(refQuadrangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    REAL(DFP) :: ans(2, 4)
  END FUNCTION RefQuadrangleCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:         Returns the degree of monomials for Lagrange polynomials

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Quadrangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION LagrangeDegree_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial on Quadrangle

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Quadrangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!
!# Introduction
!
!- Returns the total number of degree of freedom for a
! lagrange polynomial in cell/face of Quadrangle
!- These dof are strictly inside the Quadrangle

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Quadrangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LagrangeInDOF_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 Aug 2022
! summary:         Returns equidistance points in Quadrangle
!
!# Introduction
!
!- This function returns the equidistance points in Quadrangle
!- All points are inside the Quadrangle

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Quadrangle(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 4
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistanceInPoint_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 12 Aug 2022
! summary: Returns the nodal coordinates of higher order Quadrangle element
!
!# Introduction
!
!- This function returns the nodal coordinates of higher order
! Quadrangle element
!- The coordinates are distributed uniformly
!- These coordinates can be used to construct lagrange polynomials
!- The returned coordinates are in $x_{iJ}$ format.
!- The node numbering is according to Gmsh convention.

INTERFACE
  MODULE RECURSIVE PURE FUNCTION EquidistancePoint_Quadrangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
  !! coordinates of point 1 and point 2 in $x_{iJ}$ format
  !! number of rows = nsd
  !! number of cols = 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
  !! returned coordinates in $x_{iJ}$ format
  END FUNCTION EquidistancePoint_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Aug 2022
! summary:  Interpolation point
!
!# Introduction
!
! In this case order is same in both x1 and x2 direction. Therefore,
! (N+1)**2 grid points are returned.
!
! Also in both x1 and x2 same type of grid family will be used.
!
!- This routine returns the interplation points on quad
!- `xij` contains nodal coordinates of quad in xij format.
!- SIZE(xij,1) = nsd, and SIZE(xij,2)=4
!- If xij is absent then biunit quad is used
!- `ipType` is interpolation point type, it can take following values
!- `Equidistance`, uniformly/evenly distributed points
!- `GaussLegendreLobatto
!- `GaussChebyshevLobatto
!
!- `layout` specifies the arrangement of points. The nodes are always
! returned in VEFC format (vertex, edge, face, cell). 1:3 are are
! vertex points, then edge, and then internal nodes. The internal nodes
! also follow the same convention. Please read Gmsh manual  on this topic.

INTERFACE InterpolationPoint_Quadrangle
  MODULE FUNCTION InterpolationPoint_Quadrangle1(order, ipType, xij, &
    & layout) RESULT(nodecoord)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! four vertices of quadrangle in xij format
    CHARACTER(*), INTENT(IN) :: layout
    !! VEFC
    REAL(DFP), ALLOCATABLE :: nodecoord(:, :)
    !! interpolation points in xij format
  END FUNCTION InterpolationPoint_Quadrangle1
END INTERFACE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Quadrangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! ith coefficients for lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle1
END INTERFACE

INTERFACE LagrangeCoeff_Quadrangle
  MODULE PROCEDURE LagrangeCoeff_Quadrangle1
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Quadrangle2(order, i, v, isVandermonde) &
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
  END FUNCTION LagrangeCoeff_Quadrangle2
END INTERFACE

INTERFACE LagrangeCoeff_Quadrangle
  MODULE PROCEDURE LagrangeCoeff_Quadrangle2
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Quadrangle3(order, i, v, ipiv) RESULT(ans)
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
  END FUNCTION LagrangeCoeff_Quadrangle3
END INTERFACE

INTERFACE LagrangeCoeff_Quadrangle
  MODULE PROCEDURE LagrangeCoeff_Quadrangle3
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Quadrangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION LagrangeCoeff_Quadrangle4(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in xij format, size(xij,2)
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Quadrangle4
END INTERFACE

INTERFACE LagrangeCoeff_Quadrangle
  MODULE PROCEDURE LagrangeCoeff_Quadrangle4
END INTERFACE LagrangeCoeff_Quadrangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is called while forming dubiner basis on triangle domain
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
  MODULE PURE FUNCTION Dubiner_Quadrangle1(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in biunit quadrangle, shape functions will be evaluated
    !! at these points. SIZE(xij,1) = 2, and SIZE(xij, 2) = number of points
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Quadrangle1
END INTERFACE

INTERFACE Dubiner_Quadrangle
  MODULE PROCEDURE Dubiner_Quadrangle1
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Dubiner (1991) polynomials on biunit domain
!
!# Introduction
!
! Forms Dubiner basis on biunit quadrangle domain.
! This routine is same as Dubiner_Quadrangle1
! The only difference is that xij are given by outerproduct of x and y.
! This function calls `Dubiner_Quadrangle1`.

INTERFACE
  MODULE PURE FUNCTION Dubiner_Quadrangle2(order, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of polynomial space
    REAL(DFP), INTENT(IN) :: x(:)
    !! x coordinate on line
    REAL(DFP), INTENT(IN) :: y(:)
    !! y coordinate on line
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions
    !! ans(:, j), jth shape functions at all points
    !! ans(j, :), all shape functions at jth point
  END FUNCTION Dubiner_Quadrangle2
END INTERFACE

INTERFACE Dubiner_Quadrangle
  MODULE PROCEDURE Dubiner_Quadrangle2
END INTERFACE Dubiner_Quadrangle

!----------------------------------------------------------------------------
!                                            TensorProdOrthopol_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on quadrangle
!
!# Introduction
!
! This function returns the tensor product expansion of orthogonal
! polynomial on biunit quadrangle.

INTERFACE
  MODULE PURE FUNCTION TensorProdOrthopol_Quadrangle1(p, q, xij, &
    & orthopol1, orthopol2, alpha1, beta1, alpha2, beta2, lambda1, lambda2) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: orthopol1
    !! orthogonal polynomial family in x1 direction
    INTEGER(I4B), INTENT(IN) :: orthopol2
    !! orthogonal poly family in x2 direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! alpha2 needed when orthopol2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! beta2 needed when orthopol2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! lambda2 is needed when orthopol2 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
    !!
  END FUNCTION TensorProdOrthopol_Quadrangle1
END INTERFACE

INTERFACE TensorProdOrthopol_Quadrangle
  MODULE PROCEDURE TensorProdOrthopol_Quadrangle1
END INTERFACE TensorProdOrthopol_Quadrangle

!----------------------------------------------------------------------------
!                                            TensorProdOrthopol_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all tensor product orthogoanl polynomial on quadrangle
!
!# Introduction
!
! This function returns the tensor product expansion of orthogonal
! polynomial on biunit quadrangle. Here xij is obtained by
! outer product of x and y

INTERFACE
  MODULE PURE FUNCTION TensorProdOrthopol_Quadrangle2(p, q, x, y, &
    & orthopol1, orthopol2, alpha1, beta1, alpha2, beta2, lambda1, lambda2) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! highest order in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! highest order in x2 direction
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! points of evaluation in xij format
    INTEGER(I4B), INTENT(IN) :: orthopol1
    !! orthogonal polynomial family in x1 direction
    INTEGER(I4B), INTENT(IN) :: orthopol2
    !! orthogonal poly family in x2 direction
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha1
    !! alpha1 needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! beta1 is needed when orthopol1 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha2
    !! alpha2 needed when orthopol2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! beta2 needed when orthopol2 is "Jacobi"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda1
    !! lambda1 is needed when orthopol1 is "Ultraspherical"
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda2
    !! lambda2 is needed when orthopol2 is "Ultraspherical"
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (p + 1) * (q + 1))
    !!
  END FUNCTION TensorProdOrthopol_Quadrangle2
END INTERFACE

INTERFACE TensorProdOrthopol_Quadrangle
  MODULE PROCEDURE TensorProdOrthopol_Quadrangle2
END INTERFACE TensorProdOrthopol_Quadrangle

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Quadrangle(x, y) RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    VertexBasis_Quadrangle2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on biunit quadrangle

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Quadrangle2(L1, L2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(L1, 1), 4)
    !! ans(:,v1) basis function of vertex v1 at all points
  END FUNCTION VertexBasis_Quadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                               VerticalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on left, right edge of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions on left and right edge of biunit quadrangle
!
! qe1 and qe2 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION VerticalEdgeBasis_Quadrangle(qe1, qe2, x, y) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on left vertical edge (e1)
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on right vertical edge(e2)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), qe1 + qe2 - 2)
  END FUNCTION VerticalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION VerticalEdgeBasis_Quadrangle2(qe1, qe2, L1, L2) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order on left vertical edge (e1)
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order on right vertical edge(e2)
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(L1, 1), qe1 + qe2 - 2)
  END FUNCTION VerticalEdgeBasis_Quadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                             HorizontalEdgeBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis on bottom and top edge of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions on bottom and top edge of biunit quadrangle
!
! pe3 and pe4 should be greater than or equal to 2

INTERFACE
  MODULE PURE FUNCTION HorizontalEdgeBasis_Quadrangle(pe3, pe4, x, y) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on bottom vertical edge (e3)
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on top vertical edge(e4)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), pe3 + pe4 - 2)
  END FUNCTION HorizontalEdgeBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION HorizontalEdgeBasis_Quadrangle2(pe3, pe4, L1, L2) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on bottom vertical edge (e3)
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on top vertical edge(e4)
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(L1, 1), pe3 + pe4 - 2)
  END FUNCTION HorizontalEdgeBasis_Quadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of biunit quadrangle
!
!# Introduction
!
! Evaluate basis functions in the cell of biunit quadrangle

INTERFACE
  MODULE PURE FUNCTION CellBasis_Quadrangle(pb, qb, x, y) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on bottom vertical edge (e3)
    INTEGER(I4B), INTENT(IN) :: qb
    !! order on top vertical edge(e4)
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(x), (pb - 1) * (qb - 1))
  END FUNCTION CellBasis_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION CellBasis_Quadrangle2(pb, qb, L1, L2) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order on bottom vertical edge (e3)
    INTEGER(I4B), INTENT(IN) :: qb
    !! order on top vertical edge(e4)
    REAL(DFP), INTENT(IN) :: L1(1:, 0:), L2(1:, 0:)
    !! point of evaluation
    REAL(DFP) :: ans(SIZE(L1, 1), (pb - 1) * (qb - 1))
  END FUNCTION CellBasis_Quadrangle2
END INTERFACE

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle
!
!# Introduction
!
! This function returns the modal basis on orthogonal polynomial
! The modal function in 1D is given by scaled Lobatto polynomial.
! These modal functions are orthogonal with respect to H1 seminorm.
! However, these modal function are not orthogonal withrespect to L2 norm.
!
! Bubble function in 1D is proportional to Jacobi polynomial with
! alpha=beta=1. Equivalently, these bubble functions are proportional to
! Ultraspherical polynomials with lambda = 3/2.
!

INTERFACE
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle1(pb, qb, pe3, pe4, &
    & qe1, qe2, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: qb
    !! order of interpolation inside the quadrangle in x2 direction
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge e3 (bottom) in x1 direction
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge e4 (top) in x1 direction
    INTEGER(I4B), INTENT(IN) :: qe1
    !! order of interpolation on edge e1 (left) in y1 direction
    INTEGER(I4B), INTENT(IN) :: qe2
    !! order of interpolation on edge e2 (right) in y1 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), &
      & pb * qb - pb - qb + pe3 + pe4 + qe1 + qe2 + 1)
    !!
  END FUNCTION HeirarchicalBasis_Quadrangle1
END INTERFACE

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PROCEDURE HeirarchicalBasis_Quadrangle1
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Oct 2022
! summary: Evaluate all modal basis (heirarchical polynomial) on quadrangle
!
!# Introduction
!
! This function is identical to `HeirarchicalBasis_Quadrangle1`
! with qe1=qe2=qb=q, and pe3=pe4=pb=p.
!

INTERFACE
  MODULE PURE FUNCTION HeirarchicalBasis_Quadrangle2(p, q, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! order of interpolation inside the quadrangle in x1 direction
    INTEGER(I4B), INTENT(IN) :: q
    !! order of interpolation inside the quadrangle in x2 direction
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation in xij format
    REAL(DFP) :: ans(SIZE(xij, 2), (p + 1) * (q + 1))
    !!
  END FUNCTION HeirarchicalBasis_Quadrangle2
END INTERFACE

INTERFACE HeirarchicalBasis_Quadrangle
  MODULE PROCEDURE HeirarchicalBasis_Quadrangle2
END INTERFACE HeirarchicalBasis_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_Clockwise( &
    & xi, eta, temp, p, q, startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: startNode
  END SUBROUTINE IJ2VEFC_Quadrangle_Clockwise
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE RECURSIVE SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise( &
    & xi, eta, temp, p, q, startNode)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    REAL(DFP), INTENT(IN) :: eta(:, :)
    REAL(DFP), INTENT(OUT) :: temp(:, :)
    INTEGER(I4B), INTENT(IN) :: p
    INTEGER(I4B), INTENT(IN) :: q
    INTEGER(I4B), INTENT(IN) :: startNode
  END SUBROUTINE IJ2VEFC_Quadrangle_AntiClockwise
END INTERFACE

END MODULE QuadrangleInterpolationUtility
