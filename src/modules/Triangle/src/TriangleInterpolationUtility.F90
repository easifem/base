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
!

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Interpolation, basis and quadrature utilities for triangles.
!
! This module collects the procedures that build and evaluate
! polynomial bases on a triangle.
! It covers these groups.
!
! 1. Counts of nodes and degrees of freedom of a triangle.
! 2. Interpolation points: equidistant, Blyth-Pozrikidis and Isaac.
! 3. Lagrange polynomials: coefficients, values and gradients.
! 4. The orthogonal (Dubiner) basis and its gradients.
! 5. The hierarchical basis and its gradients.
! 6. Quadrature points and weights.
! 7. Edge connectivity and the reference domain of a triangle.
!
! The reference triangles are `"UNIT"` and `"BIUNIT"`.
! The `"UNIT"` triangle has the vertices (0,0), (1,0) and (0,1).
! The `"BIUNIT"` triangle has the vertices (-1,-1), (1,-1) and (-1,1).
!
! Each point is one column of an array.
! A basis evaluated at many points has one row per point and one column per
! basis function.
! A gradient adds a last axis for the coordinate direction.
!
! Procedures whose name ends with `_` do not allocate their output.
! They fill an array that you provide and return the size of the block
! they wrote.
!
! @note
! The node layout and the edge orientation must match the element
! connectivity.
! @endnote
!
! @note
! `Dubiner_Triangle` and `OrthogonalBasis_Triangle` are two names for the
! same basis.
! @endnote

MODULE TriangleInterpolationUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE
PUBLIC :: LagrangeDegree_Triangle
PUBLIC :: LagrangeDOF_Triangle
PUBLIC :: LagrangeInDOF_Triangle
PUBLIC :: EquidistanceInPoint_Triangle

PUBLIC :: EquidistancePoint_Triangle
PUBLIC :: EquidistancePoint_Triangle_

PUBLIC :: InterpolationPoint_Triangle
PUBLIC :: InterpolationPoint_Triangle_
PUBLIC :: LagrangeCoeff_Triangle
PUBLIC :: LagrangeCoeff_Triangle_

PUBLIC :: Dubiner_Triangle
PUBLIC :: Dubiner_Triangle_

PUBLIC :: OrthogonalBasis_Triangle
PUBLIC :: OrthogonalBasis_Triangle_

PUBLIC :: OrthogonalBasisGradient_Triangle
PUBLIC :: OrthogonalBasisGradient_Triangle_

PUBLIC :: VertexBasis_Triangle
PUBLIC :: EdgeBasis_Triangle
PUBLIC :: CellBasis_Triangle

PUBLIC :: HeirarchicalBasis_Triangle
PUBLIC :: HeirarchicalBasis_Triangle_

PUBLIC :: HeirarchicalBasisGradient_Triangle
PUBLIC :: HeirarchicalBasisGradient_Triangle_

PUBLIC :: LagrangeEvalAll_Triangle
PUBLIC :: LagrangeEvalAll_Triangle_

PUBLIC :: LagrangeGradientEvalAll_Triangle
PUBLIC :: LagrangeGradientEvalAll_Triangle_

PUBLIC :: QuadratureNumber_Triangle
PUBLIC :: QuadraturePoint_Triangle
PUBLIC :: QuadraturePoint_Triangle_

PUBLIC :: IJ2VEFC_Triangle
PUBLIC :: FacetConnectivity_Triangle
PUBLIC :: RefElemDomain_Triangle

PUBLIC :: GetTotalDOF_Triangle
PUBLIC :: GetTotalInDOF_Triangle

PUBLIC :: GetHierarchicalDOF_Triangle

!----------------------------------------------------------------------------
!                                               GetHierarchicalDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Counts the hierarchical modes of a triangle.
!
! Returns the number of hierarchical modes of one kind, or the total.
! The argument `opt` selects the kind.
!
! - `"V"` counts the vertex modes. The result is 3.
! - `"E"` counts the edge modes.
! - `"C"` counts the cell modes.
! - Any other value counts all modes.
!
! The routine ignores case.
! The orders of the three edges may differ.
!
! Returns:
!   `ans` : number of modes
!
! Formula:
!   $$ N_e = \sum_{i=1}^{3} (p_i - 1), \quad
!   N_c = \frac{(p-1)(p-2)}{2}, \quad N = 3 + N_e + N_c $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/TriangleHierarchicalDOF_test_1.F90)

INTERFACE
  MODULE PURE FUNCTION GetHierarchicalDOF_Triangle( &
    order, pe1, pe2, pe3, opt) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the cell modes, `order >= 1`
    INTEGER(I4B), INTENT(IN) :: pe1, pe2, pe3
    !! orders of the three edges, each `>= 1`
    CHARACTER(1), INTENT(IN) :: opt
    !! kind of modes to count:
    !! `"V"` vertex, `"E"` edge, `"C"` cell, any other letter
    !! (for example `"H"`) all modes
    INTEGER(I4B) :: ans
    !! number of modes
  END FUNCTION GetHierarchicalDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of degrees of freedom of a triangle.
!
! Returns the total number of degrees of freedom of a triangle for a
! polynomial of degree `order`.
! This counts the vertex, edge and interior nodes together.
!
! @note
! The routine ignores `baseContinuity` and `baseInterpolation`.
! The result is the Lagrange count for every basis family.
! @endnote
!
! Returns:
!   `ans` : total number of degrees of freedom
!
! Formula:
!   $$ N = \frac{(p+1)(p+2)}{2} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/TriangleInterpolationMetadata_test_1.F90)
!
! See also: [[GetTotalInDOF_Triangle]]

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Triangle(order, baseContinuity, &
                                            baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree, `order >= 0`
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis functions; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family; not used
    INTEGER(I4B) :: ans
    !! total number of degrees of freedom
  END FUNCTION GetTotalDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
! LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior degrees of freedom of a triangle.
!
! Returns the number of degrees of freedom inside a triangle, for a
! polynomial of degree `order`.
! This leaves out the vertex and edge nodes.
!
! @note
! The routine ignores `baseContinuity` and `baseInterpolation`.
! The result is the Lagrange count for every basis family.
! @endnote
!
! Returns:
!   `ans` : number of interior degrees of freedom
!
! Formula:
!   $$ N = \frac{(p-1)(p-2)}{2} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/TriangleInterpolationMetadata_test_1.F90)
!
! See also: [[GetTotalDOF_Triangle]]

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Triangle(order, baseContinuity, &
                                              baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree, `order >= 1`
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis functions; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family; not used
    INTEGER(I4B) :: ans
    !! number of interior degrees of freedom
  END FUNCTION GetTotalInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefElemDomain_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of the reference triangle domain.
!
! Returns `"UNIT"` or `"BIUNIT"`.
! The name depends on the basis family.
!
! - Lagrange and serendipity bases use `"UNIT"`.
! - Hierarchical and orthogonal bases use `"BIUNIT"`.
!
! The routine reads only the first two letters of `baseContinuity`
! and the first letter of `baseInterpol`.
! It ignores case.
!
! @note
! The routine supports only `"H1"` continuity.
! For any other value, the result is empty.
! In debug builds the routine also reports an error.
! @endnote
!
! Returns:
!   `ans` : `"UNIT"` or `"BIUNIT"`
!
! Examples
!
! - [test 1](|base|Triangle/examples/RefElemDomain_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/TriangleInterpolationMetadata_test_1.F90)
!   with Lagrange basis

INTERFACE
  MODULE FUNCTION RefElemDomain_Triangle(baseContinuity, baseInterpol) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis functions, must be `"H1"`
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! basis family, one of `"Lagrange"`, `"Serendipity"`,
    !! `"Hierarchy"` or `"Orthogonal"`
    TYPE(String) :: ans
    !! `"UNIT"` or `"BIUNIT"`
  END FUNCTION RefElemDomain_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the vertex connectivity of the edges of a triangle.
!
! Returns the two end vertices of each of the three edges.
! The edges are 1-2, 2-3 and 3-1.
! The result is the same for every basis family.
!
! @note
! The routine ignores `baseInterpol` and `baseContinuity`.
! @endnote
!
! Returns:
!   `ans` : vertex numbers of the edge end points, one edge per column
!
! Formula:
!   $$ \partial T = e_{12} \cup e_{23} \cup e_{31} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/FacetConnectivity_Triangle_test_1.F90)

INTERFACE
  MODULE FUNCTION FacetConnectivity_Triangle(baseInterpol, &
                                             baseContinuity) RESULT(ans)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseInterpol
    !! basis family; not used
    CHARACTER(*), OPTIONAL, INTENT(IN) :: baseContinuity
    !! continuity of the basis functions; not used
    INTEGER(I4B) :: ans(2, 3)
    !! rows are the two end vertices of an edge,
    !! columns are the edges (facets)
  END FUNCTION FacetConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IJ2VEFC_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Reorders points of a triangular grid into VEFC order.
!
! Copies the points of a triangular grid into `temp` in VEFC order.
! VEFC lists the vertices first, then the edge points, then the interior
! points.
!
! The grid arrays `xi` and `eta` have `order+1` rows and columns.
! Only the entries `(ii, jj)` with `ii + jj <= order + 2` belong to the
! triangle. The routine ignores the other entries.
!
! The routine works from the outer layer of the triangle inwards.
! Each layer gives its three vertices.
! Then it gives the points on edge 1-2, edge 2-3 and edge 3-1.
! If `order - 1` leaves remainder 2 on division by 3, one interior point
! ends the list.
!
! @note
! `N` must equal the number of nodes, `(order+1)(order+2)/2`.
! In debug builds, the routine reports an error otherwise.
! @endnote
!
! Formula:
!   $$ N=(p+1)(p+2)/2 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/IJ2VEFC_Triangle_test_1.F90)

INTERFACE
  MODULE SUBROUTINE IJ2VEFC_Triangle(xi, eta, temp, order, N)
    REAL(DFP), INTENT(IN) :: xi(:, :)
    !! first coordinate of the grid points
    !! at least `(order+1, order+1)`
    REAL(DFP), INTENT(IN) :: eta(:, :)
    !! second coordinate of the grid points
    !! at least `(order+1, order+1)`
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! points in VEFC order, one point per column
    !! row 1 holds `xi` and row 2 holds `eta`
    !! at least `(2, N)`
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation
    INTEGER(I4B), INTENT(IN) :: N
    !! number of points written in `temp`
  END SUBROUTINE IJ2VEFC_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the monomial degrees of a Lagrange triangle.
!
! Returns the exponents of the monomials of total degree at most `order`.
! These monomials span the Lagrange polynomial space.
!
! Row `k` holds the exponents `i` and `j` of the monomial `x^i y^j`.
! Column 1 holds `i` and column 2 holds `j`.
! The rows run over `i` first, then over `j`.
! For `order` 1 the rows are (0,0), (1,0) and (0,1).
!
! Formula:
!   $$ N=(p+1)(p+2)/2 $$
!
! See also: [[LagrangeDegree_Triangle_]], [[LagrangeDOF_Triangle]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomial
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! exponents of the monomials
    !! number of rows = `LagrangeDOF_Triangle(order)`
    !! number of cols = 2
  END FUNCTION LagrangeDegree_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-18
! summary: Fills a preallocated array with the monomial degrees of a
! Lagrange triangle.
!
! Writes the exponents of the monomials of total degree at most `order`
! into `ans`. The routine does not allocate memory.
!
! Row `k` holds the exponents `i` and `j` of the monomial `x^i y^j`.
! Column 1 holds `i` and column 2 holds `j`.
! The rows run over `i` first, then over `j`.
! For `order` 1 the rows are (0,0), (1,0) and (0,1).
!
! @note
! `ans` must have at least `(order+1)(order+2)/2` rows and 2 columns.
! @endnote
!
! See also: [[LagrangeDegree_Triangle]]

INTERFACE
  MODULE PURE SUBROUTINE LagrangeDegree_Triangle_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomial
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! exponents of the monomials
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `LagrangeDOF_Triangle(order)` and `ncol` is 2
  END SUBROUTINE LagrangeDegree_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       LagrangeDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes of a Lagrange triangle.
!
! Returns the total number of Lagrange nodes of a triangle.
! The count includes the vertices, the edge nodes and the interior nodes.
! It equals the dimension of the polynomial space of degree `order`.
!
! @note
! `order` must be at least 0.
! @endnote
!
! Formula:
!   $$ N=(p+1)(p+2)/2 $$
!
! See also: [[LagrangeInDOF_Triangle]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomial
    INTEGER(I4B) :: ans
    !! total number of nodes
  END FUNCTION LagrangeDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of interior nodes of a Lagrange triangle.
!
! Returns the number of Lagrange nodes that lie strictly inside the
! triangle. These nodes belong to no vertex and no edge.
!
! The result is 0 for `order` 1 and 2.
!
! @note
! `order` must be at least 1.
! @endnote
!
! Formula:
!   $$ N_{in}=(p-1)(p-2)/2 $$
!
! See also: [[LagrangeDOF_Triangle]]

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Triangle(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomial, at least 1
    INTEGER(I4B) :: ans
    !! number of interior nodes
  END FUNCTION LagrangeInDOF_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the equidistant points inside a triangle.
!
! Returns the equidistant points that lie inside a triangle.
! Points on the vertices and the edges are not included.
!
! If `order` is less than 3, the triangle has no interior point.
! Then `ans` has shape (0, 0).
!
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! Examples
!
! - [test 1](|base|Triangle/examples/EquidistanceInPoint_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/EquidistanceInPoint_Triangle_test_2.F90)
!   with given vertices
!
! See also: [[EquidistanceInPoint_Triangle_]],
! [[EquidistancePoint_Triangle]]

INTERFACE
  MODULE PURE FUNCTION EquidistanceInPoint_Triangle(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interior points, one point per column, in $x_{iJ}$ format.
    !! Number of rows is `SIZE(xij, 1)`, or 2 if `xij` is absent.
    !! Number of columns is (order-1)(order-2)/2.
  END FUNCTION EquidistanceInPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-14
! summary: Fills a preallocated array with the interior equidistant points.
!
! Writes the equidistant points that lie inside a triangle into `ans`.
! Points on the vertices and the edges are not included.
! The routine does not allocate memory.
!
! If `order` is less than 3, the triangle has no interior point.
! Then `nrow` and `ncol` are 0 and `ans` is not changed.
!
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! @note
! `ans` must have at least `nrow` rows and `(order-1)(order-2)/2`
! columns.
! @endnote
!
! See also: [[EquidistanceInPoint_Triangle]],
! [[EquidistancePoint_Triangle_]]

INTERFACE
  MODULE PURE SUBROUTINE EquidistanceInPoint_Triangle_(order, ans, nrow, &
                                                       ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interior points, one point per column, in $x_{iJ}$ format
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(xij, 1)`, or 2 if `xij` is absent
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
  END SUBROUTINE EquidistanceInPoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the equidistant points of a triangle.
!
! Returns all equidistant points of a triangle.
! This includes the vertices, the edge points and the interior points.
!
! The points follow this order.
!
! 1. The three vertices.
! 2. The points inside edge 1-2, then edge 2-3, then edge 3-1.
! 3. The interior points.
!
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! Formula:
!   $$ x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/EquidistancePoint_Triangle_test_1.F90)
!
! See also: [[EquidistancePoint_Triangle_]],
! [[EquidistanceInPoint_Triangle]]

INTERFACE
  MODULE RECURSIVE PURE FUNCTION EquidistancePoint_Triangle(order, &
    xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at least 1
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! points, one point per column, in $x_{iJ}$ format.
    !! Number of rows is `SIZE(xij, 1)`, or 2 if `xij` is absent.
    !! Number of columns is (order+1)(order+2)/2.
  END FUNCTION EquidistancePoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with equidistant points of a triangle.
!
! Writes all equidistant points of a triangle into `ans`.
! This includes the vertices, the edge points and the interior points.
! The routine does not allocate memory.
!
! The points follow this order.
!
! 1. The three vertices.
! 2. The points inside edge 1-2, then edge 2-3, then edge 3-1.
! 3. The interior points.
!
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! @note
! `ans` must have at least `nrow` rows and `(order+1)(order+2)/2`
! columns.
! @endnote
!
! Formula:
!   $$ x_{ij}=(i/p,j/p),\quad i,j\ge0,\quad i+j\le p $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/EquidistancePoint_Triangle__test_1.F90)
!
! See also: [[EquidistancePoint_Triangle]]

INTERFACE
  MODULE RECURSIVE PURE SUBROUTINE EquidistancePoint_Triangle_(order, ans, &
                                                              nrow, ncol, xij)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at least 1
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! points, one point per column, in $x_{iJ}$ format
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(xij, 1)`, or 2 if `xij` is absent
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
  END SUBROUTINE EquidistancePoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                BlythPozrikidis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Returns the Blyth-Pozrikidis nodes on a triangle.
!
! Returns the interpolation nodes of Blyth and Pozrikidis on a triangle.
! The nodes use a one-dimensional point family on `[0, 1]`.
! The argument `ipType` selects the family.
!
! The reference triangle has the vertices (0,0), (1,0) and (0,1).
! If `xij` is present, the nodes move to the triangle with the
! vertices in the columns of `xij`.
!
! The nodes follow the order vertices, edge nodes, interior nodes.
! The routine supports only `layout="VEFC"`.
! In debug builds, any other layout raises an error.
!
! @note
! `order` must not exceed 30.
! @endnote
!
! The method follows M. G. Blyth and C. Pozrikidis.
! The paper has the title "A Lobatto interpolation grid over the triangle".
! It appears in IMA Journal of Applied Mathematics, volume 71, 2006.
! The doi is 10.1093/imamat/hxh077.
!
! Returns:
!   `ans` : nodes, one node per column
!
! See also: [[BlythPozrikidis_Triangle_]], [[EquidistancePoint_Triangle]]

INTERFACE
  MODULE FUNCTION BlythPozrikidis_Triangle(order, ipType, layout, xij, &
                                           alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at most 30
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of the one-dimensional points: Equidistance, GaussLegendre,
    !! GaussLegendreLobatto, GaussChebyshev, GaussChebyshevLobatto,
    !! GaussJacobi or GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the target triangle, one vertex per column.
    !! Number of rows is nsd, number of columns is 3.
    !! Default is the reference triangle.
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! use `"VEFC"` only
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nodes, one node per column.
    !! Number of rows is nsd, or 2 if `xij` is absent.
    !! Number of columns is (order+1)(order+2)/2.
  END FUNCTION BlythPozrikidis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 BlythPozrikidis_Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Fills a preallocated array with the Blyth-Pozrikidis nodes.
!
! Writes the Blyth-Pozrikidis nodes of a triangle into `ans`.
! The routine does not allocate memory.
!
! The nodes use a one-dimensional point family on `[0, 1]`.
! The argument `ipType` selects the family.
! The reference triangle has the vertices (0,0), (1,0) and (0,1).
! If `xij` is present, the nodes move to the triangle with the
! vertices in the columns of `xij`.
!
! The nodes follow the order vertices, edge nodes, interior nodes.
! The routine supports only `layout="VEFC"`.
! In debug builds, any other layout raises an error.
! Otherwise, `ans` is not changed.
!
! @note
! `order` must not exceed 30.
! `ans` must have at least `nrow` rows and `(order+1)(order+2)/2`
! columns.
! @endnote
!
! The method follows M. G. Blyth and C. Pozrikidis.
! The paper has the title "A Lobatto interpolation grid over the triangle".
! It appears in IMA Journal of Applied Mathematics, volume 71, 2006.
! The doi is 10.1093/imamat/hxh077.
!
! See also: [[BlythPozrikidis_Triangle]], [[Isaac_Triangle_]]

INTERFACE
 MODULE SUBROUTINE BlythPozrikidis_Triangle_(order, ipType, ans, nrow, ncol, &
                                             layout, xij, alpha, beta, lambda)

    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at most 30
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of the one-dimensional points: Equidistance, GaussLegendre,
    !! GaussLegendreLobatto, GaussChebyshev, GaussChebyshevLobatto,
    !! GaussJacobi or GaussJacobiLobatto
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodes, one node per column
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(xij, 1)`, or 2 if `xij` is absent
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the target triangle, one vertex per column.
    !! Number of rows is nsd, number of columns is 3.
    !! Default is the reference triangle.
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! use `"VEFC"` only
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE BlythPozrikidis_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Returns the Isaac nodes on a triangle.
!
! Returns the Isaac interpolation nodes of a triangle.
! The nodes use a one-dimensional point family.
! The argument `ipType` selects the family.
!
! The nodes follow the order vertices, edge nodes, interior nodes.
! The reference triangle has the vertices (0,0), (1,0) and (0,1).
! If `xij` is present, the nodes move to the triangle with the vertices
! in the columns of `xij`.
!
! The function supports only `layout="VEFC"`.
! In debug builds, any other layout raises an error.
!
! @note
! `order` must not exceed 30.
! @endnote
!
! The method follows T. Isaac.
! The paper has the title "Recursive, parameter-free, explicitly defined
! interpolation nodes for simplices".
! It appears in SIAM Journal on Scientific Computing, volume 42, 2020.
!
! Returns:
!   `ans` : nodes, one node per column
!
! See also: [[Isaac_Triangle_]], [[BlythPozrikidis_Triangle]]

INTERFACE
  MODULE FUNCTION Isaac_Triangle(order, ipType, layout, xij, &
   alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at most 30
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of the one-dimensional points: Equidistance, GaussLegendre,
    !! GaussLegendreLobatto, GaussChebyshev, GaussChebyshevLobatto,
    !! GaussJacobi or GaussJacobiLobatto
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the target triangle, one vertex per column.
    !! Number of rows is nsd, number of columns is 3.
    !! Default is the reference triangle.
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! use `"VEFC"` only
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nodes, one node per column.
    !! Number of rows is nsd, or 2 if `xij` is absent.
    !! Number of columns is (order+1)(order+2)/2.
  END FUNCTION Isaac_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Isaac_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Fills a preallocated array with the Isaac nodes on a triangle.
!
! Writes the Isaac interpolation nodes of a triangle into `ans`.
! The routine does not allocate memory.
!
! The nodes follow the order vertices, edge nodes, interior nodes.
! The reference triangle has the vertices (0,0), (1,0) and (0,1).
! If `xij` is present, the nodes move to the triangle with the vertices
! in the columns of `xij`.
!
! The routine supports only `layout="VEFC"`.
! In debug builds, any other layout raises an error.
! Otherwise, `ans` is not changed.
!
! @note
! `order` must not exceed 30.
! `ans` must have at least `nrow` rows and `(order+1)(order+2)/2`
! columns.
! @endnote
!
! The method follows T. Isaac.
! The paper has the title "Recursive, parameter-free, explicitly defined
! interpolation nodes for simplices".
! It appears in SIAM Journal on Scientific Computing, volume 42, 2020.
!
! See also: [[Isaac_Triangle]], [[BlythPozrikidis_Triangle_]]

INTERFACE
  MODULE SUBROUTINE Isaac_Triangle_(order, ipType, ans, nrow, ncol, &
                                    layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation, at most 30
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of the one-dimensional points: Equidistance, GaussLegendre,
    !! GaussLegendreLobatto, GaussChebyshev, GaussChebyshevLobatto,
    !! GaussJacobi or GaussJacobiLobatto
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodes, one node per column
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(xij, 1)`, or 2 if `xij` is absent
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the target triangle, one vertex per column.
    !! Number of rows is nsd, number of columns is 3.
    !! Default is the reference triangle.
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout
    !! use `"VEFC"` only
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE Isaac_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                InterpolationPoint_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns interpolation points of a triangle.
!
! Returns the interpolation points of a triangle.
! The argument `ipType` selects the family of points.
!
! The function allocates `ans` and calls [[InterpolationPoint_Triangle_]].
! That routine lists the values of `ipType` and their meaning.
!
! The points follow the order vertices, edge points, interior points.
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! @note
! `layout` must be `"VEFC"` for the Blyth-Pozrikidis and Isaac points.
! The other families ignore `layout`.
! @endnote
!
! @note
! Use only the values of `ipType` that the subroutine lists.
! For other values, the function does not set the size of `ans`.
! @endnote
!
! Examples
!
! - [test 1](|base|Triangle/examples/InterpolationPoint_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/InterpolationPoint_Triangle_test_2.F90)
!   with several values of `ipType`
!
! See also: [[InterpolationPoint_Triangle_]]

INTERFACE
  MODULE FUNCTION InterpolationPoint_Triangle(order, ipType, &
                                              layout, xij, alpha, &
                                              beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of points, a value of `ipopt`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout, use `"VEFC"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation points, one point per column, in $x_{iJ}$ format
    !! number of rows = `SIZE(xij, 1)`, or 2 if `xij` is absent
  END FUNCTION InterpolationPoint_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with interpolation points of a
! triangle.
!
! Writes the interpolation points of a triangle into `ans`.
! The routine does not allocate memory.
! The argument `ipType` selects the family of points.
!
! The routine handles the following values of `ipType`.
!
! - `ipopt%Center`: one point, the centroid of the triangle.
! - `ipopt%Equidistance`: equidistant points.
! - `ipopt%BlythPozLegendre` and `ipopt%BlythPozChebyshev`: Blyth and
!   Pozrikidis points from Gauss-Lobatto points of the given kind.
! - `ipopt%IsaacLegendre` and `ipopt%GaussLegendreLobatto`: Isaac points
!   from Gauss-Legendre-Lobatto points.
! - `ipopt%IsaacChebyshev` and `ipopt%GaussChebyshevLobatto`: Isaac points
!   from Gauss-Chebyshev-Lobatto points.
! - Any other value goes to [[Isaac_Triangle_]], except `ipopt%Feket`,
!   `ipopt%Hesthaven` and `ipopt%ChenBabuska`.
!   These three are not available. In debug builds they raise an error.
!   Otherwise, `ans`, `nrow` and `ncol` are not set.
!
! The points follow the order vertices, edge points, interior points.
! If `xij` is absent, the triangle has the vertices (0,0), (1,0) and
! (0,1).
!
! @note
! `layout` must be `"VEFC"` for the Blyth-Pozrikidis and Isaac points.
! The other families ignore `layout`.
! @endnote
!
! Examples
!
! - [test 1](|base|Triangle/examples/InterpolationPoint_Triangle__test_1.F90)
!
! See also: [[InterpolationPoint_Triangle]],
! [[BlythPozrikidis_Triangle_]], [[Isaac_Triangle_]],
! [[EquidistancePoint_Triangle_]]

INTERFACE
  MODULE SUBROUTINE InterpolationPoint_Triangle_( &
    order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the interpolation
    INTEGER(I4B), INTENT(IN) :: ipType
    !! family of points, a value of `ipopt`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation points, one point per column, in $x_{iJ}$ format
    !! the routine fills only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(xij, 1)`, or 2 if `xij` is absent
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertices of the triangle, one vertex per column
    !! number of rows = nsd, number of cols = 3
    !! default: the reference triangle
    CHARACTER(*), INTENT(IN) :: layout
    !! local node numbering layout, use `"VEFC"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE InterpolationPoint_Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of one Lagrange polynomial, using the
! nodes.
!
! Returns the coefficients of the `i`-th Lagrange polynomial of a
! triangle.
! The polynomial is 1 at node `i` and 0 at the other nodes.
! The coefficients refer to the monomial basis of degree `order`.
!
! The routine builds the Vandermonde matrix at the nodes `xij`,
! factorizes it, and solves one linear system.
!
! @note
! `xij` must hold `(order+1)(order+2)/2` nodes.
! @endnote
!
! Returns:
!   `ans` : coefficients of the `i`-th Lagrange polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle1_]], [[LagrangeCoeff_Triangle2]]

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodes of the triangle, one node per column
    !! number of rows = nsd, number of cols = `SIZE(xij, 2)`
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients of the `i`-th Lagrange polynomial
  END FUNCTION LagrangeCoeff_Triangle1
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the coefficients of one Lagrange
! polynomial, using the nodes.
!
! Writes the coefficients of the `i`-th Lagrange polynomial of a triangle
! into `ans`.
! The polynomial is 1 at node `i` and 0 at the other nodes.
! The coefficients refer to the monomial basis of degree `order`.
! The routine does not allocate memory.
!
! The routine builds the Vandermonde matrix at the nodes `xij`,
! factorizes it, and solves one linear system.
!
! @note
! `xij` must hold `(order+1)(order+2)/2` nodes.
! `ans` must have at least `SIZE(xij, 2)` entries.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle__test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle1]], [[LagrangeCoeff_Triangle2_]]

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodes of the triangle, one node per column
    !! number of rows = nsd, number of cols = `SIZE(xij, 2)`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients of the `i`-th Lagrange polynomial
    !! the routine fills only `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, equal to `SIZE(xij, 2)`
  END SUBROUTINE LagrangeCoeff_Triangle1_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of one Lagrange polynomial, using the
! Vandermonde matrix.
!
! Returns the coefficients of the `i`-th Lagrange polynomial of a
! triangle.
! The coefficients refer to the basis of the Vandermonde matrix `v`.
!
! The routine copies `v`, factorizes the copy, and solves one linear
! system. The matrix `v` is not changed.
!
! @note
! `order` and `isVandermonde` are not used.
! `isVandermonde` only selects this overload.
! @endnote
!
! Returns:
!   `ans` : coefficients of the `i`-th Lagrange polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle2_]], [[LagrangeCoeff_Triangle3]]

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix
    !! size is (`SIZE(v, 1)`, `SIZE(v, 1)`)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this overload, its value is not used
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients of the `i`-th Lagrange polynomial
  END FUNCTION LagrangeCoeff_Triangle2
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the coefficients of one Lagrange
! polynomial, using the Vandermonde matrix.
!
! Writes the coefficients of the `i`-th Lagrange polynomial of a triangle
! into `ans`.
! The coefficients refer to the basis of the Vandermonde matrix `v`.
! The routine does not allocate memory.
!
! The routine copies `v`, factorizes the copy, and solves one linear
! system. The matrix `v` is not changed.
!
! @note
! `order` and `isVandermonde` are not used.
! `isVandermonde` only selects this overload.
! @endnote
!
! @note
! `ans` must have at least `SIZE(v, 1)` entries.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle__test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle2]], [[LagrangeCoeff_Triangle3_]]

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle2_(order, i, v, isVandermonde, &
                                             ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix
    !! size is (`SIZE(v, 1)`, `SIZE(v, 1)`)
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this overload, its value is not used
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients of the `i`-th Lagrange polynomial
    !! the routine fills only `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Triangle2_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                     LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of one Lagrange polynomial, using LU
! factors.
!
! Returns the coefficients of the `i`-th Lagrange polynomial of a
! triangle.
! The coefficients refer to the basis of the Vandermonde matrix.
!
! You pass the LU factors of the Vandermonde matrix in `v`.
! You pass the pivot indices of the factorization in `ipiv`.
! The routine solves one linear system and does not factorize `v`.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! @endnote
!
! Returns:
!   `ans` : coefficients of the `i`-th Lagrange polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle2]], [[LagrangeCoeff_Triangle3_]]

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix
    !! size is (`SIZE(v, 1)`, `SIZE(v, 1)`)
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices from the LU factorization
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients of the `i`-th Lagrange polynomial
  END FUNCTION LagrangeCoeff_Triangle3
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the coefficients of one Lagrange
! polynomial, using LU factors.
!
! Computes the coefficients of the `i`-th Lagrange polynomial of a
! triangle.
! The coefficients refer to the basis of the Vandermonde matrix.
!
! You pass the LU factors of the Vandermonde matrix in `v`.
! You pass the pivot indices of the factorization in `ipiv`.
! The routine solves one linear system and does not factorize `v`.
! It does not allocate `ans`.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! `ans` must have at least `SIZE(v, 1)` entries.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle__test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle3]]

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix,
    !! size is (`SIZE(v, 1)`, `SIZE(v, 1)`)
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices from the LU factorization
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients of the `i`-th Lagrange polynomial
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Triangle3_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the coefficients of all Lagrange polynomials of a
! triangle.
!
! Computes the coefficients of all Lagrange polynomials of degree `order`
! at the points `xij`.
! Column `j` of the result holds the coefficients of polynomial `j`.
! The coefficients refer to the basis selected by `basisType`.
! If `basisType` is absent, the routine uses the monomial basis.
! If `refTriangle` is absent, it is "UNIT".
!
! Returns:
!   ans : REAL(DFP) - coefficients, `ans(:, j)` belongs to polynomial `j`
!
! Formula:
!   $$ V C = I, \quad \ell_j(x) = \sum_k C_{kj} m_k(x) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeCoeff_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle_test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle4_]]

INTERFACE LagrangeCoeff_Triangle
  MODULE FUNCTION LagrangeCoeff_Triangle4(order, xij, basisType, &
                                          refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis of the coefficients: monomial, Jacobi (Dubiner) or
    !! hierarchical, default is monomial
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT",
    !! default is "UNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients, `ans(:, j)` belongs to polynomial `j`
  END FUNCTION LagrangeCoeff_Triangle4
END INTERFACE LagrangeCoeff_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the coefficients of all Lagrange
! polynomials of a triangle.
!
! Computes the coefficients of all Lagrange polynomials of degree `order`
! at the points `xij`.
! Column `j` of `ans` holds the coefficients of polynomial `j`.
! The coefficients refer to the basis selected by `basisType`.
!
! The routine works in two steps.
!
! 1. It builds the matrix `V` of the basis functions at the points.
! 2. It replaces `V` by its inverse in `ans`.
!
! The routine does not allocate `ans`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and columns.
! @endnote
!
! Formula:
!   $$ V C = I, \quad \ell_j(x) = \sum_k C_{kj} m_k(x) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeCoeff_Triangle__test_1.F90)
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle__test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle4]], [[LagrangeCoeff_Triangle5_]]

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle4_(order, xij, basisType, &
                                             refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis of the coefficients: monomial, Jacobi (Dubiner) or
    !! hierarchical
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, `ans(:, j)` belongs to polynomial `j`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
  END SUBROUTINE LagrangeCoeff_Triangle4_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                                    LagrangeCoeff_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the coefficients of all Lagrange
! polynomials of a triangle, with a given monomial degree table.
!
! Computes the coefficients of all Lagrange polynomials at the points
! `xij`.
! Column `j` of `ans` holds the coefficients of polynomial `j`.
! The coefficients refer to the basis selected by `basisType`.
!
! The routine works in two steps.
!
! 1. It builds the matrix `V` of the basis functions at the points.
! 2. It replaces `V` by its inverse in `ans`.
!
! For the monomial basis, `degree` gives the exponents of the monomials.
! For the other bases, `degree` is not used.
! The routine does not allocate `ans`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and columns.
! @endnote
!
! Formula:
!   $$ V C = I, \quad \ell_j(x) = \sum_k C_{kj} m_k(x) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeCoeff_Triangle__test_2.F90)
!
! See also: [[LagrangeCoeff_Triangle4_]], [[LagrangeDegree_Triangle]]

INTERFACE LagrangeCoeff_Triangle_
  MODULE SUBROUTINE LagrangeCoeff_Triangle5_( &
    order, xij, basisType, refTriangle, degree, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis of the coefficients: monomial, Jacobi (Dubiner) or
    !! hierarchical
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! exponents of the monomials, used only for the monomial basis
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, `ans(:, j)` belongs to polynomial `j`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
  END SUBROUTINE LagrangeCoeff_Triangle5_
END INTERFACE LagrangeCoeff_Triangle_

!----------------------------------------------------------------------------
!                                              LagrangeVandermonde_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Fills a preallocated array with the monomial Vandermonde matrix
! of a triangle.
!
! Evaluates monomials at the points `xij`.
! Row `i` belongs to point `i`.
! Column `j` holds the monomial `x**degree(j, 1) * y**degree(j, 2)`.
! The routine does not allocate `ans`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and `SIZE(degree, 1)`
! columns.
! @endnote
!
! Formula:
!   $$ V_{ij} = x_i^{d_{j1}} y_i^{d_{j2}} $$
!
! See also: [[LagrangeDegree_Triangle]]

INTERFACE LagrangeVandermonde_Triangle_
  MODULE PURE SUBROUTINE LagrangeVandermonde_Triangle1_(xij, degree, ans, &
                                                        nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in $x_{iJ}$ format, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    INTEGER(I4B), INTENT(IN) :: degree(:, :)
    !! monomial exponents, `degree(j, :)` holds the exponents of `x`
    !! and `y` in monomial `j`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! Vandermonde matrix, rows are points and columns are monomials
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `SIZE(degree, 1)`
  END SUBROUTINE LagrangeVandermonde_Triangle1_
END INTERFACE LagrangeVandermonde_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the Dubiner basis of a triangle at given points.
!
! Evaluates all Dubiner basis functions of a triangle up to degree `order`
! at the given points.
! The points are triangle points, given in the reference triangle
! `refTriangle`.
!
! Returns:
!   ans : REAL(DFP) - shape functions, `ans(i, j)` is function `j` at
!   point `i`
!
! Formula:
!   $$ \phi_{pq} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b), \quad p + q \le n $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Dubiner_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/Dubiner_Triangle_test_2.F90) compared
!   with the grid overload
!
! See also: [[Dubiner_Triangle2]], [[Dubiner_Triangle1_]]

INTERFACE Dubiner_Triangle
  MODULE PURE FUNCTION Dubiner_Triangle1(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2)
    !! shape functions, `ans(i, j)` is function `j` at point `i`
  END FUNCTION Dubiner_Triangle1
END INTERFACE Dubiner_Triangle

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. The modes run in the order
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = OrthogonalBasis_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle1
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the Dubiner basis of a triangle.
!
! Evaluates all Dubiner basis functions of a triangle up to degree `order`
! at the given points.
! The routine does not allocate `ans`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and
! `(order + 1) * (order + 2) / 2` columns.
! @endnote
!
! Formula:
!   $$ \phi_{pq} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b), \quad p + q \le n $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Dubiner_Triangle__test_1.F90)
!
! See also: [[Dubiner_Triangle1]], [[Dubiner_Triangle2_]]

INTERFACE Dubiner_Triangle_
  MODULE PURE SUBROUTINE Dubiner_Triangle1_(order, xij, refTriangle, ans, &
                                            nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! shape functions, `ans(i, j)` is function `j` at point `i`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `(order + 1) * (order + 2) / 2`
  END SUBROUTINE Dubiner_Triangle1_
END INTERFACE Dubiner_Triangle_

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. The modes run in the order
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL OrthogonalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle1_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the Dubiner basis on a grid of collapsed coordinates.
!
! Evaluates all Dubiner basis functions of a triangle up to degree `order`.
! The points form a tensor grid built from `x` and `y`.
! The coordinates are the collapsed (square) coordinates of the triangle.
! They are not the `x` and `y` coordinates of triangle points.
!
! Returns:
!   ans : REAL(DFP) - shape functions, `ans(i, j)` is function `j` at
!   point `i`
!
! Formula:
!   $$ \phi_{pq} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b), \quad p + q \le n $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/Dubiner_Triangle_test_2.F90)
!
! See also: [[Dubiner_Triangle1]], [[Dubiner_Triangle2_]]

INTERFACE Dubiner_Triangle
  MODULE PURE FUNCTION Dubiner_Triangle2(order, x, y, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial space
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! collapsed coordinates in the first and second direction,
    !! the number of points is `SIZE(x) * SIZE(y)`
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of the coordinates, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(x) * SIZE(y), (order + 1) * (order + 2) / 2)
    !! shape functions, `ans(i, j)` is function `j` at point `i`
  END FUNCTION Dubiner_Triangle2
END INTERFACE Dubiner_Triangle

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. The modes run in the order
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! y = OrthogonalBasis_Triangle(order=1_I4B, xij=points, refTriangle="UNIT")
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle
  MODULE PROCEDURE Dubiner_Triangle2
END INTERFACE OrthogonalBasis_Triangle

!----------------------------------------------------------------------------
!                                                       DubinerPolynomial
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the Dubiner basis on a grid of
! collapsed coordinates.
!
! Evaluates all Dubiner basis functions of a triangle up to degree `order`.
! The points form a tensor grid built from `x` and `y`.
! The coordinates are the collapsed (square) coordinates of the triangle.
! They are not the `x` and `y` coordinates of triangle points.
! The routine does not allocate `ans`.
!
! @note
! `ans` must have at least `SIZE(x) * SIZE(y)` rows and
! `(order + 1) * (order + 2) / 2` columns.
! @endnote
!
! Formula:
!   $$ \phi_{pq} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b), \quad p + q \le n $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/Dubiner_Triangle_test_2.F90)
!
! See also: [[Dubiner_Triangle2]], [[Dubiner_Triangle1_]]

INTERFACE Dubiner_Triangle_
  MODULE PURE SUBROUTINE Dubiner_Triangle2_(order, x, y, refTriangle, ans, &
                                            nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the polynomial space
    REAL(DFP), INTENT(IN) :: x(:), y(:)
    !! collapsed coordinates in the first and second direction,
    !! the number of points is `SIZE(x) * SIZE(y)`
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of the coordinates, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! shape functions, `ans(i, j)` is function `j` at point `i`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(x) * SIZE(y)`
    !! `ncol` is `(order + 1) * (order + 2) / 2`
  END SUBROUTINE Dubiner_Triangle2_
END INTERFACE Dubiner_Triangle_

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate OrthogonalBasis on a reference triangle.
!
! Coordinates are point columns; outputs use point rows, basis columns and a
! final derivative direction axis when present. The example uses interior UNIT
! points to avoid the collapsed coordinate apex. OrthogonalBasis aliases
! Dubiner. The separate x,y overload forms a tensor grid in collapsed square
! coordinates, not a paired point list. The modes run in the order
! (0,0),(0,1),...,(1,0),... . The gradient implementation uses UNIT chain
! factors even for BIUNIT input, so BIUNIT gradient scaling requires review.
!
! Arguments:
!   Arguments follow the selected specific procedure; see declarations below.
!
! Returns:
!   Same result as the aliased specific procedure.
!
! Formula:
! $$\phi_{pq}=P_p(a)((1-b)/2)^p P_q^{(2p+1,0)}(b),\quad p+q\le n$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE TriangleInterpolationUtility, ONLY: OrthogonalBasis_Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: points(2,2), y(2,3)
! INTEGER(I4B) :: nr,nc
!
! points(:,1) = [0.2_DFP,0.2_DFP]
! points(:,2) = [0.1_DFP,0.3_DFP]
! CALL OrthogonalBasis_Triangle_(order=1_I4B, xij=points, &
!   refTriangle="UNIT", ans=y, nrow=nr,ncol=nc)
! CALL OK(MAXVAL(ABS(y(:,1)-1.0_DFP)) < 1.0E-10_DFP, &
!   "Constant Dubiner mode")
! END PROGRAM main
!```

INTERFACE OrthogonalBasis_Triangle_
  MODULE PROCEDURE Dubiner_Triangle2_
END INTERFACE OrthogonalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the three vertex basis functions of a triangle.
!
! Evaluates the vertex basis functions at the given points.
! Each vertex function equals the barycentric coordinate of that vertex.
! It is 1 at its own vertex and 0 at the other two.
! The three functions sum to 1 at every point.
!
! Returns:
!   ans : REAL(DFP) - vertex basis values, `SIZE(ans, 1)` is the number
!   of points and `SIZE(ans, 2)` is 3
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad i = 1, 2, 3 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/VertexBasis_Triangle_test_1.F90)
!
! See also: [[HeirarchicalBasis_Triangle]]

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Triangle(xij, refTriangle) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), 3)
    !! `ans(:, i)` is the function of vertex `i` at all points
  END FUNCTION VertexBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        EdgeBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the edge basis functions of a triangle.
!
! Evaluates the edge modes of the hierarchical basis of a triangle at the
! given points.
! Edge 1 has `pe1 - 1` modes, edge 2 has `pe2 - 1` modes and edge 3 has
! `pe3 - 1` modes.
! The columns of `ans` hold the modes of edge 1, then edge 2, then edge 3.
!
! Edge 1 joins vertices 1 and 2. Edge 2 joins vertices 2 and 3.
! Edge 3 joins vertices 3 and 1.
! Each mode is the product of the barycentric coordinates of the two end
! vertices and a Lobatto kernel function.
!
! The orders must be at least 1. An edge of order 1 has no mode.
! This function uses the default orientation of the edges.
!
! Returns:
!   ans : REAL(DFP) - edge modes, `ans(i, j)` is mode `j` at point `i`
!
! Formula:
!   $$ \phi_k = \lambda_i \lambda_j \, \psi_{k-1}(\lambda_j - \lambda_i),
!   \quad k = 1, \ldots, p - 1 $$
!   Here `p` is the order of the edge.
!   `\lambda_i` and `\lambda_j` are the barycentric coordinates of its end
!   vertices.
!   `\psi` is the Lobatto kernel.
!
! Examples
!
! - [test 1](|base|Triangle/examples/EdgeBasis_Triangle_test_1.F90)
!
! See also: [[CellBasis_Triangle]], [[VertexBasis_Triangle]],
! [[HeirarchicalBasis_Triangle3_]]

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Triangle(pe1, pe2, pe3, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge 1, from vertex 1 to vertex 2, at least 1
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge 2, from vertex 2 to vertex 3, at least 1
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge 3, from vertex 3 to vertex 1, at least 1
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), pe1 + pe2 + pe3 - 3)
    !! edge modes, `ans(i, j)` is mode `j` at point `i`
  END FUNCTION EdgeBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      CellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the cell basis functions of a triangle.
!
! Evaluates the cell modes of the hierarchical basis of a triangle at the
! given points.
! The basis has `(order - 1) * (order - 2) / 2` modes.
! Each mode is the product of the three barycentric coordinates and two
! Lobatto kernel functions.
! A cell mode is zero on all three edges.
!
! Cell modes exist only if `order` is greater than 2.
! For a smaller `order`, `ans` has no columns.
! This function uses the default orientation of the cell.
!
! Returns:
!   ans : REAL(DFP) - cell modes, `ans(i, j)` is mode `j` at point `i`
!
! Formula:
!   $$ \phi_{jk} = \lambda_1 \lambda_2 \lambda_3 \,
!   \psi_{j-1}(s) \, \psi_{k-1}(t), \quad j, k \ge 1, \quad j + k \le n - 1 $$
!   Here `n` is `order`, `\lambda_i` are the barycentric coordinates,
!   `\psi` is the Lobatto kernel, and `s` and `t` are differences of two
!   barycentric coordinates.
!
! Examples
!
! - [test 1](|base|Triangle/examples/CellBasis_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/CellBasis_Triangle_test_2.F90) with
!   order 5, compared with the full basis
!
! See also: [[EdgeBasis_Triangle]], [[HeirarchicalBasis_Triangle3_]]

INTERFACE
  MODULE PURE FUNCTION CellBasis_Triangle(order, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell, cell modes need `order > 2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), INT((order - 1) * (order - 2) / 2))
    !! cell modes, `ans(i, j)` is mode `j` at point `i`
  END FUNCTION CellBasis_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the hierarchical basis of a triangle with edge orders.
!
! Evaluates the hierarchical basis of a triangle at the given points.
! The edges can have different orders `pe1`, `pe2` and `pe3`.
! The cell has order `order`.
!
! The columns of `ans` are, in this order:
!
! 1. the three vertex modes, which are the barycentric coordinates;
! 2. the modes of edge 1, then edge 2, then edge 3;
! 3. the cell modes.
!
! Edge 1 has `pe1 - 1` modes, edge 2 has `pe2 - 1` modes and edge 3 has
! `pe3 - 1` modes.
! The cell has `(order - 1) * (order - 2) / 2` modes, and none if `order`
! is 2 or less.
! The total is `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)`.
!
! This function uses the default orientation of edges and cell.
! Use [[HeirarchicalBasis_Triangle3_]] to set the orientation.
!
! Returns:
!   ans : REAL(DFP) - basis functions, `ans(i, j)` is function `j` at
!   point `i`
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_i \phi_{v_i} = 1 $$
!   Here `\lambda_i` are the barycentric coordinates and `\phi_{v_i}` are
!   the vertex modes.
!
! Examples
!
! - [test 1](|base|Triangle/examples/HeirarchicalBasis_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/HeirarchicalBasis_Triangle_test_2.F90)
!   with mixed edge orders
!
! See also: [[HeirarchicalBasis_Triangle2]],
! [[HeirarchicalBasis_Triangle1_]], [[EdgeBasis_Triangle]],
! [[CellBasis_Triangle]]

INTERFACE HeirarchicalBasis_Triangle
  MODULE PURE FUNCTION HeirarchicalBasis_Triangle1(order, pe1, pe2, pe3, &
                                                 xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell
    !! cell modes need `order > 2`
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge 1, from vertex 1 to vertex 2, at least 1
    !! edge 1 modes need `pe1 > 1`
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge 2, from vertex 2 to vertex 3, at least 1
    !! edge 2 modes need `pe2 > 1`
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge 3, from vertex 3 to vertex 1, at least 1
    !! edge 3 modes need `pe3 > 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !! basis functions, `ans(i, j)` is function `j` at point `i`
    !! vertex modes come first, then edge modes, then cell modes
  END FUNCTION HeirarchicalBasis_Triangle1
END INTERFACE HeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the hierarchical basis of a triangle of order `order`.
!
! Evaluates the hierarchical basis of a triangle at the given points.
! The three edges and the cell all have order `order`.
!
! The columns of `ans` are, in this order:
!
! 1. the three vertex modes, which are the barycentric coordinates;
! 2. the modes of edge 1, then edge 2, then edge 3;
! 3. the cell modes.
!
! Each edge has `order - 1` modes.
! The cell has `(order - 1) * (order - 2) / 2` modes, and none if `order`
! is 2 or less.
! The total is `3 * order + INT((order - 1) * (order - 2) / 2)`.
!
! This function uses the default orientation of edges and cell.
! Use [[HeirarchicalBasis_Triangle3_]] to set the orientation.
!
! Returns:
!   ans : REAL(DFP) - basis functions, `ans(i, j)` is function `j` at
!   point `i`
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_i \phi_{v_i} = 1 $$
!   Here `\lambda_i` are the barycentric coordinates and `\phi_{v_i}` are
!   the vertex modes.
!
! Examples
!
! - [test 2](|base|Triangle/examples/HeirarchicalBasis_Triangle_test_2.F90)
!   compared with the overload that has edge orders
!
! See also: [[HeirarchicalBasis_Triangle1]],
! [[HeirarchicalBasis_Triangle2_]]

INTERFACE HeirarchicalBasis_Triangle
  MODULE PURE FUNCTION HeirarchicalBasis_Triangle2(order, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell and on all edges
    !! cell modes need `order > 2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     3 * order + INT((order - 1) * (order - 2) / 2))
    !! basis functions, `ans(i, j)` is function `j` at point `i`
    !! vertex modes come first, then edge modes, then cell modes
  END FUNCTION HeirarchicalBasis_Triangle2
END INTERFACE HeirarchicalBasis_Triangle

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the hierarchical basis of a
! triangle with edge orders.
!
! Evaluates the hierarchical basis of a triangle at the given points.
! It gives the same values as [[HeirarchicalBasis_Triangle1]].
! The routine does not allocate `ans`.
! It returns the size of the part of `ans` that it fills in `nrow` and
! `ncol`.
!
! The columns of `ans` are the vertex modes, then the modes of edge 1, 2
! and 3, then the cell modes.
! This routine uses the default orientation of edges and cell.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and
! `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)` columns.
! @endnote
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_i \phi_{v_i} = 1 $$
!   Here `\lambda_i` are the barycentric coordinates and `\phi_{v_i}` are
!   the vertex modes.
!
! Examples
!
! - [test 1](|base|Triangle/examples/HeirarchicalBasis_Triangle__test_1.F90)
!
! See also: [[HeirarchicalBasis_Triangle1]],
! [[HeirarchicalBasis_Triangle2_]], [[HeirarchicalBasis_Triangle3_]]

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle1_( &
    order, pe1, pe2, pe3, xij, refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell
    !! cell modes need `order > 2`
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge 1, from vertex 1 to vertex 2, at least 1
    !! edge 1 modes need `pe1 > 1`
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge 2, from vertex 2 to vertex 3, at least 1
    !! edge 2 modes need `pe2 > 1`
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge 3, from vertex 3 to vertex 1, at least 1
    !! edge 3 modes need `pe3 > 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !! basis functions, `ans(i, j)` is function `j` at point `i`
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)`
  END SUBROUTINE HeirarchicalBasis_Triangle1_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the hierarchical basis of a
! triangle of order `order`.
!
! Evaluates the hierarchical basis of a triangle at the given points.
! The three edges and the cell all have order `order`.
! It gives the same values as [[HeirarchicalBasis_Triangle2]].
! The routine does not allocate `ans`.
! It returns the size of the part of `ans` that it fills in `nrow` and
! `ncol`.
!
! The columns of `ans` are the vertex modes, then the modes of edge 1, 2
! and 3, then the cell modes.
! This routine uses the default orientation of edges and cell.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and
! `3 * order + INT((order - 1) * (order - 2) / 2)` columns.
! @endnote
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_i \phi_{v_i} = 1 $$
!   Here `\lambda_i` are the barycentric coordinates and `\phi_{v_i}` are
!   the vertex modes.
!
! Examples
!
! - [test 2](|base|Triangle/examples/HeirarchicalBasis_Triangle__test_2.F90)
!   compared with the function overload
!
! See also: [[HeirarchicalBasis_Triangle2]],
! [[HeirarchicalBasis_Triangle1_]], [[HeirarchicalBasis_Triangle3_]]

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle2_( &
    order, xij, refTriangle, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell and on all edges
    !! cell modes need `order > 2`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & order*3 + INT((order - 1) * (order - 2) / 2))
    !! basis functions, `ans(i, j)` is function `j` at point `i`
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `3 * order + INT((order - 1) * (order - 2) / 2)`
  END SUBROUTINE HeirarchicalBasis_Triangle2_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                              HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the hierarchical basis of a
! triangle with oriented edges and cell.
!
! Evaluates the hierarchical basis of a triangle at the given points.
! The edges can have different orders `pe1`, `pe2` and `pe3`.
! The cell has order `order`.
! The routine does not allocate `ans`.
! It returns the size of the part of `ans` that it fills in `nrow` and
! `ncol`.
!
! The columns of `ans` are the vertex modes, then the modes of edge 1, 2
! and 3, then the cell modes.
!
! The orientation arguments change the signs of the modes.
! This lets neighbouring elements share the same edge and cell modes.
! An edge orientation of -1 reverses the direction of the edge.
! It changes the sign of every second mode of that edge.
! Use 1 for the default direction.
!
! `faceOrient(1)` is a rotation of the cell, 0, 1 or 2.
! `faceOrient(2)` is 1 or -1, and -1 reflects the cell.
! Use `[0, 1]` for the default orientation.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows and
! `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)` columns.
! @endnote
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_i \phi_{v_i} = 1 $$
!   Here `\lambda_i` are the barycentric coordinates and `\phi_{v_i}` are
!   the vertex modes.
!
! Examples
!
! - [test 2](|base|Triangle/examples/HeirarchicalBasis_Triangle__test_2.F90)
!   with positive and reversed edge orientation
!
! See also: [[HeirarchicalBasis_Triangle1_]],
! [[HeirarchicalBasis_Triangle2_]]

INTERFACE HeirarchicalBasis_Triangle_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Triangle3_( &
    order, pe1, pe2, pe3, xij, refTriangle, edgeOrient1, edgeOrient2, &
    edgeOrient3, faceOrient, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation in the cell
    !! cell modes need `order > 2`
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on edge 1, from vertex 1 to vertex 2, at least 1
    !! edge 1 modes need `pe1 > 1`
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge 2, from vertex 2 to vertex 3, at least 1
    !! edge 2 modes need `pe2 > 1`
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge 3, from vertex 3 to vertex 1, at least 1
    !! edge 3 modes need `pe3 > 1`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `xij`, "UNIT" or "BIUNIT"
    INTEGER(I4B), INTENT(IN) :: edgeOrient1, edgeOrient2, edgeOrient3
    !! orientation of edge 1, edge 2 and edge 3, 1 or -1
    !! 1 is the default direction
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! orientation of the cell, `SIZE(faceOrient)` is 2.
    !! `faceOrient(1)` is the rotation, 0, 1 or 2.
    !! `faceOrient(2)` is 1 or -1, and -1 reflects the cell.
    !! `[0, 1]` is the default.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans( &
    !   & SIZE(xij, 2), &
    !   & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2))
    !! basis functions, `ans(i, j)` is function `j` at point `i`
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is `SIZE(xij, 2)`
    !! `ncol` is `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)`
  END SUBROUTINE HeirarchicalBasis_Triangle3_
END INTERFACE HeirarchicalBasis_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns all Lagrange polynomials of a triangle at one point.
!
! Evaluates all Lagrange polynomials of a triangle at the point `x`.
! The result `ans(j)` is the value of the polynomial of node `j`.
!
! The Lagrange polynomial of node `j` is 1 at node `j` and 0 at the other
! nodes.
! The polynomials have degree `order`.
! `xij` holds the nodes, one per column.
! It needs `(order + 1) * (order + 2) / 2` columns.
!
! `basisType` selects the basis that builds the polynomials.
! The default is `TypePolynomialOpt%Monomial`.
! `TypePolynomialOpt%Hierarchical` uses the hierarchical basis.
! The Jacobi, Orthogonal, Legendre, Lobatto and Ultraspherical types use
! the Dubiner basis.
! The values of the Lagrange polynomials do not depend on this choice.
!
! The coefficients of the polynomials are costly to compute.
! If `coeff` is present and `firstCall` is `.TRUE.`, the routine computes
! them and stores them in `coeff`.
! If `firstCall` is `.FALSE.`, the routine reads them from `coeff`.
! If `coeff` is absent, the routine computes them in every call.
! The default of `firstCall` is `.TRUE.`.
!
! @note
! With `firstCall` set to `.FALSE.`, `coeff` must come from an earlier
! call. That call must use the same `order`, `xij`, `refTriangle` and
! `basisType`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - values at `x`, `ans(j)` is the polynomial of node `j`
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j = 1 $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeEvalAll_Triangle_test_2.F90)
!   with one point, the delta property and reuse of `coeff`
!
! See also: [[LagrangeEvalAll_Triangle2]], [[LagrangeEvalAll_Triangle1_]]

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle1( &
    order, x, xij, refTriangle, coeff, firstCall, basisType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! `x(1)` is the x coordinate
    !! `x(2)` is the y coordinate
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! nodes of the Lagrange polynomials, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of nodes
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `x` and `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! `SIZE(coeff, 1)` and `SIZE(coeff, 2)` are `SIZE(xij, 2)`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine reads `coeff`
    !! default is `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis that builds the polynomials
    !! default is `TypePolynomialOpt%Monomial`
    !! see the description for the other values
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! `ans(j)` is the Lagrange polynomial of node `j` at `x`
  END FUNCTION LagrangeEvalAll_Triangle1
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with all Lagrange polynomials of a
! triangle at one point.
!
! Evaluates all Lagrange polynomials of a triangle at the point `x`.
! It gives the same values as [[LagrangeEvalAll_Triangle1]].
! The routine does not allocate `ans`.
! It returns the number of values written in `tsize`.
!
! The Lagrange polynomial of node `j` is 1 at node `j` and 0 at the other
! nodes.
! The polynomials have degree `order`.
! `xij` holds the nodes, one per column.
! It needs `(order + 1) * (order + 2) / 2` columns.
!
! `basisType` selects the basis that builds the polynomials.
! The default is `TypePolynomialOpt%Monomial`.
! `TypePolynomialOpt%Hierarchical` uses the hierarchical basis.
! The Jacobi, Orthogonal, Legendre, Lobatto and Ultraspherical types use
! the Dubiner basis.
! The values of the Lagrange polynomials do not depend on this choice.
!
! The coefficients of the polynomials are costly to compute.
! If `coeff` is present and `firstCall` is `.TRUE.`, the routine computes
! them and stores them in `coeff`.
! If `firstCall` is `.FALSE.`, the routine reads them from `coeff`.
! If `coeff` is absent, the routine computes them in every call.
! The default of `firstCall` is `.TRUE.`.
!
! @note
! With `firstCall` set to `.FALSE.`, `coeff` must come from an earlier
! call. That call must use the same `order`, `xij`, `refTriangle` and
! `basisType`.
! @endnote
!
! @note
! `ans` must have at least `SIZE(xij, 2)` entries.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j = 1 $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeEvalAll_Triangle__test_2.F90)
!   with one point and a user workspace for `coeff`
!
! See also: [[LagrangeEvalAll_Triangle1]]

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle1_( &
    order, x, xij, ans, tsize, refTriangle, coeff, firstCall, basisType)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(2)
    !! point of evaluation
    !! `x(1)` is the x coordinate
    !! `x(2)` is the y coordinate
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! nodes of the Lagrange polynomials, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of nodes
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `x` and `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! values at `x`, `ans(j)` is the polynomial of node `j`
    !! the routine writes only `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in `ans`, `SIZE(xij, 2)`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials
    !! the routine uses `coeff(SIZE(xij, 2), SIZE(xij, 2))`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine reads `coeff`
    !! default is `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis that builds the polynomials
    !! default is `TypePolynomialOpt%Monomial`
    !! see the description for the other values
  END SUBROUTINE LagrangeEvalAll_Triangle1_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!                                                   LagrangeEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns all Lagrange polynomials of a triangle at many points.
!
! Evaluates all Lagrange polynomials of a triangle at the points `x`.
! The result `ans(i, j)` is the value of the polynomial of node `j` at
! point `i`.
!
! The Lagrange polynomial of node `j` is 1 at node `j` and 0 at the other
! nodes.
! The polynomials have degree `order`.
! `xij` holds the nodes, one per column.
! It needs `(order + 1) * (order + 2) / 2` columns.
!
! `basisType` selects the basis that builds the polynomials.
! The default is `TypePolynomialOpt%Monomial`.
! `TypePolynomialOpt%Hierarchical` uses the hierarchical basis.
! The Jacobi, Orthogonal, Legendre, Lobatto and Ultraspherical types use
! the Dubiner basis.
! The values of the Lagrange polynomials do not depend on this choice.
!
! The coefficients of the polynomials are costly to compute.
! If `coeff` is present and `firstCall` is `.TRUE.`, the routine computes
! them and stores them in `coeff`.
! If `firstCall` is `.FALSE.`, the routine reads them from `coeff`.
! If `coeff` is absent, the routine computes them in every call.
! The default of `firstCall` is `.TRUE.`.
!
! @note
! With `firstCall` set to `.FALSE.`, `coeff` must come from an earlier
! call. That call must use the same `order`, `xij`, `refTriangle` and
! `basisType`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - values at the points, `ans(i, j)` is the polynomial
!   of node `j` at point `i`
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j = 1 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeEvalAll_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/LagrangeEvalAll_Triangle_test_2.F90)
!   with many points and several values of `basisType`
!
! See also: [[LagrangeEvalAll_Triangle1]]

INTERFACE LagrangeEvalAll_Triangle
  MODULE FUNCTION LagrangeEvalAll_Triangle2( &
    order, x, xij, refTriangle, coeff, firstCall, basisType, alpha, beta, &
    lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation, `SIZE(x, 1)` is 2
    !! `x(1, :)` are the x coordinates
    !! `x(2, :)` are the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! nodes of the Lagrange polynomials, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of nodes
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `x` and `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! `SIZE(coeff, 1)` and `SIZE(coeff, 2)` are `SIZE(xij, 2)`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine reads `coeff`
    !! default is `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis that builds the polynomials
    !! default is `TypePolynomialOpt%Monomial`
    !! see the description for the other values
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! Jacobi parameters `alpha` and `beta`, and Ultraspherical parameter
    !! `lambda`, the current implementation does not use them
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! values at the points, `ans(i, j)` is the polynomial of node `j`
    !! at point `i`
  END FUNCTION LagrangeEvalAll_Triangle2
END INTERFACE LagrangeEvalAll_Triangle

!----------------------------------------------------------------------------
!                             LagrangeEvalAll_Triangle_@LagrnageBasisMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with all Lagrange polynomials of a
! triangle at many points.
!
! Evaluates all Lagrange polynomials of a triangle at the points `x`.
! It gives the same values as [[LagrangeEvalAll_Triangle2]].
! The routine does not allocate `ans`.
! It returns the size of the written block in `nrow` and `ncol`.
!
! The Lagrange polynomial of node `j` is 1 at node `j` and 0 at the other
! nodes.
! The polynomials have degree `order`.
! `xij` holds the nodes, one per column.
! It needs `(order + 1) * (order + 2) / 2` columns.
!
! `basisType` selects the basis that builds the polynomials.
! The default is `TypePolynomialOpt%Monomial`.
! `TypePolynomialOpt%Hierarchical` uses the hierarchical basis.
! The Jacobi, Orthogonal, Legendre, Lobatto and Ultraspherical types use
! the Dubiner basis.
! The values of the Lagrange polynomials do not depend on this choice.
!
! The coefficients of the polynomials are costly to compute.
! If `coeff` is present and `firstCall` is `.TRUE.`, the routine computes
! them and stores them in `coeff`.
! If `firstCall` is `.FALSE.`, the routine reads them from `coeff`.
! If `coeff` is absent, the routine computes them in every call.
! The default of `firstCall` is `.TRUE.`.
!
! @note
! With `firstCall` set to `.FALSE.`, `coeff` must come from an earlier
! call. That call must use the same `order`, `xij`, `refTriangle` and
! `basisType`.
! @endnote
!
! @note
! The routine does not use `alpha`, `beta` and `lambda`.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j = 1 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeEvalAll_Triangle__test_1.F90)
!   with order 1
!
! See also: [[LagrangeEvalAll_Triangle2]], [[LagrangeEvalAll_Triangle3_]]

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle2_( &
    order, x, xij, ans, nrow, ncol, refTriangle, coeff, firstCall, &
    basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation, one point per column
    !! `x(1, :)` are the x coordinates
    !! `x(2, :)` are the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! nodes of the Lagrange polynomials, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of nodes
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values at the points, `ans(i, j)` is the polynomial of node `j`
    !! at point `i`
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(x, 2)` and `ncol` is `SIZE(xij, 2)`
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `x` and `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials
    !! the routine uses `coeff(SIZE(xij, 2), SIZE(xij, 2))`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine reads `coeff`
    !! default is `.TRUE.`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis that builds the polynomials
    !! default is `TypePolynomialOpt%Monomial`
    !! see the description for the other values
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha, beta, lambda
    !! not used
  END SUBROUTINE LagrangeEvalAll_Triangle2_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!                                   LagrangeEvalAll_Triangle_@LagrangeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with all Lagrange polynomials of a
! triangle at many points, using workspace arrays from the caller.
!
! Evaluates all Lagrange polynomials of a triangle at the points `x`.
! It gives the same values as [[LagrangeEvalAll_Triangle2_]].
! The routine does not allocate any array.
! The caller supplies the workspace arrays `coeff`, `xx` and `degree`.
! This avoids temporary arrays inside the routine.
! The routine returns the size of the written block in `nrow` and `ncol`.
!
! The Lagrange polynomial of node `j` is 1 at node `j` and 0 at the other
! nodes.
! The polynomials have degree `order`.
! `xij` holds the nodes, one per column.
! It needs `(order + 1) * (order + 2) / 2` columns.
!
! `basisType` selects the basis that builds the polynomials.
! `TypePolynomialOpt%Monomial` uses monomials.
! `TypePolynomialOpt%Hierarchical` uses the hierarchical basis.
! The Jacobi, Orthogonal, Legendre, Lobatto and Ultraspherical types use
! the Dubiner basis.
! The values of the Lagrange polynomials do not depend on this choice.
!
! The coefficients of the polynomials are costly to compute.
! If `firstCall` is `.TRUE.`, the routine computes them and stores them
! in `coeff`.
! If `firstCall` is `.FALSE.`, the routine reads them from `coeff`.
!
! @note
! With `firstCall` set to `.FALSE.`, `coeff` must come from an earlier
! call. That call must use the same `order`, `xij`, `refTriangle` and
! `basisType`.
! @endnote
!
! @note
! `firstCall` and `basisType` have no default value here.
! Always pass them.
! @endnote
!
! Formula:
!   $$ \ell_j(x_i) = \delta_{ij}, \quad \sum_j \ell_j = 1 $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/LagrangeEvalAll_Triangle__test_2.F90)
!   with user workspace and reuse of `coeff`
!
! See also: [[LagrangeEvalAll_Triangle2_]]

INTERFACE LagrangeEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeEvalAll_Triangle3_( &
    order, x, xij, ans, nrow, ncol, refTriangle, coeff, firstCall, &
    basisType, xx, degree)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation, one point per column
    !! `x(1, :)` are the x coordinates
    !! `x(2, :)` are the y coordinates
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! nodes of the Lagrange polynomials, `SIZE(xij, 1)` is 2
    !! and `SIZE(xij, 2)` is the number of nodes
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values at the points, `ans(i, j)` is the polynomial of node `j`
    !! at point `i`
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `SIZE(x, 2)` and `ncol` is `SIZE(xij, 2)`
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle of `x` and `xij`, "UNIT" or "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials
    !! the routine uses `coeff(SIZE(xij, 2), SIZE(xij, 2))`
    LOGICAL(LGT) :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine reads `coeff`
    INTEGER(I4B), INTENT(IN) :: basisType
    !! basis that builds the polynomials, see the description
    REAL(DFP), INTENT(INOUT) :: xx(:, :)
    !! workspace, `xx(SIZE(x, 2), SIZE(xij, 2))`
    INTEGER(I4B) :: degree(:, :)
    !! workspace, `degree(SIZE(xij, 2), 2)`
    !! the routine uses it only for the monomial basis
  END SUBROUTINE LagrangeEvalAll_Triangle3_
END INTERFACE LagrangeEvalAll_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of quadrature points of a triangle rule for
! integrands of a given polynomial degree.
!
! Returns the number of points that [[QuadraturePoint_Triangle]] uses for
! polynomials of degree `order`.
!
! 1. For `order` from 1 to 20, it returns the point count of the
!    economical rule of Solin. The count is 1 for order 1 and 79 for
!    order 20.
! 2. For any other `order`, it returns the point count of the
!    tensor-product rule.
!    Let `n` be `1 + order / 2`, with integer division.
!    The count is `n * (n + 1)`.
!
! Returns:
!   ans : INTEGER(I4B) - number of quadrature points
!
! Examples
!
! - [test 1](|base|Triangle/examples/QuadratureNumber_Triangle_test_1.F90)
!
! See also: [[QuadraturePoint_Triangle]]

INTERFACE
  MODULE FUNCTION QuadratureNumber_Triangle(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    INTEGER(I4B) :: ans
    !! number of quadrature points
  END FUNCTION QuadratureNumber_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 QuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a quadrature rule of a triangle for integrands of a
! given polynomial degree.
!
! Returns a quadrature rule for polynomials of degree `order`.
! The routine picks the rule from `order`.
!
! 1. For `order` from 1 to 20, it uses an economical rule from the tables
!    of Solin. [[QuadratureNumber_Triangle]] gives its number of points.
! 2. For any other `order`, it uses a tensor-product rule.
!    See [[TensorQuadraturePoint_Triangle1]].
!
! The routine allocates `ans`.
! The last row of `ans` holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
! The weights add up to the area of that triangle.
!
! Formula:
!   $$ \int_K f(x)\,dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/QuadraturePoint_Triangle_test_2.F90)
!   with orders 2 and 22
! - [test 3](|base|Triangle/examples/QuadraturePoint_Triangle_test_3.F90)
!   with orders 25 and 0
!
! See also: [[QuadraturePoint_Triangle1_]], [[QuadraturePoint_Triangle2]]

INTERFACE QuadraturePoint_Triangle
  MODULE FUNCTION QuadraturePoint_Triangle1(order, quadType, refTriangle, &
                                            xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! `ans(1:nsd, :)` are the coordinates
    !! `ans(nsd + 1, :)` are the weights
    !! `nsd` is `SIZE(xij, 1)`, or 2 if `xij` is absent
  END FUNCTION QuadraturePoint_Triangle1
END INTERFACE QuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with a quadrature rule of a triangle
! for integrands of a given polynomial degree.
!
! Writes a quadrature rule for polynomials of degree `order` into `ans`.
! The routine picks the rule from `order`.
!
! 1. For `order` from 1 to 20, it uses an economical rule from the tables
!    of Solin. [[QuadratureNumber_Triangle]] gives its number of points.
! 2. For any other `order`, it uses a tensor-product rule.
!    See [[TensorQuadraturePoint_Triangle1_]].
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `nrow` and `ncol`.
! The last row holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
! The weights add up to the area of that triangle.
!
! @note
! `ans` must have at least `nsd + 1` rows and
! `QuadratureNumber_Triangle(order, quadType)` columns.
! Here `nsd` is `SIZE(xij, 1)`, or 2 if `xij` is absent.
! @endnote
!
! Formula:
!   $$ \int_K f(x)\,dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 3](|base|Triangle/examples/QuadraturePoint_Triangle_test_3.F90)
!   with order 25
!
! See also: [[QuadraturePoint_Triangle1]], [[QuadraturePoint_Triangle2_]]

INTERFACE QuadraturePoint_Triangle_
  MODULE SUBROUTINE QuadraturePoint_Triangle1_(order, quadType, refTriangle, &
                                               xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
  END SUBROUTINE QuadraturePoint_Triangle1_
END INTERFACE QuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns an economical quadrature rule of a triangle that has a
! given number of points.
!
! Returns a quadrature rule with `nips(1)` points.
! The rule comes from the tables of Solin.
! These tables hold rules with up to 79 points.
! The point counts are those that [[QuadratureNumber_Triangle]] returns
! for the orders 1 to 20.
!
! The routine allocates `ans`.
! The last row of `ans` holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
! The weights add up to the area of that triangle.
!
! @note
! Builds compiled with `DEBUG_VER` report an error if `nips(1)` is
! larger than 79.
! @endnote
!
! Formula:
!   $$ \int_K f(x)\,dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/QuadraturePoint_Triangle_test_1.F90)
!   with `nips` given
!
! See also: [[QuadraturePoint_Triangle2_]], [[QuadraturePoint_Triangle1]]

INTERFACE QuadraturePoint_Triangle
  MODULE FUNCTION QuadraturePoint_Triangle2(nips, quadType, refTriangle, &
                                            xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points, at most 79
    !! it must be a point count of the Solin tables
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! `ans(1:nsd, :)` are the coordinates
    !! `ans(nsd + 1, :)` are the weights
    !! `nsd` is `SIZE(xij, 1)`, or 2 if `xij` is absent
  END FUNCTION QuadraturePoint_Triangle2
END INTERFACE QuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with an economical quadrature rule
! of a triangle that has a given number of points.
!
! Writes a quadrature rule with `nips(1)` points into `ans`.
! The rule comes from the tables of Solin.
! These tables hold rules with up to 79 points.
! The point counts are those that [[QuadratureNumber_Triangle]] returns
! for the orders 1 to 20.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `nrow` and `ncol`.
! The last row holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
! The weights add up to the area of that triangle.
!
! @note
! `ans` must have at least `nsd + 1` rows and `nips(1)` columns.
! Here `nsd` is `SIZE(xij, 1)`, or 2 if `xij` is absent.
! @endnote
!
! @note
! Builds compiled with `DEBUG_VER` report an error if `nips(1)` is
! larger than 79.
! @endnote
!
! Formula:
!   $$ \int_K f(x)\,dx \simeq \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/QuadraturePoint_Triangle_test_1.F90)
!   with `nips` given
!
! See also: [[QuadraturePoint_Triangle2]], [[QuadraturePoint_Triangle1_]]

INTERFACE QuadraturePoint_Triangle_
  MODULE SUBROUTINE QuadraturePoint_Triangle2_(nips, quadType, refTriangle, &
                                               xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points, at most 79
    !! it must be a point count of the Solin tables
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `nsd + 1` and `ncol` is `nips(1)`
  END SUBROUTINE QuadraturePoint_Triangle2_
END INTERFACE QuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Returns tensor-product quadrature points of a triangle that
! integrate polynomials of a given order.
!
! Returns a tensor-product quadrature rule for integrands that are
! polynomials of degree `order`.
! Let `n` be `1 + order / 2`, with integer division.
! The rule has `n + 1` points in the x direction and `n` points in the y
! direction.
! The total is `n * (n + 1)` points.
!
! The routine allocates `ans`.
! The last row of `ans` holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Triangle2]]

INTERFACE TensorQuadraturePoint_Triangle
  MODULE FUNCTION TensorQuadraturePoint_Triangle1(order, quadType, &
    & refTriangle, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! `ans(1:nsd, :)` are the coordinates
    !! `ans(nsd + 1, :)` are the weights
    !! `nsd` is `MAX(SIZE(xij, 1), 2)`, or 2 if `xij` is absent
  END FUNCTION TensorQuadraturePoint_Triangle1
END INTERFACE TensorQuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Fills a preallocated array with tensor-product quadrature
! points of a triangle that integrate polynomials of a given order.
!
! Writes a tensor-product quadrature rule into `ans`.
! The rule suits integrands that are polynomials of degree `order`.
! Let `n` be `1 + order / 2`, with integer division.
! The rule has `n + 1` points in the x direction and `n` points in the y
! direction.
! The total is `n * (n + 1)` points.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `nrow` and `ncol`.
! The last written row holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
!
! @note
! `ans` must have at least `nsd + 1` rows and `n * (n + 1)` columns.
! Here `nsd` is `MAX(SIZE(xij, 1), 2)`, or 2 if `xij` is absent.
! @endnote
!
! See also: [[TensorQuadraturePoint_Triangle1]]

INTERFACE TensorQuadraturePoint_Triangle_
  MODULE SUBROUTINE TensorQuadraturePoint_Triangle1_(order, quadType, &
                                            refTriangle, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `nsd + 1` and `ncol` is `n * (n + 1)`
  END SUBROUTINE TensorQuadraturePoint_Triangle1_
END INTERFACE TensorQuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!                                            TensorQuadraturePoints_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Returns tensor-product quadrature points of a triangle, given
! the point counts in each direction.
!
! Returns a quadrature rule with `nipsx(1) * nipsy(1)` points.
! The rule is a tensor-product rule collapsed onto the triangle.
! It uses `nipsx(1)` points in the x direction and `nipsy(1)` points in
! the y direction.
!
! The routine allocates `ans`.
! The last row of `ans` holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Triangle1]]

INTERFACE TensorQuadraturePoint_Triangle
  MODULE FUNCTION TensorQuadraturePoint_Triangle2(nipsx, nipsy, quadType, &
     & refTriangle, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in the x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in the y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! `ans(1:nsd, :)` are the coordinates
    !! `ans(nsd + 1, :)` are the weights
    !! `nsd` is `MAX(SIZE(xij, 1), 2)`, or 2 if `xij` is absent
  END FUNCTION TensorQuadraturePoint_Triangle2
END INTERFACE TensorQuadraturePoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Fills a preallocated array with tensor-product quadrature
! points of a triangle, given the point counts in each direction.
!
! Writes a quadrature rule with `nipsx(1) * nipsy(1)` points into `ans`.
! The rule is a tensor-product rule collapsed onto the triangle.
! It uses `nipsx(1)` points in the x direction and `nipsy(1)` points in
! the y direction.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `nrow` and `ncol`.
! The last written row holds the weights.
! The rows above it hold the coordinates of the points.
! Each column is one point.
!
! If `xij` is present, the points lie in the triangle with the vertices
! `xij(:, 1)`, `xij(:, 2)` and `xij(:, 3)`.
! The weights then include the area of that triangle.
! In this case, `refTriangle` has no effect.
!
! If `xij` is absent, the points lie in the reference triangle.
! `refTriangle` selects it: `"BIUNIT"` or `"UNIT"`.
! Any name that does not start with `B` or `b` selects `"UNIT"`.
!
! @note
! `ans` must have at least `nsd + 1` rows and `nipsx(1) * nipsy(1)`
! columns.
! Here `nsd` is `MAX(SIZE(xij, 1), 2)`, or 2 if `xij` is absent.
! @endnote
!
! See also: [[TensorQuadraturePoint_Triangle2]],
! [[TensorQuadraturePoint_Triangle1_]]

INTERFACE TensorQuadraturePoint_Triangle_
  MODULE SUBROUTINE TensorQuadraturePoint_Triangle2_(nipsx, nipsy, quadType, &
                                            refTriangle, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in the x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in the y direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! not used if `xij` is present
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the triangle, one vertex per column
    !! `SIZE(xij, 1)` is the number of space dimensions
    !! `SIZE(xij, 2)` is 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights, one point per column
    !! the routine writes only `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in `ans`
    !! `nrow` is `nsd + 1` and `ncol` is `nipsx(1) * nipsy(1)`
  END SUBROUTINE TensorQuadraturePoint_Triangle2_
END INTERFACE TensorQuadraturePoint_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradients of all Lagrange polynomials of a
! triangle at many points.
!
! Returns the gradients of the Lagrange polynomials of degree `order`.
! The polynomials are the nodal basis of the nodes `xij`.
! The routine evaluates them at every point of `x`.
!
! `basisType` selects the polynomial family that builds the basis.
! The default is `poly%Monomial`.
! The other choices are `poly%Hierarchical`, `poly%Jacobi`,
! `poly%Orthogonal`, `poly%Legendre`, `poly%Lobatto` and
! `poly%Ultraspherical`.
! The last five choices all use the orthogonal Dubiner basis.
! The arguments `alpha`, `beta` and `lambda` are currently not used.
!
! The coefficients that turn the basis into Lagrange polynomials depend
! only on `order`, `xij`, `basisType` and `refTriangle`.
! You can keep them in `coeff` and reuse them in later calls.
!
! 1. If `coeff` is absent, the routine computes the coefficients on
!    every call.
! 2. If `coeff` is present and `firstCall` is true, the routine computes
!    the coefficients and stores them in `coeff`.
! 3. If `coeff` is present and `firstCall` is false, the routine reads
!    the coefficients from `coeff`.
!
! @note
! `firstCall` is true by default. Pass `.FALSE.` from the second call on
! to reuse `coeff`.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \ell_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeGradientEvalAll_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/LagrangeGradientEvalAll_Triangle_test_2.F90)
!
! See also: [[LagrangeGradientEvalAll_Triangle1_]]

INTERFACE LagrangeGradientEvalAll_Triangle
  MODULE FUNCTION LagrangeGradientEvalAll_Triangle1( &
    & order, &
    & x, &
    & xij, &
    & refTriangle, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one point per column
    !! `SIZE(x, 1)` is 2
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one node per column
    !! `SIZE(xij, 1)` is 2
    !! `SIZE(xij, 2)` is the number of Lagrange polynomials
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! the routine computes them or reads them, see `firstCall`
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if true, the routine computes `coeff`
    !! if false, the routine reads `coeff`
    !! default is true
    !! it has no effect if `coeff` is absent
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`
    !! default is `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter, currently not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter, currently not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, currently not used
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 2)
    !! gradients of the Lagrange polynomials
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! polynomial `j` at point `i`
  END FUNCTION LagrangeGradientEvalAll_Triangle1
END INTERFACE LagrangeGradientEvalAll_Triangle

!----------------------------------------------------------------------------
!                                          LagrangeGradientEvalAll_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the gradients of all Lagrange
! polynomials of a triangle at many points.
!
! Writes the gradients of the Lagrange polynomials of degree `order`
! into `ans`.
! The polynomials are the nodal basis of the nodes `xij`.
! The routine evaluates them at every point of `x`.
! The routine does not allocate `ans`.
! It returns the size of the written block in `dim1`, `dim2` and `dim3`.
!
! `basisType` selects the polynomial family that builds the basis.
! The default is `poly%Monomial`.
! The other choices are `poly%Hierarchical`, `poly%Jacobi`,
! `poly%Orthogonal`, `poly%Legendre`, `poly%Lobatto` and
! `poly%Ultraspherical`.
! The last five choices all use the orthogonal Dubiner basis.
! The arguments `alpha`, `beta` and `lambda` are currently not used.
!
! The coefficients that turn the basis into Lagrange polynomials depend
! only on `order`, `xij`, `basisType` and `refTriangle`.
! You can keep them in `coeff` and reuse them in later calls.
!
! 1. If `coeff` is absent, the routine computes the coefficients on
!    every call.
! 2. If `coeff` is present and `firstCall` is true, the routine computes
!    the coefficients and stores them in `coeff`.
! 3. If `coeff` is present and `firstCall` is false, the routine reads
!    the coefficients from `coeff`.
!
! @note
! `ans` must have at least `SIZE(x, 2)` rows, `SIZE(xij, 2)` columns and
! 2 pages.
! @endnote
!
! @note
! `firstCall` is true by default. Pass `.FALSE.` from the second call on
! to reuse `coeff`.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \ell_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/LagrangeGradientEvalAll_Triangle__test_1.F90)
!
! See also: [[LagrangeGradientEvalAll_Triangle1]]

INTERFACE LagrangeGradientEvalAll_Triangle_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Triangle1_( &
    order, x, xij, ans, dim1, dim2, dim3, refTriangle, coeff, firstCall, &
    basisType, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, one point per column
    !! `SIZE(x, 1)` is 2
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one node per column
    !! `SIZE(xij, 1)` is 2
    !! `SIZE(xij, 2)` is the number of Lagrange polynomials
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the Lagrange polynomials
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! polynomial `j` at point `i`
    !! the routine writes only `ans(1:dim1, 1:dim2, 1:dim3)`
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! size of the block written in `ans`
    !! `dim1` is `SIZE(x, 2)`, `dim2` is `SIZE(xij, 2)` and `dim3` is 2
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials.
    !! The routine computes or reads them, see `firstCall`.
    !! The leading block must have `SIZE(xij, 2)` rows and columns.
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if true, the routine computes `coeff`.
    !! if false, the routine reads `coeff`.
    !! default is true.
    !! it has no effect if `coeff` is absent.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, for example `poly%Monomial`
    !! default is `poly%Monomial`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter, currently not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter, currently not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! ultraspherical parameter, currently not used
  END SUBROUTINE LagrangeGradientEvalAll_Triangle1_
END INTERFACE LagrangeGradientEvalAll_Triangle_

!----------------------------------------------------------------------------
!                                        HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradients of the hierarchical basis of a triangle.
!
! Returns the gradients of the hierarchical basis functions of a triangle
! at the points `xij`.
! The functions come in this order.
!
! 1. The three vertex functions.
! 2. The edge functions of edge 1, then edge 2, then edge 3.
! 3. The cell functions.
!
! Edge `i` has `pe_i - 1` functions, which exist only if `pe_i > 1`.
! The cell has `(order - 1) * (order - 2) / 2` functions, which exist only
! if `order > 2`.
! The total is `pe1 + pe2 + pe3 + (order - 1) * (order - 2) / 2`.
!
! This version uses edge orientation 1 on all edges and the face
! orientation `[0, 1]`.
! To choose other orientations, use [[HeirarchicalBasisGradient_Triangle2_]].
!
! @note
! Use points in the interior of the triangle.
! The collapsed coordinates are singular at the apex of the triangle.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \phi_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/HeirarchicalBasisGradient_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/HeirarchicalBasisGradient_Triangle_test_2.F90)
!
! See also: [[HeirarchicalBasisGradient_Triangle1_]]

INTERFACE HeirarchicalBasisGradient_Triangle
  MODULE FUNCTION HeirarchicalBasisGradient_Triangle1( &
    order, pe1, pe2, pe3, xij, refTriangle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the triangle
    !! it should be greater than 2 for cell functions to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge 1
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge 2
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge 3
    !! it should be greater than 1 for edge functions to exist
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column
    !! `SIZE(xij, 1)` is 2
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! `xij` lies in this triangle
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2), 2)
    !! gradients of the basis functions
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! function `j` at point `i`
  END FUNCTION HeirarchicalBasisGradient_Triangle1
END INTERFACE HeirarchicalBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                       HeirarchicalBasisGradient_Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the gradients of the
! hierarchical basis of a triangle.
!
! Writes the gradients of the hierarchical basis functions of a triangle
! at the points `xij` into `ans`.
! The functions come in this order.
!
! 1. The three vertex functions.
! 2. The edge functions of edge 1, then edge 2, then edge 3.
! 3. The cell functions.
!
! Edge `i` has `pe_i - 1` functions, which exist only if `pe_i > 1`.
! The cell has `(order - 1) * (order - 2) / 2` functions, which exist only
! if `order > 2`.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `tsize1`, `tsize2` and
! `tsize3`.
!
! This version uses edge orientation 1 on all edges and the face
! orientation `[0, 1]`.
! To choose other orientations, use [[HeirarchicalBasisGradient_Triangle2_]].
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows,
! `pe1 + pe2 + pe3 + (order - 1) * (order - 2) / 2` columns and 2 pages.
! @endnote
!
! @note
! Use points in the interior of the triangle.
! The collapsed coordinates are singular at the apex of the triangle.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \phi_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/HeirarchicalBasisGradient_Triangle__test_1.F90)
! - [test 2](|base|Triangle/examples/HeirarchicalBasisGradient_Triangle__test_2.F90)
!   compares with the function form and with explicit orientations
!
! See also: [[HeirarchicalBasisGradient_Triangle1]]

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle1_( &
    order, pe1, pe2, pe3, xij, refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the triangle
    !! it should be greater than 2 for cell functions to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge 1
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge 2
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge 3
    !! it should be greater than 1 for edge functions to exist
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column
    !! `SIZE(xij, 1)` is 2
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! `xij` lies in this triangle
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the basis functions
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! function `j` at point `i`
    !! the routine writes only `ans(1:tsize1, 1:tsize2, 1:tsize3)`
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
    !! size of the block written in `ans`
    !! `tsize1` is `SIZE(xij, 2)`
    !! `tsize2` is `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)`
    !! `tsize3` is 2
  END SUBROUTINE HeirarchicalBasisGradient_Triangle1_
END INTERFACE HeirarchicalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the gradients of the
! hierarchical basis of a triangle, with chosen orientations.
!
! Writes the gradients of the hierarchical basis functions of a triangle
! at the points `xij` into `ans`.
! The functions come in this order.
!
! 1. The three vertex functions.
! 2. The edge functions of edge 1, then edge 2, then edge 3.
! 3. The cell functions.
!
! Edge `i` has `pe_i - 1` functions, which exist only if `pe_i > 1`.
! The cell has `(order - 1) * (order - 2) / 2` functions, which exist only
! if `order > 2`.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `tsize1`, `tsize2` and
! `tsize3`.
!
! `edgeOrient1`, `edgeOrient2` and `edgeOrient3` give the direction of
! each edge, 1 or -1.
! The value 1 is the default direction.
! `faceOrient(1)` is a rotation of the cell, 0, 1 or 2.
! `faceOrient(2)` is 1 or -1, and -1 reflects the cell.
! Use `[0, 1]` for the default orientation.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows,
! `pe1 + pe2 + pe3 + (order - 1) * (order - 2) / 2` columns and 2 pages.
! @endnote
!
! @note
! Use points in the interior of the triangle.
! The collapsed coordinates are singular at the apex of the triangle.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \phi_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 2](|base|Triangle/examples/HeirarchicalBasisGradient_Triangle__test_2.F90)
!   with explicit edge and face orientations
!
! See also: [[HeirarchicalBasisGradient_Triangle1_]]

INTERFACE HeirarchicalBasisGradient_Triangle_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Triangle2_( &
    order, pe1, pe2, pe3, xij, edgeOrient1, edgeOrient2, edgeOrient3, &
    faceOrient, refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation inside the triangle
    !! it should be greater than 2 for cell functions to exist
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge 1
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge 2
    !! it should be greater than 1 for edge functions to exist
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge 3
    !! it should be greater than 1 for edge functions to exist
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column
    !! `SIZE(xij, 1)` is 2
    INTEGER(I4B), INTENT(IN) :: edgeOrient1
    !! orientation of edge 1, 1 or -1
    INTEGER(I4B), INTENT(IN) :: edgeOrient2
    !! orientation of edge 2, 1 or -1
    INTEGER(I4B), INTENT(IN) :: edgeOrient3
    !! orientation of edge 3, 1 or -1
    INTEGER(I4B), INTENT(IN) :: faceOrient(:)
    !! orientation of the cell, `SIZE(faceOrient)` is 2.
    !! `faceOrient(1)` is the rotation, 0, 1 or 2.
    !! `faceOrient(2)` is 1 or -1, and -1 reflects the cell.
    !! `[0, 1]` is the default.
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`.
    !! `xij` lies in this triangle.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the basis functions
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! function `j` at point `i`
    !! the routine writes only `ans(1:tsize1, 1:tsize2, 1:tsize3)`
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
    !! size of the block written in `ans`
    !! `tsize1` is `SIZE(xij, 2)`
    !! `tsize2` is `pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)`
    !! `tsize3` is 2
  END SUBROUTINE HeirarchicalBasisGradient_Triangle2_
END INTERFACE HeirarchicalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!                                         OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the gradients of the orthogonal (Dubiner) basis of a
! triangle.
!
! Returns the gradients of the orthogonal basis functions of degree up to
! `order` at the points `xij`.
! The basis has `(order + 1) * (order + 2) / 2` functions.
! The orthogonal basis is the Dubiner basis.
!
! @note
! The routine applies the chain-rule factors of the `"UNIT"` triangle,
! also when `refTriangle` is `"BIUNIT"`.
! The gradients for `"BIUNIT"` therefore need review before you rely on
! them.
! @endnote
!
! @note
! Use points in the interior of the triangle.
! The collapsed coordinates are singular at the apex of the triangle.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \phi_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/OrthogonalBasisGradient_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/OrthogonalBasisGradient_Triangle_test_2.F90)
!
! See also: [[OrthogonalBasisGradient_Triangle1_]]

INTERFACE OrthogonalBasisGradient_Triangle
  MODULE FUNCTION OrthogonalBasisGradient_Triangle1(order, xij, refTriangle) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! degree of the polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column
    !! `SIZE(xij, 1)` is 2
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! `xij` lies in this triangle
    REAL(DFP) :: ans(SIZE(xij, 2), (order + 1) * (order + 2) / 2, 2)
    !! gradients of the basis functions
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! function `j` at point `i`
  END FUNCTION OrthogonalBasisGradient_Triangle1
END INTERFACE OrthogonalBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                         OrthogonalBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Fills a preallocated array with the gradients of the
! orthogonal (Dubiner) basis of a triangle.
!
! Writes the gradients of the orthogonal basis functions of degree up to
! `order` at the points `xij` into `ans`.
! The basis has `(order + 1) * (order + 2) / 2` functions.
! The orthogonal basis is the Dubiner basis.
!
! The routine does not allocate `ans`.
! It returns the size of the written block in `tsize1`, `tsize2` and
! `tsize3`.
!
! @note
! `ans` must have at least `SIZE(xij, 2)` rows,
! `(order + 1) * (order + 2) / 2` columns and 2 pages.
! @endnote
!
! @note
! The routine applies the chain-rule factors of the `"UNIT"` triangle,
! also when `refTriangle` is `"BIUNIT"`.
! The gradients for `"BIUNIT"` therefore need review before you rely on
! them.
! @endnote
!
! @note
! Use points in the interior of the triangle.
! The collapsed coordinates are singular at the apex of the triangle.
! @endnote
!
! Formula:
!   $$ ans(i, j, k) = \frac{\partial \phi_j}{\partial x_k}(x_i) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/OrthogonalBasisGradient_Triangle__test_1.F90)
!
! See also: [[OrthogonalBasisGradient_Triangle1]]

INTERFACE OrthogonalBasisGradient_Triangle_
  MODULE SUBROUTINE OrthogonalBasisGradient_Triangle1_(order, xij, &
                                     refTriangle, ans, tsize1, tsize2, tsize3)
    INTEGER(I4B), INTENT(IN) :: order
    !! degree of the polynomial space
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one point per column
    !! `SIZE(xij, 1)` is 2
    !! `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! reference triangle, `"BIUNIT"` or `"UNIT"`
    !! `xij` lies in this triangle
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the basis functions
    !! `ans(i, j, k)` is the derivative with respect to coordinate `k` of
    !! function `j` at point `i`
    !! the routine writes only `ans(1:tsize1, 1:tsize2, 1:tsize3)`
    INTEGER(I4B), INTENT(OUT) :: tsize1, tsize2, tsize3
    !! size of the block written in `ans`
    !! `tsize1` is `SIZE(xij, 2)`
    !! `tsize2` is `(order + 1) * (order + 2) / 2`
    !! `tsize3` is 2
  END SUBROUTINE OrthogonalBasisGradient_Triangle1_
END INTERFACE OrthogonalBasisGradient_Triangle_

!----------------------------------------------------------------------------
!                                                                 Triangle
!----------------------------------------------------------------------------

END MODULE TriangleInterpolationUtility
