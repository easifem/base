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
! summary: Interpolation, basis and mode-counting routines for a tetrahedron.
!
! This module collects routines for polynomial interpolation on the
! reference tetrahedron. They cover Lagrange, hierarchical and
! orthogonal bases, interpolation points and quadrature points.
!
! Each point is one column. Cartesian coordinates use three rows.
! Barycentric coordinates use four rows.
!
! A complete polynomial space of degree `p` has `(p+1)(p+2)(p+3)/6`
! modes.
!
! Examples
!
! - [mode counts](|base|Tetrahedron/examples/TetrahedronEntityDOF_test_1.F90)
! - [metadata](|base|Tetrahedron/examples/TetrahedronInterpolationMetadata_test_1.F90)
!
! See also: [[ReferenceTetrahedron_Method]]

MODULE TetrahedronInterpolationUtility
USE GlobalData
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

PUBLIC :: LagrangeDegree_Tetrahedron
PUBLIC :: LagrangeDOF_Tetrahedron
PUBLIC :: LagrangeInDOF_Tetrahedron
PUBLIC :: EquidistanceInPoint_Tetrahedron
PUBLIC :: EquidistancePoint_Tetrahedron
PUBLIC :: EquidistancePoint_Tetrahedron_
PUBLIC :: LagrangeCoeff_Tetrahedron
PUBLIC :: LagrangeCoeff_Tetrahedron_
PUBLIC :: InterpolationPoint_Tetrahedron
PUBLIC :: InterpolationPoint_Tetrahedron_

PUBLIC :: OrthogonalBasis_Tetrahedron
PUBLIC :: OrthogonalBasis_Tetrahedron_

PUBLIC :: BarycentricVertexBasis_Tetrahedron
PUBLIC :: BarycentricEdgeBasis_Tetrahedron
PUBLIC :: BarycentricFacetBasis_Tetrahedron
PUBLIC :: BarycentricCellBasis_Tetrahedron
PUBLIC :: BarycentricHeirarchicalBasis_Tetrahedron
PUBLIC :: VertexBasis_Tetrahedron
PUBLIC :: EdgeBasis_Tetrahedron
PUBLIC :: FacetBasis_Tetrahedron
PUBLIC :: CellBasis_Tetrahedron

PUBLIC :: HeirarchicalBasis_Tetrahedron
PUBLIC :: HeirarchicalBasis_Tetrahedron_

PUBLIC :: FacetConnectivity_Tetrahedron
PUBLIC :: EdgeConnectivity_Tetrahedron
PUBLIC :: GetVertexDOF_Tetrahedron
PUBLIC :: GetEdgeDOF_Tetrahedron
PUBLIC :: GetFacetDOF_Tetrahedron
PUBLIC :: GetCellDOF_Tetrahedron

PUBLIC :: LagrangeEvalAll_Tetrahedron
PUBLIC :: LagrangeEvalAll_Tetrahedron_

PUBLIC :: QuadraturePoint_Tetrahedron
PUBLIC :: QuadraturePoint_Tetrahedron_
PUBLIC :: QuadratureNumber_Tetrahedron

PUBLIC :: RefElemDomain_Tetrahedron
PUBLIC :: LagrangeGradientEvalAll_Tetrahedron
PUBLIC :: LagrangeGradientEvalAll_Tetrahedron_

PUBLIC :: HeirarchicalBasisGradient_Tetrahedron
PUBLIC :: HeirarchicalBasisGradient_Tetrahedron_

PUBLIC :: OrthogonalBasisGradient_Tetrahedron
PUBLIC :: OrthogonalBasisGradient_Tetrahedron_

PUBLIC :: GetTotalDOF_Tetrahedron
PUBLIC :: GetTotalInDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                      GetTotalDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Total number of modes of a tetrahedron of a given order.
!
! Returns the number of modes of a complete polynomial space of degree
! `order` on a tetrahedron. This is the number of nodes of a Lagrange
! tetrahedron of that order.
!
! The result depends only on `order`. The routine ignores
! `baseContinuity` and `baseInterpolation`.
!
! Returns:
!   ans : INTEGER(I4B) - total number of modes
!
! Formula:
!   $$ N = \frac{(p+1)(p+2)(p+3)}{6} $$
!
! See also: [[GetTotalInDOF_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION GetTotalDOF_Tetrahedron(order, baseContinuity, &
                                               baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order `p`, at least 0
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family; not used
    INTEGER(I4B) :: ans
    !! total number of modes
  END FUNCTION GetTotalDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of interior modes of a tetrahedron of a given order.
!
! Returns the number of modes that belong to the cell interior. Vertices,
! edges and facets hold none of them.
!
! The result depends only on `order`. The routine ignores
! `baseContinuity` and `baseInterpolation`.
!
! @note
! `order` must be at least 1. The result is 0 for `order` below 4.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - number of interior modes
!
! Formula:
!   $$ N = \frac{(p-1)(p-2)(p-3)}{6} $$
!
! See also: [[GetTotalDOF_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION GetTotalInDOF_Tetrahedron(order, baseContinuity, &
                                                baseInterpolation) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order `p`, at least 1
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis; not used
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! basis family; not used
    INTEGER(I4B) :: ans
    !! number of interior modes
  END FUNCTION GetTotalInDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 RefElemDomain_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Name of the reference tetrahedron used by a basis family.
!
! Returns the name of the reference domain for a basis family. The name
! is `"UNIT"` or `"BIUNIT"`.
!
! The name is `"UNIT"` for Lagrange, Serendipity and Hermit bases. It is
! `"BIUNIT"` for Hierarchy and Orthogonal bases.
!
! The routine ignores letter case. It accepts only `baseContinuity` equal
! to `"H1"`. For any other value, it writes an error message to `stderr`
! and leaves `ans` unset. It does the same for an unknown `baseInterpol`.
!
! Returns:
!   ans : TYPE(String) - `"UNIT"` or `"BIUNIT"`
!
! See also: [[GetTotalDOF_Tetrahedron]]

INTERFACE
  MODULE FUNCTION RefElemDomain_Tetrahedron(baseContinuity, baseInterpol) &
    & RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis functions, only `"H1"` works
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! basis family: Lagrange, Hierarchy, Serendipity, Hermit or Orthogonal
    TYPE(String) :: ans
    !! name of the reference domain
  END FUNCTION RefElemDomain_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetVertexDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of vertex modes of a tetrahedron.
!
! Returns the number of hierarchical vertex modes of a tetrahedron.
! The result is always 4, one per vertex.
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex modes, always 4
!
! See also: [[GetEdgeDOF_Tetrahedron1]], [[GetFacetDOF_Tetrahedron1]]

INTERFACE
  MODULE PURE FUNCTION GetVertexDOF_Tetrahedron() RESULT(ans)
    INTEGER(I4B) :: ans
    !! number of vertex modes, always 4
  END FUNCTION GetVertexDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of edge modes of a tetrahedron, one order per edge.
!
! Returns the number of hierarchical edge modes of a tetrahedron.
! Each of the six edges has its own polynomial order.
!
! An edge of order `pe` has `pe-1` modes. The result is the sum over the
! six edges. An edge of order 1 has no modes.
!
! @note
! Each order must be at least 1.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - total number of edge modes
!
! Formula:
!   $$ N = \sum_{i=1}^{6} (pe_i-1) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetEdgeDOF_Tetrahedron_test_1.F90)
!
! See also: [[GetEdgeDOF_Tetrahedron2]]

INTERFACE GetEdgeDOF_Tetrahedron
  MODULE PURE FUNCTION GetEdgeDOF_Tetrahedron1(pe1, pe2, pe3, &
    & pe4, pe5, pe6) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1, pe2, pe3, pe4, pe5, pe6
    !! polynomial orders on edges 1 to 6, each at least 1
    INTEGER(I4B) :: ans
    !! total number of edge modes
  END FUNCTION GetEdgeDOF_Tetrahedron1
END INTERFACE GetEdgeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                     GetEdgeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of edge modes of a tetrahedron, one order for all edges.
!
! Returns the number of hierarchical edge modes of a tetrahedron.
! All six edges use the same polynomial order `p`.
!
! Each edge has `p-1` modes. The result is six times this number. It is 0
! for `p = 1`.
!
! @note
! `p` must be at least 1.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - total number of edge modes
!
! Formula:
!   $$ N = 6 (p-1) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetEdgeDOF_Tetrahedron_test_1.F90)
!
! See also: [[GetEdgeDOF_Tetrahedron1]]

INTERFACE GetEdgeDOF_Tetrahedron
  MODULE PURE FUNCTION GetEdgeDOF_Tetrahedron2(p)  &
  & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order on all six edges, at least 1
    INTEGER(I4B) :: ans
    !! total number of edge modes
  END FUNCTION GetEdgeDOF_Tetrahedron2
END INTERFACE GetEdgeDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                    GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of facet modes of a tetrahedron, one order per facet.
!
! Returns the number of hierarchical facet modes of a tetrahedron.
! Each of the four triangular facets has its own polynomial order.
!
! A facet of order `ps` has `(ps-1)(ps-2)/2` modes. The result is the sum
! over the four facets. A facet of order below 3 has no modes.
!
! @note
! Each order must be at least 1.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - total number of facet modes
!
! Formula:
!   $$ N = \sum_{i=1}^{4} \frac{(ps_i-1)(ps_i-2)}{2} $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetFacetDOF_Tetrahedron_test_1.F90)
!
! See also: [[GetFacetDOF_Tetrahedron2]]

INTERFACE GetFacetDOF_Tetrahedron
  MODULE PURE FUNCTION GetFacetDOF_Tetrahedron1( &
    & ps1, ps2, &
    & ps3, ps4) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! polynomial order on facet 1, at least 1
    INTEGER(I4B), INTENT(IN) :: ps2
    !! polynomial order on facet 2, at least 1
    INTEGER(I4B), INTENT(IN) :: ps3
    !! polynomial order on facet 3, at least 1
    INTEGER(I4B), INTENT(IN) :: ps4
    !! polynomial order on facet 4, at least 1
    INTEGER(I4B) :: ans
    !! total number of facet modes
  END FUNCTION GetFacetDOF_Tetrahedron1
END INTERFACE GetFacetDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                     GetFacetDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of facet modes of a tetrahedron, one order for all facets.
!
! Returns the number of hierarchical facet modes of a tetrahedron.
! All four triangular facets use the same polynomial order `p`.
!
! Each facet has `(p-1)(p-2)/2` modes. The result is four times this
! number. It is 0 for `p` below 3.
!
! @note
! `p` must be at least 1.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - total number of facet modes
!
! Formula:
!   $$ N = 4 \frac{(p-1)(p-2)}{2} $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetFacetDOF_Tetrahedron_test_1.F90)
!
! See also: [[GetFacetDOF_Tetrahedron1]]

INTERFACE GetFacetDOF_Tetrahedron
  MODULE PURE FUNCTION GetFacetDOF_Tetrahedron2(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order on all four facets, at least 1
    INTEGER(I4B) :: ans
    !! total number of facet modes
  END FUNCTION GetFacetDOF_Tetrahedron2
END INTERFACE GetFacetDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                                    GetCellDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of cell modes of a tetrahedron.
!
! Returns the number of modes that belong to the interior of the
! tetrahedron. A cell mode starts at order 4.
!
! The result is zero for `p` equal to 1, 2 or 3.
!
! @note
! Use `p >= 1`. For `p = 0` the formula gives a negative number.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - number of cell modes
!
! Formula:
!   $$ ans = (p-1)(p-2)(p-3)/6 $$
!
! See also: [[GetVertexDOF_Tetrahedron]], [[GetEdgeDOF_Tetrahedron]],
! [[GetFacetDOF_Tetrahedron]]

INTERFACE GetCellDOF_Tetrahedron
  MODULE PURE FUNCTION GetCellDOF_Tetrahedron1(p) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: p
    !! polynomial order of the tetrahedron
    INTEGER(I4B) :: ans
    !! number of cell modes
  END FUNCTION GetCellDOF_Tetrahedron1
END INTERFACE GetCellDOF_Tetrahedron

!----------------------------------------------------------------------------
!                                               FacetConnectivity_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex numbers of the four facets of a tetrahedron.
!
! Returns the local vertex numbers of each facet. Column `j` of `ans`
! holds the three vertices of facet `j`.
!
! The vertex order depends on the basis family. Hierarchy and orthogonal
! bases use the order (1,2,3), (1,2,4), (1,3,4), (2,3,4).
!
! Every other family uses the nodal order (1,3,2), (1,2,4), (1,4,3),
! (2,3,4).
!
! The routine ignores letter case of `baseInterpol`. It does not use
! `baseContinuity`.
!
! @warning
! The nodal and hierarchy orders differ for facets 1 and 3. Do not
! mix them.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B)(3, 4) - vertex numbers of the four facets
!
! See also: [[EdgeConnectivity_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION FacetConnectivity_Tetrahedron( &
    & baseInterpol, &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! interpolation family, for example `"Lagrange"` or `"Hierarchy"`
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, for example `"H1"`; not used
    INTEGER(I4B) :: ans(3, 4)
    !! vertex numbers, one column per facet
  END FUNCTION FacetConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               EdgeConnectivity_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex numbers of the six edges of a tetrahedron.
!
! Returns the local vertex numbers of each edge. Column `j` of `ans`
! holds the two vertices of edge `j`.
!
! The edges are (1,2), (1,3), (1,4), (2,3), (2,4) and (3,4).
!
! The result does not depend on the basis family. The routine ignores
! both arguments.
!
! Returns:
!   ans : INTEGER(I4B)(2, 6) - vertex numbers of the six edges
!
! See also: [[FacetConnectivity_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION EdgeConnectivity_Tetrahedron( &
    & baseInterpol,  &
    & baseContinuity) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseInterpol
    !! interpolation family, for example `"Lagrange"`; not used
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! continuity of the basis, for example `"H1"`; not used
    INTEGER(I4B) :: ans(2, 6)
    !! vertex numbers, one column per edge
  END FUNCTION EdgeConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Monomial exponents of a complete polynomial on a tetrahedron.
!
! Returns the exponents of the monomials $x^i y^j z^k$ with
! $i + j + k \le$ `order`. Row `m` of `ans` holds `i`, `j` and `k` of the
! `m`-th monomial.
!
! The index `i` changes fastest, then `j`, then `k`.
!
! The routine allocates `ans`. To fill an array you already own, use
! [[LagrangeDegree_Tetrahedron_]].
!
! Returns:
!   ans : INTEGER(I4B)(:, :) - exponents, one row per monomial
!
! Formula:
!   $$ SIZE(ans, 1) = (p+1)(p+2)(p+3)/6, \quad SIZE(ans, 2) = 3 $$
!
! See also: [[LagrangeDOF_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDegree_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! exponents, `LagrangeDOF_Tetrahedron(order)` rows and 3 columns
  END FUNCTION LagrangeDegree_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                LagrangeDegree_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Monomial exponents of a complete polynomial, into `ans`.
!
! Fills `ans` with the exponents of the monomials $x^i y^j z^k$ with
! $i + j + k \le$ `order`. Row `m` holds `i`, `j` and `k` of the `m`-th
! monomial.
!
! The index `i` changes fastest, then `j`, then `k`.
!
! The routine never allocates. It writes only the first `nrow` rows and
! `ncol` columns of `ans`.
!
! @note
! `ans` must have at least `LagrangeDOF_Tetrahedron(order)` rows and 3
! columns. The routine does not check this.
! @endnote
!
! See also: [[LagrangeDegree_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE LagrangeDegree_Tetrahedron_(order, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! exponents; active part is `ans(1:nrow, 1:ncol)`
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of active rows and columns of `ans`
  END SUBROUTINE LagrangeDegree_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   LagrangeDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of Lagrange nodes of a tetrahedron of a given order.
!
! Returns the number of nodes, or degrees of freedom, of a Lagrange
! tetrahedron. This equals the dimension of the complete polynomial space
! of degree `order`.
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes
!
! Formula:
!   $$ ans = (p+1)(p+2)(p+3)/6 $$
!
! See also: [[LagrangeInDOF_Tetrahedron]], [[LagrangeDegree_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION LagrangeDOF_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION LagrangeDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 LagrangeInDOF_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of interior Lagrange nodes of a tetrahedron.
!
! Returns the number of nodes that lie strictly inside the tetrahedron.
! Nodes on vertices, edges and facets are not counted.
!
! The result is zero for `order` equal to 1, 2 or 3. The first interior
! node appears at order 4.
!
! @note
! Use `order >= 1`. For `order = 0` the formula gives a negative number.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - number of interior nodes
!
! Formula:
!   $$ ans = (p-1)(p-2)(p-3)/6 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeInDOF_Tetrahedron_test_1.F90)
!
! See also: [[LagrangeDOF_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION LagrangeInDOF_Tetrahedron(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B) :: ans
    !! number of interior nodes
  END FUNCTION LagrangeInDOF_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-14
! summary: Older version of the interior equidistant points of a tetrahedron.
!
! Returns equidistant points that lie strictly inside the tetrahedron.
! The points have no vertex, edge or facet nodes.
!
! The routine returns an empty array for `order` below 4. For `order`
! equal to 4 it returns one point, the centroid.
!
! @note
! This is the older version. Use [[EquidistanceInPoint_Tetrahedron]]
! instead.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(:, :) - interior points, one column per point
!
! See also: [[EquidistanceInPoint_Tetrahedron]]

INTERFACE
  MODULE FUNCTION EquidistanceInPoint_Tetrahedron_old(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(nsd, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interior points, one column per point
  END FUNCTION EquidistanceInPoint_Tetrahedron_old
END INTERFACE

!----------------------------------------------------------------------------
!                                           EquidistanceInPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Equidistant points strictly inside a tetrahedron.
!
! Returns the equidistant Lagrange nodes that lie strictly inside the
! tetrahedron. The result has no nodes on vertices, edges or facets.
!
! By default the tetrahedron is the unit tetrahedron. Pass `xij` to use
! another one. The result has `LagrangeInDOF_Tetrahedron(order)` columns.
!
! The result is empty for `order` below 4. For `order` equal to 4 it holds
! one point, the centroid.
!
! Returns:
!   ans : REAL(DFP)(3, :) - interior points, one column per point
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/EquidistanceInPoint_Tetrahedron_test_1.F90)
!
! See also: [[EquidistancePoint_Tetrahedron]],
! [[LagrangeInDOF_Tetrahedron]]

INTERFACE
  MODULE FUNCTION EquidistanceInPoint_Tetrahedron(order, xij) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interior points, one column per point
  END FUNCTION EquidistanceInPoint_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Equidistant Lagrange nodes of a tetrahedron.
!
! Returns the equidistant nodes of a Lagrange tetrahedron of order `order`.
! The result has `LagrangeDOF_Tetrahedron(order)` columns.
!
! The nodes follow the VEFC layout. Vertex nodes come first, then edge
! nodes, facet nodes and cell nodes.
!
! By default the tetrahedron is the unit tetrahedron. Pass `xij` to map
! the nodes to another tetrahedron.
!
! Returns:
!   ans : REAL(DFP)(3, :) - nodal coordinates, one column per node
!
! Formula:
!   $$ x_i = i/p, \quad y_j = j/p, \quad z_k = k/p, \quad i+j+k \le p $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/EquidistancePoint_Tetrahedron_test_1.F90)
!
! See also: [[EquidistanceInPoint_Tetrahedron]],
! [[LagrangeDOF_Tetrahedron]]

INTERFACE
  MODULE FUNCTION EquidistancePoint_Tetrahedron(order, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nodal coordinates, one column per node
  END FUNCTION EquidistancePoint_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Equidistant Lagrange nodes of a tetrahedron, into `ans`.
!
! Computes the equidistant nodes of a Lagrange tetrahedron of order
! `order` and stores them in `ans`.
! The number of nodes is `LagrangeDOF_Tetrahedron(order)`.
!
! The nodes follow the VEFC layout. Vertex nodes come first, then edge
! nodes, facet nodes and cell nodes.
!
! By default the tetrahedron is the unit tetrahedron. Pass `xij` to map
! the nodes to another tetrahedron.
!
! @note
! `ans` must be large enough before the call. It needs at least 3 rows
! and `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! Formula:
!   $$ x_i = i/p, \quad y_j = j/p, \quad z_k = k/p, \quad i+j+k \le p $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/EquidistancePoint_Tetrahedron__test_1.F90)
!
! See also: [[EquidistancePoint_Tetrahedron]]

INTERFACE
  MODULE SUBROUTINE EquidistancePoint_Tetrahedron_(order, xij, ans, nrow, &
                                                   ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodal coordinates, one column per node
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, always 3
    !! number of columns of `ans` written, one per node
  END SUBROUTINE EquidistancePoint_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                              EquidistancePoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-08-12
! summary: Older version of the equidistant nodes of a tetrahedron.
!
! Returns the equidistant nodes of a Lagrange tetrahedron of order
! `order`. The nodes lie on a uniform grid. They can build Lagrange
! polynomials. The result has `LagrangeDOF_Tetrahedron(order)` columns.
!
! The order of the nodes is: vertex nodes, edge nodes, facet nodes, cell
! nodes. Edge nodes exist for `order` above 1. Facet nodes exist for
! `order` above 2. Cell nodes exist for `order` above 3.
!
! By default the tetrahedron is the unit tetrahedron. Pass `xij` to map
! the nodes to another tetrahedron.
!
! @note
! This is the older version. Use [[EquidistancePoint_Tetrahedron]]
! instead.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(nsd, :) - nodal coordinates, one column per node
!
! See also: [[EquidistancePoint_Tetrahedron]]

INTERFACE
  MODULE RECURSIVE FUNCTION EquidistancePoint_Tetrahedron_old( &
    & order, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(nsd, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nodal coordinates, one column per node
  END FUNCTION EquidistancePoint_Tetrahedron_old
END INTERFACE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Interpolation nodes of a tetrahedron.
!
! Returns the interpolation nodes of a Lagrange tetrahedron of order
! `order`. The `ipType` argument selects the node family.
!
! By default the nodes lie on the unit tetrahedron. Pass `xij` to map
! them to another tetrahedron.
!
! If `layout` is `"VEFC"`, the nodes come in this order: vertex nodes,
! edge nodes, facet nodes, cell nodes. The comparison is case sensitive.
! Any other value keeps the order of the node generator.
!
! `alpha`, `beta` and `lambda` are the parameters of the Jacobi and
! ultraspherical families.
!
! The result has 3 rows and `LagrangeDOF_Tetrahedron(order)` columns.
!
! Returns:
!   ans : REAL(DFP)(3, :) - interpolation nodes, one column per node
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/InterpolationPoint_Tetrahedron_test_1.F90)
! - [test 2](|base|Tetrahedron/examples/InterpolationPoint_Tetrahedron_test_2.F90) with the `"VEFC"` layout
!
! See also: [[InterpolationPoint_Tetrahedron_]]

INTERFACE
  MODULE FUNCTION InterpolationPoint_Tetrahedron(order, ipType, layout, &
                                         xij, alpha, beta, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! node family: `Equidistance`, `GaussLegendre`,
    !! `GaussLegendreLobatto`, `GaussChebyshev`, `GaussChebyshevLobatto`,
    !! `GaussJacobi` or `GaussJacobiLobatto`
    CHARACTER(*), INTENT(IN) :: layout
    !! node order, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(3, 4)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! interpolation nodes, one column per node
  END FUNCTION InterpolationPoint_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            InterpolationPoint_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Interpolation nodes of a tetrahedron, into `ans`.
!
! Computes the interpolation nodes of a Lagrange tetrahedron of order
! `order` and stores them in `ans`. The `ipType` argument selects the
! node family.
!
! By default the nodes lie on the unit tetrahedron. Pass `xij` to map
! them to another tetrahedron.
!
! If `layout` is `"VEFC"`, the nodes come in this order: vertex nodes,
! edge nodes, facet nodes, cell nodes. The comparison is case sensitive.
! Any other value keeps the order of the node generator.
!
! `alpha`, `beta` and `lambda` are the parameters of the Jacobi and
! ultraspherical families.
!
! @note
! `ans` must be large enough before the call. It needs at least 3 rows
! and `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/InterpolationPoint_Tetrahedron__test_1.F90)
!
! See also: [[InterpolationPoint_Tetrahedron]]

INTERFACE
  MODULE SUBROUTINE InterpolationPoint_Tetrahedron_(order, ipType, ans, &
                                 nrow, ncol, layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! node family: `Equidistance`, `GaussLegendre`,
    !! `GaussLegendreLobatto`, `GaussChebyshev`, `GaussChebyshevLobatto`,
    !! `GaussJacobi` or `GaussJacobiLobatto`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! interpolation nodes, one column per node
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, always 3
    !! number of columns of `ans` written, one per node
    CHARACTER(*), INTENT(IN) :: layout
    !! node order, `"VEFC"` or `"INCREASING"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(3, 4)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE InterpolationPoint_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial on a tetrahedron.
!
! Returns the coefficients of the `i`-th Lagrange polynomial for the
! nodes `xij`. The polynomial uses monomials up to degree `order`.
! It is 1 at node `i` and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. The matrix `V` holds the
! monomials at the nodes.
!
! @note
! `xij` must have one column per monomial. That is
! `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2)) - coefficients of the `i`-th polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_2.F90)
!
! See also: [[LagrangeCoeff_Tetrahedron2]],
! [[LagrangeCoeff_Tetrahedron3]], [[LagrangeCoeff_Tetrahedron4]]

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron1(order, i, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates, one column per node
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! coefficients
  END FUNCTION LagrangeCoeff_Tetrahedron1
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                   LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial, from a Vandermonde matrix.
!
! Returns the coefficients of the `i`-th Lagrange polynomial. The nodal
! Vandermonde matrix `v` is an input. The polynomial is 1 at node `i`
! and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. The routine works on a copy of
! `v`, so `v` does not change. The value of `isVandermonde` is not used.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)) - coefficients of the `i`-th polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_2.F90)
!
! See also: [[LagrangeCoeff_Tetrahedron1]],
! [[LagrangeCoeff_Tetrahedron3]]

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron2(order, i, v, isVandermonde) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix of the nodes, a square matrix
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this specific procedure, pass `.TRUE.`
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Tetrahedron2
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial, from LU factors.
!
! Returns the coefficients of the `i`-th Lagrange polynomial. The input
! is the LU factorization of the nodal Vandermonde matrix and its pivot
! indices. The polynomial is 1 at node `i` and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. Reuse the factors to get
! many polynomials without factoring `V` again.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(SIZE(v, 1)) - coefficients of the `i`-th polynomial
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_2.F90)
!
! See also: [[LagrangeCoeff_Tetrahedron1]],
! [[LagrangeCoeff_Tetrahedron2]]

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron3(order, i, v, ipiv) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the Vandermonde matrix, a square matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices of the factorization, from `GetLU`
    REAL(DFP) :: ans(SIZE(v, 1))
    !! coefficients
  END FUNCTION LagrangeCoeff_Tetrahedron3
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!                                                  LagrangeCoeff_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of all Lagrange polynomials on a tetrahedron.
!
! Returns the coefficients of all Lagrange polynomials for the nodes
! `xij`. Column `j` of the result belongs to the polynomial of node `j`.
! That polynomial is 1 at node `j` and 0 at the other nodes.
!
! The result is the inverse of the matrix `V`. This matrix holds the
! basis functions at the nodes. `basisType` selects the basis.
! The default is `Monomial`. Other choices are `Heirarchical` and the
! orthogonal basis. Any other value selects the orthogonal basis.
!
! `refTetrahedron` selects the reference tetrahedron of the basis. The
! default is `"UNIT"`.
!
! @note
! `xij` must have one column per basis function. That is
! `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! @note
! The routine accepts `alpha`, `beta` and `lambda` but does not use them.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(SIZE(xij, 2), SIZE(xij, 2)) - coefficients, one column
!   per node
!
! Formula:
!   $$ V C = I, \quad \ell_j(x) = \sum_k C_{kj} m_k(x) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_1.F90) with the monomial basis
! - [test 3](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_3.F90) with the orthogonal basis
!
! See also: [[LagrangeCoeff_Tetrahedron1]]

INTERFACE LagrangeCoeff_Tetrahedron
  MODULE FUNCTION LagrangeCoeff_Tetrahedron4( &
    & order, &
    & xij, &
    & basisType, &
    & refTetrahedron, &
    & alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates, one column per node
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis family, `Monomial` (default), `Heirarchical` or orthogonal
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter, not used
    REAL(DFP) :: ans(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients, column `j` belongs to node `j`
  END FUNCTION LagrangeCoeff_Tetrahedron4
END INTERFACE LagrangeCoeff_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial, into `ans`.
!
! Writes the coefficients of the `i`-th Lagrange polynomial for the
! nodes `xij` into `ans`. The polynomial uses monomials up to degree
! `order`. It is 1 at node `i` and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. The matrix `V` holds the
! monomials at the nodes.
!
! @note
! `xij` must have one column per monomial. That is
! `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron_test_2.F90) compares with the function form
!
! See also: [[LagrangeCoeff_Tetrahedron1]]

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron1_(order, i, xij, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(xij, 2)`
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates, one column per node
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes only `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(xij, 2)`
  END SUBROUTINE LagrangeCoeff_Tetrahedron1_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial, from a Vandermonde matrix.
!
! Writes the coefficients of the `i`-th Lagrange polynomial into `ans`.
! The nodal Vandermonde matrix `v` is an input. The polynomial is 1 at
! node `i` and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. The routine works on a copy of
! `v`, so `v` does not change. The value of `isVandermonde` is not used.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! See also: [[LagrangeCoeff_Tetrahedron2]],
! [[LagrangeCoeff_Tetrahedron1_]]

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron2_(order, i, v, isVandermonde, &
                                                ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(IN) :: v(:, :)
    !! Vandermonde matrix of the nodes, a square matrix
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
    !! only selects this specific procedure, pass `.TRUE.`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes only `ans(1:tsize)`
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Tetrahedron2_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of one Lagrange polynomial, from LU factors.
!
! Writes the coefficients of the `i`-th Lagrange polynomial into `ans`.
! The caller passes the LU factors `v` of the nodal Vandermonde matrix
! and the pivot array `ipiv`. Both come from `GetLU`.
! The polynomial is 1 at node `i` and 0 at every other node.
!
! The coefficients `c` solve `V c = e_i`. The routine solves this system
! with the LU factors.
!
! @note
! `order` is not used. The size of `v` sets the number of coefficients.
! @endnote
!
! Formula:
!   $$ V c = e_i, \quad \ell_i(x) = \sum_k c_k m_k(x) $$
!
! See also: [[LagrangeCoeff_Tetrahedron3]],
! [[LagrangeCoeff_Tetrahedron2_]]

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron3_(order, i, v, ipiv, ans, tsize)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order, not used
    INTEGER(I4B), INTENT(IN) :: i
    !! index of the Lagrange polynomial, from 1 to `SIZE(v, 1)`
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU factors of the nodal Vandermonde matrix, a square matrix
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! pivot indices that `GetLU` returns with `v`
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! coefficients, the routine writes only `ans(1:tsize)`
    !! The size must be at least `SIZE(v, 1)`.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coefficients, equal to `SIZE(v, 1)`
  END SUBROUTINE LagrangeCoeff_Tetrahedron3_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Coefficients of all Lagrange polynomials, into `ans`.
!
! Computes the coefficients of all Lagrange polynomials for the nodes
! `xij` and stores them in `ans`. Column `j` of `ans` holds the
! coefficients of the polynomial that is 1 at node `j` and 0 at the
! other nodes.
!
! The routine builds the matrix `V` of basis functions at the nodes.
! It then stores the inverse of `V` in `ans`.
! The basis functions come from `basisType`:
!
! 1. `Monomial` uses monomials. This is the default.
! 2. `Heirarchical` uses the hierarchical basis.
! 3. Any other value uses the orthogonal basis of Dubiner.
!
! The number of nodes must equal the number of basis functions of
! degree `order`. That is `LagrangeDOF_Tetrahedron(order)`.
!
! @note
! For the orthogonal basis, always pass `refTetrahedron`. The default
! `"UNIT"` does not apply to this case.
! @endnote
!
! @note
! `alpha`, `beta` and `lambda` are not used.
! @endnote
!
! Formula:
!   $$ V C = I, \quad \ell_j(x) = \sum_k C_{kj} m_k(x) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeCoeff_Tetrahedron__test_1.F90) with monomials
!
! See also: [[LagrangeCoeff_Tetrahedron4]]

INTERFACE LagrangeCoeff_Tetrahedron_
  MODULE SUBROUTINE LagrangeCoeff_Tetrahedron4_(order, xij, basisType, &
                         refTetrahedron, alpha, beta, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! nodal coordinates, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis family: `Monomial`, `Heirarchical` or an orthogonal family
    !! The default is `Monomial`.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The default is `"UNIT"`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter, not used
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter, not used
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! coefficients, one column per Lagrange polynomial
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` written
    !! Both equal `SIZE(xij, 2)`.
  END SUBROUTINE LagrangeCoeff_Tetrahedron4_
END INTERFACE LagrangeCoeff_Tetrahedron_

!----------------------------------------------------------------------------
!                                                            Isaac_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Isaac recursive nodes of a tetrahedron.
!
! Computes the recursive interpolation nodes of Isaac for a tetrahedron
! of order `order` and stores them in `ans`.
! The number of nodes is `LagrangeDOF_Tetrahedron(order)`.
! The `ipType` argument selects the one-dimensional point family.
!
! By default the nodes lie on the unit tetrahedron. Pass `xij` to map
! them to another tetrahedron.
!
! If `layout` is `"VEFC"`, the nodes come in this order: vertex nodes,
! edge nodes, facet nodes, cell nodes. Any other value keeps the order
! of the node generator.
!
! @note
! `ans` must be large enough before the call. It needs at least 3 rows
! and `LagrangeDOF_Tetrahedron(order)` columns.
! @endnote
!
! Isaac explains the method at https://tisaac.gitlab.io/recursivenodes/
!
! See also: [[InterpolationPoint_Tetrahedron_]]

INTERFACE
  MODULE SUBROUTINE Isaac_Tetrahedron(order, ipType, ans, nrow, ncol, &
                                      layout, xij, alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! node family: `Equidistance`, `GaussLegendre`,
    !! `GaussLegendreLobatto`, `GaussChebyshev`, `GaussChebyshevLobatto`,
    !! `GaussJacobi` or `GaussJacobiLobatto`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! nodal coordinates, one column per node
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, always 3
    !! number of columns of `ans` written, one per node
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    !! The shape is `(3, 4)`.
    !! The default is the unit tetrahedron.
    CHARACTER(*), INTENT(IN) :: layout
    !! node order, `"VEFC"` or any other value for the generator order
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
  END SUBROUTINE Isaac_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                BlythPozrikidis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Blyth and Pozrikidis nodes of a tetrahedron, not implemented.
!
! Will return the Lobatto interpolation nodes of Blyth and Pozrikidis
! for a tetrahedron.
!
! @warning
! This function is under development. It writes an error message and
! returns without allocating `ans`. Use [[Isaac_Tetrahedron]] instead.
! @endwarning
!
! The method comes from M. G. Blyth and C. Pozrikidis. A lobatto
! interpolation grid over the Tetrahedron. IMA Journal of Applied
! Mathematics, Feb 2006.
!
! See also: [[Isaac_Tetrahedron]]

INTERFACE
  MODULE FUNCTION BlythPozrikidis_Tetrahedron(order, ipType, layout, xij,  &
    & alpha, beta, lambda) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: ipType
    !! node family, as in [[Isaac_Tetrahedron]]
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one column per vertex
    CHARACTER(*), INTENT(IN) :: layout
    !! node order
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi polynomial parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical polynomial parameter
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! nodal coordinates, not allocated at present
  END FUNCTION BlythPozrikidis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IJK2VEFC_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Reorder tetrahedron nodes from IJK order to VEFC order.
!
! Reads the nodes from the arrays `xi`, `eta` and `zeta`. It writes
! them to `temp` in VEFC order: vertex nodes first, then edge nodes,
! facet nodes and cell nodes.
!
! Entry `(i + 1, j + 1, k + 1)` of each array holds the node with
! indices `(i, j, k)`. The routine reads only nodes with
! `i + j + k <= order`.
!
! @note
! `Isaac_Tetrahedron` calls this routine. Most callers do not need it.
! @endnote
!
! See also: [[Isaac_Tetrahedron]]

INTERFACE
  MODULE RECURSIVE SUBROUTINE IJK2VEFC_Tetrahedron(xi, eta, zeta, temp, &
                                                   order, N)
    REAL(DFP), INTENT(IN) :: xi(:, :, :)
    !! first coordinate of the nodes
    !! The shape is `(order + 1, order + 1, order + 1)`.
    REAL(DFP), INTENT(IN) :: eta(:, :, :)
    !! second coordinate of the nodes, same shape as `xi`
    REAL(DFP), INTENT(IN) :: zeta(:, :, :)
    !! third coordinate of the nodes, same shape as `xi`
    REAL(DFP), INTENT(INOUT) :: temp(:, :)
    !! nodal coordinates in VEFC order, one column per node
    !! It needs at least 3 rows and `N` columns.
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    INTEGER(I4B), INTENT(IN) :: N
    !! total number of nodes
  END SUBROUTINE IJK2VEFC_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                               OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal basis of a tetrahedron at a set of points.
!
! Evaluates the orthogonal (Dubiner) polynomials of a tetrahedron at the
! points `xij`. It returns one row per point and one column per mode.
!
! The number of modes is `(order + 1) * (order + 2) * (order + 3) / 6`.
! The modes follow the index order `p`, `q`, `r`. The index `r` changes
! fastest, then `q`, then `p`. The total degree `p + q + r` is at most
! `order`.
!
! The reference tetrahedron is `"UNIT"` or `"BIUNIT"`. The case of the
! string does not matter.
!
! @note
! The basis uses collapsed coordinates. Use interior points. Points on
! the boundary can make the collapsed coordinates singular.
! @endnote
!
! Formula:
!   $$ \phi_{pqr} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b) \left(\frac{1-c}{2}\right)^{p+q}
!   P_r^{(2p+2q+2,0)}(c) $$
!
! Here `a`, `b` and `c` are the collapsed coordinates of the point.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/OrthogonalBasis_Tetrahedron_test_1.F90)
!
! See also: [[OrthogonalBasis_Tetrahedron1_]],
! [[OrthogonalBasis_Tetrahedron2]]

INTERFACE OrthogonalBasis_Tetrahedron
  MODULE FUNCTION OrthogonalBasis_Tetrahedron1( &
    & order, &
    & xij, &
    & refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6)
    !! basis values
    !! `ans(:, j)` holds the `j`-th mode at all points.
    !! `ans(j, :)` holds all modes at the `j`-th point.
  END FUNCTION OrthogonalBasis_Tetrahedron1
END INTERFACE OrthogonalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal basis of a tetrahedron at a set of points, into `ans`.
!
! Evaluates the orthogonal (Dubiner) polynomials of a tetrahedron at the
! points `xij` and stores them in `ans`. It gives one row per point and
! one column per mode.
!
! The number of modes is `(order + 1) * (order + 2) * (order + 3) / 6`.
! The modes follow the index order `p`, `q`, `r`. The index `r` changes
! fastest, then `q`, then `p`. The total degree `p + q + r` is at most
! `order`.
!
! The reference tetrahedron is `"UNIT"` or `"BIUNIT"`. The case of the
! string does not matter.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(xij, 2)` rows and the number of modes as columns.
! @endnote
!
! @note
! The basis uses collapsed coordinates. Use interior points. Points on
! the boundary can make the collapsed coordinates singular.
! @endnote
!
! Formula:
!   $$ \phi_{pqr} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b) \left(\frac{1-c}{2}\right)^{p+q}
!   P_r^{(2p+2q+2,0)}(c) $$
!
! Here `a`, `b` and `c` are the collapsed coordinates of the point.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/OrthogonalBasis_Tetrahedron__test_1.F90)
!
! See also: [[OrthogonalBasis_Tetrahedron1]],
! [[OrthogonalBasis_Tetrahedron2_]]

INTERFACE OrthogonalBasis_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasis_Tetrahedron1_(order, xij, &
                                              refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! evaluation points, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values
    !! `ans(:, j)` holds the `j`-th mode at all points.
    !! `ans(j, :)` holds all modes at the `j`-th point.
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` written
    !! `nrow = SIZE(xij, 2)`
    !! `ncol = (order + 1) * (order + 2) * (order + 3) / 6`
  END SUBROUTINE OrthogonalBasis_Tetrahedron1_
END INTERFACE OrthogonalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                                OrthogonalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal basis of a tetrahedron on a tensor grid.
!
! Evaluates the orthogonal (Dubiner) polynomials of a tetrahedron on a
! grid of points. It returns one row per grid point and one column per
! mode.
!
! The grid is the tensor product of `x`, `y` and `z`. These are the
! collapsed coordinates `a`, `b` and `c` of the points, not the
! Cartesian coordinates. The routine builds all
! `SIZE(x) * SIZE(y) * SIZE(z)` points. The index of `z` changes
! fastest, then `y`, then `x`.
!
! The number of modes is `(order + 1) * (order + 2) * (order + 3) / 6`.
! The modes follow the index order `p`, `q`, `r`. The index `r` changes
! fastest, then `q`, then `p`. The total degree `p + q + r` is at most
! `order`.
!
! If `refTetrahedron` is `"BIUNIT"`, the coordinates lie in `[-1, 1]`.
! If it is `"UNIT"`, they lie in `[0, 1]` and the routine maps them to
! `[-1, 1]`. The case of the string does not matter.
!
! @note
! Use interior values. Values on the boundary can make the collapsed
! coordinates singular.
! @endnote
!
! Formula:
!   $$ \phi_{pqr} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b) \left(\frac{1-c}{2}\right)^{p+q}
!   P_r^{(2p+2q+2,0)}(c) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/OrthogonalBasis_Tetrahedron_test_2.F90)
!
! See also: [[OrthogonalBasis_Tetrahedron2_]],
! [[OrthogonalBasis_Tetrahedron1]]

INTERFACE OrthogonalBasis_Tetrahedron
  MODULE FUNCTION OrthogonalBasis_Tetrahedron2(order, x, y, z, &
                                               refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: x(:)
    !! first collapsed coordinate of the grid
    !! The grid has `SIZE(x) * SIZE(y) * SIZE(z)` points.
    REAL(DFP), INTENT(IN) :: y(:)
    !! second collapsed coordinate of the grid
    REAL(DFP), INTENT(IN) :: z(:)
    !! third collapsed coordinate of the grid
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(x) * SIZE(y) * SIZE(z), &
      & (order + 1) * (order + 2) * (order + 3) / 6)
    !! basis values
    !! `ans(:, j)` holds the `j`-th mode at all points.
    !! `ans(j, :)` holds all modes at the `j`-th point.
  END FUNCTION OrthogonalBasis_Tetrahedron2
END INTERFACE OrthogonalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Orthogonal basis of a tetrahedron on a tensor grid, into `ans`.
!
! Evaluates the orthogonal (Dubiner) polynomials of a tetrahedron on a
! grid of points and stores them in `ans`. It gives one row per grid
! point and one column per mode.
!
! The grid is the tensor product of `x`, `y` and `z`. These are the
! collapsed coordinates `a`, `b` and `c` of the points, not the
! Cartesian coordinates. The routine builds all
! `SIZE(x) * SIZE(y) * SIZE(z)` points. The index of `z` changes
! fastest, then `y`, then `x`.
!
! The number of modes is `(order + 1) * (order + 2) * (order + 3) / 6`.
! The modes follow the index order `p`, `q`, `r`. The index `r` changes
! fastest, then `q`, then `p`. The total degree `p + q + r` is at most
! `order`.
!
! If `refTetrahedron` is `"BIUNIT"`, the coordinates lie in `[-1, 1]`.
! If it is `"UNIT"`, they lie in `[0, 1]` and the routine maps them to
! `[-1, 1]`. The case of the string does not matter.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(x) * SIZE(y) * SIZE(z)` rows and the number of modes as columns.
! @endnote
!
! @note
! Use interior values. Values on the boundary can make the collapsed
! coordinates singular.
! @endnote
!
! Formula:
!   $$ \phi_{pqr} = P_p(a) \left(\frac{1-b}{2}\right)^p
!   P_q^{(2p+1,0)}(b) \left(\frac{1-c}{2}\right)^{p+q}
!   P_r^{(2p+2q+2,0)}(c) $$
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/OrthogonalBasis_Tetrahedron_test_2.F90)
!
! See also: [[OrthogonalBasis_Tetrahedron2]],
! [[OrthogonalBasis_Tetrahedron1_]]

INTERFACE OrthogonalBasis_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasis_Tetrahedron2_(order, x, y, z, &
                                              refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order
    REAL(DFP), INTENT(IN) :: x(:)
    !! first collapsed coordinate of the grid
    !! The grid has `SIZE(x) * SIZE(y) * SIZE(z)` points.
    REAL(DFP), INTENT(IN) :: y(:)
    !! second collapsed coordinate of the grid
    REAL(DFP), INTENT(IN) :: z(:)
    !! third collapsed coordinate of the grid
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values
    !! `ans(:, j)` holds the `j`-th mode at all points.
    !! `ans(j, :)` holds all modes at the `j`-th point.
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` written
    !! `nrow = SIZE(x) * SIZE(y) * SIZE(z)`
    !! `ncol = (order + 1) * (order + 2) * (order + 3) / 6`
  END SUBROUTINE OrthogonalBasis_Tetrahedron2_
END INTERFACE OrthogonalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                          BarycentricVertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex basis of a tetrahedron in barycentric coordinates.
!
! Returns the four vertex basis functions at the given points. The
! `i`-th vertex function is the `i`-th barycentric coordinate. It is 1 at
! vertex `i` and 0 at the other vertices.
!
! The result has one row per point and one column per vertex.
!
! @note
! Each column of `lambda` must hold four barycentric coordinates that sum
! to one. The routine does not check this.
! @endnote
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_{i=1}^{4} \lambda_i = 1 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/BarycentricVertexBasis_Tetrahedron_test_1.F90)
!
! See also: [[BarycentricVertexBasis_Tetrahedron_]]

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasis_Tetrahedron(lambda) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans(SIZE(lambda, 2), 4)
    !! basis values
    !! `ans(:, i)` holds the function of vertex `i` at all points.
  END FUNCTION BarycentricVertexBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricVertexBasis_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex basis of a tetrahedron, into `ans`.
!
! Computes the four vertex basis functions at the given points and
! writes them to `ans(1:nrow, 1:ncol)`.
! The `i`-th function is the `i`-th barycentric coordinate.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(lambda, 2)` rows and 4 columns.
! @endnote
!
! See also: [[BarycentricVertexBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricVertexBasis_Tetrahedron_(lambda, ans, &
                                                             nrow, ncol)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values
    !! `ans(:, i)` holds the function of vertex `i` at all points.
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, `SIZE(lambda, 2)`
    !! number of columns of `ans` written, always 4
  END SUBROUTINE BarycentricVertexBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                 BarycentricVertexBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Gradient of the vertex basis in barycentric coordinates.
!
! Returns the derivatives of the four vertex basis functions with
! respect to the four barycentric coordinates.
!
! The `i`-th vertex function is `lambda_i`. Its derivative with respect
! to `lambda_j` is 1 if `i = j` and 0 otherwise. Every point therefore
! gets the same 4 by 4 identity matrix.
!
! Formula:
!   $$ \frac{\partial \phi_{v_i}}{\partial \lambda_j} = \delta_{ij} $$
!
! See also: [[BarycentricVertexBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION BarycentricVertexBasisGradient_Tetrahedron(lambda) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans(SIZE(lambda, 2), 4, 4)
    !! derivatives of the vertex functions
    !! - `ans(:, :, i)` is the derivative with respect to `lambda_i`
    !! - first index: point
    !! - second index: vertex function
  END FUNCTION BarycentricVertexBasisGradient_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Edge basis of a tetrahedron in barycentric coordinates.
!
! Returns the edge basis functions at the given points. Each function
! is the product of the two barycentric coordinates of an edge and a
! Lobatto kernel function.
!
! The result has one row per point and one column per edge function.
! An edge of order `pe` has `pe - 1` functions.
! The columns of edge 1 come first, then edges 2 to 6.
!
! @note
! Each column of `lambda` must hold four barycentric coordinates that sum
! to one. The routine does not check this.
! @endnote
!
! Formula:
!   $$ \phi_e \propto \lambda_i \lambda_j $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/BarycentricEdgeBasis_Tetrahedron_test_1.F90)
!
! See also: [[BarycentricEdgeBasis_Tetrahedron_]],
! [[BarycentricEdgeBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron(pe1, pe2, pe3, pe4, &
                                                 pe5, pe6, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of edge 1, between vertices 1 and 2
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of edge 2, between vertices 1 and 3
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of edge 3, between vertices 1 and 4
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of edge 4, between vertices 2 and 3
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of edge 5, between vertices 2 and 4
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of edge 6, between vertices 3 and 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
  END FUNCTION BarycentricEdgeBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricEdgeBasis_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Edge basis of a tetrahedron, into `ans`.
!
! Computes the edge basis functions at the given points and writes them
! to `ans(1:nrow, 1:ncol)`.
! The routine evaluates the Lobatto kernel values itself.
!
! The result has one row per point and one column per edge function.
! An edge of order `pe` has `pe - 1` functions.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(lambda, 2)` rows and `ncol` columns.
! @endnote
!
! See also: [[BarycentricEdgeBasis_Tetrahedron]],
! [[BarycentricEdgeBasis_Tetrahedron2_]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricEdgeBasis_Tetrahedron_(pe1, pe2, pe3, &
                                       pe4, pe5, pe6, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of edge 1, between vertices 1 and 2
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of edge 2, between vertices 1 and 3
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of edge 3, between vertices 1 and 4
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of edge 4, between vertices 2 and 3
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of edge 5, between vertices 2 and 4
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of edge 6, between vertices 3 and 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per function
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, `SIZE(lambda, 2)`
    !! number of columns of `ans` written,
    !! `pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6`
  END SUBROUTINE BarycentricEdgeBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                           BarycentricEdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-30
! summary: Edge basis of a tetrahedron from precomputed Lobatto values.
!
! Returns the edge basis functions at the given points. The caller
! supplies the Lobatto kernel values `phi`, so the routine does not
! evaluate them again.
!
! The result has one row per point and one column per edge function.
! An edge of order `pe` has `pe - 1` functions.
!
! See also: [[BarycentricEdgeBasis_Tetrahedron]],
! [[BarycentricEdgeBasis_Tetrahedron2_]]

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasis_Tetrahedron2(pe1, pe2, pe3, &
                                       pe4, pe5, pe6, lambda, phi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of edge 1, between vertices 1 and 2
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of edge 2, between vertices 1 and 3
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of edge 3, between vertices 1 and 4
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of edge 4, between vertices 2 and 3
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of edge 5, between vertices 2 and 4
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of edge 6, between vertices 3 and 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index runs at least from 0 to the largest edge order
    !! minus 2.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
  END FUNCTION BarycentricEdgeBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricEdgeBasis_Tetrahedron2_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Edge basis of a tetrahedron from Lobatto values, into `ans`.
!
! Computes the edge basis functions at the given points and writes them
! to `ans(1:nrow, 1:ncol)`. The caller supplies the Lobatto kernel
! values `phi`, so the routine does not evaluate them again.
!
! The result has one row per point and one column per edge function.
! An edge of order `pe` has `pe - 1` functions.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(lambda, 2)` rows and `ncol` columns.
! @endnote
!
! See also: [[BarycentricEdgeBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricEdgeBasis_Tetrahedron2_(pe1, pe2, pe3, &
                                  pe4, pe5, pe6, lambda, phi, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of edge 1, between vertices 1 and 2
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of edge 2, between vertices 1 and 3
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of edge 3, between vertices 1 and 4
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of edge 4, between vertices 2 and 3
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of edge 5, between vertices 2 and 4
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of edge 6, between vertices 3 and 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index runs at least from 0 to the largest edge order
    !! minus 2.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per function
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, `SIZE(lambda, 2)`
    !! number of columns of `ans` written,
    !! `pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6`
  END SUBROUTINE BarycentricEdgeBasis_Tetrahedron2_
END INTERFACE

!----------------------------------------------------------------------------
!                                 BarycentricEdgeBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-30
! summary: Gradient of the edge basis in barycentric coordinates.
!
! Returns the derivatives of the edge basis functions with respect to
! the four barycentric coordinates. The caller supplies the Lobatto
! kernel values `phi` and their derivatives `dphi`.
!
! The result has one entry per point, per edge function and per
! coordinate. Entry `ans(:, :, i)` is the derivative with respect to
! `lambda_i`. Only the two coordinates of an edge give a nonzero value
! for the functions of that edge.
!
! See also: [[BarycentricEdgeBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricEdgeBasisGradient_Tetrahedron2( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & lambda, &
    & phi,  &
    & dphi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on  edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on  edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on  edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on  edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on  edge parallel to yz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index runs at least from 0 to the largest edge order
    !! minus 2.
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! derivative of the Lobatto kernel
    !! It has the same layout as `phi`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6, 4)
    !! derivatives of the edge functions
    !! - `ans(:, :, i)` is the derivative with respect to `lambda_i`
    !! - first index: point
    !! - second index: edge function
  END FUNCTION BarycentricEdgeBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Facet basis of a tetrahedron in barycentric coordinates.
!
! Returns the facet basis functions at the given points. Each function
! is a product of the three barycentric coordinates of a facet and two
! Lobatto kernel functions.
!
! The result has one row per point and one column per facet function.
! Each facet of order `ps` has `(ps - 1) * (ps - 2) / 2` functions.
! The columns of facet 1 come first, then facets 2, 3 and 4.
!
! @note
! Each column of `lambda` must hold four barycentric coordinates that sum
! to one. The routine does not check this.
! @endnote
!
! @warning
! The loops over the functions use `ps1` as the order of every facet.
! `ps2`, `ps3` and `ps4` only change the number of columns of `ans`.
! Pass the same order in all four arguments.
! @endwarning
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/BarycentricFacetBasis_Tetrahedron_test_1.F90)
! - [test 2](|base|Tetrahedron/examples/BarycentricFacetBasis_Tetrahedron_test_2.F90)
!
! See also: [[BarycentricFacetBasis_Tetrahedron_]],
! [[BarycentricFacetBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasis_Tetrahedron( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda  &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of facet 1, at least 3 for any function
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of facet 2
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of facet 3
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of facet 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
  END FUNCTION BarycentricFacetBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                        BarycentricFacetBasis_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Facet basis of a tetrahedron, into `ans`.
!
! Computes the facet basis functions at the given points and writes them
! to `ans(1:nrow, 1:ncol)`.
! The routine evaluates the Lobatto kernel values itself.
!
! The result has one row per point and one column per facet function.
! Each facet of order `ps` has `(ps - 1) * (ps - 2) / 2` functions.
!
! @note
! `ans` must be large enough before the call. It needs at least
! `SIZE(lambda, 2)` rows and `ncol` columns.
! @endnote
!
! @warning
! The loops over the functions use `ps1` as the order of every facet.
! `ps2`, `ps3` and `ps4` only change `ncol`.
! Pass the same order in all four arguments.
! @endwarning
!
! See also: [[BarycentricFacetBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricFacetBasis_Tetrahedron_(ps1, ps2, ps3, &
                                                 ps4, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of facet 1, at least 3 for any function
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of facet 2
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of facet 3
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of facet 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per function
    !! The routine writes only `ans(1:nrow, 1:ncol)`.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows of `ans` written, `SIZE(lambda, 2)`
    !! number of columns of `ans` written, the sum of
    !! `(ps - 1) * (ps - 2) / 2` over the four facets
  END SUBROUTINE BarycentricFacetBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                         BarycentricFacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Facet basis of a tetrahedron from precomputed Lobatto values.
!
! Returns the facet basis functions at the given points. The caller
! supplies the Lobatto kernel values `phi`, so the routine does not
! evaluate them again.
!
! The result has one row per point and one column per facet function.
! Each facet of order `ps` has `(ps - 1) * (ps - 2) / 2` functions.
!
! @warning
! The loops over the functions use `ps1` as the order of every facet.
! `ps2`, `ps3` and `ps4` only change the number of columns of `ans`.
! Pass the same order in all four arguments.
! @endwarning
!
! See also: [[BarycentricFacetBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasis_Tetrahedron2( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda,  &
    & phi &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of facet 1, at least 3 for any function
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of facet 2
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of facet 3
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of facet 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates of the points, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index must run at least from 0 to `ps1 - 2`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
  END FUNCTION BarycentricFacetBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Facet basis of a tetrahedron from Lobatto values, into `ans`.
!
! Writes the facet basis functions at the given points into `ans`.
! The caller supplies the Lobatto kernel values `phi`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
! Each facet of order `ps` has `(ps - 1) * (ps - 2) / 2` functions.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! @warning
! The loops over the functions use `ps1` as the order of every facet.
! `ps2`, `ps3` and `ps4` only change `ncol`.
! Pass the same order in all four arguments.
! @endwarning
!
! See also: [[BarycentricFacetBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricFacetBasis_Tetrahedron2_(ps1, ps2, ps3, &
                                            ps4, lambda, phi, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of facet 1, at least 3 for any function
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of facet 2
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of facet 3
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of facet 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index must run at least from 0 to `ps1 - 2`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! facet basis values, one row per point and one column per function
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(lambda, 2)`.
    !! `ncol` is the number of columns written, the sum of
    !! `(ps - 1) * (ps - 2) / 2` over the four facets.
  END SUBROUTINE BarycentricFacetBasis_Tetrahedron2_
END INTERFACE

!----------------------------------------------------------------------------
!                                 BarycentricFacetBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Gradient of the facet basis of a tetrahedron, from Lobatto values.
!
! Returns the gradient of the facet basis functions at the given points.
! The variables are the four barycentric coordinates.
! The caller supplies the Lobatto kernel values `phi` and their
! derivatives `dphi`.
!
! The result has one row per point, one column per facet function and one
! plane per barycentric coordinate. Plane `i` holds the derivative with
! respect to `lambda(i, :)`.
! Each facet of order `ps` has `(ps - 1) * (ps - 2) / 2` functions.
!
! @warning
! The loops over the functions use `ps1` as the order of every facet.
! `ps2`, `ps3` and `ps4` only change the number of columns of `ans`.
! Pass the same order in all four arguments.
! @endwarning
!
! @warning
! The routine reads one row of `phi` and `dphi` per block, not one row
! per point. The result is right only for a single point.
! @endwarning
!
! See also: [[BarycentricFacetBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricFacetBasisGradient_Tetrahedron2( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & lambda,  &
    & phi, &
    & dphi &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of facet 1, at least 3 for any function
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of facet 2
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of facet 3
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of facet 4
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form six blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1`, `lambda4-lambda1`,
    !! `lambda3-lambda2`, `lambda4-lambda2` and `lambda4-lambda3`.
    !! The second index must run at least from 0 to `ps1 - 2`.
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! derivatives of the Lobatto kernel values
    !! It has the same layout as `phi`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2, 4)
    !! gradient of the facet basis
    !! - index 1: point of evaluation
    !! - index 2: facet function number
    !! - index 3: barycentric coordinate `i` of the derivative
  END FUNCTION BarycentricFacetBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricCellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Cell basis of a tetrahedron in barycentric coordinates.
!
! Returns the cell basis functions at a set of points.
! Each point is a column of `lambda`, in barycentric coordinates.
!
! The result has one row per point and one column per cell function.
! The basis has `(pb - 1) * (pb - 2) * (pb - 3) / 6` cell functions.
! It has none if `pb` is less than 4.
!
! Each function is the product of the four barycentric coordinates and
! three Lobatto kernel values.
!
! Returns:
!   ans : REAL(DFP) - cell basis values, shape
!   `(SIZE(lambda, 2), (pb - 1) * (pb - 2) * (pb - 3) / 6)`
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/BarycentricCellBasis_Tetrahedron_test_1.F90)
!
! See also: [[BarycentricCellBasis_Tetrahedron_]],
! [[BarycentricCellBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasis_Tetrahedron( &
    & pb, lambda  &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of the cell basis
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
  END FUNCTION BarycentricCellBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Cell basis of a tetrahedron, into `ans`.
!
! Writes the cell basis functions at the given points into `ans`.
! The routine evaluates the Lobatto kernel values itself.
!
! The routine does not allocate `ans`. It sets all of `ans` to zero, then
! fills the first `nrow` rows and `ncol` columns.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! See also: [[BarycentricCellBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCellBasis_Tetrahedron_(pb, lambda, ans, &
                                                           nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of the cell basis
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! cell basis values, one row per point and one column per function
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(lambda, 2)`.
    !! `ncol` is the number of columns written,
    !! `(pb - 1) * (pb - 2) * (pb - 3) / 6`.
  END SUBROUTINE BarycentricCellBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                          BarycentricCellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Cell basis of a tetrahedron from precomputed Lobatto values.
!
! Returns the cell basis functions at the given points.
! The caller supplies the Lobatto kernel values `phi`, so the routine
! does not evaluate them again.
!
! The result has one row per point and one column per cell function.
! The basis has `(pb - 1) * (pb - 2) * (pb - 3) / 6` cell functions.
! It has none if `pb` is less than 4.
!
! @note
! The routine reads only the first three row blocks of `phi`.
! They belong to `lambda2-lambda1`, `lambda3-lambda1` and
! `lambda4-lambda1`.
! @endnote
!
! See also: [[BarycentricCellBasis_Tetrahedron]],
! [[BarycentricCellBasis_Tetrahedron2_]]

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasis_Tetrahedron2( &
    & pb, lambda, phi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of the cell basis
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1` and `lambda4-lambda1`.
    !! The second index must run at least from 0 to `pb - 4`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
  END FUNCTION BarycentricCellBasis_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Cell basis of a tetrahedron from Lobatto values, into `ans`.
!
! Writes the cell basis functions at the given points into `ans`.
! The caller supplies the Lobatto kernel values `phi`.
!
! The routine does not allocate `ans`. It sets all of `ans` to zero, then
! fills the first `nrow` rows and `ncol` columns.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! @note
! The routine reads only the first three row blocks of `phi`.
! They belong to `lambda2-lambda1`, `lambda3-lambda1` and
! `lambda4-lambda1`.
! @endnote
!
! See also: [[BarycentricCellBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCellBasis_Tetrahedron2_(pb, lambda, phi, &
                                                            ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of the cell basis
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1` and `lambda4-lambda1`.
    !! The second index must run at least from 0 to `pb - 4`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! cell basis values, one row per point and one column per function
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(lambda, 2)`.
    !! `ncol` is the number of columns written,
    !! `(pb - 1) * (pb - 2) * (pb - 3) / 6`.
  END SUBROUTINE BarycentricCellBasis_Tetrahedron2_
END INTERFACE

!----------------------------------------------------------------------------
!                                   BarycentricCellBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-25
! summary: Gradient of the cell basis of a tetrahedron, from Lobatto values.
!
! Returns the gradient of the cell basis functions at the given points.
! The variables are the four barycentric coordinates.
! The caller supplies the Lobatto kernel values `phi` and their
! derivatives `dphi`.
!
! The result has one row per point, one column per cell function and one
! plane per barycentric coordinate. Plane `i` holds the derivative with
! respect to `lambda(i, :)`.
!
! The basis has `(pb - 1) * (pb - 2) * (pb - 3) / 6` cell functions.
! It has none if `pb` is less than 4.
!
! @note
! The routine reads only the first three row blocks of `phi` and `dphi`.
! They belong to `lambda2-lambda1`, `lambda3-lambda1` and
! `lambda4-lambda1`.
! @endnote
!
! See also: [[BarycentricCellBasis_Tetrahedron2]]

INTERFACE
  MODULE PURE FUNCTION BarycentricCellBasisGradient_Tetrahedron2( &
    & pb, lambda, phi, dphi) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order of the cell basis
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! Lobatto kernel values
    !! The rows form blocks of one row per point. The blocks hold
    !! `lambda2-lambda1`, `lambda3-lambda1` and `lambda4-lambda1`.
    !! The second index must run at least from 0 to `pb - 4`.
    REAL(DFP), INTENT(IN) :: dphi(1:, 0:)
    !! derivatives of the Lobatto kernel values
    !! It has the same layout as `phi`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B, 4)
    !! gradient of the cell basis
    !! - index 1: point of evaluation
    !! - index 2: cell function number
    !! - index 3: barycentric coordinate `i` of the derivative
  END FUNCTION BarycentricCellBasisGradient_Tetrahedron2
END INTERFACE

!----------------------------------------------------------------------------
!                                  BarycentricHeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron, one order per entity.
!
! Returns the hierarchical basis at a set of points.
! Each edge and each facet has its own order. The cell has order `order`.
!
! The result has one row per point and one column per mode.
! The columns hold the vertex, edge, facet and cell modes, in this order.
! Each point is a column of `lambda`, in barycentric coordinates.
!
! 1. Edge modes exist only if at least one of `pe1` to `pe6` is 2 or more.
! 2. Facet modes exist only if at least one of `ps1` to `ps4` is 3 or more.
! 3. Cell modes exist only if `order` is 4 or more.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! @warning
! The facet modes use `ps1` as the order of every facet.
! Pass the same order in `ps1`, `ps2`, `ps3` and `ps4`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - basis values, one row per point and one column per
!   mode. The number of columns follows the formula below.
!
! Formula:
!   $$ ncol = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2}
!   + \frac{(p - 1)(p - 2)(p - 3)}{6} $$
!
! Here `p` is `order`.
!
! See also: [[BarycentricHeirarchicalBasis_Tetrahedron2]],
! [[BarycentricHeirarchicalBasis_Tetrahedron1_]]

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasis_Tetrahedron1(order, &
         pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell, 4 or more for any cell mode
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B)
  END FUNCTION BarycentricHeirarchicalBasis_Tetrahedron1
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron, per-entity orders, into `ans`.
!
! Writes the hierarchical basis of a tetrahedron into `ans`.
! Each edge and each facet has its own order. The cell has order `order`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! The columns of `ans` hold the vertex, edge, facet and cell modes, in
! this order.
!
! 1. Edge modes exist only if at least one of `pe1` to `pe6` is 2 or more.
! 2. Facet modes exist only if at least one of `ps1` to `ps4` is 3 or more.
! 3. Cell modes exist only if `order` is 4 or more.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! @warning
! The facet modes use `ps1` as the order of every facet.
! Pass the same order in `ps1`, `ps2`, `ps3` and `ps4`.
! @endwarning
!
! Formula:
!   $$ nrow = N_p $$
!   $$ ncol = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2}
!   + \frac{(p - 1)(p - 2)(p - 3)}{6} $$
!
! Here `p` is `order` and `N_p` is the number of points.
!
! See also: [[BarycentricHeirarchicalBasis_Tetrahedron1]],
! [[BarycentricHeirarchicalBasis_Tetrahedron2_]]

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron1_(order, &
    pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell, 4 or more for any cell mode
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(lambda, 2)`.
    !! `ncol` is the number of columns written.
    !! It follows the formula in the description.
  END SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron1_
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron of one order.
!
! Returns the hierarchical basis at a set of points.
! Every edge, facet and the cell use the same `order`.
!
! The result has one row per point and one column per mode.
! The columns hold the vertex, edge, facet and cell modes, in this order.
! Each point is a column of `lambda`, in barycentric coordinates.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape
!   `(SIZE(lambda, 2), (order + 1) * (order + 2) * (order + 3) / 6)`
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_{i=1}^{4} \lambda_i = 1 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/BarycentricHeirarchicalBasis_Tetrahedron_test_1.F90)
!
! See also: [[BarycentricHeirarchicalBasis_Tetrahedron1]],
! [[BarycentricHeirarchicalBasis_Tetrahedron2_]]

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION &
    BarycentricHeirarchicalBasis_Tetrahedron2(order, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the basis on every edge, facet and in the cell
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans(SIZE(lambda, 2), &
                     (order + 1) * (order + 2) * (order + 3) / 6_I4B)
  END FUNCTION BarycentricHeirarchicalBasis_Tetrahedron2
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron of one order, into `ans`.
!
! Writes the hierarchical basis of a tetrahedron into `ans`.
! Every edge, facet and the cell use the same `order`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! The columns of `ans` hold the vertex, edge, facet and cell modes, in
! this order.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad ncol = \frac{(p+1)(p+2)(p+3)}{6} $$
!
! Here `p` is `order` and `N_p` is the number of points.
!
! See also: [[BarycentricHeirarchicalBasis_Tetrahedron2]],
! [[BarycentricHeirarchicalBasis_Tetrahedron1_]]

INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron2_( &
    order, lambda, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the basis on every edge, facet and in the cell
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(lambda, 2)`.
    !! `ncol` is the number of columns written,
    !! `(order + 1) * (order + 2) * (order + 3) / 6`.
  END SUBROUTINE BarycentricHeirarchicalBasis_Tetrahedron2_
END INTERFACE BarycentricHeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                         BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-27
! summary: Gradient of the hierarchical basis, one order per entity.
!
! Returns the gradient of the hierarchical basis at a set of points.
! Each edge and each facet has its own order. The cell has order `order`.
!
! The result has three indices: point, mode and barycentric coordinate.
! `ans(i, j, k)` is the derivative of mode `j` at point `i` with respect
! to the `k`-th barycentric coordinate.
! The modes hold the vertex, edge, facet and cell modes, in this order.
! Each point is a column of `lambda`, in barycentric coordinates.
!
! 1. Edge modes exist only if at least one of `pe1` to `pe6` is 2 or more.
! 2. Facet modes exist only if at least one of `ps1` to `ps4` is 3 or more.
! 3. Cell modes exist only if `order` is 4 or more.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! Returns:
!   ans : REAL(DFP) - gradient values, shape `(N_p, nmode, 4)`
!
! Formula:
!   $$ nmode = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2}
!   + \frac{(p - 1)(p - 2)(p - 3)}{6} $$
!
! Here `p` is `order` and `N_p` is the number of points.
!
! See also: [[BarycentricHeirarchicalBasisGradient_Tetrahedron2]],
! [[BarycentricHeirarchicalBasis_Tetrahedron1]]

INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron1( &
  order, pe1, pe2, pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell, 4 or more for any cell mode
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B, 4_I4B)
    !! gradient of each mode at each point, see the description
  END FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron1
END INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                        BarycentricHeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-25
! summary: Gradient of the hierarchical basis of a tetrahedron, one order.
!
! Returns the gradient of the hierarchical basis at a set of points.
! Every edge, facet and the cell use the same `order`.
!
! The result has three indices: point, mode and barycentric coordinate.
! `ans(i, j, k)` is the derivative of mode `j` at point `i` with respect
! to the `k`-th barycentric coordinate.
! The modes hold the vertex, edge, facet and cell modes, in this order.
! Each point is a column of `lambda`, in barycentric coordinates.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! Returns:
!   ans : REAL(DFP) - gradient values, shape
!   `(SIZE(lambda, 2), (order + 1) * (order + 2) * (order + 3) / 6, 4)`
!
! See also: [[BarycentricHeirarchicalBasisGradient_Tetrahedron1]],
! [[BarycentricHeirarchicalBasis_Tetrahedron2]]

INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron
  MODULE PURE FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron2( &
    & order, lambda) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order on every edge, facet and in the cell
    REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! barycentric coordinates, one column per point
    !! The shape is `(4, number of points)`.
    REAL(DFP) :: ans( &
      & SIZE(lambda, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B, 4)
    !! gradient of each mode at each point, see the description
  END FUNCTION BarycentricHeirarchicalBasisGradient_Tetrahedron2
END INTERFACE BarycentricHeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                                   VertexBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex basis of a tetrahedron at a set of points.
!
! Returns the four vertex basis functions at a set of points.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The result has one row per point and one column per vertex.
! Each value is the barycentric coordinate of the point for that vertex.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape `(SIZE(xij, 2), 4)`
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_{i=1}^{4} \lambda_i = 1 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/VertexBasis_Tetrahedron_test_1.F90)
!
! See also: [[VertexBasis_Tetrahedron_]],
! [[BarycentricVertexBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION VertexBasis_Tetrahedron(xij, refTetrahedron) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans(SIZE(xij, 2), 4)
    !! `ans(:, i)` is the basis function of vertex `i` at all points
  END FUNCTION VertexBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Vertex basis of a tetrahedron at a set of points, into `ans`.
!
! Writes the four vertex basis functions at a set of points into `ans`.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad ncol = 4 $$
!
! Here `N_p` is the number of points.
!
! See also: [[VertexBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE VertexBasis_Tetrahedron_(xij, refTetrahedron, &
                                                  ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per vertex
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written, 4.
  END SUBROUTINE VertexBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     EdgeBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Edge basis of a tetrahedron at a set of points.
!
! Returns the edge basis functions at a set of points.
! Each of the six edges has its own order.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The result has one row per point and one column per edge mode.
! An edge of order `pe` contributes `pe - 1` modes.
! The modes of edge 1 come first, then those of edge 2, and so on.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape
!   `(SIZE(xij, 2), pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)`
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/EdgeBasis_Tetrahedron_test_1.F90)
!
! See also: [[EdgeBasis_Tetrahedron_]],
! [[BarycentricEdgeBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION EdgeBasis_Tetrahedron( &
    & pe1, &
    & pe2, &
    & pe3, &
    & pe4, &
    & pe5, &
    & pe6, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6)
    !! edge basis values, one row per point and one column per mode
  END FUNCTION EdgeBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Edge basis of a tetrahedron at a set of points, into `ans`.
!
! Writes the edge basis functions at a set of points into `ans`.
! Each of the six edges has its own order.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! An edge of order `pe` contributes `pe - 1` modes.
! The modes of edge 1 come first, then those of edge 2, and so on.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad ncol = pe_1 + pe_2 + pe_3 + pe_4 + pe_5
!   + pe_6 - 6 $$
!
! Here `N_p` is the number of points.
!
! See also: [[EdgeBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE EdgeBasis_Tetrahedron_(pe1, pe2, pe3, pe4, pe5, &
                                    pe6, xij, refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! edge basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written,
    !! `pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6`.
  END SUBROUTINE EdgeBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Facet basis of a tetrahedron at a set of points.
!
! Returns the facet basis functions at a set of points.
! Each of the four triangular facets has its own order.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The result has one row per point and one column per facet mode.
! A facet of order `ps` contributes `(ps - 1) * (ps - 2) / 2` modes.
! Facet modes exist only for orders of 3 or more.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape `(SIZE(xij, 2), ncol)`
!
! Formula:
!   $$ ncol = \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2} $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/FacetBasis_Tetrahedron_test_1.F90)
!
! See also: [[FacetBasis_Tetrahedron_]],
! [[BarycentricFacetBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION FacetBasis_Tetrahedron( &
    & ps1, &
    & ps2, &
    & ps3, &
    & ps4, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      &   (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2)
    !! facet basis values, one row per point and one column per mode
  END FUNCTION FacetBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Facet basis of a tetrahedron at a set of points, into `ans`.
!
! Writes the facet basis functions at a set of points into `ans`.
! Each of the four triangular facets has its own order.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! A facet of order `ps` contributes `(ps - 1) * (ps - 2) / 2` modes.
! Facet modes exist only for orders of 3 or more.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad
!   ncol = \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2} $$
!
! Here `N_p` is the number of points.
!
! See also: [[FacetBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE FacetBasis_Tetrahedron_(ps1, ps2, ps3, ps4, xij, &
                                              refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! facet basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written, see the formula.
  END SUBROUTINE FacetBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     CellBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Cell basis of a tetrahedron at a set of points.
!
! Returns the cell basis functions at a set of points.
! These are the modes inside the tetrahedron.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The result has one row per point and one column per cell mode.
! Cell modes exist only if `pb` is 4 or more.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape
!   `(SIZE(xij, 2), (pb - 1) * (pb - 2) * (pb - 3) / 6)`
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/CellBasis_Tetrahedron_test_1.F90)
!
! See also: [[CellBasis_Tetrahedron_]],
! [[BarycentricCellBasis_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION CellBasis_Tetrahedron( &
    & pb, &
    & xij, &
    & refTetrahedron) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in the cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (pb - 1) * (pb - 2) * (pb - 3) / 6_I4B)
    !! cell basis values, one row per point and one column per mode
  END FUNCTION CellBasis_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Cell basis of a tetrahedron at a set of points, into `ans`.
!
! Writes the cell basis functions at a set of points into `ans`.
! These are the modes inside the tetrahedron.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! Cell modes exist only if `pb` is 4 or more.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad ncol = \frac{(pb - 1)(pb - 2)(pb - 3)}{6} $$
!
! Here `N_p` is the number of points.
!
! See also: [[CellBasis_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE CellBasis_Tetrahedron_(pb, xij, refTetrahedron, &
                                                ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: pb
    !! order in the cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! cell basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written,
    !! `(pb - 1) * (pb - 2) * (pb - 3) / 6`.
  END SUBROUTINE CellBasis_Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron, one order per entity.
!
! Returns the hierarchical basis at a set of points.
! Each edge and each facet has its own order. The cell has order `order`.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The columns hold the vertex, edge, facet and cell modes, in this order.
!
! 1. Edge modes exist only if at least one of `pe1` to `pe6` is 2 or more.
! 2. Facet modes exist only if at least one of `ps1` to `ps4` is 3 or more.
! 3. Cell modes exist only if `order` is 4 or more.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! @warning
! The facet modes use `ps1` as the order of every facet.
! Pass the same order in `ps1`, `ps2`, `ps3` and `ps4`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - basis values, one row per point and one column per
!   mode. The number of columns follows the formula below.
!
! Formula:
!   $$ ncol = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2}
!   + \frac{(p - 1)(p - 2)(p - 3)}{6} $$
!
! Here `p` is `order`.
!
! See also: [[HeirarchicalBasis_Tetrahedron1_]],
! [[HeirarchicalBasis_Tetrahedron2]]

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron1(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell, 4 or more for any cell mode
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B)
    !! basis values, one row per point and one column per mode
  END FUNCTION HeirarchicalBasis_Tetrahedron1
END INTERFACE HeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron, per-entity orders, into `ans`.
!
! Writes the hierarchical basis at a set of points into `ans`.
! Each edge and each facet has its own order. The cell has order `order`.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! The columns hold the vertex, edge, facet and cell modes, in this order.
!
! 1. Edge modes exist only if at least one of `pe1` to `pe6` is 2 or more.
! 2. Facet modes exist only if at least one of `ps1` to `ps4` is 3 or more.
! 3. Cell modes exist only if `order` is 4 or more.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! @warning
! The facet modes use `ps1` as the order of every facet.
! Pass the same order in `ps1`, `ps2`, `ps3` and `ps4`.
! @endwarning
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p $$
!   $$ ncol = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{j=1}^{4} \frac{(ps_j - 1)(ps_j - 2)}{2}
!   + \frac{(p - 1)(p - 2)(p - 3)}{6} $$
!
! Here `p` is `order` and `N_p` is the number of points.
!
! See also: [[HeirarchicalBasis_Tetrahedron1]],
! [[HeirarchicalBasis_Tetrahedron2_]]

INTERFACE HeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Tetrahedron1_(order, pe1, pe2, &
           pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron, ans, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell, 4 or more for any cell mode
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order on the edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order on the edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order on the edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order on the edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order on the edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order on the edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order on the facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order on the facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order on the facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order on the facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written.
    !! It follows the formula in the description.
  END SUBROUTINE HeirarchicalBasis_Tetrahedron1_
END INTERFACE HeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                             HeirarchicalBasis_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron of one order.
!
! Returns the hierarchical basis at a set of points.
! Every edge, facet and the cell use the same `order`.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The columns hold the vertex, edge, facet and cell modes, in this order.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! Returns:
!   ans : REAL(DFP) - basis values, shape
!   `(SIZE(xij, 2), (order + 1) * (order + 2) * (order + 3) / 6)`
!
! Formula:
!   $$ \phi_{v_i} = \lambda_i, \quad \sum_{i=1}^{4} \lambda_i = 1 $$
!
! The first four modes are the vertex modes.
! Mode `i` equals the `i`-th barycentric coordinate of the point.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/HeirarchicalBasis_Tetrahedron_test_1.F90)
! - [test 2](|base|Tetrahedron/examples/HeirarchicalBasis_Tetrahedron_test_2.F90) with order 4
!
! See also: [[HeirarchicalBasis_Tetrahedron2_]],
! [[HeirarchicalBasis_Tetrahedron1]]

INTERFACE HeirarchicalBasis_Tetrahedron
  MODULE PURE FUNCTION HeirarchicalBasis_Tetrahedron2(order, xij, &
                                                   refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the basis on every edge, facet and in the cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B)
    !! basis values, one row per point and one column per mode
  END FUNCTION HeirarchicalBasis_Tetrahedron2
END INTERFACE HeirarchicalBasis_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Hierarchical basis of a tetrahedron of one order, into `ans`.
!
! Writes the hierarchical basis at a set of points into `ans`.
! Every edge, facet and the cell use the same `order`.
! Each point is a column of `xij`, in Cartesian coordinates.
! The reference tetrahedron `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
!
! The routine does not allocate `ans`. It returns the used part of `ans`
! in `nrow` and `ncol`.
!
! The columns hold the vertex, edge, facet and cell modes, in this order.
!
! The routine spells `Heirarchical` as shown. Use that spelling in calls.
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ nrow = N_p, \quad ncol = \frac{(p + 1)(p + 2)(p + 3)}{6} $$
!
! Here `p` is `order` and `N_p` is the number of points.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/HeirarchicalBasis_Tetrahedron__test_1.F90)
!
! See also: [[HeirarchicalBasis_Tetrahedron2]],
! [[HeirarchicalBasis_Tetrahedron1_]]

INTERFACE HeirarchicalBasis_Tetrahedron_
  MODULE PURE SUBROUTINE HeirarchicalBasis_Tetrahedron2_(order, xij, &
                                              refTetrahedron, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the basis on every edge, facet and in the cell
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! basis values, one row per point and one column per mode
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(xij, 2)`.
    !! `ncol` is the number of columns written,
    !! `(order + 1) * (order + 2) * (order + 3) / 6`.
  END SUBROUTINE HeirarchicalBasis_Tetrahedron2_
END INTERFACE HeirarchicalBasis_Tetrahedron_

!----------------------------------------------------------------------------
!                                             LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Values of all Lagrange polynomials at one point.
!
! Evaluates all Lagrange polynomials of degree `order` at one point `x`.
! The polynomials belong to the nodes in the columns of `xij`.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! Pass `coeff` to keep them between calls.
!
! 1. On the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and stores them in `coeff`.
! 2. On later calls, set `firstCall` to `.FALSE.`.
!    The routine reuses the values in `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call. The default of `firstCall` is `.TRUE.`.
!
! @warning
! Use the same `xij`, `order`, `refTetrahedron` and `basisType`
! whenever you reuse `coeff`. The routine does not check this.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials, size `SIZE(xij, 2)`
!
! Formula:
!   $$ ans_i = \ell_i(x), \quad \ell_i(x^{node}_k) = \delta_{ik} $$
!
! The point `x^{node}_k` is the `k`-th column of `xij`.
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/LagrangeEvalAll_Tetrahedron_test_2.F90)
!
! See also: [[LagrangeEvalAll_Tetrahedron1_]],
! [[LagrangeEvalAll_Tetrahedron2]]

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron1(order, x, xij, &
           refTetrahedron, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(3)
    !! point of evaluation
    !! `x(1)`, `x(2)` and `x(3)` are the x, y and z coordinates.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine uses the given `coeff`
    !! The default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, `Monomial` (default) or `Heirarchical`
    !! Any other value selects an orthogonal family.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(xij, 2))
    !! values of the Lagrange polynomials at `x`
  END FUNCTION LagrangeEvalAll_Tetrahedron1
END INTERFACE LagrangeEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Values of all Lagrange polynomials at one point, into `ans`.
!
! Evaluates all Lagrange polynomials of degree `order` at one point `x`.
! The polynomials belong to the nodes in the columns of `xij`.
!
! The routine does not allocate `ans`.
! It returns the number of values written in `tsize`.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! Pass `coeff` to keep them between calls.
!
! 1. On the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and stores them in `coeff`.
! 2. On later calls, set `firstCall` to `.FALSE.`.
!    The routine reuses the values in `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call. The default of `firstCall` is `.TRUE.`.
!
! @warning
! Use the same `xij`, `order`, `refTetrahedron` and `basisType`
! whenever you reuse `coeff`. The routine does not check this.
! @endwarning
!
! @warning
! `ans` must have at least `SIZE(xij, 2)` entries.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ ans_i = \ell_i(x), \quad \ell_i(x^{node}_k) = \delta_{ik} $$
!
! The point `x^{node}_k` is the `k`-th column of `xij`.
!
! See also: [[LagrangeEvalAll_Tetrahedron1]],
! [[LagrangeEvalAll_Tetrahedron2_]]

INTERFACE LagrangeEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeEvalAll_Tetrahedron1_(order, x, xij, ans, &
            tsize, refTetrahedron, coeff, firstCall, basisType, alpha, beta, &
                                                  lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(3)
    !! point of evaluation
    !! `x(1)`, `x(2)` and `x(3)` are the x, y and z coordinates.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! values of the Lagrange polynomials at `x`
    !! The routine fills the first `tsize` entries.
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in `ans`, `SIZE(xij, 2)`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials
    !! The shape is `(SIZE(xij, 2), SIZE(xij, 2))`.
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine uses the given `coeff`
    !! The default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, `Monomial` (default) or `Heirarchical`
    !! Any other value selects an orthogonal family.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Tetrahedron1_
END INTERFACE LagrangeEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!                                                LagrangeEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Values of all Lagrange polynomials on a tetrahedron.
!
! Evaluates all Lagrange polynomials of degree `order` at several points.
! Each point is a column of `x`.
! The polynomials belong to the nodes in the columns of `xij`.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! Pass `coeff` to keep them between calls.
!
! 1. On the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and stores them in `coeff`.
! 2. On later calls, set `firstCall` to `.FALSE.`.
!    The routine reuses the values in `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call. The default of `firstCall` is `.TRUE.`.
!
! @warning
! Use the same `xij`, `order`, `refTetrahedron` and `basisType`
! whenever you reuse `coeff`. The routine does not check this.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - values of the polynomials, shape
!   `(SIZE(x, 2), SIZE(xij, 2))`
!
! Formula:
!   $$ ans_{ij} = \ell_j(x_i), \quad \ell_j(x^{node}_k) = \delta_{jk} $$
!
! Here `x_i` is the `i`-th point of `x`.
! The point `x^{node}_k` is the `k`-th column of `xij`.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeEvalAll_Tetrahedron_test_1.F90)
!
! See also: [[LagrangeEvalAll_Tetrahedron2_]],
! [[LagrangeEvalAll_Tetrahedron1]]

INTERFACE LagrangeEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeEvalAll_Tetrahedron2(order, x, xij, &
           refTetrahedron, coeff, firstCall, basisType, alpha, beta, lambda) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine uses the given `coeff`
    !! The default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, `Monomial` (default) or `Heirarchical`
    !! Any other value selects an orthogonal family.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2))
    !! values of the Lagrange polynomials
    !! One row per point and one column per node.
  END FUNCTION LagrangeEvalAll_Tetrahedron2
END INTERFACE LagrangeEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Values of all Lagrange polynomials on a tetrahedron, into `ans`.
!
! Evaluates all Lagrange polynomials of degree `order` at several points.
! Each point is a column of `x`.
! The polynomials belong to the nodes in the columns of `xij`.
!
! The routine does not allocate `ans`.
! It returns the used part of `ans` in `nrow` and `ncol`.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! Pass `coeff` to keep them between calls.
!
! 1. On the first call, set `firstCall` to `.TRUE.`.
!    The routine computes the coefficients and stores them in `coeff`.
! 2. On later calls, set `firstCall` to `.FALSE.`.
!    The routine reuses the values in `coeff`.
!
! If `coeff` is absent, the routine computes the coefficients on every
! call. The default of `firstCall` is `.TRUE.`.
!
! @warning
! Use the same `xij`, `order`, `refTetrahedron` and `basisType`
! whenever you reuse `coeff`. The routine does not check this.
! @endwarning
!
! @warning
! `ans` must have at least `nrow` rows and `ncol` columns.
! The routine does not check this.
! @endwarning
!
! Formula:
!   $$ ans_{ij} = \ell_j(x_i), \quad \ell_j(x^{node}_k) = \delta_{jk} $$
!
! Here `x_i` is the `i`-th point of `x`.
! The point `x^{node}_k` is the `k`-th column of `xij`.
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeEvalAll_Tetrahedron__test_1.F90)
!
! See also: [[LagrangeEvalAll_Tetrahedron2]],
! [[LagrangeEvalAll_Tetrahedron1_]]

INTERFACE LagrangeEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeEvalAll_Tetrahedron2_(order, x, xij, ans, &
       nrow, ncol, refTetrahedron, coeff, firstCall, basisType, alpha, beta, &
                                                  lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! points of evaluation, one column per point
    !! The shape is `(3, number of points)`.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! values of the Lagrange polynomials
    !! The routine fills the first `nrow` rows and `ncol` columns.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! `nrow` is the number of rows written, `SIZE(x, 2)`.
    !! `ncol` is the number of columns written, `SIZE(xij, 2)`.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(:, :)
    !! coefficients of the Lagrange polynomials
    !! The shape is `(SIZE(xij, 2), SIZE(xij, 2))`.
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! if `.TRUE.`, the routine computes `coeff`
    !! if `.FALSE.`, the routine uses the given `coeff`
    !! The default is `.TRUE.`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! polynomial family, `Monomial` (default) or `Heirarchical`
    !! Any other value selects an orthogonal family.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeEvalAll_Tetrahedron2_
END INTERFACE LagrangeEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Number of quadrature points of a tetrahedron rule.
!
! Returns how many points the quadrature rule of a given `order` has.
! The rule integrates polynomials of degree `order` exactly.
!
! For `order` from 0 to 21, it returns the size of the economical rule
! from the Solin tables.
! For a higher `order`, it returns the size of a tensor product rule.
! With `n = 1 + order / 2`, that size is `n * (n + 1) * n`.
!
! @note
! `quadType` is not used. The result depends only on `order`.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - number of quadrature points
!
! See also: [[QuadraturePoint_Tetrahedron]],
! [[QuadraturePoint_Tetrahedron_]]

INTERFACE
  MODULE FUNCTION QuadratureNumber_Tetrahedron(order, quadType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the integrand, the rule is exact up to this degree
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    INTEGER(I4B) :: ans
    !! number of quadrature points
  END FUNCTION QuadratureNumber_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Quadrature points and weights of a tetrahedron, by order.
!
! Returns the quadrature points and weights for an integrand of
! polynomial degree `order`.
! The result has one column per point.
! Rows 1 to 3 hold the x, y and z coordinates.
! Row 4 holds the weight.
!
! For `order` up to 21, it uses an economical rule from the Solin tables.
! For a higher `order`, it uses a tensor product rule instead.
! [[QuadratureNumber_Tetrahedron]] gives the number of points.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! Formula:
!   $$ \int_K f\,dx \approx \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/QuadraturePoint_Tetrahedron_test_1.F90) with `order` and `xij`
! - [test 2](|base|Tetrahedron/examples/QuadraturePoint_Tetrahedron_test_2.F90) with `order` above 21
!
! See also: [[QuadraturePoint_Tetrahedron_]],
! [[TensorQuadraturePoint_Tetrahedron]]

INTERFACE QuadraturePoint_Tetrahedron
  MODULE FUNCTION QuadraturePoint_Tetrahedron1(&
    & order, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    !! The rule is exact up to this degree.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, shape `(4, n)`
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
  END FUNCTION QuadraturePoint_Tetrahedron1
END INTERFACE QuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Quadrature points and weights of a tetrahedron, into `ans`.
!
! Computes the quadrature points and weights for an integrand of
! polynomial degree `order`, and stores them in `ans`.
! Column `i` of `ans` holds the coordinates of point `i` in rows 1 to 3.
! Its weight is in row 4.
!
! For `order` up to 21, it uses an economical rule from the Solin tables.
! For a higher `order`, it uses a tensor product rule instead.
! [[QuadratureNumber_Tetrahedron]] gives the number of points.
!
! The routine sets `nrow` and `ncol`.
! Only `ans(1:nrow, 1:ncol)` holds data.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! Formula:
!   $$ \int_K f\,dx \approx \sum_i w_i f(x_i) $$
!
! See also: [[QuadraturePoint_Tetrahedron]]

INTERFACE QuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE QuadraturePoint_Tetrahedron1_(order, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    !! The rule is exact up to this degree.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights
    !! It needs at least 4 rows and one column per point.
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` that hold data
  END SUBROUTINE QuadraturePoint_Tetrahedron1_
END INTERFACE QuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                            QuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Quadrature points and weights of a tetrahedron, by point count.
!
! Returns the quadrature points and weights of the rule with `nips(1)`
! points.
! The result has one column per point.
! Rows 1 to 3 hold the x, y and z coordinates.
! Row 4 holds the weight.
!
! Only the economical rules of the Solin tables are available.
! They have 1, 4, 5, 11, 14, 24, 31, 43, 53, 126, 210, 330, 495, 715 or
! 1001 points.
! For any other `nips(1)`, the routine writes an error message to `stderr`.
! The result then holds no valid points.
! Use [[TensorQuadraturePoint_Tetrahedron]] for other point counts.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! Formula:
!   $$ \int_K f\,dx \approx \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/QuadraturePoint_Tetrahedron_test_1.F90) with `nips = 4`
!
! See also: [[QuadraturePoint_Tetrahedron_]],
! [[TensorQuadraturePoint_Tetrahedron]]

INTERFACE QuadraturePoint_Tetrahedron
  MODULE FUNCTION QuadraturePoint_Tetrahedron2(&
    & nips, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points.
    !! It must be a count that has a Solin rule.
    !! Other values give an error.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, shape `(4, nips(1))`
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
  END FUNCTION QuadraturePoint_Tetrahedron2
END INTERFACE QuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Quadrature points and weights by point count, into `ans`.
!
! Computes the quadrature points and weights of the rule with `nips(1)`
! points, and stores them in `ans`.
! Column `i` of `ans` holds the coordinates of point `i` in rows 1 to 3.
! Its weight is in row 4.
!
! Only the economical rules of the Solin tables are available.
! They have 1, 4, 5, 11, 14, 24, 31, 43, 53, 126, 210, 330, 495, 715 or
! 1001 points.
! For any other `nips(1)`, the routine writes an error message to `stderr`.
! It then sets `nrow` and `ncol` to 0 and leaves `ans` unchanged.
! Use [[TensorQuadraturePoint_Tetrahedron]] for other point counts.
!
! Otherwise only `ans(1:nrow, 1:ncol)` holds data.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! Formula:
!   $$ \int_K f\,dx \approx \sum_i w_i f(x_i) $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/QuadraturePoint_Tetrahedron_test_1.F90) with `nips = 7`, which has no rule
!
! See also: [[QuadraturePoint_Tetrahedron]]

INTERFACE QuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE QuadraturePoint_Tetrahedron2_(nips, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nips(1)
    !! number of quadrature points.
    !! It must be a count that has a Solin rule.
    !! Other values give an error.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights
    !! It needs at least 4 rows and `nips(1)` columns.
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` that hold data
  END SUBROUTINE QuadraturePoint_Tetrahedron2_
END INTERFACE QuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
! TensorQuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor product quadrature of a tetrahedron, by order.
!
! Returns the points and weights of a tensor product rule on a
! tetrahedron.
! The result has 4 rows and one column per point.
! Rows 1 to 3 hold the x, y and z coordinates.
! Row 4 holds the weight.
!
! With `n = 1 + order / 2`, the rule has `n` points in the x direction,
! `n + 1` in the y direction and `n` in the z direction.
! It is exact for an integrand of polynomial degree `order`.
! The rule has `n * (n + 1) * n` points.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Tetrahedron_]],
! [[QuadraturePoint_Tetrahedron]]

INTERFACE TensorQuadraturePoint_Tetrahedron
  MODULE FUNCTION TensorQuadraturePoint_Tetrahedron1(order, quadType, &
    & refTetrahedron, xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    !! The rule is exact up to this degree.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, shape `(4, n)`
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
  END FUNCTION TensorQuadraturePoint_Tetrahedron1
END INTERFACE TensorQuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Tensor product quadrature of a tetrahedron, into `ans`.
!
! Computes the points and weights of a tensor product rule on a
! tetrahedron, and stores them in `ans`.
! Column `i` of `ans` holds the coordinates of point `i` in rows 1 to 3.
! Its weight is in row 4.
! The routine sets `nrow` to 4 and `ncol` to the number of points.
!
! With `n = 1 + order / 2`, the rule has `n` points in the x direction,
! `n + 1` in the y direction and `n` in the z direction.
! It is exact for an integrand of polynomial degree `order`.
! The rule has `n * (n + 1) * n` points.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Tetrahedron]]

INTERFACE TensorQuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE TensorQuadraturePoint_Tetrahedron1_(order, quadType, &
                                         refTetrahedron, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial degree of the integrand
    !! The rule is exact up to this degree.
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights
    !! It needs at least 4 rows and one column per point.
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` that hold data
  END SUBROUTINE TensorQuadraturePoint_Tetrahedron1_
END INTERFACE TensorQuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                        TensorQuadraturePoints_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-20
! summary: Tensor product quadrature of a tetrahedron, by point counts.
!
! Returns the points and weights of a tensor product rule on a
! tetrahedron.
! The result has 4 rows and one column per point.
! Rows 1 to 3 hold the x, y and z coordinates.
! Row 4 holds the weight.
!
! The rule has `nipsx(1)` points in the x direction, `nipsy(1)` in the y
! direction and `nipsz(1)` in the z direction.
! It has `nipsx(1) * nipsy(1) * nipsz(1)` points.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Tetrahedron_]],
! [[QuadraturePoint_Tetrahedron]]

INTERFACE TensorQuadraturePoint_Tetrahedron
  MODULE FUNCTION TensorQuadraturePoint_Tetrahedron2( &
    & nipsx, &
    & nipsy, &
    & nipsz, &
    & quadType, &
    & refTetrahedron, &
    & xij) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in the x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in the y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in the z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), ALLOCATABLE :: ans(:, :)
    !! quadrature points and weights, shape `(4, n)`
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
  END FUNCTION TensorQuadraturePoint_Tetrahedron2
END INTERFACE TensorQuadraturePoint_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Tensor product quadrature by point counts, into `ans`.
!
! Computes the points and weights of a tensor product rule on a
! tetrahedron, and stores them in `ans`.
! Column `i` of `ans` holds the coordinates of point `i` in rows 1 to 3.
! Its weight is in row 4.
! The routine sets `nrow` to 4 and `ncol` to the number of points.
!
! The rule has `nipsx(1)` points in the x direction, `nipsy(1)` in the y
! direction and `nipsz(1)` in the z direction.
! It has `nipsx(1) * nipsy(1) * nipsz(1)` points.
!
! If `xij` is present, the points lie on the tetrahedron with vertices
! `xij`, and the weights sum to its volume.
! Otherwise `refTetrahedron` selects the reference tetrahedron.
! `"UNIT"` gives weights that sum to 1/6.
! `"BIUNIT"` gives weights that sum to 4/3.
! Any name that does not start with `B` selects `"UNIT"`.
!
! See also: [[TensorQuadraturePoint_Tetrahedron]]

INTERFACE TensorQuadraturePoint_Tetrahedron_
  MODULE SUBROUTINE TensorQuadraturePoint_Tetrahedron2_(nipsx, nipsy, &
                        nipsz, quadType, refTetrahedron, xij, ans, nrow, ncol)
    INTEGER(I4B), INTENT(IN) :: nipsx(1)
    !! number of integration points in the x direction
    INTEGER(I4B), INTENT(IN) :: nipsy(1)
    !! number of integration points in the y direction
    INTEGER(I4B), INTENT(IN) :: nipsz(1)
    !! number of integration points in the z direction
    INTEGER(I4B), INTENT(IN) :: quadType
    !! quadrature point type, not used
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` or `"BIUNIT"`
    !! The routine ignores it if `xij` is present.
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! vertex coordinates of the tetrahedron, shape `(3, 4)`
    !! One column per vertex.
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! quadrature points and weights
    !! It needs at least 4 rows and one column per point.
    !! Rows 1 to 3 are the coordinates and row 4 is the weight.
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns of `ans` that hold data
  END SUBROUTINE TensorQuadraturePoint_Tetrahedron2_
END INTERFACE TensorQuadraturePoint_Tetrahedron_

!----------------------------------------------------------------------------
!                                       LagrangeGradientEvalAll_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradients of all Lagrange polynomials of a tetrahedron.
!
! Evaluates the gradients of all Lagrange polynomials of degree `order`.
! The polynomials are cardinal on the nodes `xij`.
! The routine evaluates each gradient at every point of `x`.
!
! Element `(i, j, k)` of the result is the derivative of polynomial `j`
! with respect to coordinate `k`, at point `i`.
! The coordinates are x, y and z, so `k` runs from 1 to 3.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! If `coeff` is absent, the routine computes them in every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them and
! stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reads them from
! `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! @note
! Reuse `coeff` only with the same `xij`, `order`, `refTetrahedron` and
! `basisType`.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - gradients, shape `(SIZE(x, 2), SIZE(xij, 2), 3)`
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeGradientEvalAll_Tetrahedron_test_1.F90) with order 1
! - [test 2](|base|Tetrahedron/examples/LagrangeGradientEvalAll_Tetrahedron_test_2.F90)
!
! See also: [[LagrangeGradientEvalAll_Tetrahedron_]]

INTERFACE LagrangeGradientEvalAll_Tetrahedron
  MODULE FUNCTION LagrangeGradientEvalAll_Tetrahedron1( &
    & order, &
    & x, &
    & xij, &
    & refTetrahedron, &
    & coeff, &
    & firstCall, &
    & basisType, &
    & alpha, &
    & beta, &
    & lambda &
    & ) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, shape `(3, SIZE(x, 2))`
    !! `x(1, :)`, `x(2, :)` and `x(3, :)` are the x, y and z coordinates.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! The routine fills it if `firstCall` is `.TRUE.`.
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! `.TRUE.` computes `coeff`, `.FALSE.` reuses it, default `.TRUE.`
    !! It has an effect only if `coeff` is present.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis used to build the polynomials, default `Monomial`
    !! Other values are `Heirarchical` or an orthogonal family such as
    !! `Legendre`, `Lobatto`, `Chebyshev`, `Jacobi` or `Ultraspherical`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
    REAL(DFP) :: ans(SIZE(x, 2), SIZE(xij, 2), 3)
    !! gradients of the Lagrange polynomials at the points `x`.
    !! The first index is the point, the second is the polynomial and
    !! the third is the coordinate.
  END FUNCTION LagrangeGradientEvalAll_Tetrahedron1
END INTERFACE LagrangeGradientEvalAll_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradients of all Lagrange polynomials, into `ans`.
!
! Evaluates the gradients of all Lagrange polynomials of degree `order`.
! The polynomials are cardinal on the nodes `xij`.
! The routine evaluates each gradient at every point of `x`.
!
! Element `(i, j, k)` of `ans` is the derivative of polynomial `j`
! with respect to coordinate `k`, at point `i`.
! The coordinates are x, y and z, so `k` runs from 1 to 3.
! The routine sets `dim1`, `dim2` and `dim3` to the sizes that hold data.
! Only `ans(1:dim1, 1:dim2, 1:dim3)` is meaningful.
!
! The coefficients of the polynomials depend only on `xij`, `order`,
! `refTetrahedron` and `basisType`.
! If `coeff` is absent, the routine computes them in every call.
! If `coeff` is present and `firstCall` is `.TRUE.`, it computes them and
! stores them in `coeff`.
! If `coeff` is present and `firstCall` is `.FALSE.`, it reads them from
! `coeff`.
! `firstCall` is `.TRUE.` by default.
!
! @note
! Reuse `coeff` only with the same `xij`, `order`, `refTetrahedron` and
! `basisType`.
! @endnote
!
! Formula:
!   $$ \sum_j \nabla \ell_j(x) = 0 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/LagrangeGradientEvalAll_Tetrahedron__test_1.F90) with order 1
!
! See also: [[LagrangeGradientEvalAll_Tetrahedron]]

INTERFACE LagrangeGradientEvalAll_Tetrahedron_
  MODULE SUBROUTINE LagrangeGradientEvalAll_Tetrahedron1_(order, x, xij, &
         ans, dim1, dim2, dim3, refTetrahedron, coeff, firstCall, basisType, &
                                                          alpha, beta, lambda)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the Lagrange polynomials
    REAL(DFP), INTENT(IN) :: x(:, :)
    !! evaluation points, shape `(3, SIZE(x, 2))`
    !! `x(1, :)`, `x(2, :)` and `x(3, :)` are the x, y and z coordinates.
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! interpolation nodes, one column per node
    !! The shape is `(3, LagrangeDOF_Tetrahedron(order))`.
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradients of the Lagrange polynomials at the points `x`.
    !! The first index is the point, the second is the polynomial and
    !! the third is the coordinate.
    !! It needs at least shape `(SIZE(x, 2), SIZE(xij, 2), 3)`.
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! sizes of `ans` that hold data
    !! They are `SIZE(x, 2)`, `SIZE(xij, 2)` and 3.
    CHARACTER(*), OPTIONAL, INTENT(IN) :: refTetrahedron
    !! reference tetrahedron, `"UNIT"` (default) or `"BIUNIT"`
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
    !! coefficients of the Lagrange polynomials
    !! The routine fills it if `firstCall` is `.TRUE.`.
    LOGICAL(LGT), OPTIONAL :: firstCall
    !! `.TRUE.` computes `coeff`, `.FALSE.` reuses it, default `.TRUE.`
    !! It has an effect only if `coeff` is present.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: basisType
    !! basis used to build the polynomials, default `Monomial`
    !! Other values are `Heirarchical` or an orthogonal family such as
    !! `Legendre`, `Lobatto`, `Chebyshev`, `Jacobi` or `Ultraspherical`.
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Jacobi parameter
    REAL(DFP), OPTIONAL, INTENT(IN) :: lambda
    !! Ultraspherical parameter
  END SUBROUTINE LagrangeGradientEvalAll_Tetrahedron1_
END INTERFACE LagrangeGradientEvalAll_Tetrahedron_

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the orthogonal basis of a tetrahedron, as a function.
!
! Returns the gradient of the orthogonal polynomial basis at a set of
! points of the reference tetrahedron.
! The basis has `(order + 1) * (order + 2) * (order + 3) / 6` functions.
!
! Each column of `xij` is one point.
! `refTetrahedron` selects the reference tetrahedron.
! It must be `"UNIT"` or `"BIUNIT"`, in any letter case.
!
! `ans(i, j, k)` is the derivative of basis function `j` with respect to
! coordinate `k` at point `i`.
!
! @warning
! The current implementation does not fill the derivative terms
! correctly. It overwrites the second term and never sets the third.
! Do not rely on the values.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - gradient, with shape (npoints, nbasis, 3)
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/OrthogonalBasisGradient_Tetrahedron_test_1.F90)
!
! See also: [[OrthogonalBasisGradient_Tetrahedron1_]],
! [[OrthogonalBasis_Tetrahedron]]

INTERFACE OrthogonalBasisGradient_Tetrahedron
  MODULE FUNCTION OrthogonalBasisGradient_Tetrahedron1(order, xij, &
                                                   refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans(SIZE(xij, 2), &
                     (order + 1) * (order + 2) * (order + 3) / 6, 3)
    !! gradient of the basis functions
    !! `ans(i, j, k)` is the derivative of function `j` with respect
    !! to coordinate `k` at point `i`
  END FUNCTION OrthogonalBasisGradient_Tetrahedron1
END INTERFACE OrthogonalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                       OrthogonalBasisGradient_Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the orthogonal basis of a tetrahedron, into `ans`.
!
! Computes the gradient of the orthogonal polynomial basis at a set of
! points of the reference tetrahedron.
! It writes the result into the caller's array `ans`.
! The basis has `(order + 1) * (order + 2) * (order + 3) / 6` functions.
!
! Each column of `xij` is one point.
! `refTetrahedron` selects the reference tetrahedron.
! Only its first letter counts: `U` for `"UNIT"` or `B` for `"BIUNIT"`.
!
! `ans(i, j, k)` is the derivative of basis function `j` with respect to
! coordinate `k` at point `i`.
! The routine fills `ans(1:dim1, 1:dim2, 1:3)`.
!
! @note
! The caller must allocate `ans` with at least `dim1` by `dim2` by 3
! entries before the call.
! @endnote
!
! @warning
! The current implementation does not fill the derivative terms
! correctly. It overwrites the second term and never sets the third.
! Do not rely on the values.
! @endwarning
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/OrthogonalBasisGradient_Tetrahedron_test_1.F90)
!
! See also: [[OrthogonalBasisGradient_Tetrahedron1]],
! [[OrthogonalBasis_Tetrahedron_]]

INTERFACE OrthogonalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE OrthogonalBasisGradient_Tetrahedron1_(order, xij, &
                                        refTetrahedron, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of the basis functions
    !! first dimension = point
    !! second dimension = basis function
    !! third dimension = coordinate
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! `dim1 = SIZE(xij, 2)`
    !! `dim2 = (order + 1) * (order + 2) * (order + 3) / 6`
    !! `dim3 = 3`
  END SUBROUTINE OrthogonalBasisGradient_Tetrahedron1_
END INTERFACE OrthogonalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!                                    HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the hierarchical basis, one order per entity.
!
! Returns the gradient of the hierarchical basis of a tetrahedron at a
! set of points.
! The derivatives are with respect to the Cartesian coordinates.
!
! Each edge and each triangular facet has its own polynomial order.
! `pe1` to `pe6` are the edge orders.
! `ps1` to `ps4` are the facet orders.
! `order` is the order of the cell modes.
! The cell has modes only when `order >= 4`.
!
! Each column of `xij` is one point.
! `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
! For `"BIUNIT"`, the routine scales the derivatives by one half.
!
! `ans(i, j, k)` is the derivative of basis function `j` with respect to
! coordinate `k` at point `i`.
!
! Returns:
!   ans : REAL(DFP) - gradient, with shape (npoints, nbasis, 3)
!
! Formula:
!   The number of basis functions is
!   $$ nbasis = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{i=1}^{4} \frac{(ps_i - 1)(ps_i - 2)}{2}
!   + \frac{(order - 1)(order - 2)(order - 3)}{6} $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/HeirarchicalBasisGradient_Tetrahedron_test_1.F90)
!
! See also: [[HeirarchicalBasisGradient_Tetrahedron1_]],
! [[HeirarchicalBasisGradient_Tetrahedron2]]

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron1(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! order of the cell modes, which exist only when `order >= 4`
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & 4 &
      & + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
      & + (ps1 - 1) * (ps1 - 2) / 2  &
      & + (ps2 - 1) * (ps2 - 2) / 2  &
      & + (ps3 - 1) * (ps3 - 2) / 2  &
      & + (ps4 - 1) * (ps4 - 2) / 2 &
      & + (order - 1) * (order - 2) * (order - 3) / 6_I4B, 3)
    !! gradient of the basis functions
    !! `ans(i, j, k)` is the derivative of function `j` with respect
    !! to coordinate `k` at point `i`
  END FUNCTION HeirarchicalBasisGradient_Tetrahedron1
END INTERFACE HeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the hierarchical basis, per-entity orders, into `ans`.
!
! Computes the gradient of the hierarchical basis of a tetrahedron at a
! set of points.
! It writes the result into the caller's array `ans`.
! The derivatives are with respect to the Cartesian coordinates.
!
! Each edge and each triangular facet has its own polynomial order.
! `pe1` to `pe6` are the edge orders.
! `ps1` to `ps4` are the facet orders.
! `order` is the order of the cell modes.
! The cell has modes only when `order >= 4`.
!
! Each column of `xij` is one point.
! `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
! Only its first letter counts.
! For `"BIUNIT"`, the routine scales the derivatives by one half.
!
! `ans(i, j, k)` is the derivative of basis function `j` with respect to
! coordinate `k` at point `i`.
! The routine fills `ans(1:dim1, 1:dim2, 1:3)`.
!
! @note
! The caller must allocate `ans` with at least `dim1` by `dim2` by 3
! entries before the call.
! @endnote
!
! Formula:
!   The number of basis functions is
!   $$ dim2 = 4 + \sum_{i=1}^{6} (pe_i - 1)
!   + \sum_{i=1}^{4} \frac{(ps_i - 1)(ps_i - 2)}{2}
!   + \frac{(order - 1)(order - 2)(order - 3)}{6} $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/HeirarchicalBasisGradient_Tetrahedron__test_1.F90)
!
! See also: [[HeirarchicalBasisGradient_Tetrahedron1]],
! [[HeirarchicalBasisGradient_Tetrahedron2_]]

INTERFACE HeirarchicalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Tetrahedron1_(order, pe1, pe2, &
                pe3, pe4, pe5, pe6, ps1, ps2, ps3, ps4, xij, refTetrahedron, &
                                                        ans, dim1, dim2, dim3)

    INTEGER(I4B), INTENT(IN) :: order
    !! order of the cell modes, which exist only when `order >= 4`
    INTEGER(I4B), INTENT(IN) :: pe1
    !! order of interpolation on edge parallel to x
    INTEGER(I4B), INTENT(IN) :: pe2
    !! order of interpolation on edge parallel to y
    INTEGER(I4B), INTENT(IN) :: pe3
    !! order of interpolation on edge parallel to z
    INTEGER(I4B), INTENT(IN) :: pe4
    !! order of interpolation on edge parallel to xy
    INTEGER(I4B), INTENT(IN) :: pe5
    !! order of interpolation on edge parallel to xz
    INTEGER(I4B), INTENT(IN) :: pe6
    !! order of interpolation on edge parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps1
    !! order of interpolation on facet parallel to xy
    INTEGER(I4B), INTENT(IN) :: ps2
    !! order of interpolation on facet parallel to xz
    INTEGER(I4B), INTENT(IN) :: ps3
    !! order of interpolation on facet parallel to yz
    INTEGER(I4B), INTENT(IN) :: ps4
    !! order of interpolation on facet parallel to xyz
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of the basis functions
    !! first dimension = point
    !! second dimension = basis function
    !! third dimension = coordinate
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = 4  + pe1 + pe2 + pe3 + pe4 + pe5 + pe6 - 6 &
    !! + (ps1 - 1) * (ps1 - 2) / 2  &
    !! + (ps2 - 1) * (ps2 - 2) / 2  &
    !! + (ps3 - 1) * (ps3 - 2) / 2  &
    !! + (ps4 - 1) * (ps4 - 2) / 2 &
    !! + (order - 1) * (order - 2) * (order - 3) / 6_I4B
    !! dim3 = 3
  END SUBROUTINE HeirarchicalBasisGradient_Tetrahedron1_
END INTERFACE HeirarchicalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!                                     HeirarchicalBasisGradient_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the hierarchical basis of one order, into `ans`.
!
! Would compute the gradient of the hierarchical basis of a tetrahedron.
! All edges, facets and the cell would share the same `order`.
!
! @warning
! This specific procedure has no implementation in the current source
! tree. A call fails at link time. Use the function
! `HeirarchicalBasisGradient_Tetrahedron` with `order`, or pass every edge
! and facet order to [[HeirarchicalBasisGradient_Tetrahedron1_]].
! @endwarning
!
! The planned result is `ans(i, j, k)`, the derivative of basis function
! `j` with respect to coordinate `k` at point `i`.
! The basis would have `(order + 1) * (order + 2) * (order + 3) / 6`
! functions.
!
! See also: [[HeirarchicalBasisGradient_Tetrahedron1_]],
! [[HeirarchicalBasisGradient_Tetrahedron2]]

INTERFACE HeirarchicalBasisGradient_Tetrahedron_
  MODULE SUBROUTINE HeirarchicalBasisGradient_Tetrahedron2_(order, xij, &
                                        refTetrahedron, ans, dim1, dim2, dim3)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
    !! gradient of the basis functions
    !! first dimension = point
    !! second dimension = basis function
    !! third dimension = coordinate
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = SIZE(xij, 2)
    !! dim2 = (order + 1) * (order + 2) * (order + 3) / 6_I4B
    !! dim3 = 3
  END SUBROUTINE HeirarchicalBasisGradient_Tetrahedron2_
END INTERFACE HeirarchicalBasisGradient_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Gradient of the hierarchical basis of a tetrahedron, one order.
!
! Returns the gradient of the hierarchical basis of a tetrahedron at a
! set of points.
! All edges, all facets and the cell use the same polynomial `order`.
! The derivatives are with respect to the Cartesian coordinates.
!
! The basis has `(order + 1) * (order + 2) * (order + 3) / 6` functions.
! `ans(i, j, k)` is the derivative of basis function `j` with respect to
! coordinate `k` at point `i`.
!
! Each column of `xij` is one point.
! `refTetrahedron` is `"UNIT"` or `"BIUNIT"`.
! For `"BIUNIT"`, the routine scales the derivatives by one half.
!
! Returns:
!   ans : REAL(DFP) - gradient, with shape (npoints, nbasis, 3)
!
! Examples
!
! - [test 2](|base|Tetrahedron/examples/HeirarchicalBasisGradient_Tetrahedron_test_2.F90)
!   for orders 2 to 4
!
! See also: [[HeirarchicalBasisGradient_Tetrahedron1]]

INTERFACE HeirarchicalBasisGradient_Tetrahedron
  MODULE FUNCTION HeirarchicalBasisGradient_Tetrahedron2(order, xij, &
    refTetrahedron) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the basis
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! points in the reference tetrahedron, one point per column
    !! `SIZE(xij, 1) = 3` and `SIZE(xij, 2)` is the number of points
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans( &
      & SIZE(xij, 2), &
      & (order + 1) * (order + 2) * (order + 3) / 6_I4B, 3)
    !! gradient of the basis functions
  END FUNCTION HeirarchicalBasisGradient_Tetrahedron2
END INTERFACE HeirarchicalBasisGradient_Tetrahedron

!----------------------------------------------------------------------------
!                                                                 Tetrahedron
!----------------------------------------------------------------------------

END MODULE TetrahedronInterpolationUtility
