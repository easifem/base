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
! summary: Methods for the reference quadrangle element.
!
! This module has routines for the reference quadrangle.
! This is a four-sided element of order 1 to 10.
! You can use it to:
!
! 1. build a reference quadrangle, see [[ReferenceQuadrangle]];
! 2. get the name, type, order and entity counts of an element;
! 3. get the edge and face connectivity;
! 4. build the edge elements and topology;
! 5. compute the area of a quadrangle.
!
! @note
! `MaxOrder_Quadrangle` is 2 by default.
! Define `MAX_QUADRANGLE_ORDER` at compile time to change it.
! @endnote
!
! `DEFAULT_OPT_QUADRANGLE_EDGE_CON` sets the default edge numbering.
! It is 2 by default. The edges are then [1,2], [2,3], [3,4], [4,1].
! Define `QUADRANGLE_EDGE_CON_DEFAULT_OPT_1` at compile time to get
! [1,2], [4,3], [1,4], [2,3] instead.

MODULE ReferenceQuadrangle_Method
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE BaseType, ONLY: ReferenceQuadrangle_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTopology_

IMPLICIT NONE

PRIVATE

PUBLIC :: Initiate
PUBLIC :: ReferenceQuadrangle
PUBLIC :: ReferenceQuadrangle_Pointer
PUBLIC :: HighorderElement_Quadrangle
PUBLIC :: Measure_Simplex_Quadrangle
PUBLIC :: Quadrangle_Quality
PUBLIC :: Quality_Quadrangle
PUBLIC :: QuadArea3D, QuadrangleArea3D
PUBLIC :: QuadArea2D, QuadrangleArea2D
PUBLIC :: QuadrangleName
PUBLIC :: GetEdgeConnectivity_Quadrangle
PUBLIC :: GetFaceConnectivity_Quadrangle
PUBLIC :: RefQuadrangleCoord
PUBLIC :: RefCoord_Quadrangle
PUBLIC :: FaceShapeMetaData_Quadrangle
PUBLIC :: FacetElements_Quadrangle
PUBLIC :: DEFAULT_OPT_QUADRANGLE_EDGE_CON
PUBLIC :: ElementOrder_Quadrangle
PUBLIC :: ElementType_Quadrangle
PUBLIC :: TotalNodesInElement_Quadrangle
PUBLIC :: TotalEntities_Quadrangle
PUBLIC :: FacetTopology_Quadrangle
PUBLIC :: ElementName_Quadrangle
PUBLIC :: MaxOrder_Quadrangle
PUBLIC :: GetFaceElemType_Quadrangle

#ifdef MAX_QUADRANGLE_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Quadrangle = MAX_QUADRANGLE_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Quadrangle = 2_I4B
#endif

INTEGER(I4B), PUBLIC, PARAMETER :: HelpFaceData_Quadrangle(3, 4) = &
                                   RESHAPE([ &
                                           2, 3, 4, &
                                           3, 4, 1, &
                                           4, 1, 2, &
                                           1, 2, 3 &
                                           ], [3, 4])

#ifdef QUADRANGLE_EDGE_CON_DEFAULT_OPT_1
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_QUADRANGLE_EDGE_CON = 1_I4B
!! This means edges are [1,2], [4,3], [1,4], [2, 3]
#else
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_QUADRANGLE_EDGE_CON = 2_I4B
!! This means edges are [1,2], [2,3], [3,4], [4,1]
!! This is default option
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of a quadrangle element type.
!
! The name is the string `Quadrangle` followed by the number of nodes.
! For example, it returns `Quadrangle9` for the 9-node quadrangle.
! If `elemType` is not a quadrangle type, it returns an empty string.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - element name
!
! See also: [[ElementType_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION ElementName_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle
    CHARACTER(:), ALLOCATABLE :: ans
    !! element name
  END FUNCTION ElementName_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the topology of the four edges of a quadrangle.
!
! Sets the topology of each edge in `ans(1:4)`.
! For each edge, it picks the edge nodes out of `nptrs`.
! It then sets the dimension to 1 and the name to the line element
! of the same order.
! The edges follow the default edge connectivity of this module.
! See [[GetEdgeConnectivity_Quadrangle]].
!
! @note
! The caller must allocate `ans` with at least four entries.
! @endnote
!
! See also: [[FacetElements_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Quadrangle(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    !! node numbers of the element, in the local node order
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
    !! the topology of the four edges, `SIZE(ans) >= 4`
  END SUBROUTINE FacetTopology_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes, edges, faces and cells.
!
! The result has four entries:
!
! 1. the number of nodes of `elemType`, including higher order nodes;
! 2. the number of edges, always 4;
! 3. the number of faces, always 1;
! 4. the number of cells, always 0.
!
! If `elemType` is not a quadrangle type, the first entry is 0.
! The other entries do not change.
!
! Returns:
!   ans : INTEGER(I4B) - entity counts, `SIZE(ans) = 4`
!
! See also: [[TotalNodesInElement_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle
    INTEGER(I4B) :: ans(4)
    !! counts of nodes, edges, faces and cells
  END FUNCTION TotalEntities_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             TotalNodesInElement_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes of a quadrangle element type.
!
! The count includes the vertices and all higher order nodes.
! For example, `Quadrangle4` has 4 nodes and `Quadrangle9` has 9 nodes.
! If `elemType` is not a quadrangle type, it returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes, or 0 for an unknown type
!
! See also: [[TotalEntities_Quadrangle]], [[ElementOrder_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION TotalNodesInElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ElementType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element type for a quadrangle element name.
!
! The name must match exactly, including case.
! Valid names are `Quadrangle4`, `Quadrangle8`, `Quadrangle9`,
! `Quadrangle16`, `Quadrangle25`, `Quadrangle36`, `Quadrangle49`,
! `Quadrangle64`, `Quadrangle81`, `Quadrangle100` and `Quadrangle121`.
! The name `Quadrangle` is an alias of `Quadrangle4`.
! For any other name, it returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type, or 0 for an unknown name
!
! See also: [[ElementName_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION ElementType_Quadrangle(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    !! element name, for example `"Quadrangle9"`
    INTEGER(I4B) :: ans
    !! element type
  END FUNCTION ElementType_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ElementOrder_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the polynomial order of a quadrangle element type.
!
! Both `Quadrangle8` and `Quadrangle9` have order 2.
! The types from `Quadrangle4` to `Quadrangle121` have orders 1 to 10.
! If `elemType` is not a quadrangle type, it returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - polynomial order, or 0 for an unknown type
!
! See also: [[ElementType_Quadrangle]], [[QuadrangleName1]]

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle
    INTEGER(I4B) :: ans
    !! polynomial order
  END FUNCTION ElementOrder_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the four edge elements of a reference quadrangle.
!
! Fills `ans(1:4)` with the reference elements of the four edges.
! Each edge is a line element.
! The order, the interpolation point type and `nsd` of the edges come
! from `refelem`.
! The node numbers of each edge come from the edge topology of `refelem`.
!
! @note
! The caller must allocate `ans` with at least four entries.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/FacetElements_Quadrangle_test_1.F90)
!
! See also: [[FacetElements_Quadrangle2]], [[FacetTopology_Quadrangle]]

INTERFACE FacetElements_Quadrangle
  MODULE SUBROUTINE FacetElements_Quadrangle1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference quadrangle with its topology already built
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the four edge elements, `SIZE(ans) >= 4`
  END SUBROUTINE FacetElements_Quadrangle1
END INTERFACE FacetElements_Quadrangle

!----------------------------------------------------------------------------
!                                                   FacetElements_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the four edge elements of a quadrangle from its type.
!
! Fills `ans(1:4)` with the reference elements of the four edges.
! Each edge is a line element with the same order as the quadrangle.
! The edge nodes use equidistant interpolation points.
! Each edge stores `nsd` as its spatial dimension.
! The edges follow the default edge connectivity of this module.
! See [[GetEdgeConnectivity_Quadrangle]].
!
! @note
! The caller must allocate `ans` with at least four entries.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/FacetElements_Quadrangle_test_1.F90)
! - [test 2](|base|Quadrangle/examples/FacetElements_Quadrangle_test_2.F90)
!   compares this routine with [[FacetElements_Quadrangle1]]
!
! See also: [[FacetElements_Quadrangle1]], [[FacetTopology_Quadrangle]]

INTERFACE FacetElements_Quadrangle
  MODULE SUBROUTINE FacetElements_Quadrangle2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle, for example `Quadrangle9`
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension stored in each edge element
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the four edge elements, `SIZE(ans) >= 4`
  END SUBROUTINE FacetElements_Quadrangle2
END INTERFACE FacetElements_Quadrangle

!----------------------------------------------------------------------------
!                                                       QuadrangleName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the quadrangle element type for a given order.
!
! Returns the element type of the Lagrange quadrangle of order `order`.
! The element has `(order + 1)**2` nodes.
! Order 2 therefore gives `Quadrangle9`, not `Quadrangle8`.
! It handles the orders 1 to 10.
! For any other order, it returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type, or 0 if `order` is not supported
!
! See also: [[ElementType_Quadrangle]], [[ElementOrder_Quadrangle]]

INTERFACE QuadrangleName
  MODULE PURE FUNCTION QuadrangleName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the quadrangle, from 1 to 10
    INTEGER(I4B) :: ans
    !! element type
  END FUNCTION QuadrangleName1
END INTERFACE QuadrangleName

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Initiates a linear reference quadrangle.
!
! Clears `obj` and sets it up as a quadrangle of order 1.
! It has four vertices, four edges and one face.
! The array `obj%xij` holds the vertex coordinates, in two rows.
! `NSD` does not change the number of rows.
!
! The vertex coordinates and the domain name come from one of three sources.
!
! 1. If `xij` is present, the routine copies `xij(1:2, 1:4)`.
!    The name is "UNIT" when the vertices match the unit quadrangle.
!    It is "BIUNIT" when they match the biunit quadrangle.
!    Otherwise it is "GENERAL".
!    The routine skips `domainName`.
! 2. Otherwise, if `domainName` is present, the routine converts it to
!    upper case and uses it as the name.
!    For "UNIT" and "BIUNIT" it also sets the vertex coordinates.
! 3. If neither is present, the domain is "BIUNIT".
!
! @warning
! If `domainName` is neither "UNIT" nor "BIUNIT", and `xij` is absent,
! `obj%xij` is not set.
! @endwarning
!
! See also: [[ReferenceQuadrangle]], [[ReferenceQuadrangle_Pointer]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Quadrangle(obj, NSD, xij, domainName)
    CLASS(ReferenceQuadrangle_), INTENT(INOUT) :: obj
    !! reference quadrangle to initiate
    INTEGER(I4B), INTENT(IN) :: NSD
    !! number of spatial dimensions of the space the element lives in
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column
    !! only rows 1:2 and columns 1:4 matter
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain name, ignored when `xij` is present
    !! "UNIT" or "BIUNIT", default "BIUNIT"
  END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a new linear reference quadrangle.
!
! Returns a `ReferenceQuadrangle_` of order 1 with four vertices.
! It builds the element like [[Initiate]] does for a reference quadrangle.
! See that routine for the rules on `xij` and `domainName`.
!
! Returns:
!   obj : the new element of type `ReferenceQuadrangle_`
!
! See also: [[ReferenceQuadrangle_Pointer]]

INTERFACE ReferenceQuadrangle
  MODULE PURE FUNCTION reference_Quadrangle(NSD, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    !! number of spatial dimensions of the space the element lives in
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column
    !! only rows 1:2 and columns 1:4 matter
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain name, ignored when `xij` is present
    !! "UNIT" or "BIUNIT", default "BIUNIT"
    TYPE(ReferenceQuadrangle_) :: obj
    !! new reference quadrangle
  END FUNCTION reference_Quadrangle
END INTERFACE ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                     ReferenceQuadrangle_Pointer@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a pointer to a new reference quadrangle.
!
! Allocates a new `ReferenceQuadrangle_` and returns a pointer to it.
! It builds the element like [[Initiate]] does for a reference quadrangle.
! See that routine for the rules on `xij` and `domainName`.
!
! @warning
! The caller owns the target.
! Deallocate the pointer when it is no longer needed.
! @endwarning
!
! Returns:
!   obj : pointer to the new element of type `ReferenceQuadrangle_`
!
! See also: [[ReferenceQuadrangle]]

INTERFACE ReferenceQuadrangle_Pointer
  MODULE FUNCTION reference_Quadrangle_Pointer(NSD, xij, domainName) &
    RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    !! number of spatial dimensions of the space the element lives in
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column
    !! only rows 1:2 and columns 1:4 matter
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain name, ignored when `xij` is present
    !! "UNIT" or "BIUNIT", default "BIUNIT"
    CLASS(ReferenceQuadrangle_), POINTER :: obj
    !! new reference quadrangle
  END FUNCTION reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds a high order quadrangle from a linear quadrangle.
!
! Builds the Lagrange quadrangle of order `order` in `obj`.
! The element has `(order + 1)**2` nodes.
! Order 2 therefore gives 9 nodes.
!
! 1. If `order` is 1, `obj` becomes a copy of `refelem`.
!    The argument `ipType` is not used.
! 2. Otherwise the routine places the nodes inside the vertex
!    coordinates `refelem%xij`, using the node family `ipType`.
!    It then sets the element name, the order and the entity counts.
!    It also builds the topology of the vertices, edges and the face.
!
! The node numbers run over the vertices first, then the edge interiors,
! then the face interior.
!
! @note
! The routine clears `obj` first, so `obj` loses its old content.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/HighOrderElement_Quadrangle_test_1.F90) with order 2
! - [test 2](|base|Quadrangle/examples/HighOrderElement_Quadrangle_test_2.F90) with order 3
!
! See also: [[QuadrangleName]], [[GetEdgeConnectivity_Quadrangle]]

INTERFACE
  MODULE SUBROUTINE HighorderElement_Quadrangle(refelem, order, obj, &
                                                ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! linear reference quadrangle, it gives the vertex coordinates
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the new element, 1 or more
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! high order element, cleared on entry
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type, for example `Equidistance`
  END SUBROUTINE HighorderElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the area of a quadrangle from its vertex coordinates.
!
! Computes the area of the quadrangle with the vertices in `xij`.
! The result depends on `refelem%nsd`.
!
! 1. If `refelem%nsd` is 2, it returns the signed area, see
!    [[QuadrangleArea2D]]. It is negative for clockwise vertices.
! 2. Otherwise it returns the area in three dimensions, see
!    [[QuadrangleArea3D]]. It is never negative.
!
! Only the first four columns of `xij` matter.
!
! Returns:
!   Ans : REAL(DFP) - area of the quadrangle
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/Measure_Simplex_Quadrangle_test_1.F90)
!
! See also: [[Quality_Quadrangle]]

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Quadrangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element, only `refelem%nsd` matters
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one vertex per column, in cyclic order
    !! it needs at least 4 columns and `refelem%nsd` rows
    REAL(DFP) :: Ans
    !! area of the quadrangle
  END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a quality measure of a quadrangle (placeholder).
!
! This routine is not implemented yet.
! It always returns 0, whatever the arguments are.
!
! @note
! Do not use the result to judge the shape of an element.
! @endnote
!
! Returns:
!   Ans : REAL(DFP) - always 0
!
! See also: [[Measure_Simplex_Quadrangle]]

INTERFACE Quality_Quadrangle
  MODULE FUNCTION Quadrangle_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference element, not used
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one vertex per column, not used
    INTEGER(I4B), INTENT(IN) :: measure
    !! quality measure selector, not used
    REAL(DFP) :: Ans
    !! always 0
  END FUNCTION Quadrangle_Quality
END INTERFACE Quality_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a quadrangle in three dimensions.
!
! Returns the area of the quadrangle with vertices `q(:, 1)` to
! `q(:, 4)`.
! The vertices must be in cyclic order.
!
! 1. Join the midpoints of the four sides. This gives a parallelogram.
! 2. Compute the area of the parallelogram from a cross product.
! 3. Double it to get the area of the quadrangle.
!
! The result is not negative, so it does not show the vertex orientation.
!
! @note
! The result is exact for a planar quadrangle.
! For a warped quadrangle it is an approximation.
! @endnote
!
! See also: [[QuadrangleArea2D]]

INTERFACE QuadrangleArea3D
  MODULE PURE SUBROUTINE QuadArea3D(q, ans)
    REAL(DFP), INTENT(IN) :: q(3, 4)
    !! vertex coordinates, one vertex per column, in cyclic order
    REAL(DFP), INTENT(OUT) :: ans
    !! area of the quadrangle, never negative
  END SUBROUTINE QuadArea3D
END INTERFACE QuadrangleArea3D

!----------------------------------------------------------------------------
!                                                          QuadrangleArea2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Computes the area of a quadrangle in two dimensions.
!
! Returns the signed area of the quadrangle with vertices `q(:, 1)` to
! `q(:, 4)`.
! The vertices must be in cyclic order.
! The area is positive when they run counterclockwise.
! It is negative when they run clockwise.
!
! The routine splits the quadrangle into the triangles (1,2,3) and (3,4,1)
! and adds their signed areas.
!
! @note
! The result is only correct for a simple quadrangle.
! A self-intersecting quadrangle gives a meaningless value.
! @endnote
!
! Formula:
! $$A=\tfrac12\sum_{i=1}^4(x_i y_{i+1}-x_{i+1}y_i)$$
!
! See also: [[QuadrangleArea3D]]

INTERFACE QuadrangleArea2D
  MODULE PURE SUBROUTINE QuadArea2D(q, ans)
    REAL(DFP), INTENT(IN) :: q(2, 4)
    !! vertex coordinates, one vertex per column, in cyclic order
    REAL(DFP), INTENT(OUT) :: ans
    !! signed area of the quadrangle
  END SUBROUTINE QuadArea2D
END INTERFACE QuadrangleArea2D

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the node numbers of the four edges of a quadrangle.
!
! Fills `con` with the node numbers of each edge.
! Column `j` of `con` holds the nodes of edge `j`.
!
! Each column lists the two end vertices first.
! The interior nodes of the edge follow, in increasing order.
! The numbering starts at 5 and runs edge by edge, `order - 1` nodes per
! edge.
!
! The argument `opt` selects the edge numbering.
!
! 1. `opt = 1` is the default. It is for hierarchical bases.
!    The vertex pairs are [1,2], [4,3], [1,4] and [2,3].
! 2. Any other value is for Lagrange bases.
!    The vertex pairs are [1,2], [2,3], [3,4] and [4,1].
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/GetEdgeConnectivity_Quadrangle_test_1.F90)
!
! See also: [[GetFaceConnectivity_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Quadrangle(con, opt, order, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! edge connectivity, one edge per column
    !! the caller must allocate `con`
    !! it needs at least `order + 1` rows and 4 columns
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! edge numbering, default 1
    !! 1 is for hierarchical bases
    !! any other value is for Lagrange bases
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! polynomial order of the element, default 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`, equal to `order + 1`
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 4
  END SUBROUTINE GetEdgeConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the node numbers of the four faces of a quadrangle.
!
! Fills `con` with the node numbers of each face.
! In two dimensions the faces of a quadrangle are its four edges.
! Column `j` of `con` holds the nodes of face `j`.
!
! Each column lists the two end vertices first.
! The interior nodes of the edge follow, in increasing order.
!
! This routine always uses the Lagrange edge numbering of
! [[GetEdgeConnectivity_Quadrangle]] with `opt = 2`.
! The vertex pairs are [1,2], [2,3], [3,4] and [4,1].
!
! @note
! The argument `opt` is not used.
! @endnote
!
! Examples
!
! - [test 1](|base|Quadrangle/examples/GetEdgeConnectivity_Quadrangle_test_1.F90)
!
! See also: [[GetEdgeConnectivity_Quadrangle]]

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Quadrangle(con, opt, order, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! face connectivity, one face per column
    !! the caller must allocate `con`
    !! it needs at least `order + 1` rows and 4 columns
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! polynomial order of the element, default 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`, equal to `order + 1`
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 4
  END SUBROUTINE GetFaceConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefQuadrangleCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the vertex coordinates of the reference quadrangle.
!
! This function returns the coordinates of the four vertices of the
! reference quadrangle.
! The first letter of `refQuadrangle` selects the domain.
!
! 1. `"U"` or `"u"` gives the unit square, with corners 0 and 1.
! 2. Any other value gives the bi-unit square, with corners -1 and 1.
!
! Vertex 1 is the lower left corner.
! The vertices follow in counterclockwise order.
!
! Returns:
!   ans : REAL(DFP) - coordinates, size `(2, 4)`.
!   Column `j` holds the coordinates of vertex `j`.
!
! Formula:
!   $$ Q = [0, 1]^2 \quad \text{or} \quad Q = [-1, 1]^2 $$

INTERFACE RefCoord_Quadrangle
  MODULE PURE FUNCTION RefQuadrangleCoord(refQuadrangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    !! reference domain, `"UNIT"` or `"BIUNIT"`
    REAL(DFP) :: ans(2, 4)
    !! vertex coordinates, one column per vertex
  END FUNCTION RefQuadrangleCoord
END INTERFACE RefCoord_Quadrangle

!----------------------------------------------------------------------------
!                                           FaceShapeMetaData_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Sorts the vertices of a quadrangle face and finds its orientation.
!
! This routine takes the four global vertex numbers of a quadrangle face.
! It returns them in a canonical order and, optionally, the orientation
! of the face.
!
! 1. Find the smallest vertex number in `face`.
! 2. Start from that vertex and walk around the face. This gives
!    `localFaces`.
! 3. Keep the first and third vertex in place in `sorted_face`.
! 4. Sort the second and fourth vertex in ascending order.
!
! The orientation `faceOrient` has three entries, each `1` or `-1`.
!
! 1. `faceOrient(1)` is the sign of `localFaces(2) - localFaces(1)`.
! 2. `faceOrient(2)` is the sign of `localFaces(4) - localFaces(1)`.
! 3. `faceOrient(3)` is `1` if `localFaces(2) < localFaces(4)`.
!    Otherwise it is `-1`.
!
! @note
! This orientation code is not the one used for triangle faces.
! @endnote
!
! @note
! `face` is not changed.
! @endnote

INTERFACE
  MODULE SUBROUTINE FaceShapeMetaData_Quadrangle(face, sorted_face, &
                                                 faceOrient, localFaces)
    INTEGER(I4B), INTENT(INOUT) :: face(:)
    !! global vertex numbers of the face, size 4
    INTEGER(I4B), INTENT(INOUT) :: sorted_face(:)
    !! vertex numbers in canonical order, size 4
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceOrient(:)
    !! orientation code, size 3, each entry is `1` or `-1`
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaces(:)
    !! vertex numbers of the face, rotated to start at the smallest one,
    !! size 4
  END SUBROUTINE FaceShapeMetaData_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetFaceElemType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the line element types of the four faces of a quadrangle.
!
! This routine fills the element type and the number of nodes of the four
! faces of a quadrangle.
! The faces of a quadrangle are lines, and all four have the same type.
! The type depends on the order of the quadrangle `elemType`.
! If `elemType` is absent, the routine uses a linear quadrangle.
!
! The caller allocates `faceElemType` and `tFaceNodes`.
! Each needs at least four entries.
!
! @note
! `opt` is not used.
! @endnote
!
! See also: [[GetFaceElemType_Quadrangle2]]

INTERFACE GetFaceElemType_Quadrangle
  MODULE PURE SUBROUTINE GetFaceElemType_Quadrangle1(elemType, faceElemType, &
                                                     opt, tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type of the quadrangle; default is the linear quadrangle
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! line element type of each face, size at least 4
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! number of nodes in each face, size at least 4
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Quadrangle1
END INTERFACE GetFaceElemType_Quadrangle

!----------------------------------------------------------------------------
!                                                GetFaceElemType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the line element type of one face of a quadrangle.
!
! This routine returns the element type and the number of nodes of a
! face of a quadrangle.
! The faces of a quadrangle are lines, and all four have the same type.
! The type depends on the order of the quadrangle `elemType`.
!
! @note
! `localFaceNumber` and `opt` are not used.
! @endnote
!
! See also: [[GetFaceElemType_Quadrangle1]]

INTERFACE GetFaceElemType_Quadrangle
  MODULE PURE SUBROUTINE GetFaceElemType_Quadrangle2( &
    elemType, localFaceNumber, faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the quadrangle, for example `Quadrangle4`
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number; not used
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! line element type of the face
    INTEGER(I4B), INTENT(INOUT) :: tFaceNodes
    !! number of nodes in the face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Quadrangle2
END INTERFACE GetFaceElemType_Quadrangle

END MODULE ReferenceQuadrangle_Method
