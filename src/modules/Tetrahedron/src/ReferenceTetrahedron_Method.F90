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
! summary: Reference tetrahedron: construction, topology and geometry.
!
! This module builds the reference tetrahedron `ReferenceTetrahedron_`.
! The tetrahedron has `nsd = 3`. Its vertex coordinates are either
! `"UNIT"` or `"BIUNIT"`.
!
! The module also answers questions about the element. It gives the
! number of nodes, edges, faces and cells of an element type. It gives
! the node numbers of edges and faces. It gives the volume of a
! tetrahedron.
!
! The constant `MaxOrder_Tetrahedron` is the highest order the module
! supports. Its default is 2.
!
! @note
! `Tetrahedron_Quality` and `Quality_Tetrahedron` are placeholders.
! They always return 0.
! @endnote
!
! Formula:
!   $$ V = |\det[v_2-v_1, v_3-v_1, v_4-v_1]| / 6 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ReferenceTetrahedron_test_1.F90)
! - [constructors](|base|Tetrahedron/examples/ReferenceTetrahedronConstructors_test_1.F90)
! - [connectivity](|base|Tetrahedron/examples/ReferenceTetrahedronConnectivity_test_1.F90)
! - [measure](|base|Tetrahedron/examples/ReferenceTetrahedronMeasure_test_1.F90)
! - [metadata](|base|Tetrahedron/examples/ReferenceTetrahedronMetadata_test_1.F90)
!
! See also: [[TetrahedronInterpolationUtility]]

MODULE ReferenceTetrahedron_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceTetrahedron
PUBLIC :: ReferenceTetrahedron_Pointer
PUBLIC :: HighOrderElement_Tetrahedron
PUBLIC :: Measure_Simplex_Tetrahedron
PUBLIC :: Tetrahedron_Quality
PUBLIC :: TetrahedronVolume3D
PUBLIC :: Quality_Tetrahedron
PUBLIC :: GetEdgeConnectivity_Tetrahedron
PUBLIC :: GetFaceConnectivity_Tetrahedron
PUBLIC :: RefCoord_Tetrahedron
PUBLIC :: GetFaceElemType_Tetrahedron
PUBLIC :: FacetElements_Tetrahedron
PUBLIC :: ElementOrder_Tetrahedron
PUBLIC :: ElementType_Tetrahedron
PUBLIC :: TotalNodesInElement_Tetrahedron
PUBLIC :: TotalEntities_Tetrahedron
PUBLIC :: FacetTopology_Tetrahedron
PUBLIC :: ElementName_Tetrahedron
PUBLIC :: MaxOrder_Tetrahedron

#ifdef MAX_TETRAHEDRON_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Tetrahedron = MAX_TETRAHEDRON_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Tetrahedron = 2_I4B
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the name of a tetrahedron element type.
!
! Converts an element type such as `Tetrahedron10` to its name, here
! `"Tetrahedron10"`. The valid names are `"Tetrahedron4"`,
! `"Tetrahedron10"`, `"Tetrahedron20"`, `"Tetrahedron35"` and
! `"Tetrahedron56"`. Any other `elemType` gives `"NONE"`.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - element name
!
! See also: [[ElementType_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION ElementName_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, for example `Tetrahedron4`
    CHARACTER(:), ALLOCATABLE :: ans
    !! element name
  END FUNCTION ElementName_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the topology of the four triangular facets of a tetrahedron.
!
! Fills `ans(1:4)` with one entry per face. For each face, it sets:
!
! 1. `nptrs`, the node numbers of the face, taken from `nptrs`;
! 2. `xiDimension`, which is 2;
! 3. `name`, the triangle element type of the face.
!
! The face type and node order follow the order of the tetrahedron.
!
! @note
! The caller must size `ans` to at least 4 before the call.
! @endnote
!
! @note
! `nptrs` must hold one node number for each node of the element.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/FacetTopology_Tetrahedron_test_1.F90)
!   for a quadratic tetrahedron
! - [test 2](|base|Tetrahedron/examples/FacetTopology_Tetrahedron_test_2.F90)
!   for a cubic tetrahedron
!
! See also: [[FacetElements_Tetrahedron1]], [[FacetElements_Tetrahedron2]]

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Tetrahedron(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! tetrahedron element type, for example `Tetrahedron10`
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    !! node numbers of the element, one for each node
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
    !! topology of the four faces, `SIZE(ans) >= 4`
  END SUBROUTINE FacetTopology_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 TotalEntities_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the number of nodes, edges, faces and cells of a tetrahedron.
!
! Returns four counts in this order: nodes, edges, faces and cells.
! The edge, face and cell counts are always 6, 4 and 1.
! The node count includes the higher order nodes.
!
! For an unknown `elemType`, the node count is 0.
!
! Returns:
!   ans : INTEGER(I4B)(4) - counts of nodes, edges, faces and cells
!
! See also: [[TotalNodesInElement_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, for example `Tetrahedron4`
    INTEGER(I4B) :: ans(4)
    !! counts of nodes, edges, faces and cells
  END FUNCTION TotalEntities_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the number of nodes of a tetrahedron element type.
!
! Returns 4 for `Tetrahedron4`, 10 for `Tetrahedron10`, 20 for
! `Tetrahedron20`, 35 for `Tetrahedron35` and 56 for `Tetrahedron56`.
! Any other value of `elemType` gives 0.
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes, or 0 for an unknown type
!
! See also: [[TotalEntities_Tetrahedron]], [[ElementOrder_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, for example `Tetrahedron4`
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION TotalNodesInElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the element type of a tetrahedron from its name.
!
! Converts a name such as `"Tetrahedron10"` to the matching element type.
! The names are `"Tetrahedron4"`, `"Tetrahedron10"`, `"Tetrahedron20"`,
! `"Tetrahedron35"` and `"Tetrahedron56"`. The name `"Tetrahedron"` is an
! alias of `"Tetrahedron4"`.
!
! The name is case sensitive. An unknown name gives 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type, or 0 for an unknown name
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ElementType_Tetrahedron_test_1.F90)
!
! See also: [[ElementName_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION ElementType_Tetrahedron(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    !! element name, case sensitive
    INTEGER(I4B) :: ans
    !! element type
  END FUNCTION ElementType_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the polynomial order of a tetrahedron element type.
!
! Returns 1 for `Tetrahedron4`, 2 for `Tetrahedron10`, 3 for
! `Tetrahedron20`, 4 for `Tetrahedron35` and 5 for `Tetrahedron56`.
! Any other value of `elemType` gives 0.
!
! Returns:
!   ans : INTEGER(I4B) - polynomial order, or 0 for an unknown type
!
! See also: [[ElementType_Tetrahedron]], [[TotalNodesInElement_Tetrahedron]]

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, for example `Tetrahedron4`
    INTEGER(I4B) :: ans
    !! polynomial order
  END FUNCTION ElementOrder_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the four triangular facets from a reference tetrahedron.
!
! Fills `ans(1:4)` with the reference triangles that bound `refelem`.
! Each facet copies `nsd` and the interpolation point type of `refelem`.
! Its element type, order and node numbers come from the topology of
! `refelem`.
!
! @note
! The caller must size `ans` to at least 4 before the call.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/FacetElements_Tetrahedron_test_1.F90)
!
! See also: [[FacetElements_Tetrahedron2]], [[FacetTopology_Tetrahedron]]

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! the reference tetrahedron
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the four facets, `SIZE(ans) >= 4`
  END SUBROUTINE FacetElements_Tetrahedron1
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                  FacetElements_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the four triangular facets from an element type.
!
! Fills `ans(1:4)` with the reference triangles that bound the
! tetrahedron. Each facet has the same order as the tetrahedron.
! Its points use equidistant interpolation.
!
! Each facet stores its points, edges and face. The node numbers come
! from the face connectivity of the tetrahedron.
!
! @note
! The caller must size `ans` to at least 4 before the call.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/FacetElements_Tetrahedron_test_1.F90)
!   also calls this form
! - [test 2](|base|Tetrahedron/examples/FacetElements_Tetrahedron_test_2.F90)
!   for a cubic tetrahedron
!
! See also: [[FacetElements_Tetrahedron1]], [[FacetTopology_Tetrahedron]]

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! tetrahedron element type, for example `Tetrahedron10`
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension stored in each facet
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the four facets, `SIZE(ans) >= 4`
  END SUBROUTINE FacetElements_Tetrahedron2
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                       Initiate@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Initiate a reference tetrahedron with four vertices.
!
! Resets `obj` and builds a linear reference tetrahedron. The result has
! order 1, element type `Tetrahedron4` and `xidimension` equal to `nsd`.
! It stores the topology of 4 points, 6 edges, 4 faces and 1 cell.
!
! If `xij` is present, it gives the vertices. The `domainName` of `obj` is
! then `"UNIT"`, `"BIUNIT"` or `"GENERAL"`, whichever matches `xij`.
!
! If `xij` is absent, `domainName` selects the vertices. The default is
! `"UNIT"`. The name is not case sensitive.
!
! @note
! `nsd` must be 3. The vertex array has three rows.
! @endnote
!
! @note
! If `domainName` is neither `"UNIT"` nor `"BIUNIT"`, and `xij` is
! absent, the vertex coordinates `obj%xij` stay unset.
! @endnote
!
! See also: [[reference_Tetrahedron]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Tetrahedron(obj, nsd, xij, domainName)
    CLASS(ReferenceTetrahedron_), INTENT(INOUT) :: obj
    !! the reference tetrahedron to initiate
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension, must be 3
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column; the routine reads only
    !! `xij(1:3, 1:4)`. It takes precedence over `domainName`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain, `"UNIT"` or `"BIUNIT"`; default `"UNIT"`
  END SUBROUTINE Initiate_Ref_Tetrahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return a reference tetrahedron as a function result.
!
! Returns a `ReferenceTetrahedron_` object with order 1 and four vertices.
! It behaves like the subroutine [[Initiate_Ref_Tetrahedron]].
!
! If `xij` is present, it gives the vertices. The result `domainName` is
! then `"UNIT"`, `"BIUNIT"` or `"GENERAL"`, whichever matches `xij`.
!
! If `xij` is absent, `domainName` selects the vertices. The default is
! `"UNIT"`. The name is not case sensitive.
!
! @note
! `nsd` must be 3. The vertex array has three rows.
! @endnote
!
! @note
! If `domainName` is neither `"UNIT"` nor `"BIUNIT"`, and `xij` is
! absent, the vertex coordinates `obj%xij` stay unset.
! @endnote
!
! Returns:
!   obj : `TYPE(ReferenceTetrahedron_)` - the reference tetrahedron
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ReferenceTetrahedron_test_1.F90)
!
! See also: [[Initiate_Ref_Tetrahedron]]

INTERFACE ReferenceTetrahedron
  MODULE PURE FUNCTION reference_Tetrahedron(nsd, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension, must be 3
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column; the function reads only
    !! `xij(1:3, 1:4)`. It takes precedence over `domainName`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain, `"UNIT"` or `"BIUNIT"`; default `"UNIT"`
    TYPE(ReferenceTetrahedron_) :: obj
    !! the reference tetrahedron
  END FUNCTION reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return a pointer to a new reference tetrahedron.
!
! Allocates a new `ReferenceTetrahedron_` object with order 1 and four
! vertices. It returns a pointer to the object. The vertices follow the
! same rules as [[Initiate_Ref_Tetrahedron]].
!
! @note
! The caller owns the target and releases it with `DEALLOCATE`.
! @endnote
!
! Returns:
!   obj : `CLASS(ReferenceTetrahedron_), POINTER` - the new object
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ReferenceTetrahedronConstructors_test_1.F90)
!
! See also: [[Initiate_Ref_Tetrahedron]], [[reference_Tetrahedron]]

INTERFACE ReferenceTetrahedron_Pointer
  MODULE FUNCTION reference_Tetrahedron_Pointer(nsd, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension, must be 3
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column; it takes precedence
    !! over `domainName`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain, `"UNIT"` or `"BIUNIT"`; default `"UNIT"`
    CLASS(ReferenceTetrahedron_), POINTER :: obj
    !! the new reference tetrahedron
  END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build a high order tetrahedron from a linear tetrahedron.
!
! Builds the Lagrange tetrahedron of the given `order` on the four
! vertices of `refelem`. The result goes in `obj`. The routine first
! clears `obj`.
!
! The result has these parts.
!
! 1. Interpolation points of family `ipType` in `obj%xij`.
! 2. The element type, such as `Tetrahedron10` for order 2.
! 3. The topology of its points, edges, faces and cell.
!
! The domain name, the spatial dimension and the `highOrderElement`
! pointer come from `refelem`.
!
! @warning
! The face topology is complete only for orders 1 and 2. See
! [[GetFaceConnectivity_Tetrahedron]].
! @endwarning
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/HighOrderElement_Tetrahedron_test_1.F90)
! - [test 2](|base|Tetrahedron/examples/HighOrderElement_Tetrahedron_test_2.F90)
!
! See also: [[GetEdgeConnectivity_Tetrahedron]]

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Tetrahedron(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! the linear reference tetrahedron
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the new element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! the high order element; the routine clears it first
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point family, such as `Equidistance`
  END SUBROUTINE HighOrderElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute the volume of a tetrahedron from its vertex columns.
!
! Returns the volume of the tetrahedron whose vertices are the first four
! columns of `XiJ`. The routine uses only the first three rows. The volume
! is never negative. The routine does not use `RefElem`.
!
! The unit reference tetrahedron has volume 1/6.
!
! Returns:
!   ans : REAL(DFP) - the volume
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ReferenceTetrahedronMeasure_test_1.F90)
!
! See also: [[TetrahedronVolume3D]]

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Tetrahedron(RefElem, XiJ) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    !! the reference tetrahedron; not used
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    !! vertex coordinates, one vertex per column, at least 3 by 4
    REAL(DFP) :: ans
    !! the volume
  END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Tetrahedron_Quality
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Quality measure of a tetrahedron, not implemented yet.
!
! This routine will return a quality measure of the tetrahedron with the
! vertices `xij`. Today it is a placeholder that always returns 0.
!
! @warning
! Do not use the result. The routine ignores `refelem`, `xij` and `measure`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - always 0
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/ReferenceTetrahedronMeasure_test_1.F90)
!
! See also: [[Measure_Simplex_Tetrahedron]]

INTERFACE Quality_Tetrahedron
  MODULE FUNCTION Tetrahedron_Quality(refelem, xij, measure) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! the reference tetrahedron; not used
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates, one vertex per column; not used
    INTEGER(I4B), INTENT(IN) :: measure
    !! quality measure selector; not used
    REAL(DFP) :: ans
    !! quality value, always 0
  END FUNCTION Tetrahedron_Quality
END INTERFACE Quality_Tetrahedron

!----------------------------------------------------------------------------
!                                                       TetrahedronVolume3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute the volume of a tetrahedron from its four vertices.
!
! Computes the volume of the tetrahedron with the vertices in `xij`. The
! volume is never negative. It does not depend on the vertex order.
!
! Formula:
!   $$ V = |\det[v_2-v_1,\, v_3-v_1,\, v_4-v_1]| / 6 $$
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/TetrahedronVolume3D_test_1.F90)
!
! See also: [[Measure_Simplex_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE TetrahedronVolume3D(xij, ans)
    REAL(DFP), INTENT(IN) :: xij(3, 4)
    !! vertex coordinates, one vertex per column
    REAL(DFP), INTENT(OUT) :: ans
    !! the volume
  END SUBROUTINE TetrahedronVolume3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the node numbers of the six edges of a tetrahedron.
!
! Fills `con` with the local node numbers of the six edges. Each column
! is one edge. The first two rows hold the end vertices. The edges are
! `1-2`, `1-3`, `1-4`, `2-3`, `2-4` and `3-4`, in this order.
!
! For `order` above 1, each edge has `order - 1` more nodes. The
! element numbers them after the four vertices. They go in rows 3 to
! `order + 1`.
!
! The routine sets `nrow` to `order + 1` and `ncol` to 6. The default
! `order` is 1.
!
! @note
! The caller allocates `con` with `order + 1` rows and 6 columns. The
! argument `opt` is not used.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetEdgeConnectivity_Tetrahedron_test_1.F90)
!
! See also: [[GetFaceConnectivity_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Tetrahedron(con, opt,  &
    & order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! edge connectivity, one edge per column
    !! the caller allocates it
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! polynomial order of the edge; default 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`, equal to `order + 1`
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 6
  END SUBROUTINE GetEdgeConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the node numbers of the four faces of a tetrahedron.
!
! Fills `con` with the local node numbers of the four faces. Each column
! is one face. The rows hold the three vertices of the face. For order 2,
! they also hold the three edge nodes.
!
! The routine sets `nrow` to 3 for order 1 and to 6 for order 2. It sets
! `ncol` to 4. The default `order` is 1.
!
! @note
! The caller allocates `con` with 3 rows for order 1, or 6 rows for
! order 2, and 4 columns. The argument `opt` is not used.
! @endnote
!
! @warning
! The routine handles only orders 1 and 2. For a higher `order`, it
! writes only the three vertex rows and sets `nrow` to 3.
! @endwarning
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetFaceConnectivity_Tetrahedron_test_1.F90)
!
! See also: [[GetEdgeConnectivity_Tetrahedron]]

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Tetrahedron(con, opt, order, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! face connectivity, one face per column
    !! the caller allocates it
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! polynomial order, 1 or 2; default 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 4
  END SUBROUTINE GetFaceConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefCoord_Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the vertex coordinates of a reference tetrahedron.
!
! Returns the four vertices of the reference tetrahedron, one vertex per
! column. Only the first letter of `refTetrahedron` counts. It is not
! case sensitive.
!
! - `"UNIT"` gives the vertices `(0,0,0)`, `(1,0,0)`, `(0,1,0)` and
!   `(0,0,1)`.
! - `"BIUNIT"` gives the vertices `(-1,-1,-1)`, `(1,-1,-1)`, `(-1,1,-1)`
!   and `(-1,-1,1)`.
!
! @warning
! If the first letter is neither `U` nor `B`, `ans` stays unset.
! @endwarning
!
! Returns:
!   ans : REAL(DFP)(3, 4) - vertex coordinates, one vertex per column
!
! See also: [[ReferenceTetrahedron]]

INTERFACE
  MODULE PURE FUNCTION RefCoord_Tetrahedron(refTetrahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! `"UNIT"` or `"BIUNIT"`; only the first letter counts
    REAL(DFP) :: ans(3, 4)
    !! vertex coordinates, one vertex per column
  END FUNCTION RefCoord_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the element type and node count of the faces of a tetrahedron.
!
! Returns the face element type and the number of nodes of the four faces
! of a tetrahedron. All four faces have the same type. The routine writes
! the first four entries of each output array.
!
! The face type is `Triangle3` for `Tetrahedron4`, `Triangle6` for
! `Tetrahedron10`, and so on up to `Triangle21` for `Tetrahedron56`.
!
! If `elemType` is absent, it is `Tetrahedron4`. Each output array is
! optional. The routine fills only the arrays that are present.
!
! @note
! For any other `elemType`, the arrays stay unchanged.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetFaceElemType_Tetrahedron_test_1.F90)
!
! See also: [[GetFaceElemType_Tetrahedron2]]

INTERFACE GetFaceElemType_Tetrahedron
 MODULE PURE SUBROUTINE GetFaceElemType_Tetrahedron1(elemType, faceElemType, &
                                                      tFaceNodes, opt)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! tetrahedron element type; default `Tetrahedron4`
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! face element type of each face, at least 4 entries
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! number of nodes in each face, at least 4 entries
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Tetrahedron1
END INTERFACE GetFaceElemType_Tetrahedron

!----------------------------------------------------------------------------
!                                             GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Get the element type and node count of one face of a tetrahedron.
!
! Returns the element type of local face `localFaceNumber` of the
! tetrahedron `elemType`. It also returns the number of nodes of that face.
! The face type is `Triangle3` for `Tetrahedron4`, `Triangle6` for
! `Tetrahedron10`, and so on up to `Triangle21` for `Tetrahedron56`.
!
! All four faces of a tetrahedron have the same type. So
! `localFaceNumber` does not change the result.
!
! @note
! For any other `elemType`, `faceElemType` and `tFaceNodes` stay unset.
! @endnote
!
! Examples
!
! - [test 1](|base|Tetrahedron/examples/GetFaceElemType_Tetrahedron_test_1.F90)
!
! See also: [[GetFaceElemType_Tetrahedron1]]

INTERFACE GetFaceElemType_Tetrahedron
  MODULE PURE SUBROUTINE GetFaceElemType_Tetrahedron2( &
    elemType, localFaceNumber, faceElemType, tFaceNodes, opt)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! tetrahedron element type
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number, 1 to 4; not used
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! face element type
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! number of nodes in the face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Tetrahedron2
END INTERFACE GetFaceElemType_Tetrahedron

END MODULE ReferenceTetrahedron_Method
