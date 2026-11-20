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
! summary: Reference triangle constructors, geometry and topology methods.
!
! This module works with the reference element `ReferenceTriangle_`.
! It covers these groups.
!
! 1. Constructors: `Initiate`, `ReferenceTriangle` and
!    `ReferenceTriangle_Pointer`.
! 2. Geometry of a triangle given by its vertices: `Area`, `Angles`,
!    `Centroid`, `CircumCenter`, `Incenter`, `EdgeLength`, `Barycentric`
!    and similar functions.
! 3. Point tests and queries: `Contains_Point_Triangle`, `NearestPoint`,
!    `DistanceFromPoint` and `RandomPoint`.
! 4. Topology: edge and face connectivity, facet elements, element names,
!    element types and entity counts.
! 5. Reference coordinates: `RefTriangleCoord` and `RefCoord_Triangle`.
!
! Some geometry functions use only the planar coordinates of the vertices.
!
! @warning
! `HighorderElement_Triangle` reads a fourth column of the face
! connectivity, which has only three columns.
! Do not rely on it until someone fixes this.
! @endwarning

MODULE ReferenceTriangle_Method
USE GlobalData
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceTriangle_
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceTriangle
PUBLIC :: ReferenceTriangle_Pointer
PUBLIC :: HighorderElement_Triangle
PUBLIC :: Measure_Simplex_Triangle
PUBLIC :: Triangle_Contains_Point
PUBLIC :: Contains_Point_Triangle
PUBLIC :: Angles
PUBLIC :: Area
PUBLIC :: ArealVector
PUBLIC :: Barycentric
PUBLIC :: Centroid
PUBLIC :: CircumCenter
PUBLIC :: CircumCircle
PUBLIC :: CircumRadius
PUBLIC :: ContainsLine
PUBLIC :: Diameter
PUBLIC :: EdgeLength
PUBLIC :: Incenter
PUBLIC :: Incircle
PUBLIC :: Inradius
PUBLIC :: Orthocenter
PUBLIC :: DistanceFromPoint
PUBLIC :: NearestPoint
PUBLIC :: RandomPoint
PUBLIC :: Triangle_Quality
PUBLIC :: Quality_Triangle
PUBLIC :: TriangleArea3D
PUBLIC :: TriangleArea2D
PUBLIC :: GetEdgeConnectivity_Triangle
PUBLIC :: GetFaceConnectivity_Triangle
PUBLIC :: RefTriangleCoord
PUBLIC :: RefCoord_Triangle
PUBLIC :: FacetElements_Triangle
PUBLIC :: DEFAULT_OPT_TRIANGLE_EDGE_CON
PUBLIC :: ElementOrder_Triangle
PUBLIC :: ElementType_Triangle
PUBLIC :: TotalNodesInElement_Triangle
PUBLIC :: TotalEntities_Triangle
PUBLIC :: FacetTopology_Triangle
PUBLIC :: ElementName_Triangle
PUBLIC :: MaxOrder_Triangle
PUBLIC :: FaceShapeMetaData_Triangle
PUBLIC :: GetFaceElemType_Triangle

#ifdef MAX_TRIANGLE_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Triangle = MAX_TRIANGLE_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Triangle = 2_I4B
#endif

#ifdef TRIANGLE_EDGE_CON_DEFAULT_OPT_1
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_TRIANGLE_EDGE_CON = 1_I4B
!! This means edges are [1,2], [1,3], [2,3]
#else
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_TRIANGLE_EDGE_CON = 2_I4B
!! This means edges are [1,2], [2,3], [3,1]
!! This is default option
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of a triangle element type.
!
! For example, `Triangle6` gives `"Triangle6"`.
! If `elemType` is not a triangle type, the result is `"NONE"`.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - name of the element type
!
! See also: [[ElementType_Triangle]]

INTERFACE
  MODULE PURE FUNCTION ElementName_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle6`
    CHARACTER(:), ALLOCATABLE :: ans
    !! name of the element type
  END FUNCTION ElementName_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the topology of the three edges of a triangle.
!
! Sets the topology of each edge of the triangle `elemType`.
! The edges follow the order given by `DEFAULT_OPT_TRIANGLE_EDGE_CON`.
! Each edge is a line element with the same order as the triangle.
!
! For edge `ii`, `ans(ii)%nptrs` holds the entries of `nptrs` that
! belong to the edge. `ans(ii)%name` is the line type and
! `ans(ii)%xiDimension` is 1.
!
! The caller allocates `ans` with size 3.
!
! See also: [[FacetElements_Triangle]], [[GetFaceConnectivity_Triangle]]

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Triangle(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle3`
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    !! node numbers of the triangle, one entry per node of `elemType`
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
    !! topology of the edges, size 3
  END SUBROUTINE FacetTopology_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes, edges, faces and cells of a triangle.
!
! The result has four entries.
!
! 1. The number of nodes, including high order nodes.
! 2. The number of edges, always 3.
! 3. The number of faces, always 1.
! 4. The number of cells, always 0.
!
! If `elemType` is not a triangle type, the node count is 0.
!
! Returns:
!   ans : INTEGER(I4B) - the four counts, size 4
!
! See also: [[TotalNodesInElement_Triangle]]

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle6`
    INTEGER(I4B) :: ans(4)
    !! counts of nodes, edges, faces and cells
  END FUNCTION TotalEntities_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               TotalNodesInElement_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes of a triangle element type.
!
! For example, `Triangle3` has 3 nodes and `Triangle6` has 6 nodes.
! If `elemType` is not a triangle type, the result is 0.
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes, or 0 for an unknown type
!
! See also: [[TotalEntities_Triangle]], [[ElementOrder_Triangle]]

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle6`
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION TotalNodesInElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the triangle element type for an element name.
!
! The name is case sensitive. For example, `"Triangle6"` gives
! `Triangle6`. The name `"Triangle"` is an alias for `Triangle3`.
! If the name is not a triangle name, the result is 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type, or 0 if the name is unknown
!
! Examples
!
! - [test 1](|base|Triangle/examples/ElementType_Triangle_test_1.F90)
!
! See also: [[ElementName_Triangle]]

INTERFACE
  MODULE PURE FUNCTION ElementType_Triangle(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    !! element name, for example `"Triangle6"`
    INTEGER(I4B) :: ans
    !! element type
  END FUNCTION ElementType_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ElementOrder_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the polynomial order of a triangle element type.
!
! For example, `Triangle3` has order 1 and `Triangle6` has order 2.
! `Triangle15a` has order 4 and `Triangle15b` has order 5.
!
! @warning
! The result is not set when `elemType` is not a triangle type.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B) - polynomial order
!
! See also: [[ElementType_Triangle]], [[TotalNodesInElement_Triangle]]

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle6`
    INTEGER(I4B) :: ans
    !! polynomial order
  END FUNCTION ElementOrder_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the three facets of a reference triangle.
!
! Each facet is a line element. Its name, order and node numbers come from
! the edge topology of `refelem`. Its points use the interpolation point
! type of `refelem`. Each facet keeps the `nsd` of `refelem`.
!
! The caller allocates `ans` with size 3. The topology of each facet lists
! its nodes as points, then the line itself.
!
! Examples
!
! - [test 1](|base|Triangle/examples/FacetElements_Triangle_test_1.F90)
!
! See also: [[FacetElements_Triangle2]]

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! the reference triangle
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the facets, size 3
  END SUBROUTINE FacetElements_Triangle1
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                    FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the three facets of a triangle from its element type.
!
! Each facet is a line element with the same order as the triangle.
! The facets follow the edge order `[1,2]`, `[2,3]`, `[3,1]`.
! Their points are equidistant.
!
! The caller allocates `ans` with size 3. Each facet gets its order,
! name, points, `nsd` and topology. The topology lists the facet nodes
! as points, then the line itself.
!
! See also: [[FacetElements_Triangle1]]

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! triangle element type, for example `Triangle3`
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension stored in each facet
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! the facets, size 3
  END SUBROUTINE FacetElements_Triangle2
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                         Initiate@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Initiates a linear reference triangle.
!
! Fills `obj` as a three node triangle with three edges and one face.
! It first deallocates the old content of `obj`.
!
! If `xij` is present, its first three columns set the vertices.
! `domainName` is then ignored. The method sets it to `"UNIT"`,
! `"BIUNIT"` or `"GENERAL"`, by comparing the vertices with the two
! reference triangles.
!
! If `xij` is absent, `domainName` selects the vertices. It is not case
! sensitive. The default is `"UNIT"`. The vertices are only set for
! `"UNIT"` and `"BIUNIT"`.
!
! @note
! `nsd` only records the spatial dimension. It does not change the number
! of rows of `obj%xij`, which is 2.
! @endnote
!
! See also: [[Reference_Triangle]], [[Reference_Triangle_Pointer]]

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Triangle(obj, nsd, xij, domainName)
    CLASS(ReferenceTriangle_), INTENT(INOUT) :: obj
    !! the reference triangle to initiate
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the space that holds the triangle
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column.
    !! At least 2 rows and 3 columns. Only the first 2 rows and
    !! 3 columns count
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain, `"UNIT"` or `"BIUNIT"`; default `"UNIT"`.
    !! Ignored when `xij` is present
  END SUBROUTINE Initiate_Ref_Triangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 ReferenceTriangle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a linear reference triangle as a function result.
!
! Builds a `ReferenceTriangle_` with three nodes, three edges and one face.
! It does the same work as [[Initiate_Ref_Triangle]], but it returns the
! object instead of filling an argument.
!
! If `xij` is present, its first three columns set the vertices.
! `domainName` is then ignored. The method sets it to `"UNIT"`,
! `"BIUNIT"` or `"GENERAL"`, by comparing the vertices with the two
! reference triangles.
!
! If `xij` is absent, `domainName` selects the vertices. It is not case
! sensitive. The default is `"UNIT"`. The vertices are only set for
! `"UNIT"` and `"BIUNIT"`.
!
! Returns:
!   obj : `ReferenceTriangle_` - the reference triangle
!
! See also: [[Initiate_Ref_Triangle]], [[Reference_Triangle_Pointer]]

INTERFACE ReferenceTriangle
  MODULE PURE FUNCTION Reference_Triangle(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the space that holds the triangle
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates, one vertex per column.
    !! At least 2 rows and 3 columns. Only the first 2 rows and
    !! 3 columns count
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! reference domain, `"UNIT"` or `"BIUNIT"`; default `"UNIT"`.
    !! Ignored when `xij` is present
    TYPE(ReferenceTriangle_) :: obj
    !! the reference triangle
  END FUNCTION Reference_Triangle
END INTERFACE ReferenceTriangle

!----------------------------------------------------------------------------
!                                         ReferenceTriangle_Pointer@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a pointer to a new linear reference triangle.
!
! Allocates a new `ReferenceTriangle_` and returns a pointer to it.
! The element has three nodes and order 1.
! The caller owns the pointer and must deallocate it.
!
! The routine picks the vertex coordinates in this order.
!
! 1. If `xij` is present, its first two rows and first three columns
!    are the coordinates. The domain name becomes `"UNIT"` or `"BIUNIT"`
!    when they match that triangle, and `"GENERAL"` otherwise.
! 2. If `domainName` is `"UNIT"` or `"BIUNIT"` in any case, the
!    coordinates are those of that reference triangle.
! 3. If `domainName` has another value, the routine stores the name
!    and leaves the coordinates unset.
! 4. If neither argument is present, the domain is `"UNIT"`.
!
! @note
! `nsd` only records the dimension of the space around the triangle.
! The coordinate array always has two rows.
! @endnote
!
! Returns:
!   obj : CLASS(`ReferenceTriangle_`), POINTER - the new reference triangle
!
! See also: [[ReferenceTriangle]]

INTERFACE ReferenceTriangle_Pointer
  MODULE FUNCTION Reference_Triangle_Pointer(nsd, xij, domainName) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the space around the triangle
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    !! domain name, `"UNIT"` or `"BIUNIT"`; used when `xij` is absent
    CLASS(ReferenceTriangle_), POINTER :: obj
    !! pointer to the new reference triangle
  END FUNCTION Reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds a higher order triangle from a linear reference triangle.
!
! Fills `obj` with a Lagrange triangle of the given `order`.
! The routine does the following.
!
! 1. Clears `obj`.
! 2. Sets the nodes of `obj` to interpolation points of degree `order`
!    on the three vertices of `refelem`.
! 3. Copies the domain name, `nsd`, and the dimension of the
!    parametric space from `refelem`.
! 4. Sets the element name, the entity counts and the topology of
!    points, edges and the cell.
!
! The number of nodes is `(order + 1) * (order + 2) / 2`.
!
! @warning
! The code reads a fourth edge column of the connectivity. A triangle
! has only three edges, so this read is out of bounds. Do not rely on
! this routine until someone fixes the code.
! @endwarning
!
! Examples
!
! - [test 1](|base|Triangle/examples/HighorderElement_Triangle_test_1.F90)
! - [test 2](|base|Triangle/examples/HighorderElement_Triangle_test_2.F90)
!
! See also: [[InterpolationPoint_Triangle]]

INTERFACE
  MODULE SUBROUTINE HighorderElement_Triangle(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! linear reference triangle
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the new element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! the higher order element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! type of interpolation points, for example `Equidistance`
  END SUBROUTINE HighorderElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the area of a triangle.
!
! Returns the measure of a triangle, which is its area.
! The vertices are the columns of `xij`.
! The result is the same as [[Area]].
!
! If `refelem%nsd` is 2, the area has a sign. It is positive when the
! vertices run counterclockwise.
! If `refelem%nsd` is 3, the area is never negative.
!
! Returns:
!   ans : REAL(DFP) - area of the triangle
!
! Formula:
!   $$ A = \frac{1}{2} \det [v_2 - v_1, v_3 - v_1] $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Measure_Simplex_Triangle_test_1.F90)
!
! See also: [[Area]]

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Triangle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans
    !! area of the triangle
  END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Angles@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the three interior angles of a triangle.
!
! Returns the interior angles in radians.
! The vertices are the columns of `xij`.
! The value `ans(i)` is the angle at vertex `i`.
! The angles add up to pi.
!
! The routine uses the first `refelem%nsd` rows of `xij`.
! It supports `refelem%nsd` equal to 2 or 3.
!
! Returns:
!   ans : REAL(DFP) - angles at the three vertices, in radians
!
! Formula:
!   $$ \theta_1 + \theta_2 + \theta_3 = \pi $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Angles_test_1.F90)

INTERFACE Angles
  MODULE PURE FUNCTION Triangle_angles(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans(3)
    !! angles at the three vertices, in radians
  END FUNCTION Triangle_angles
END INTERFACE Angles

!----------------------------------------------------------------------------
!                                                             Area@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the area of a triangle.
!
! The vertices are the columns of `xij`.
!
! If `refelem%nsd` is 2, the area has a sign. It is positive when the
! vertices run counterclockwise.
! If `refelem%nsd` is 3, the area is never negative.
!
! Returns:
!   ans : REAL(DFP) - area of the triangle
!
! Formula:
!   $$ A = \frac{1}{2} \det [v_2 - v_1, v_3 - v_1] $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Area_test_1.F90)
!
! See also: [[ArealVector]]

INTERFACE Area
  MODULE PURE FUNCTION Triangle_area(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans
    !! area of the triangle
  END FUNCTION Triangle_area
END INTERFACE Area

!----------------------------------------------------------------------------
!                                                        ArealVector@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the area vector of a triangle.
!
! The vertices are the columns of `xij`.
! The result has three components.
!
! If `refelem%nsd` is 2, the result is `[0, 0, A]`.
! Here `A` is the signed area.
!
! If `refelem%nsd` is 3, the result is the cross product of the
! vectors from vertex 1 to vertices 2 and 3.
!
! @note
! The two cases use different scales. In 3D the vector has length
! `2 * A`, which is twice the area.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - area vector of the triangle
!
! Formula:
!   $$ a_{2D} = (0, 0, A), \quad
!      a_{3D} = (v_2 - v_1) \times (v_3 - v_1) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/ArealVector_test_1.F90)
!
! See also: [[Area]]

INTERFACE ArealVector
  MODULE PURE FUNCTION Triangle_arealVector(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans(3)
    !! area vector of the triangle
  END FUNCTION Triangle_arealVector
END INTERFACE ArealVector

!----------------------------------------------------------------------------
!                                                      Barycentric@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the barycentric coordinates of a point in a triangle.
!
! Returns the three barycentric coordinates of the point `x`.
! The vertices are the columns of `xij`.
! The coordinates add up to 1.
! They are all non-negative when `x` lies inside the triangle.
!
! The routine works in the plane. It uses only the first two rows
! of `xij` and the first two entries of `x`. It ignores `refelem%nsd`.
!
! Returns:
!   ans : REAL(DFP) - barycentric coordinates of `x`
!
! Formula:
!   $$ x = \sum_{i=1}^{3} \lambda_i v_i $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Barycentric_test_1.F90)

INTERFACE Barycentric
  MODULE PURE FUNCTION Triangle_barycentric(refelem, xij, x) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP), INTENT(IN) :: x(:)
    !! the point, at least 2 entries
    REAL(DFP) :: ans(3)
    !! barycentric coordinates of `x`
  END FUNCTION Triangle_barycentric
END INTERFACE Barycentric

!----------------------------------------------------------------------------
!                                                          Centroid@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the centroid of a triangle.
!
! The vertices are the columns of `xij`.
! The result has three components.
! If `refelem%nsd` is 2, the third component is 0.
!
! Returns:
!   ans : REAL(DFP) - coordinates of the centroid
!
! Formula:
!   $$ c = \frac{v_1 + v_2 + v_3}{3} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Centroid_test_1.F90)

INTERFACE Centroid
  MODULE PURE FUNCTION Triangle_centroid(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans(3)
    !! coordinates of the centroid
  END FUNCTION Triangle_centroid
END INTERFACE Centroid

!----------------------------------------------------------------------------
!                                                      CircumCenter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the circumcentre of a triangle.
!
! The circumcentre is the centre of the circle through the three
! vertices. The vertices are the columns of `xij`.
! The result has three components.
! If `refelem%nsd` is 2, the third component is 0.
!
! Returns:
!   ans : REAL(DFP) - coordinates of the circumcentre
!
! Formula:
!   $$ \| c - v_i \| = R, \quad i = 1, 2, 3 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/CircumCenter_test_1.F90)
!
! See also: [[CircumCircle]]

INTERFACE CircumCenter
  MODULE PURE FUNCTION Triangle_circumcentre(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, `nsd` rows and 3 columns
    REAL(DFP) :: ans(3)
    !! coordinates of the circumcentre
  END FUNCTION Triangle_circumcentre
END INTERFACE CircumCenter

!----------------------------------------------------------------------------
!                                                      CircumCircle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the radius and centre of the circle through a triangle.
!
! The vertices are the columns of `xij`.
! The routine works in the plane. It uses the first two rows of `xij`
! and ignores `refelem%nsd`.
!
! The result holds the radius first, then the centre.
! The last entry is always 0.
!
! If the three vertices lie on a line, the radius is -1 and the centre
! is zero.
!
! Returns:
!   ans : REAL(DFP) - radius `R` and centre `c` of the circle
!
! Formula:
!   $$ ans = (R, c_x, c_y, 0) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/CircumCircle_test_1.F90)
!
! See also: [[CircumCenter]]

INTERFACE CircumCircle
  MODULE PURE FUNCTION Triangle_circumcircle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans(4)
    !! `ans(1)` is the radius, `ans(2:3)` is the centre, `ans(4)` is 0
  END FUNCTION Triangle_circumcircle
END INTERFACE CircumCircle

!----------------------------------------------------------------------------
!                                                      CircumRadius@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the radius of the circle through the three vertices.
!
! Returns the circumradius of a triangle.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
! It treats the triangle as a plane triangle.
!
! If the three vertices lie on one line, the result is `-1`.
!
! Returns:
!   ans : REAL(DFP) - circumradius, or -1 for a degenerate triangle
!
! Formula:
!   $$ R = \frac{abc}{4A} $$
!
!   Here a, b and c are the edge lengths and A is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/CircumRadius_test_1.F90)
!
! See also: [[CircumCircle]]

INTERFACE CircumRadius
  MODULE PURE FUNCTION Triangle_circumradius(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans
    !! circumradius
  END FUNCTION Triangle_circumradius
END INTERFACE CircumRadius

!----------------------------------------------------------------------------
!                                                     ContainsLine@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checks whether a line in 3D meets a triangle.
!
! Finds where a line crosses the plane of a triangle.
! It sets `inside` to true when the crossing point lies in the triangle.
! The routine stores the point in `xint`.
!
! The routine describes the line in one of two ways.
!
! 1. If `parametricLine` is true, the line passes through `x1` and has
!    the direction `x2`.
! 2. Otherwise, the line passes through the two points `x1` and `x2`.
!
! The routine works in 3D, so `xij` needs three rows.
!
! Formula:
!   $$ x = x_1 + s x_2 $$
!
!   This is the parametric line. For two points, replace `x2` by
!   `x2 - x1`.
!
! Examples
!
! - [test 1](|base|Triangle/examples/ContainsLine_test_1.F90)
!
! See also: [[Contains_Point_Triangle]]

INTERFACE ContainsLine
  MODULE PURE SUBROUTINE Triangle_contains_line(refelem, xij, x1, x2, &
    & parametricLine, inside, xint)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :), x1(3), x2(3)
    !! `xij`: vertex coordinates in columns, 3 rows and 3 columns.
    !! `x1`: point on the line.
    !! `x2`: direction of the line if `parametricLine` is true, else a
    !! second point on the line.
    LOGICAL(LGT), INTENT(IN) :: parametricLine
    !! true if `x2` is a direction, false if `x2` is a point
    LOGICAL(LGT), INTENT(OUT) :: inside
    !! true if the line meets the triangle
    REAL(DFP), INTENT(OUT) :: xint(3)
    !! point where the line meets the plane of the triangle
  END SUBROUTINE Triangle_contains_line
END INTERFACE ContainsLine

!----------------------------------------------------------------------------
!                                                    ContainsPoint@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checks whether a point lies in a triangle.
!
! Returns true if the point `x` lies in the triangle.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij` and the first two
! entries of `x`. It treats the triangle as a plane triangle.
!
! Returns:
!   ans : LOGICAL(LGT) - true if `x` lies in the triangle
!
! Formula:
!   $$ x \in T \iff \lambda_i(x) \ge 0, \quad i = 1, 2, 3 $$
!
!   Here $\lambda_i$ are the barycentric coordinates of `x`.
!
! Examples
!
! - [test 1](|base|Triangle/examples/Contains_Point_Triangle_test_1.F90)
!
! See also: [[Barycentric]]

INTERFACE Contains_Point_Triangle
  MODULE PURE FUNCTION Triangle_Contains_Point(refelem, xij, x) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    !! `xij`: vertex coordinates in columns, at least 2 rows and 3 columns.
    !! `x`: point to test, at least 2 entries.
    LOGICAL(LGT) :: ans
    !! true if `x` lies in the triangle
  END FUNCTION Triangle_Contains_Point
END INTERFACE Contains_Point_Triangle

!----------------------------------------------------------------------------
!                                                         Diameter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the diameter of the smallest circle around a triangle.
!
! Returns the diameter of the smallest circle that holds the triangle.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
!
! 1. For an acute triangle, this is the circumcircle diameter.
! 2. For an obtuse triangle, this is the longest edge.
!
! @warning
! The backend sums the coordinate differences before it squares them.
! It does not square each difference first. The edge lengths it uses
! can be wrong, and the result can be wrong too.
! For the triangle with vertices (0,0), (2,0) and (0,1), the routine
! returns 2, not `SQRT(5)`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - diameter of the smallest circle around the triangle
!
! Formula:
!   $$ D = \frac{abc}{2A} $$
!
!   This holds for an acute triangle. Here a, b and c are the edge
!   lengths and A is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/Diameter_test_1.F90)
!
! See also: [[CircumRadius]]

INTERFACE Diameter
  MODULE PURE FUNCTION triangle_diameter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans
    !! diameter
  END FUNCTION triangle_diameter
END INTERFACE Diameter

!----------------------------------------------------------------------------
!                                                       EdgeLength@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the lengths of the three edges of a triangle.
!
! The vertices are the columns of `xij`.
! The value `ans(i)` is the length of the edge from vertex `i` to vertex
! `i+1`. The last edge runs from vertex 3 to vertex 1.
! The routine uses only the first two rows of `xij`.
!
! Returns:
!   ans : REAL(DFP) - the three edge lengths
!
! Formula:
!   $$ \ell_i = \| v_{i+1} - v_i \| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/EdgeLength_test_1.F90)

INTERFACE EdgeLength
  MODULE PURE FUNCTION triangle_edge_length(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans(3)
    !! lengths of the three edges
  END FUNCTION triangle_edge_length
END INTERFACE EdgeLength

!----------------------------------------------------------------------------
!                                                         Incenter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the incenter of a triangle.
!
! The incenter is the centre of the largest circle inside the triangle.
! It is where the three angle bisectors meet.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
! The third entry of the result is always zero.
!
! If all three vertices coincide, the result is that vertex.
!
! Returns:
!   ans : REAL(DFP) - coordinates of the incenter, `ans(3)` is 0
!
! Formula:
!   $$ c = \frac{b v_1 + c' v_2 + a v_3}{a + b + c'} $$
!
!   Here a is the length of the edge from `v_1` to `v_2`.
!   The value b is the length of the edge from `v_2` to `v_3`.
!   The value c' is the length of the edge from `v_3` to `v_1`.
!
! Examples
!
! - [test 1](|base|Triangle/examples/Incenter_test_1.F90)
!
! See also: [[Incircle]], [[Inradius]]

INTERFACE Incenter
  MODULE PURE FUNCTION triangle_incenter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans(3)
    !! incenter, `ans(3)` is 0
  END FUNCTION triangle_incenter
END INTERFACE Incenter

!----------------------------------------------------------------------------
!                                                         Incircle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the radius and centre of the circle inside a triangle.
!
! The incircle is the largest circle inside the triangle.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
!
! The result holds the radius first, then the centre.
! The last entry is always zero.
!
! If all three vertices coincide, the radius is zero and the centre is
! that vertex.
!
! Returns:
!   ans : REAL(DFP) - radius in `ans(1)`, centre in `ans(2:3)`
!
! Formula:
!   $$ ans = (r, c_x, c_y, 0) $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Incircle_test_1.F90)
!
! See also: [[Incenter]], [[Inradius]]

INTERFACE Incircle
  MODULE PURE FUNCTION triangle_incircle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans(4)
    !! radius, x and y of the centre, and 0
  END FUNCTION triangle_incircle
END INTERFACE Incircle

!----------------------------------------------------------------------------
!                                                         Inradius@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the radius of the circle inside a triangle.
!
! The inradius is the radius of the largest circle inside the triangle.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
! If all three vertices coincide, the result is zero.
!
! Returns:
!   ans : REAL(DFP) - inradius
!
! Formula:
!   $$ r = \frac{2A}{a + b + c} $$
!
!   Here a, b and c are the edge lengths and A is the area.
!
! Examples
!
! - [test 1](|base|Triangle/examples/Inradius_test_1.F90)
!
! See also: [[Incircle]], [[Incenter]]

INTERFACE Inradius
  MODULE PURE FUNCTION triangle_inradius(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans
    !! inradius
  END FUNCTION triangle_inradius
END INTERFACE Inradius

!----------------------------------------------------------------------------
!                                                      Orthocenter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the orthocenter of a triangle.
!
! The orthocenter is the point where the three altitudes meet.
! The vertices are the columns of `xij`.
! The routine uses only the first two rows of `xij`.
! The third entry of the result is always zero.
!
! For a right triangle, the orthocenter is the vertex at the right angle.
!
! If the routine cannot find the point, it sets the first two entries to
! `HUGE(1.0_DFP)`. This happens for a degenerate triangle.
!
! Returns:
!   ans : REAL(DFP) - coordinates of the orthocenter, `ans(3)` is 0
!
! Formula:
!   $$ h = 3 c_{centroid} - 2 c_{circumcenter} $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/Orthocenter_test_1.F90)
!
! See also: [[Centroid]], [[CircumCenter]]

INTERFACE Orthocenter
  MODULE PURE FUNCTION triangle_orthocenter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle; the routine does not use it
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! vertex coordinates in columns, at least 2 rows and 3 columns
    REAL(DFP) :: ans(3)
    !! orthocenter, `ans(3)` is 0
  END FUNCTION triangle_orthocenter
END INTERFACE Orthocenter

!----------------------------------------------------------------------------
!                                                DistanceFromPoint@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the distance from a point to the edges of a triangle.
!
! Returns the shortest distance from the point `x` to the boundary of the
! triangle. The vertices are the columns of `xij`.
!
! The routine reads `refelem%nsd`.
!
! 1. If `refelem%nsd` is 2, it uses the first two rows of `xij` and the
!    first two entries of `x`.
! 2. If `refelem%nsd` is 3, it uses the first three rows of `xij` and
!    the first three entries of `x`.
!
! A point inside the triangle has a positive distance to the edges.
!
! @note
! For any other value of `refelem%nsd`, the result is not set.
! @endnote
!
! Returns:
!   ans : REAL(DFP) - distance from `x` to the nearest edge
!
! Formula:
!   $$ d = \min_{q \in \partial T} \| x - q \| $$
!
! See also: [[NearestPoint]]

INTERFACE DistanceFromPoint
  MODULE PURE FUNCTION triangle_point_dist(refelem, xij, x) &
    & RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference triangle, `refelem%nsd` must be 2 or 3
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    !! `xij`: vertex coordinates in columns, `nsd` rows and 3 columns.
    !! `x`: point, at least `nsd` entries.
    REAL(DFP) :: ans
    !! distance from `x` to the edges
  END FUNCTION triangle_point_dist
END INTERFACE DistanceFromPoint

!----------------------------------------------------------------------------
!                                                      NearestPoint@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Finds the point of a triangle nearest to a given point.
!
! Finds the point `xn` on the boundary of a triangle that is closest to
! the point `x`. It also returns the distance `dist` between `x` and `xn`.
!
! The routine works in 2D. It reads the first two rows of `xij` and the
! first two entries of `x`. It sets only `xn(1:2)`.
!
! @note
! The routine does not use `refelem`.
! @endnote
!
! Formula:
!   $$ q = \mathop{\mathrm{argmin}}_{q \in \partial T} \|x - q\| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/NearestPoint_test_1.F90)

INTERFACE NearestPoint
  MODULE PURE SUBROUTINE triangle_get_nearest_point(refelem, xij, x, xn,  &
    & dist)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference triangle. The routine does not use it.
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    !! `xij(1:2, 1:3)` holds the vertex coordinates, one vertex per column.
    !! `x(1:2)` is the query point.
    REAL(DFP), INTENT(INOUT) :: xn(:)
    !! Nearest boundary point. The routine sets `xn(1:2)`.
    REAL(DFP), INTENT(OUT) :: dist
    !! Distance between `x` and `xn`.
  END SUBROUTINE triangle_get_nearest_point
END INTERFACE NearestPoint

!----------------------------------------------------------------------------
!                                                       RandomPoint@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns random points inside a triangle.
!
! Returns `n` random points inside a triangle.
! The routine works in 2D. It reads `xij(1:2, 1:3)`.
! Each column of the result is one point. The third row is zero.
!
! @warning
! Always pass `seed`. The sampler needs it, and the routine gives no
! default.
! @endwarning
!
! @note
! The routine does not use `refelem`.
! @endnote
!
! Returns:
!   ans : REAL(DFP)(3, n) - the points, one per column
!
! Formula:
!   $$ x = \sum_i \lambda_i v_i, \quad \lambda_i \ge 0, \quad
!   \sum_i \lambda_i = 1 $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/RandomPoint_test_1.F90)

INTERFACE RandomPoint
  MODULE PURE FUNCTION triangle_random_point(refelem, xij, n, seed) &
    & RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference triangle. The routine does not use it.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Vertex coordinates, one vertex per column.
    !! The routine reads only `xij(1:2, 1:3)`.
    INTEGER(I4B), INTENT(IN) :: n
    !! Number of points.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    !! Seed of the random generator. Always pass it.
    REAL(DFP) :: ans(3, n)
    !! Points in columns. The third row is zero.
  END FUNCTION triangle_random_point
END INTERFACE RandomPoint

!----------------------------------------------------------------------------
!                                                          Quality@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a quality measure of a triangle.
!
! Returns the quality measure of a triangle that `measure` selects.
! Use a member of `QualityMeasure` for `measure`.
! The supported measures are:
!
! 1. `QualityMeasure%area`: the area.
! 2. `QualityMeasure%maxangle`: the largest angle.
! 3. `QualityMeasure%minangle`: the smallest angle.
! 4. `QualityMeasure%angleratio`: three times the smallest angle,
!    divided by pi.
! 5. `QualityMeasure%radiusRatio`: twice the inradius, divided by the
!    circumradius.
! 6. `QualityMeasure%edgeRatio`: the shortest edge, divided by the
!    longest edge.
! 7. `QualityMeasure%aspectRatio`: the longest edge times the perimeter,
!    divided by four times the square root of 3 times the area.
!
! Angles are in radians. The measures are not all scaled to [0, 1].
!
! @warning
! For any other value of `measure`, the routine does not set `ans`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP) - the selected quality measure
!
! Examples
!
! - [test 1](|base|Triangle/examples/Quality_Triangle_test_1.F90)

INTERFACE Quality_Triangle
  MODULE PURE FUNCTION Triangle_Quality(refelem, xij, measure) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Reference triangle.
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! Vertex coordinates, one vertex per column.
    INTEGER(I4B), INTENT(IN) :: measure
    !! Quality measure, a member of `QualityMeasure`.
    REAL(DFP) :: ans
    !! Value of the selected quality measure.
  END FUNCTION Triangle_Quality
END INTERFACE Quality_Triangle

!----------------------------------------------------------------------------
!                                                            TriangleArea3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the area of a triangle in 3D.
!
! Computes the area of a triangle with vertices in 3D.
! The area is never negative.
! It is half the length of the cross product of two edges.
!
! Formula:
!   $$ A = \tfrac12 \|(v_2 - v_1) \times (v_3 - v_1)\| $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/TriangleArea3D_test_1.F90)
!
! See also: [[TriangleArea2D]]

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea3D(t, ans)
    REAL(DFP), INTENT(IN) :: t(3, 3)
    !! Vertex coordinates, one vertex per column.
    REAL(DFP), INTENT(OUT) :: ans
    !! Area of the triangle.
  END SUBROUTINE TriangleArea3D
END INTERFACE

!----------------------------------------------------------------------------
!                                                              TriangleArea2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the signed area of a triangle in 2D.
!
! Computes the signed area of a triangle with vertices in 2D.
! The area is positive when the vertices run counterclockwise.
! It is negative when they run clockwise.
!
! Formula:
!   $$ A = \tfrac12 \left[ x_1 (y_2 - y_3) + x_2 (y_3 - y_1)
!   + x_3 (y_1 - y_2) \right] $$
!
! Examples
!
! - [test 1](|base|Triangle/examples/TriangleArea2D_test_1.F90)
!
! See also: [[TriangleArea3D]]

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea2D(t, ans)
    REAL(DFP), INTENT(IN) :: t(2, 3)
    !! Vertex coordinates, one vertex per column.
    REAL(DFP), INTENT(OUT) :: ans
    !! Signed area of the triangle.
  END SUBROUTINE TriangleArea2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the local node numbers of the edges of a triangle.
!
! Fills `con` with the local node numbers of the three edges.
! Column `j` of `con` holds edge `j`.
! The first two rows hold the end vertices of the edge.
! The next rows hold the nodes inside the edge, if the order is above 1.
!
! The option `opt` selects the numbering.
!
! 1. If `opt` is 1, the edges are [1,2], [1,3] and [2,3].
!    This is the hierarchical numbering and the default.
! 2. If `opt` is 2, the edges are [1,2], [2,3] and [3,1].
!    This is the Lagrange numbering.
!
! The routine sets `nrow` to `order + 1` and `ncol` to 3.
!
! @note
! The caller allocates `con` with at least `order + 1` rows and 3
! columns.
! @endnote
!
! Examples
!
! - [test 1](|base|Triangle/examples/GetEdgeConnectivity_Triangle_test_1.F90)
!
! See also: [[GetFaceConnectivity_Triangle]]

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Triangle(con, opt, order, &
                                                      nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Edge connectivity.
    !! Column `j` holds the local node numbers of edge `j`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Numbering option, 1 or 2. Default 1.
    !! 1 gives [1,2], [1,3], [2,3] for the hierarchical basis.
    !! 2 gives [1,2], [2,3], [3,1] for the Lagrange basis.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Polynomial order of the element. Default 1.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in `con`, equal to `order + 1`.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Number of columns written in `con`, equal to 3.
  END SUBROUTINE GetEdgeConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetFaceConnectivity_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the local node numbers of the faces of a triangle.
!
! Fills `con` with the local node numbers of the three faces.
! The faces of a triangle are its edges.
! Column `j` of `con` holds face `j`.
! The faces are always [1,2], [2,3] and [3,1].
!
! The routine sets `nrow` to `order + 1` and `ncol` to 3.
!
! @note
! The routine ignores `opt`. It always uses the Lagrange numbering.
! The caller allocates `con` with at least `order + 1` rows and 3
! columns.
! @endnote
!
! See also: [[GetEdgeConnectivity_Triangle]]

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Triangle(con, opt, order, &
                                                      nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Face connectivity.
    !! Column `j` holds the local node numbers of face `j`.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Polynomial order of the element. Default 1.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in `con`, equal to `order + 1`.
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Number of columns written in `con`, equal to 3.
  END SUBROUTINE GetFaceConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                           RefTriangleCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the vertex coordinates of a reference triangle.
!
! Returns the coordinates of the three vertices of a reference triangle.
! Column `j` of the result holds vertex `j`.
! Only the first letter of `refTriangle` matters, in upper or lower case.
!
! 1. `"UNIT"` gives the vertices (0,0), (1,0) and (0,1).
! 2. `"BIUNIT"` gives the vertices (-1,-1), (1,-1) and (-1,1).
!
! @warning
! For any other first letter, the routine does not set `ans`.
! @endwarning
!
! Returns:
!   ans : REAL(DFP)(2, 3) - vertex coordinates, one vertex per column

INTERFACE RefCoord_Triangle
  MODULE PURE FUNCTION RefTriangleCoord(refTriangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! Name of the reference triangle, `"UNIT"` or `"BIUNIT"`.
    REAL(DFP) :: ans(2, 3)
    !! Vertex coordinates, one vertex per column.
  END FUNCTION RefTriangleCoord
END INTERFACE RefCoord_Triangle

!----------------------------------------------------------------------------
!                                           FaceShapeMetaData_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Puts a triangular face in canonical order and finds its
! orientation.
!
! Takes the three vertex numbers of a triangular face in `face`.
! The routine does the following.
!
! 1. It rotates the vertices so that the smallest number comes first.
!    It stores the result in `localFaces`, if present.
! 2. It sorts the other two numbers in ascending order.
!    It stores the result in `sorted_face`.
! 3. It stores the orientation in `faceOrient`, if present.
!
! The orientation has two entries.
! The first is the number of places the routine rotates the face: 0, 1
! or 2.
! The second is 1 if the other two numbers were already ascending.
! Otherwise, it is -1.
!
! @note
! The routine does not change `face`.
! @endnote

INTERFACE
  MODULE SUBROUTINE FaceShapeMetaData_Triangle(face, sorted_face, &
                                               faceOrient, localFaces)
    INTEGER(I4B), INTENT(INOUT) :: face(:)
    !! Vertex numbers of the face, `face(1:3)`. It is not changed.
    INTEGER(I4B), INTENT(INOUT) :: sorted_face(:)
    !! Canonical face `sorted_face(1:3)`, smallest number first.
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceOrient(:)
    !! Orientation `faceOrient(1:2)`: the rotation and the sign.
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaces(:)
    !! Rotated vertex numbers `localFaces(1:3)`, smallest first.
  END SUBROUTINE FaceShapeMetaData_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element type of the faces of a triangle element.
!
! Returns the element type and the node count of the three faces of a
! triangle element. The faces are line elements.
! The routine sets the first three entries of `faceElemType` and
! `tFaceNodes`, whichever are present.
!
! The faces depend on `elemType`.
!
! 1. `Triangle3` has `Line2` faces with 2 nodes.
! 2. `Triangle6` has `Line3` faces with 3 nodes.
! 3. `Triangle9` and `Triangle10` have `Line4` faces with 4 nodes.
! 4. `Triangle15` has `Line5` faces with 5 nodes.
! 5. `Triangle21a` and `Triangle21b` have `Line6` faces with 6 nodes.
! 6. `Triangle18` has `Line7` faces with 7 nodes.
!
! For any other `elemType`, the routine changes nothing.
!
! Examples
!
! - [test 1](|base|Triangle/examples/GetFaceElemType_Triangle_test_1.F90)

INTERFACE GetFaceElemType_Triangle
MODULE PURE SUBROUTINE GetFaceElemType_Triangle1(elemType, faceElemType, &
    opt, &
                                                   tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type of the triangle. Default `Triangle`.
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Element types of the faces. It needs at least 3 entries.
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! Number of nodes of each face. It needs at least 3 entries.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used.
  END SUBROUTINE GetFaceElemType_Triangle1
END INTERFACE GetFaceElemType_Triangle

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element type of one face of a triangle element.
!
! Returns the element type and the node count of the face
! `localFaceNumber`. The faces are line elements.
! All three faces of a triangle have the same type.
!
! The face depends on `elemType`.
!
! 1. `Triangle3` has a `Line2` face with 2 nodes.
! 2. `Triangle6` has a `Line3` face with 3 nodes.
! 3. `Triangle9` and `Triangle10` have a `Line4` face with 4 nodes.
! 4. `Triangle15` has a `Line5` face with 5 nodes.
! 5. `Triangle21a` and `Triangle21b` have a `Line6` face with 6 nodes.
! 6. `Triangle18` has a `Line7` face with 7 nodes.
!
! For any other `elemType`, the routine leaves `faceElemType` and
! `tFaceNodes` unchanged. Their values are then undefined.
!
! @note
! The routine does not use `localFaceNumber` and `opt`.
! @endnote
!
! Examples
!
! - [test 1](|base|Triangle/examples/GetFaceElemType_Triangle_test_1.F90)
!
! See also: [[GetFaceElemType_Triangle1]]

INTERFACE GetFaceElemType_Triangle
 MODULE PURE SUBROUTINE GetFaceElemType_Triangle2(elemType, localFaceNumber, &
                                                faceElemType, opt, tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! Element type of the triangle. Always pass it.
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! Local face number. Not used.
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Element type of the face.
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! Number of nodes of the face.
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! Not used.
  END SUBROUTINE GetFaceElemType_Triangle2
END INTERFACE GetFaceElemType_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceTriangle_Method
