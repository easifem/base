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
! summary: ReferenceLine geometry and interpolation utilities.
!
! ReferenceLine_ stores one intrinsic coordinate row even when nsd is larger.
! Constructors default to BIUNIT; specify domainName explicitly. Line_Quality
! is a placeholder returning zero. Metadata identifiers distinguish element
! order from node count.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$N=p+1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceLine_
! USE ReferenceLine_Method, ONLY: Initiate
! USE ReferenceLine_Method, ONLY: ReferenceLine
! USE ReferenceLine_Method, ONLY: ReferenceLine_Pointer
! USE ReferenceLine_Method, ONLY: RefLineCoord
! USE ReferenceLine_Method, ONLY: RefCoord_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceLine_) :: a, b
! CLASS(ReferenceLine_), POINTER :: p
! REAL(DFP) :: x(1, 2)
!
! CALL Initiate(obj=a, nsd=1_I4B, domainName="UNIT")
! b = ReferenceLine(nsd=1_I4B, domainName="UNIT")
! p => ReferenceLine_Pointer(nsd=1_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefLineCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Line("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

MODULE ReferenceLine_Method
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceLine_

USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: Initiate
PUBLIC :: ReferenceLine
PUBLIC :: ReferenceLine_Pointer
PUBLIC :: HighOrderElement_Line
PUBLIC :: Measure_Simplex_Line
PUBLIC :: Line_Quality
PUBLIC :: Quality_Line
PUBLIC :: LineName
PUBLIC :: RefLineCoord
PUBLIC :: RefCoord_Line
PUBLIC :: DEFAULT_Ref_LINE_COORD
PUBLIC :: FacetElements_Line
PUBLIC :: ElementType_Line
PUBLIC :: ElementOrder_Line
PUBLIC :: TotalNodesInElement_Line
PUBLIC :: TotalEntities_Line
PUBLIC :: FacetTopology_Line
PUBLIC :: ElementName_Line
PUBLIC :: MaxOrder_Line
PUBLIC :: GetFaceElemType_Line
PUBLIC :: GetEdgeConnectivity_Line
PUBLIC :: GetFaceConnectivity_Line

#ifdef MAX_LINE_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Line = MAX_LINE_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Line = 5_I4B
#endif

#ifdef REF_LINE_IS_UNIT
REAL(DFP), PARAMETER :: DEFAULT_Ref_LINE_COORD(3, 2) = &
                        RESHAPE([0, 0, 0, 1, 0, 0], [3, 2])
#else
REAL(DFP), PARAMETER :: DEFAULT_Ref_LINE_COORD(3, 2) = &
                        RESHAPE([-1, 0, 0, 1, 0, 0], [3, 2])
#endif

!----------------------------------------------------------------------------
!                                                              ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the name of a line element type.
!
! Returns the name of a line element type as a string.
!
! The names are `Point1` and `Line2` to `Line11`. For any other
! `elemType`, the function returns `"NONE"`.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE - element name, for example `"Line3"`
!
! Examples
!
! - [test 1](|base|Line/examples/ElementName_Line_test_1.F90)
!
! See also: [[ElementType_Line]]

INTERFACE
  MODULE PURE FUNCTION ElementName_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, for example `Line2` from `GlobalData`
    CHARACTER(:), ALLOCATABLE :: ans
    !! element name
  END FUNCTION ElementName_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         FacetTopology_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the topology of the two facets of a line.
!
! The two facets of a line are its end points.
! This routine sets `ans(1)` to the first node of `nptrs` and `ans(2)` to
! the second node.
! Both facets have type `Point1` and `xiDimension` 0.
!
! @note
! The routine ignores `elemType`. The result is the same for every line
! type. It uses only the first two entries of `nptrs`.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/FacetTopology_Line_test_1.F90)
!
! See also: [[FacetElements_Line]]

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Line(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, not used
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    !! node numbers of the element, `SIZE(nptrs) >= 2`
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
    !! topology of the facets, `SIZE(ans) >= 2`; the caller allocates it
  END SUBROUTINE FacetTopology_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes, edges, faces and cells of a line.
!
! Returns four entity counts of a line element type.
! The first entry is the number of nodes, including higher order nodes.
! The second entry is always 1, the line itself.
! The third and fourth entries are always 0.
!
! @note
! The second entry is 1 for every `elemType`, also for an unknown one.
! The first entry is 0 for an unknown `elemType`.
! @endnote
!
! Returns:
!   ans : INTEGER(I4B)(4) - counts of nodes, edges, faces and cells
!
! Examples
!
! - [test 1](|base|Line/examples/TotalEntities_Line_test_1.F90)
!
! See also: [[TotalNodesInElement_Line]]

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, for example `Line2` from `GlobalData`
    INTEGER(I4B) :: ans(4)
    !! entity counts
  END FUNCTION TotalEntities_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   TotalNodesInElement_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the number of nodes of a line element type.
!
! Returns the number of nodes of `Point1` (1 node) or of `Line2` to
! `Line11` (2 to 11 nodes). For any other `elemType`, it returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes
!
! Examples
!
! - [test 1](|base|Line/examples/TotalNodesInElement_Line_test_1.F90)
!
! See also: [[TotalEntities_Line]]

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, for example `Line2` from `GlobalData`
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION TotalNodesInElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementOrder_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the polynomial order of a line element type.
!
! Returns the order of `Line2` to `Line11`, which is 1 to 10.
! For any other `elemType`, it returns 0.
! This includes `Point1`.
!
! Returns:
!   ans : INTEGER(I4B) - polynomial order
!
! Examples
!
! - [test 1](|base|Line/examples/ElementOrder_Line_test_1.F90)
!
! See also: [[LineName1]]

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, for example `Line2` from `GlobalData`
    INTEGER(I4B) :: ans
    !! polynomial order
  END FUNCTION ElementOrder_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementType_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element type number of a line element name.
!
! Converts an element name to its type number.
! The name is case sensitive.
! The names `Line1`, `Point` and `Point1` give `Point1`.
! The names `Line2` and `Line` give `Line2`.
! The names `Line3` to `Line11` give the type with the same name.
! For any other name, the function returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type number
!
! Examples
!
! - [test 1](|base|Line/examples/ElementType_Line_test_1.F90)
!
! See also: [[ElementName_Line]]

INTERFACE
  MODULE PURE FUNCTION ElementType_Line(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    !! element name, for example `"Line3"`
    INTEGER(I4B) :: ans
    !! element type number
  END FUNCTION ElementType_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the two facet elements of a reference line.
!
! The facets of a line are its two end points.
! This routine fills `ans(1)` and `ans(2)` with `Point1` elements of
! order 0.
!
! Each facet gets these values:
!
! 1. the coordinates of the end point from the default reference line,
! 2. `nsd` and `interpolationPointType` copied from `refelem`,
! 3. the node number of the matching vertex from `refelem%topology`.
!
! @note
! The facet coordinates come from the default reference line. They do not
! follow `refelem%xij`.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/FacetElements_Line_test_1.F90)
!
! See also: [[FacetElements_Line2]]

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference line; `refelem%topology(1:2)` holds the vertices
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! facet elements, `SIZE(ans) >= 2`; the caller allocates it
  END SUBROUTINE FacetElements_Line1
END INTERFACE FacetElements_Line

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds the two facet elements of a line from `nsd`.
!
! The facets of a line are its two end points.
! This routine fills `ans(1)` and `ans(2)` with `Point1` elements of
! order 0.
!
! Each facet gets these values:
!
! 1. the coordinates of the end point from the default reference line,
! 2. `nsd` as spatial dimension,
! 3. `Equidistance` as interpolation point type,
! 4. the vertex number 1 for `ans(1)` and 2 for `ans(2)`.
!
! @note
! `elemType` is not used. The result is the same for every line type.
! @endnote
!
! Examples
!
! - [test 2](|base|Line/examples/FacetElements_Line_test_2.F90)
!
! See also: [[FacetElements_Line1]]

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type number, not used
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the problem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
    !! facet elements, `SIZE(ans) >= 2`; the caller allocates it
  END SUBROUTINE FacetElements_Line2
END INTERFACE FacetElements_Line

!----------------------------------------------------------------------------
!                                                                 LineName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the line element type number for a polynomial order.
!
! Returns the type of the line element with the given order.
! Order 1 gives `Line2`, order 2 gives `Line3`, and so on up to order 10,
! which gives `Line11`.
! For any other `order`, the function returns 0.
!
! Returns:
!   ans : INTEGER(I4B) - element type number
!
! Examples
!
! - [test 1](|base|Line/examples/LineName_test_1.F90)
!
! See also: [[ElementOrder_Line]]

INTERFACE LineName
  MODULE PURE FUNCTION LineName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order, from 1 to 10
    INTEGER(I4B) :: ans
    !! element type number
  END FUNCTION LineName1
END INTERFACE LineName

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Initiates a linear reference line element.
!
! Sets `obj` to a two-node line of order 1 with the given `nsd`.
! It first deallocates the old content of `obj`.
!
! The coordinates and the domain name follow these rules:
!
! 1. If `xij` is present, `obj%xij` is `xij(1:1, 1:2)`.
!    The domain name is `"UNIT"` for the coordinates `[0, 1]` and
!    `"BIUNIT"` for `[-1, 1]`. Otherwise it is `"GENERAL"`.
!    In this case the routine ignores `domainName`.
! 2. Else, if `domainName` is present, the routine converts it to upper
!    case. For `"UNIT"` and `"BIUNIT"`, it sets the coordinates to
!    `[0, 1]` and `[-1, 1]`.
! 3. Else, the domain name is `"BIUNIT"` and the coordinates are
!    `[-1, 1]`.
!
! @note
! With any other `domainName` and no `xij`, the routine keeps the name
! but does not set `obj%xij`.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/Initiate_test_1.F90)

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Line(obj, nsd, xij, domainName)
    CLASS(ReferenceLine_), INTENT(INOUT) :: obj
    !! The instance
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension of the problem
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Coordinates of the two end points, one per column.
    !! The routine reads only the first row and the first two columns.
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name, not case sensitive: `"UNIT"` or `"BIUNIT"`.
    !! Ignored when `xij` is present. Default is `"BIUNIT"`.
  END SUBROUTINE Initiate_Ref_Line
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                     ReferenceLine@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a linear reference line element.
!
! Returns a reference line with two nodes.
! The element has order 1 and type `Line2`.
!
! The routine sets the reference coordinates in one of two ways.
!
! 1. If `xij` is present, it stores the first row and the first two
!    columns of `xij`. The domain name becomes `"UNIT"` or `"BIUNIT"`
!    when the coordinates match, and `"GENERAL"` otherwise.
! 2. Otherwise, `domainName` selects the coordinates. `"UNIT"` gives
!    `[0, 1]` and `"BIUNIT"` gives `[-1, 1]`. The default is `"BIUNIT"`.
!
! @note
! With `domainName="GENERAL"` and no `xij`, the routine leaves the
! coordinates unset.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/ReferenceLine_test_1.F90)
!
! See also: [[ReferenceLine_Pointer]], [[RefCoord_Line]]

INTERFACE ReferenceLine
  MODULE PURE FUNCTION Reference_Line(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the element
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! end point coordinates, the routine reads only `xij(1, 1:2)`
    TYPE(ReferenceLine_) :: obj
    !! the reference line
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! domain name, `"UNIT"` or `"BIUNIT"`, not case sensitive
    !! ignored when `xij` is present, default `"BIUNIT"`
  END FUNCTION Reference_Line
END INTERFACE ReferenceLine

!----------------------------------------------------------------------------
!                                              ReferenceLine_Pointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns a pointer to a new linear reference line element.
!
! Allocates a reference line with two nodes and returns a pointer to it.
! The caller owns the target and must deallocate it.
!
! The element has order 1 and type `Line2`.
! The coordinates follow the same rules as [[ReferenceLine]]:
!
! 1. If `xij` is present, it stores the first row and the first two
!    columns of `xij`. The domain name becomes `"UNIT"` or `"BIUNIT"`
!    when the coordinates match, and `"GENERAL"` otherwise.
! 2. Otherwise, `domainName` selects the coordinates. `"UNIT"` gives
!    `[0, 1]` and `"BIUNIT"` gives `[-1, 1]`. The default is `"BIUNIT"`.
!
! Examples
!
! - [test 1](|base|Line/examples/ReferenceLine_Pointer_test_1.F90)
!
! See also: [[ReferenceLine]]

INTERFACE ReferenceLine_Pointer
  MODULE FUNCTION Reference_Line_Pointer_1(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    !! spatial dimension of the element
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! end point coordinates, the routine reads only `xij(1, 1:2)`
    CLASS(ReferenceLine_), POINTER :: obj
    !! pointer to the new reference line
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! domain name, `"UNIT"` or `"BIUNIT"`, not case sensitive
    !! ignored when `xij` is present, default `"BIUNIT"`
  END FUNCTION Reference_Line_Pointer_1
END INTERFACE ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Builds a high order Lagrange line element from a linear line.
!
! Fills `obj` with a line element of polynomial order `order`.
! The nodes are interpolation points on the end points of `refelem`.
! The element has `order + 1` nodes and one edge.
! It inherits the domain name and the spatial dimension from `refelem`.
!
! @note
! `obj` must be a fresh element. The routine allocates its topology,
! so a second call on the same `obj` fails.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/HighOrderElement_Line_test_1.F90)
!
! See also: [[ReferenceLine]]

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Line(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! linear line element, its `xij` holds the two end points
    INTEGER(I4B), INTENT(IN) :: order
    !! polynomial order of the generated element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! high order Lagrange line element
    INTEGER(I4B), INTENT(IN) :: ipType
    !! interpolation point type, for example `Equidistance`
  END SUBROUTINE HighOrderElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                    MeasureSimplex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the length of a line segment.
!
! Returns the distance between the first two columns of `xij`.
! The number of rows of `xij` sets the dimension, 1, 2 or 3.
! With more than 3 rows, it uses only the first 3.
!
! Formula:
!   $$ L = \| x_2 - x_1 \| $$
!
! Examples
!
! - [test 1](|base|Line/examples/Measure_Simplex_Line_test_1.F90)

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Line(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference line element, not used
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! end point coordinates, one point per column
    !! the size is (nsd, 2)
    REAL(DFP) :: Ans
    !! length of the segment
  END FUNCTION Measure_Simplex_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      line_quality@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Placeholder for the quality of a line element.
!
! Always returns zero. It does not compute a quality measure yet.
! It ignores all arguments.
!
! @note
! Do not use the result to judge a line element.
! @endnote
!
! Examples
!
! - [test 1](|base|Line/examples/Line_Quality_test_1.F90)
!
! See also: [[Measure_Simplex_Line]]

INTERFACE Quality_Line
  MODULE FUNCTION Line_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! reference line element, not used
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! end point coordinates, not used
    INTEGER(I4B), INTENT(IN) :: measure
    !! quality measure, not used
    REAL(DFP) :: Ans
    !! always zero
  END FUNCTION Line_Quality
END INTERFACE Quality_Line

!----------------------------------------------------------------------------
!                                                           RefLineCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the end point coordinates of a reference line.
!
! Returns a 1-by-2 array with the end points of the reference line.
! `"UNIT"` gives `[0, 1]`. `"BIUNIT"` gives `[-1, 1]`.
!
! The routine reads only the first letter of `refLine`, in upper or
! lower case.
!
! @warning
! For any other first letter the routine leaves `ans` undefined.
! @endwarning
!
! Formula:
!   $$ \text{UNIT}: [0, 1], \quad \text{BIUNIT}: [-1, 1] $$
!
! Examples
!
! - [test 1](|base|Line/examples/RefLineCoord_test_1.F90)
!
! See also: [[ReferenceLine]]

INTERFACE RefCoord_Line
  MODULE PURE FUNCTION RefLineCoord(refLine) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refLine
    !! name of the reference line, `"UNIT"` or `"BIUNIT"`
    !! lower case names are also accepted
    REAL(DFP) :: ans(1, 2)
    !! end point coordinates, `ans(1, 1)` is the first end point
  END FUNCTION RefLineCoord
END INTERFACE RefCoord_Line

!----------------------------------------------------------------------------
!                                                    GetFaceConnectivity_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the face connectivity of a line element.
!
! The faces of a line are its two end points.
! The routine writes `[1, 2]` into the first row of `con`.
! Column `j` holds the local node number of face `j`.
! The result is the same for every element order.
!
! The caller must allocate `con` with at least 1 row and 2 columns.
!
! Examples
!
! - [test 1](|base|Line/examples/GetFaceConnectivity_Line_test_1.F90)
!
! See also: [[GetEdgeConnectivity_Line]], [[GetFaceElemType_Line]]

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! face connectivity, allocated by the caller
    !! the routine writes the first row, column `j` is face `j`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of the element, not used
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`, always 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 2
  END SUBROUTINE GetFaceConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetEdgeConnectivity_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the edge connectivity of a line element.
!
! A line has one edge, which joins its two end nodes.
! The routine writes `[1, 2]` into the first row of `con`.
! The result is the same for every element order.
!
! The caller must allocate `con` with at least 1 row and 2 columns.
!
! Examples
!
! - [test 1](|base|Line/examples/GetEdgeConnectivity_Line_test_1.F90)
!
! See also: [[GetFaceConnectivity_Line]]

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! edge connectivity, allocated by the caller
    !! the routine writes `[1, 2]` in the first row
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of the element, not used
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! number of rows written in `con`, always 1
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! number of columns written in `con`, always 2
  END SUBROUTINE GetEdgeConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element types of all faces of a line.
!
! The two faces of a line are points.
! The routine sets `faceElemType(1:2)` to `Point1`.
! It sets `tFaceNodes(1:2)` to 1.
!
! The result does not depend on `elemType` or `opt`.
! The caller must give arrays with at least 2 entries.
!
! Examples
!
! - [test 1](|base|Line/examples/GetFaceElemType_Line_test_1.F90)
!
! See also: [[GetFaceElemType_Line]]

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line1(elemType, faceElemType, opt, &
                                               tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type of the line, not used
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! element types of the faces, size at least 2
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! number of nodes in each face, size at least 2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Line1
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Returns the element type of one face of a line.
!
! Every face of a line is a point.
! The routine sets `faceElemType` to `Point1`.
! It sets `tFaceNodes` to 1.
!
! The result does not depend on `elemType`, `localFaceNumber` or `opt`.
!
! Examples
!
! - [test 1](|base|Line/examples/GetFaceElemType_Line_test_1.F90)
!
! See also: [[GetFaceElemType_Line]]

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line2(elemType, localFaceNumber, &
                                               faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type of the line, not used
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number, not used
    INTEGER(I4B), INTENT(INOUT) :: faceElemType
    !! element type of the face
    INTEGER(I4B), INTENT(INOUT) :: tFaceNodes
    !! number of nodes in the face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! not used
  END SUBROUTINE GetFaceElemType_Line2
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method
