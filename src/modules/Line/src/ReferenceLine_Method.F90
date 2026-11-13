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
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE.
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementName_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                         FacetTopology_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!   nptrs : INTEGER(I4B), INTENT(IN)(:). Element node identifiers to select
! into facet topology.
!   ans : TYPE(ReferenceTopology_), INTENT(INOUT)(:). Result buffer; allocate
! the full active shape before the call.
!
! Returns:
!   Updates ans(:).
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Line(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B)(4).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                   TotalNodesInElement_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementOrder_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Line(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       ElementType_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   elemName : CHARACTER(*), INTENT(IN). Case sensitive element name string.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementType_Line(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   ans : TYPE(ReferenceElement_), INTENT(INOUT)(:). Result buffer; allocate
! the full active shape before the call.
!
! Returns:
!   Updates ans(:).
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Line1
END INTERFACE FacetElements_Line

!----------------------------------------------------------------------------
!                                                       FacetElements_Line
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   ans : TYPE(ReferenceElement_), INTENT(INOUT)(:). Result buffer; allocate
! the full active shape before the call.
!
! Returns:
!   Updates ans(:).
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE FacetElements_Line
  MODULE SUBROUTINE FacetElements_Line2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Line2
END INTERFACE FacetElements_Line

!----------------------------------------------------------------------------
!                                                                 LineName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query reference element identifiers, orders and topology metadata.
!
! Element name strings are case sensitive. TotalEntities stores node, edge,
! face and cell counts; the node count includes higher order nodes. MaxOrder
! is a build time limit, not the extent of every metadata lookup table.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_{\mathrm{line}}=p+1,\quad N_{\mathrm{triangle}}=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceLine_Method, ONLY: ElementName_Line
! USE ReferenceLine_Method, ONLY: ElementType_Line
! USE ReferenceLine_Method, ONLY: ElementOrder_Line
! USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
! USE ReferenceLine_Method, ONLY: TotalEntities_Line
! USE ReferenceLine_Method, ONLY: MaxOrder_Line
! USE ReferenceLine_Method, ONLY: LineName
! USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Line("Line2")
! CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
! CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
! entities = TotalEntities_Line(kind)
! CALL OK(entities(1) == 2, "Entity counts start with node count")
! CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
! CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
! CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
!   "Compile time default coordinate storage")
! END PROGRAM main
!```

INTERFACE LineName
  MODULE PURE FUNCTION LineName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION LineName1
END INTERFACE LineName

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a linear reference element or obtain reference
! coordinates.
!
! Pass UNIT or BIUNIT explicitly for reproducibility. Constructors retain only
! intrinsic coordinate rows (one for Line and two for Triangle); nsd records
! the embedding dimension. The pointer constructor allocates a new target
! which the caller owns.
!
! Arguments:
!   obj : CLASS(ReferenceLine_), INTENT(INOUT). Reference element result;
! pointer constructors allocate a new target.
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), INTENT(IN), OPTIONAL. Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   Updates obj.
!
! Formula:
! $$\widehat K_{\mathrm{line}}=[0,1],\quad\widehat T=\{(x,y):x,y\ge0,\
! x+y\le1\}$$
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

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Line(obj, nsd, xij, domainName)
    CLASS(ReferenceLine_), INTENT(INOUT) :: obj
    !! The instance
    INTEGER(I4B), INTENT(IN) :: nsd
    !! Spatial dimension of the problem
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    !! Coords of element
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END SUBROUTINE Initiate_Ref_Line
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                     ReferenceLine@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a linear reference element or obtain reference
! coordinates.
!
! Pass UNIT or BIUNIT explicitly for reproducibility. Constructors retain only
! intrinsic coordinate rows (one for Line and two for Triangle); nsd records
! the embedding dimension. The pointer constructor allocates a new target
! which the caller owns.
!
! Arguments:
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), INTENT(IN), OPTIONAL. Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : TYPE(ReferenceLine_).
!
! Formula:
! $$\widehat K_{\mathrm{line}}=[0,1],\quad\widehat T=\{(x,y):x,y\ge0,\
! x+y\le1\}$$
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

INTERFACE ReferenceLine
  MODULE PURE FUNCTION Reference_Line(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceLine_) :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION Reference_Line
END INTERFACE ReferenceLine

!----------------------------------------------------------------------------
!                                              ReferenceLine_Pointer@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a linear reference element or obtain reference
! coordinates.
!
! Pass UNIT or BIUNIT explicitly for reproducibility. Constructors retain only
! intrinsic coordinate rows (one for Line and two for Triangle); nsd records
! the embedding dimension. The pointer constructor allocates a new target
! which the caller owns.
!
! Arguments:
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), INTENT(IN), OPTIONAL. Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : CLASS(ReferenceLine_), POINTER.
!
! Formula:
! $$\widehat K_{\mathrm{line}}=[0,1],\quad\widehat T=\{(x,y):x,y\ge0,\
! x+y\le1\}$$
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

INTERFACE ReferenceLine_Pointer
  MODULE FUNCTION Reference_Line_Pointer_1(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceLine_), POINTER :: obj
    CHARACTER(*), INTENT(IN), OPTIONAL :: domainName
    !! Domain name
    !! UNIT
    !! BIUNIT
    !! GENERAL
  END FUNCTION Reference_Line_Pointer_1
END INTERFACE ReferenceLine_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct higher order lines and query line measure.
!
! Line_Quality and Quality_Line are aliases of a placeholder which always
! returns zero; they do not compute a quality metric. Use a fresh output
! object for HighOrderElement_Line.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!   obj : CLASS(ReferenceElement_), INTENT(INOUT). Reference element result;
! pointer constructors allocate a new target.
!   ipType : INTEGER(I4B), INTENT(IN). Interpolation node family selector.
!
! Returns:
!   Updates obj.
!
! Formula:
! $$L=\|x_2-x_1\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE BaseType, ONLY: ReferenceLine_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: HighOrderElement_Line
! USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
! USE ReferenceLine_Method, ONLY: Line_Quality
! USE ReferenceLine_Method, ONLY: Quality_Line
! USE ReferenceLine_Method, ONLY: ReferenceLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceLine_) :: ref
! TYPE(ReferenceElement_) :: high
! REAL(DFP) :: x(1, 2), q
!
! ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")
! x(1, :) = [-1.0_DFP, 1.0_DFP]
! CALL HighOrderElement_Line(refelem=ref, order=2_I4B, &
!   obj=high, ipType=Equidistance)
! CALL OK(SIZE(high%xij, 2) == 3, "Quadratic line has three nodes")
! CALL OK(ABS(Measure_Simplex_Line(ref, x)-2.0_DFP) < 1.0E-12_DFP, &
!   "Segment length")
! q = Line_Quality(ref, x, measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Line(ref, x, 1_I4B) == q, &
!   "Quality routine currently returns its placeholder value")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Line(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    !! Linear line element
    INTEGER(I4B), INTENT(IN) :: order
    !! order or generated element
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    !! High order lagrange line element
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighOrderElement_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                    MeasureSimplex@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct higher order lines and query line measure.
!
! Line_Quality and Quality_Line are aliases of a placeholder which always
! returns zero; they do not compute a quality metric. Use a fresh output
! object for HighOrderElement_Line.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   Ans : REAL(DFP).
!
! Formula:
! $$L=\|x_2-x_1\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE BaseType, ONLY: ReferenceLine_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: HighOrderElement_Line
! USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
! USE ReferenceLine_Method, ONLY: Line_Quality
! USE ReferenceLine_Method, ONLY: Quality_Line
! USE ReferenceLine_Method, ONLY: ReferenceLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceLine_) :: ref
! TYPE(ReferenceElement_) :: high
! REAL(DFP) :: x(1, 2), q
!
! ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")
! x(1, :) = [-1.0_DFP, 1.0_DFP]
! CALL HighOrderElement_Line(refelem=ref, order=2_I4B, &
!   obj=high, ipType=Equidistance)
! CALL OK(SIZE(high%xij, 2) == 3, "Quadratic line has three nodes")
! CALL OK(ABS(Measure_Simplex_Line(ref, x)-2.0_DFP) < 1.0E-12_DFP, &
!   "Segment length")
! q = Line_Quality(ref, x, measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Line(ref, x, 1_I4B) == q, &
!   "Quality routine currently returns its placeholder value")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Line(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                                      line_quality@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct higher order lines and query line measure.
!
! Line_Quality and Quality_Line are aliases of a placeholder which always
! returns zero; they do not compute a quality metric. Use a fresh output
! object for HighOrderElement_Line.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   measure : INTEGER(I4B), INTENT(IN). Quality measure selector; Line quality
! currently returns a placeholder zero.
!
! Returns:
!   Ans : REAL(DFP).
!
! Formula:
! $$L=\|x_2-x_1\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE BaseType, ONLY: ReferenceLine_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: HighOrderElement_Line
! USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
! USE ReferenceLine_Method, ONLY: Line_Quality
! USE ReferenceLine_Method, ONLY: Quality_Line
! USE ReferenceLine_Method, ONLY: ReferenceLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceLine_) :: ref
! TYPE(ReferenceElement_) :: high
! REAL(DFP) :: x(1, 2), q
!
! ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")
! x(1, :) = [-1.0_DFP, 1.0_DFP]
! CALL HighOrderElement_Line(refelem=ref, order=2_I4B, &
!   obj=high, ipType=Equidistance)
! CALL OK(SIZE(high%xij, 2) == 3, "Quadratic line has three nodes")
! CALL OK(ABS(Measure_Simplex_Line(ref, x)-2.0_DFP) < 1.0E-12_DFP, &
!   "Segment length")
! q = Line_Quality(ref, x, measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Line(ref, x, 1_I4B) == q, &
!   "Quality routine currently returns its placeholder value")
! END PROGRAM main
!```

INTERFACE Quality_Line
  MODULE FUNCTION Line_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Line_Quality
END INTERFACE Quality_Line

!----------------------------------------------------------------------------
!                                                           RefLineCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a linear reference element or obtain reference
! coordinates.
!
! Pass UNIT or BIUNIT explicitly for reproducibility. Constructors retain only
! intrinsic coordinate rows (one for Line and two for Triangle); nsd records
! the embedding dimension. The pointer constructor allocates a new target
! which the caller owns.
!
! Arguments:
!   refLine : CHARACTER(*), INTENT(IN). Reference interval name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(1, 2).
!
! Formula:
! $$\widehat K_{\mathrm{line}}=[0,1],\quad\widehat T=\{(x,y):x,y\ge0,\
! x+y\le1\}$$
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

INTERFACE RefCoord_Line
  MODULE PURE FUNCTION RefLineCoord(refLine) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refLine
    !! "unit"
    !! "biunit"
    REAL(DFP) :: ans(1, 2)
  END FUNCTION RefLineCoord
END INTERFACE RefCoord_Line

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   con : INTEGER(I4B), INTENT(INOUT)(:, :). Caller supplied connectivity
! storage, with entities in columns.
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!   order : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial degree, except
! quadrature where it is requested exactness degree.
!   nrow : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output
! columns.
!
! Returns:
!   Updates con(:, :), nrow, ncol.
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the Face number
    !! The row represents a Face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! This option is ignored now
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element
    !! Currently any order is valid
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetEdgeElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   con : INTEGER(I4B), INTENT(INOUT)(:, :). Caller supplied connectivity
! storage, with entities in columns.
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!   order : INTEGER(I4B), OPTIONAL, INTENT(IN). Polynomial degree, except
! quadrature where it is requested exactness degree.
!   nrow : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output rows.
!   ncol : INTEGER(I4B), OPTIONAL, INTENT(OUT). Number of active output
! columns.
!
! Returns:
!   Updates con(:, :), nrow, ncol.
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Line(con, opt, order, nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! [1,2], [1,3], [2,3]. This is DEFAULT
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! [1,2], [2,3], [3,1]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element
    !! Currently order is used only when opt=2
    !! Currently any order is valid
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Line
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   elemType : INTEGER(I4B), OPTIONAL, INTENT(IN). Element identifier from
! GlobalData or the element type lookup.
!   faceElemType : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Output facet
! element identifier(s).
!   tFaceNodes : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Output facet node
! count(s).
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!
! Returns:
!   Updates faceElemType(:), tFaceNodes(:).
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line1(elemType, faceElemType, opt, &
                                               tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Element names of faces
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Line1
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct facet connectivity and reference topology.
!
! Use caller allocated output arrays. Line facets are points. Triangle face
! connectivity always uses [1,2], [2,3], [3,1]; GetFaceConnectivity_Triangle
! ignores opt and forces option 2. Edge connectivity option 1 uses [1,2],
! [1,3], [2,3]. The example uses linear elements to avoid higher order
! indexing assumptions.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!   localFaceNumber : INTEGER(I4B), INTENT(IN). One based local facet index.
!   faceElemType : INTEGER(I4B), INTENT(INOUT). Output facet element
! identifier(s).
!   tFaceNodes : INTEGER(I4B), INTENT(INOUT). Output facet node count(s).
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!
! Returns:
!   Updates faceElemType, tFaceNodes.
!
! Formula:
! $$\partial[a,b]=\{a,b\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Line2
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
! USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
! USE ReferenceLine_Method, ONLY: FacetTopology_Line
! USE ReferenceLine_Method, ONLY: FacetElements_Line
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Line2
! INTEGER(I4B) :: edge(1, 2), face(1, 2)
! INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
! TYPE(ReferenceTopology_) :: topology(2)
! TYPE(ReferenceElement_) :: facets(2)
!
! CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
! CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
! CALL GetFaceElemType_Line(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
! CALL FacetTopology_Line(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
! CALL OK(ALL(facets%order == 0), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Line
  MODULE PURE SUBROUTINE GetFaceElemType_Line2(elemType, localFaceNumber, &
                                               faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(INOUT) :: faceElemType
    !! Element names of faces
    INTEGER(I4B), INTENT(INOUT) :: tFaceNodes
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Line2
END INTERFACE GetFaceElemType_Line

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceLine_Method
