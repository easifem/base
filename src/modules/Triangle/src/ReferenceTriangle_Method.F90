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

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: ReferenceTriangle geometry and interpolation utilities.
!
! ReferenceTriangle_ stores two intrinsic coordinate rows and records
! embedding dimension separately. Constructors default to UNIT. Many geometry
! wrappers use only planar coordinates. HighorderElement_Triangle currently
! accesses a fourth face column outside its three column allocation; use
! interpolation node generation directly until repaired.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$N=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Initiate
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
! USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
! USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTriangle_) :: a, b
! CLASS(ReferenceTriangle_), POINTER :: p
! REAL(DFP) :: x(2, 3)
!
! CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
! b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefTriangleCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

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

!> author: OpenAI Codex (documentation)
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
! USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
! USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Triangle("Triangle3")
! CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
! CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
! entities = TotalEntities_Triangle(kind)
! CALL OK(entities(1) == 3, "Entity counts start with node count")
! CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
! CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementName_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Triangle(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
! USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Triangle("Triangle3")
! CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
! CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
! entities = TotalEntities_Triangle(kind)
! CALL OK(entities(1) == 3, "Entity counts start with node count")
! CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
! CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               TotalNodesInElement_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
! USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Triangle("Triangle3")
! CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
! CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
! entities = TotalEntities_Triangle(kind)
! CALL OK(entities(1) == 3, "Entity counts start with node count")
! CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
! CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
! USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Triangle("Triangle3")
! CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
! CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
! entities = TotalEntities_Triangle(kind)
! CALL OK(entities(1) == 3, "Entity counts start with node count")
! CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
! CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementType_Triangle(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ElementOrder_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
! USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
! USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
! USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
! USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind, entities(4)
!
! kind = ElementType_Triangle("Triangle3")
! CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
! CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
! CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
! entities = TotalEntities_Triangle(kind)
! CALL OK(entities(1) == 3, "Entity counts start with node count")
! CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
! CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Triangle1
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                    FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Triangle2
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                         Initiate@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   obj : CLASS(ReferenceTriangle_), INTENT(INOUT). Reference element result;
! pointer constructors allocate a new target.
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
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
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Initiate
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
! USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
! USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTriangle_) :: a, b
! CLASS(ReferenceTriangle_), POINTER :: p
! REAL(DFP) :: x(2, 3)
!
! CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
! b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefTriangleCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Triangle(obj, nsd, xij, domainName)
    CLASS(ReferenceTriangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE Initiate_Ref_Triangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 ReferenceTriangle@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : TYPE(ReferenceTriangle_).
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
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Initiate
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
! USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
! USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTriangle_) :: a, b
! CLASS(ReferenceTriangle_), POINTER :: p
! REAL(DFP) :: x(2, 3)
!
! CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
! b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefTriangleCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE ReferenceTriangle
  MODULE PURE FUNCTION Reference_Triangle(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceTriangle_) :: obj
  END FUNCTION Reference_Triangle
END INTERFACE ReferenceTriangle

!----------------------------------------------------------------------------
!                                         ReferenceTriangle_Pointer@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : CLASS(ReferenceTriangle_), POINTER.
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
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Initiate
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
! USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
! USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTriangle_) :: a, b
! CLASS(ReferenceTriangle_), POINTER :: p
! REAL(DFP) :: x(2, 3)
!
! CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
! b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefTriangleCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE ReferenceTriangle_Pointer
  MODULE FUNCTION Reference_Triangle_Pointer(nsd, xij, domainName) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceTriangle_), POINTER :: obj
  END FUNCTION Reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Construct higher order triangle metadata (currently unsafe).
!
! Do not call the current constructor: facecon is allocated with three columns
! but the implementation accesses facecon(:,4). The example instead constructs
! nodes through InterpolationPoint_Triangle; it does not validate the unsafe
! constructor.
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
!   No reliable result from the affected backend; see the workaround example.
!
! Formula:
! $$N=(p+1)(p+2)/2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE ReferenceTriangle_Method, ONLY: HighorderElement_Triangle
! USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP), ALLOCATABLE :: nodes(:, :)
!
! ! HighorderElement_Triangle indexes a nonexistent fourth face column.
! ! Construct the interpolation nodes directly until that backend is repaired.
! nodes = InterpolationPoint_Triangle(order=2_I4B, &
!   ipType=Equidistance, layout="VEFC")
! CALL OK(ALL(SHAPE(nodes) == [2, 6]), "Quadratic triangle node workaround")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HighorderElement_Triangle(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighorderElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Measure_Simplex_Triangle through a reference
! element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\det[v_2-v_1,v_3-v_1]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Measure_Simplex_Triangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Measure_Simplex_Triangle(ref, t2)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Simplex area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Triangle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Angles@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Angles through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$\sum_i\theta_i=\pi$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Angles
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Angles(ref, t2)
! CALL OK(ABS(SUM(y)-ACOS(-1.0_DFP)) < 1.0E-11_DFP, "Angle sum")
! END PROGRAM main
!```

INTERFACE Angles
  MODULE PURE FUNCTION Triangle_angles(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION Triangle_angles
END INTERFACE Angles

!----------------------------------------------------------------------------
!                                                             Area@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Area through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$A=\tfrac12\det[v_2-v_1,v_3-v_1]$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Area
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Area(ref, t2)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Signed planar area")
! END PROGRAM main
!```

INTERFACE Area
  MODULE PURE FUNCTION Triangle_area(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION Triangle_area
END INTERFACE Area

!----------------------------------------------------------------------------
!                                                        ArealVector@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle ArealVector through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero. Scaling differs between embedding
! dimensions: the 3D branch returns twice the area vector.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$a_{\mathrm{2D}}=(0,0,A),\quad a_{\mathrm{3D}}=(v_2-v_1)\times(v_3-v_1)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: ArealVector
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = ArealVector(ref, t2)
! CALL OK(MAXVAL(ABS(y-[0.0_DFP, 0.0_DFP, 1.0_DFP])) &
!   < 1.0E-11_DFP, "Planar area vector")
! END PROGRAM main
!```

INTERFACE ArealVector
  MODULE PURE FUNCTION Triangle_arealVector(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION Triangle_arealVector
END INTERFACE ArealVector

!----------------------------------------------------------------------------
!                                                      Barycentric@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Barycentric through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$x=\sum_i\lambda_i v_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Barycentric
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Barycentric(ref, t2, [0.5_DFP, 0.25_DFP])
! CALL OK(MAXVAL(ABS(MATMUL(t2,y)- &
!   [0.5_DFP, 0.25_DFP])) < 1.0E-11_DFP, "Barycentric reconstruction")
! END PROGRAM main
!```

INTERFACE Barycentric
  MODULE PURE FUNCTION Triangle_barycentric(refelem, xij, x) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: ans(3)
  END FUNCTION Triangle_barycentric
END INTERFACE Barycentric

!----------------------------------------------------------------------------
!                                                          Centroid@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Centroid through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$c=(v_1+v_2+v_3)/3$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Centroid
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Centroid(ref, t2)
! CALL OK(MAXVAL(ABS(y-SUM(t3,DIM=2)/3.0_DFP)) &
!   < 1.0E-11_DFP, "Centroid")
! END PROGRAM main
!```

INTERFACE Centroid
  MODULE PURE FUNCTION Triangle_centroid(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION Triangle_centroid
END INTERFACE Centroid

!----------------------------------------------------------------------------
!                                                      CircumCenter@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle CircumCenter through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$\|c-v_i\|=R$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: CircumCenter
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = CircumCenter(ref,t2)
! CALL OK(MAXVAL(ABS(y-[1.0_DFP, 0.5_DFP, 0.0_DFP])) &
!   < 1.0E-11_DFP, "Circumcenter")
! END PROGRAM main
!```

INTERFACE CircumCenter
  MODULE PURE FUNCTION Triangle_circumcentre(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION Triangle_circumcentre
END INTERFACE CircumCenter

!----------------------------------------------------------------------------
!                                                      CircumCircle@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle CircumCircle through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(4).
!
! Formula:
! $$y=(R,c_x,c_y,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: CircumCircle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(4)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = CircumCircle(ref,t2)
! CALL OK(ABS(y(1)-SQRT(5.0_DFP)/2.0_DFP) < 1.0E-11_DFP, &
!   "Circle result stores radius before center coordinates")
! END PROGRAM main
!```

INTERFACE CircumCircle
  MODULE PURE FUNCTION Triangle_circumcircle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(4)
    !! ans(1) = radius and ans(2:4) center
  END FUNCTION Triangle_circumcircle
END INTERFACE CircumCircle

!----------------------------------------------------------------------------
!                                                      CircumRadius@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle CircumRadius through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$R=abc/(4A)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: CircumRadius
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = CircumRadius(ref,t2)
! CALL OK(ABS(y-SQRT(5.0_DFP)/2.0_DFP) < 1.0E-11_DFP, "Radius")
! END PROGRAM main
!```

INTERFACE CircumRadius
  MODULE PURE FUNCTION Triangle_circumradius(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION Triangle_circumradius
END INTERFACE CircumRadius

!----------------------------------------------------------------------------
!                                                     ContainsLine@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Intersect a parametric or explicit spatial line with a triangle.
!
! Provide three coordinate rows. With parametricLine=.TRUE., x2 is a
! direction; otherwise x1 and x2 are two points.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   x1 : REAL(DFP), INTENT(IN)(3). Use the declaration and description for
! this overload.
!   x2 : REAL(DFP), INTENT(IN)(3). Use the declaration and description for
! this overload.
!   parametricLine : LOGICAL(LGT), INTENT(IN). If true x2 is a direction;
! otherwise x1,x2 are explicit points.
!   inside : LOGICAL(LGT), INTENT(OUT). Whether the point or line intersection
! is contained.
!   xint : REAL(DFP), INTENT(OUT)(3). Use the declaration and description for
! this overload.
!
! Returns:
!   Updates inside, xint(3).
!
! Formula:
! $$x=x_1+s x_2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: ContainsLine
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! LOGICAL(LGT) :: inside
! REAL(DFP) :: point(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! CALL ContainsLine(ref, t3, [0.5_DFP,0.25_DFP,-1.0_DFP], &
!   [0.0_DFP,0.0_DFP,1.0_DFP], .TRUE., inside, point)
! CALL OK(inside .AND. ABS(point(3)) < 1.0E-11_DFP, &
!   "Parametric line crosses triangle")
! END PROGRAM main
!```

INTERFACE ContainsLine
  MODULE PURE SUBROUTINE Triangle_contains_line(refelem, xij, x1, x2, &
    & parametricLine, inside, xint)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x1(3), x2(3)
    LOGICAL(LGT), INTENT(IN) :: parametricLine
    LOGICAL(LGT), INTENT(OUT) :: inside
    REAL(DFP), INTENT(OUT) :: xint(3)
  END SUBROUTINE Triangle_contains_line
END INTERFACE ContainsLine

!----------------------------------------------------------------------------
!                                                    ContainsPoint@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle containment or a selected quality measure.
!
! Containment uses the 2D barycentric test. Select a supported quality
! measure from QualityMeasure; measures are not all normalized to [0,1].
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!
! Returns:
!   ans : LOGICAL(LGT).
!
! Formula:
! $$x\in T\iff\lambda_i(x)\ge0$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Triangle_Contains_Point
! USE ReferenceTriangle_Method, ONLY: Contains_Point_Triangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! LOGICAL(LGT) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Triangle_Contains_Point(ref,t2,[0.5_DFP,0.25_DFP])
! CALL OK(y .AND. Contains_Point_Triangle(ref,t2,[0.5_DFP,0.25_DFP]), &
!   "Containment aliases agree")
! END PROGRAM main
!```

INTERFACE Contains_Point_Triangle
  MODULE PURE FUNCTION Triangle_Contains_Point(refelem, xij, x) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    LOGICAL(LGT) :: ans
  END FUNCTION Triangle_Contains_Point
END INTERFACE Contains_Point_Triangle

!----------------------------------------------------------------------------
!                                                         Diameter@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Diameter through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero. The backend uses squared coordinate
! sums rather than squared norms; this example records the existing result 2
! instead of the geometric sqrt(5).
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$D_{\mathrm{intended}}=\text{smallest enclosing circle diameter}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Diameter
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Diameter(ref,t2)
! CALL OK(ABS(y-2.0_DFP) < 1.0E-11_DFP, &
!   "Current backend diameter behavior; see implementation limitation")
! END PROGRAM main
!```

INTERFACE Diameter
  MODULE PURE FUNCTION triangle_diameter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION triangle_diameter
END INTERFACE Diameter

!----------------------------------------------------------------------------
!                                                       EdgeLength@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle EdgeLength through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$\ell_i=\|v_{i+1}-v_i\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: EdgeLength
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = EdgeLength(ref,t2)
! CALL OK(ABS(SUM(y)-3.0_DFP-SQRT(5.0_DFP)) &
!   < 1.0E-11_DFP, "Perimeter")
! END PROGRAM main
!```

INTERFACE EdgeLength
  MODULE PURE FUNCTION triangle_edge_length(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION triangle_edge_length
END INTERFACE EdgeLength

!----------------------------------------------------------------------------
!                                                         Incenter@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Incenter through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$r=2A/(a+b+c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Incenter
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Incenter(ref,t2)
! CALL OK(ABS(y(1)-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) &
!   < 1.0E-11_DFP, "Incenter")
! END PROGRAM main
!```

INTERFACE Incenter
  MODULE PURE FUNCTION triangle_incenter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION triangle_incenter
END INTERFACE Incenter

!----------------------------------------------------------------------------
!                                                         Incircle@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Incircle through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(4).
!
! Formula:
! $$y=(r,c_x,c_y,0)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Incircle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(4)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Incircle(ref,t2)
! CALL OK(ABS(y(1)-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) &
!   < 1.0E-11_DFP, "Incircle radius is the first entry")
! END PROGRAM main
!```

INTERFACE Incircle
  MODULE PURE FUNCTION triangle_incircle(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(4)
  END FUNCTION triangle_incircle
END INTERFACE Incircle

!----------------------------------------------------------------------------
!                                                         Inradius@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Inradius through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$r=2A/(a+b+c)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Inradius
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Inradius(ref,t2)
! CALL OK(ABS(y-(3.0_DFP-SQRT(5.0_DFP))/2.0_DFP) &
!   < 1.0E-11_DFP, "Inradius")
! END PROGRAM main
!```

INTERFACE Inradius
  MODULE PURE FUNCTION triangle_inradius(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans
  END FUNCTION triangle_inradius
END INTERFACE Inradius

!----------------------------------------------------------------------------
!                                                      Orthocenter@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle Orthocenter through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP)(3).
!
! Formula:
! $$h=3c_{\mathrm{centroid}}-2c_{\mathrm{circumcenter}}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Orthocenter
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Orthocenter(ref,t2)
! CALL OK(MAXVAL(ABS(y)) < 1.0E-11_DFP, "Right angle vertex")
! END PROGRAM main
!```

INTERFACE Orthocenter
  MODULE PURE FUNCTION triangle_orthocenter(refelem, xij) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: ans(3)
  END FUNCTION triangle_orthocenter
END INTERFACE Orthocenter

!----------------------------------------------------------------------------
!                                                DistanceFromPoint@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle DistanceFromPoint through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!
! Returns:
!   ans : REAL(DFP).
!
! Formula:
! $$d=\min_{q\in\partial T}\|x-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: DistanceFromPoint
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = DistanceFromPoint(ref,t2,[-1.0_DFP,0.0_DFP])
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Boundary distance")
! END PROGRAM main
!```

INTERFACE DistanceFromPoint
  MODULE PURE FUNCTION triangle_point_dist(refelem, xij, x) &
    & RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP) :: ans
  END FUNCTION triangle_point_dist
END INTERFACE DistanceFromPoint

!----------------------------------------------------------------------------
!                                                      NearestPoint@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle NearestPoint through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   x : REAL(DFP), INTENT(IN)(:). Evaluation point(s); columns for rank two
! arrays.
!   xn : REAL(DFP), INTENT(INOUT)(:). Use the declaration and description for
! this overload.
!   dist : REAL(DFP), INTENT(OUT). Distance from the query point to the
! segment or triangle boundary.
!
! Returns:
!   Updates xn(:), dist.
!
! Formula:
! $$q=\mathop{\mathrm{argmin}}_{q\in\partial T}\|x-q\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: NearestPoint
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y, nearest(2)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! CALL NearestPoint(ref,t2,[-1.0_DFP,0.0_DFP],nearest,y)
! CALL OK(MAXVAL(ABS(nearest)) < 1.0E-11_DFP, "Nearest boundary vertex")
! END PROGRAM main
!```

INTERFACE NearestPoint
  MODULE PURE SUBROUTINE triangle_get_nearest_point(refelem, xij, x, xn,  &
    & dist)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP), INTENT(INOUT) :: xn(:)
    REAL(DFP), INTENT(OUT) :: dist
  END SUBROUTINE triangle_get_nearest_point
END INTERFACE NearestPoint

!----------------------------------------------------------------------------
!                                                       RandomPoint@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle RandomPoint through a reference element.
!
! The reference object selects embedding dimension. Coordinates are point
! columns. Several geometry wrappers project onto the first two coordinate
! rows; their output z component is zero.
!
! Arguments:
!   refelem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   xij : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   n : INTEGER(I4B), INTENT(IN). Number of samples, coordinate dimension or
! grid count as specified by the routine.
!   seed : INTEGER(I4B), OPTIONAL, INTENT(IN). Random generator seed; this
! interface does not return an advanced seed.
!
! Returns:
!   ans : REAL(DFP)(3, n).
!
! Formula:
! $$x=\sum_i\lambda_i v_i,\quad\lambda_i\ge0,\quad\sum_i\lambda_i=1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: RandomPoint
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y(3,4)
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = RandomPoint(ref,t2,n=4_I4B,seed=12345_I4B)
! CALL OK(MINVAL(y(1:2,:)) >= 0.0_DFP, "Nonnegative coordinates")
! CALL OK(MAXVAL(y(1,:)/2.0_DFP+y(2,:)) <= 1.0_DFP, "Interior samples")
! END PROGRAM main
!```

INTERFACE RandomPoint
  MODULE PURE FUNCTION triangle_random_point(refelem, xij, n, seed) &
    & RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: ans(3, n)
  END FUNCTION triangle_random_point
END INTERFACE RandomPoint

!----------------------------------------------------------------------------
!                                                          Quality@Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Evaluate triangle containment or a selected quality measure.
!
! Containment uses the 2D barycentric test. Select a supported quality
! measure from QualityMeasure; measures are not all normalized to [0,1].
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
!   ans : REAL(DFP).
!
! Formula:
! $$Q_{\mathrm{area}}=A$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTriangle_
! USE BaseType, ONLY: QualityMeasure
! USE ReferenceTriangle_Method, ONLY: Triangle_Quality
! USE ReferenceTriangle_Method, ONLY: Quality_Triangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! TYPE(ReferenceTriangle_) :: ref
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! y = Triangle_Quality(ref,t2,QualityMeasure%area)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP .AND. &
!   ABS(Quality_Triangle(ref,t2,QualityMeasure%area)-y) < 1.0E-11_DFP, &
!   "Area quality aliases agree")
! END PROGRAM main
!```

INTERFACE Quality_Triangle
  MODULE PURE FUNCTION Triangle_Quality(refelem, xij, measure) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: ans
  END FUNCTION Triangle_Quality
END INTERFACE Quality_Triangle

!----------------------------------------------------------------------------
!                                                            TriangleArea3D
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute triangle area from vertex columns.
!
! The 2D routine returns signed area; the 3D routine returns nonnegative area.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(3, 3). Triangle vertex coordinates, one vertex
! per column.
!   ans : REAL(DFP), INTENT(OUT). Result buffer; allocate the full active
! shape before the call.
!
! Returns:
!   Updates ans.
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceTriangle_Method, ONLY: TriangleArea3D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL TriangleArea3D(t3,y)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea3D(t, ans)
    REAL(DFP), INTENT(IN) :: t(3, 3)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TriangleArea3D
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Compute triangle area from vertex columns.
!
! The 2D routine returns signed area; the 3D routine returns nonnegative area.
!
! Arguments:
!   t : REAL(DFP), INTENT(IN)(2, 3). Triangle vertex coordinates, one vertex
! per column.
!   ans : REAL(DFP), INTENT(OUT). Result buffer; allocate the full active
! shape before the call.
!
! Returns:
!   Updates ans.
!
! Formula:
! $$A=\tfrac12\|(v_2-v_1)\times(v_3-v_1)\|$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceTriangle_Method, ONLY: TriangleArea2D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: t2(2, 3), t3(3, 3)
! REAL(DFP) :: y
!
! t2 = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
!   0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 3])
! t3 = 0.0_DFP
! t3(1:2, :) = t2
! CALL TriangleArea2D(t2,y)
! CALL OK(ABS(y-1.0_DFP) < 1.0E-11_DFP, "Triangle area")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea2D(t, ans)
    REAL(DFP), INTENT(IN) :: t(2, 3)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TriangleArea2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Triangle(con, opt, order, &
                                                      nrow, ncol)
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
  END SUBROUTINE GetEdgeConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Triangle(con, opt, order, &
                                                      nrow, ncol)
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
  END SUBROUTINE GetFaceConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                           RefTriangleCoord
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   refTriangle : CHARACTER(*), INTENT(IN). Reference triangle name, UNIT or
! BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(2, 3).
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
! USE BaseType, ONLY: ReferenceTriangle_
! USE ReferenceTriangle_Method, ONLY: Initiate
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
! USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
! USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
! USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTriangle_) :: a, b
! CLASS(ReferenceTriangle_), POINTER :: p
! REAL(DFP) :: x(2, 3)
!
! CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
! b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
! p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
! x = RefTriangleCoord("UNIT")
! CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
!   "Coordinate aliases agree")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE RefCoord_Triangle
  MODULE PURE FUNCTION RefTriangleCoord(refTriangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTriangle
    REAL(DFP) :: ans(2, 3)
  END FUNCTION RefTriangleCoord
END INTERFACE RefCoord_Triangle

!----------------------------------------------------------------------------
!                                           FaceShapeMetaData_Triangle
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
! date: 2026-10-30
! summary: Canonicalize triangular face vertices and return orientation
! metadata.
!
! faceOrient contains a zero based rotation followed by a sign. localFaces
! receives the cyclic vertex values, not a permutation of local indices.
!
! Arguments:
!   face : INTEGER(I4B), INTENT(INOUT)(:). Triangular face vertex identifiers.
!   sorted_face : INTEGER(I4B), INTENT(INOUT)(:). Canonical sorted face
! identifiers.
!   faceOrient : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Two entry zero
! based rotation and sign orientation code.
!   localFaces : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Cyclic vertex
! identifiers used for orientation.
!
! Returns:
!   Updates face(:), sorted_face(:), faceOrient(:), localFaces(:).
!
! Formula:
! $$o=(r,s),\quad r\in\{0,1,2\},\quad s\in\{-1,1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceTriangle_Method, ONLY: FaceShapeMetaData_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: face(3), sorted(3), orientation(2), local(3)
!
! face = [3_I4B, 1_I4B, 2_I4B]
! CALL FaceShapeMetaData_Triangle(face, sorted, orientation, local)
! CALL OK(ALL(sorted == [1_I4B, 2_I4B, 3_I4B]), "Canonical vertex order")
! CALL OK(ALL(orientation == [1_I4B, 1_I4B]), &
!   "Rotation and orientation code")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE FaceShapeMetaData_Triangle(face, sorted_face, &
                                               faceOrient, localFaces)
    INTEGER(I4B), INTENT(INOUT) :: face(:)
    INTEGER(I4B), INTENT(INOUT) :: sorted_face(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceOrient(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaces(:)
  END SUBROUTINE FaceShapeMetaData_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Triangle
MODULE PURE SUBROUTINE GetFaceElemType_Triangle1(elemType, faceElemType, &
    opt, &
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
  END SUBROUTINE GetFaceElemType_Triangle1
END INTERFACE GetFaceElemType_Triangle

!----------------------------------------------------------------------------
!                                           GetFaceElemType@GeometryMethods
!----------------------------------------------------------------------------

!> author: OpenAI Codex (documentation)
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
!   localFaceNumber : INTEGER(I4B), INTENT(IN). One based local facet index.
!   faceElemType : INTEGER(I4B), INTENT(OUT). Output facet element
! identifier(s).
!   tFaceNodes : INTEGER(I4B), INTENT(OUT). Output facet node count(s).
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!
! Returns:
!   Updates faceElemType, tFaceNodes.
!
! Formula:
! $$\partial T=e_{12}\cup e_{23}\cup e_{31}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Triangle3
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
! USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
! USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Triangle3
! INTEGER(I4B) :: edge(2, 3), face(2, 3)
! INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
! TYPE(ReferenceTopology_) :: topology(3)
! TYPE(ReferenceElement_) :: facets(3)
!
! CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
!   nrow=nr, ncol=nc)
! CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
! CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
! CALL GetFaceElemType_Triangle(elemType=kind, &
!   faceElemType=faceTypes, tFaceNodes=nodeCounts)
! CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
! CALL FacetTopology_Triangle(elemType=kind, &
!   nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
! CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
! CALL OK(ALL(facets%order == 1), "Facet polynomial order")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Triangle
 MODULE PURE SUBROUTINE GetFaceElemType_Triangle2(elemType, localFaceNumber, &
                                                faceElemType, opt, tFaceNodes)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Element names of faces
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Triangle2
END INTERFACE GetFaceElemType_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceTriangle_Method
