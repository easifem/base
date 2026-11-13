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
! summary: ReferenceQuadrangle geometry and interpolation utilities.
!
! Construct reference quadrilaterals, query element metadata, and compute area
! and connectivity. Constructors default to BIUNIT and retain two intrinsic
! coordinate rows. Quality routines currently return a placeholder zero.
! Planar area is signed; the spatial area example uses a planar convex
! quadrilateral.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$|Q_{\mathrm{unit}}|=1,\quad |Q_{\mathrm{biunit}}|=4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Initiate
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: a,b
! CLASS(ReferenceQuadrangle_), POINTER :: p
! REAL(DFP) :: x(2,4)
!
! CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
! b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
! x = RefCoord_Quadrangle("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
!   < 1.0E-12_DFP, "Coordinate aliases agree")
! END PROGRAM main
!```

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
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : CHARACTER(:), ALLOCATABLE.
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementName_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Quadrangle(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B)(4).
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             TotalNodesInElement_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ElementType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   elemName : CHARACTER(*), INTENT(IN). Case sensitive element name string.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementType_Quadrangle(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ElementOrder_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Quadrangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE FacetElements_Quadrangle
  MODULE SUBROUTINE FacetElements_Quadrangle1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Quadrangle1
END INTERFACE FacetElements_Quadrangle

!----------------------------------------------------------------------------
!                                                   FacetElements_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE FacetElements_Quadrangle
  MODULE SUBROUTINE FacetElements_Quadrangle2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Quadrangle2
END INTERFACE FacetElements_Quadrangle

!----------------------------------------------------------------------------
!                                                       QuadrangleName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Query element identifiers, polynomial order and entity counts.
!
! Name lookup strings are case sensitive. Entity counts include all higher
! order nodes, not just geometric vertices. MaxOrder is a build time limit;
! individual metadata tables can have smaller supported ranges.
!
! Arguments:
!   order : INTEGER(I4B), INTENT(IN). Polynomial degree, except quadrature
! where it is requested exactness degree.
!
! Returns:
!   ans : INTEGER(I4B).
!
! Formula:
! $$N_Q=(p+1)^2,\quad N_T=(p+1)(p+2)(p+3)/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
! USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Quadrangle("Quadrangle4")
! CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
! counts = TotalEntities_Quadrangle(kind)
! CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
! CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
! CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
!   DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
! END PROGRAM main
!```

INTERFACE QuadrangleName
  MODULE PURE FUNCTION QuadrangleName1(order) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B) :: ans
  END FUNCTION QuadrangleName1
END INTERFACE QuadrangleName

!----------------------------------------------------------------------------
!                                                       Initiate@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a reference element or return its vertex coordinates.
!
! Use UNIT or BIUNIT explicitly. Quadrangle defaults to BIUNIT and stores two
! intrinsic coordinate rows; Tetrahedron defaults to UNIT and requires nsd=3.
! Pointer constructors allocate new targets owned by the caller.
!
! Arguments:
!   obj : CLASS(ReferenceQuadrangle_), INTENT(INOUT). Reference element
! result; pointer constructors allocate a new target.
!   NSD : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
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
! $$Q=[0,1]^2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Initiate
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: a,b
! CLASS(ReferenceQuadrangle_), POINTER :: p
! REAL(DFP) :: x(2,4)
!
! CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
! b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
! x = RefCoord_Quadrangle("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
!   < 1.0E-12_DFP, "Coordinate aliases agree")
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE initiate_ref_Quadrangle(obj, NSD, xij, domainName)
    CLASS(ReferenceQuadrangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE initiate_ref_Quadrangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceQuadrangle@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a reference element or return its vertex coordinates.
!
! Use UNIT or BIUNIT explicitly. Quadrangle defaults to BIUNIT and stores two
! intrinsic coordinate rows; Tetrahedron defaults to UNIT and requires nsd=3.
! Pointer constructors allocate new targets owned by the caller.
!
! Arguments:
!   NSD : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : TYPE(ReferenceQuadrangle_).
!
! Formula:
! $$Q=[0,1]^2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Initiate
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: a,b
! CLASS(ReferenceQuadrangle_), POINTER :: p
! REAL(DFP) :: x(2,4)
!
! CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
! b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
! x = RefCoord_Quadrangle("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
!   < 1.0E-12_DFP, "Coordinate aliases agree")
! END PROGRAM main
!```

INTERFACE ReferenceQuadrangle
  MODULE PURE FUNCTION reference_Quadrangle(NSD, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceQuadrangle_) :: obj
  END FUNCTION reference_Quadrangle
END INTERFACE ReferenceQuadrangle

!----------------------------------------------------------------------------
!                                     ReferenceQuadrangle_Pointer@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a reference element or return its vertex coordinates.
!
! Use UNIT or BIUNIT explicitly. Quadrangle defaults to BIUNIT and stores two
! intrinsic coordinate rows; Tetrahedron defaults to UNIT and requires nsd=3.
! Pointer constructors allocate new targets owned by the caller.
!
! Arguments:
!   NSD : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : CLASS(ReferenceQuadrangle_), POINTER.
!
! Formula:
! $$Q=[0,1]^2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Initiate
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: a,b
! CLASS(ReferenceQuadrangle_), POINTER :: p
! REAL(DFP) :: x(2,4)
!
! CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
! b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
! x = RefCoord_Quadrangle("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
!   < 1.0E-12_DFP, "Coordinate aliases agree")
! END PROGRAM main
!```

INTERFACE ReferenceQuadrangle_Pointer
  MODULE FUNCTION reference_Quadrangle_Pointer(NSD, xij, domainName) &
    RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: NSD
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceQuadrangle_), POINTER :: obj
  END FUNCTION reference_Quadrangle_Pointer
END INTERFACE ReferenceQuadrangle_Pointer

!----------------------------------------------------------------------------
!                                                LagrangeElement@Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a quadratic reference element from a linear element.
!
! The example uses a fresh output object. For Tetrahedron, use orders 1 or 2
! until higher order face connectivity is implemented; metadata lookup of
! higher orders alone does not make the topology complete.
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
! $$N_Q=9,\quad N_T=10\quad(p=2)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Equidistance
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: HighOrderElement_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: ref
! TYPE(ReferenceElement_) :: high
!
! ref = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! CALL HighOrderElement_Quadrangle(refelem=ref,order=2_I4B, &
!   obj=high,ipType=Equidistance)
! CALL OK(SIZE(high%xij,2) == 9, "Quadratic node count")
! CALL OK(high%order == 2, "Stored interpolation order")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HighorderElement_Quadrangle(refelem, order, obj, &
                                                ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighorderElement_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Geometry
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Measure a reference element and expose current quality behavior.
!
! The quality routines are placeholders that always return zero, not
! implemented mesh quality measures. Quadrangle planar area is signed; spatial
! area and Tetrahedron volume are nonnegative.
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
! $$|Q_{\mathrm{unit}}|=1,\quad|T_{\mathrm{unit}}|=1/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Quadrangle_Quality
! USE ReferenceQuadrangle_Method, ONLY: Quality_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: ref
! REAL(DFP) :: y,q
!
! ref = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! y = Measure_Simplex_Quadrangle(ref,ref%xij)
! CALL OK(ABS(y-(1.0_DFP)) < 1.0E-11_DFP, "Reference measure")
! q = Quadrangle_Quality(ref,ref%xij,measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Quadrangle(ref,ref%xij,1_I4B) == q, &
!   "Quality aliases currently return a placeholder zero")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Quadrangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Quadrangle_quality
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Measure a reference element and expose current quality behavior.
!
! The quality routines are placeholders that always return zero, not
! implemented mesh quality measures. Quadrangle planar area is signed; spatial
! area and Tetrahedron volume are nonnegative.
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
! $$|Q_{\mathrm{unit}}|=1,\quad|T_{\mathrm{unit}}|=1/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Quadrangle_Quality
! USE ReferenceQuadrangle_Method, ONLY: Quality_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: ref
! REAL(DFP) :: y,q
!
! ref = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! y = Measure_Simplex_Quadrangle(ref,ref%xij)
! CALL OK(ABS(y-(1.0_DFP)) < 1.0E-11_DFP, "Reference measure")
! q = Quadrangle_Quality(ref,ref%xij,measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Quadrangle(ref,ref%xij,1_I4B) == q, &
!   "Quality aliases currently return a placeholder zero")
! END PROGRAM main
!```

INTERFACE Quality_Quadrangle
  MODULE FUNCTION Quadrangle_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Quadrangle_Quality
END INTERFACE Quality_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute quadrilateral area from four cyclic vertex columns.
!
! The 2D routine returns signed area; the 3D routine uses the midpoint
! parallelogram construction. The example is planar and convex; do not
! interpret it as validation for arbitrary warped or self intersecting quads.
!
! Arguments:
!   q : REAL(DFP), INTENT(IN)(3, 4). Four cyclic vertex coordinate columns.
!   ans : REAL(DFP), INTENT(OUT). Result buffer; allocate the full active
! shape before the call.
!
! Returns:
!   Updates ans.
!
! Formula:
! $$A_{\mathrm{2D}}=\tfrac12\sum_{i=1}^4(x_i y_{i+1}-x_{i+1}y_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: QuadArea2D
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea2D
! USE ReferenceQuadrangle_Method, ONLY: QuadArea3D
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea3D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: q2(2,4),q3(3,4),a,b,c,d
!
! q2 = RESHAPE([0.0_DFP,0.0_DFP,2.0_DFP,0.0_DFP, &
!   2.0_DFP,1.0_DFP,0.0_DFP,1.0_DFP],[2,4])
! q3 = 0.0_DFP
! q3(1:2,:) = q2
! CALL QuadArea2D(q2,a)
! CALL QuadrangleArea2D(q2,b)
! CALL QuadArea3D(q3,c)
! CALL QuadrangleArea3D(q3,d)
! CALL OK(MAXVAL(ABS([a,b,c,d]-2.0_DFP)) < 1.0E-11_DFP, &
!   "Area aliases agree for a planar rectangle")
! END PROGRAM main
!```

INTERFACE QuadrangleArea3D
  MODULE PURE SUBROUTINE QuadArea3D(q, ans)
    REAL(DFP), INTENT(IN) :: q(3, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE QuadArea3D
END INTERFACE QuadrangleArea3D

!----------------------------------------------------------------------------
!                                                          QuadrangleArea2D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute quadrilateral area from four cyclic vertex columns.
!
! The 2D routine returns signed area; the 3D routine uses the midpoint
! parallelogram construction. The example is planar and convex; do not
! interpret it as validation for arbitrary warped or self intersecting quads.
!
! Arguments:
!   q : REAL(DFP), INTENT(IN)(2, 4). Four cyclic vertex coordinate columns.
!   ans : REAL(DFP), INTENT(OUT). Result buffer; allocate the full active
! shape before the call.
!
! Returns:
!   Updates ans.
!
! Formula:
! $$A_{\mathrm{2D}}=\tfrac12\sum_{i=1}^4(x_i y_{i+1}-x_{i+1}y_i)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: QuadArea2D
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea2D
! USE ReferenceQuadrangle_Method, ONLY: QuadArea3D
! USE ReferenceQuadrangle_Method, ONLY: QuadrangleArea3D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: q2(2,4),q3(3,4),a,b,c,d
!
! q2 = RESHAPE([0.0_DFP,0.0_DFP,2.0_DFP,0.0_DFP, &
!   2.0_DFP,1.0_DFP,0.0_DFP,1.0_DFP],[2,4])
! q3 = 0.0_DFP
! q3(1:2,:) = q2
! CALL QuadArea2D(q2,a)
! CALL QuadrangleArea2D(q2,b)
! CALL QuadArea3D(q3,c)
! CALL QuadrangleArea3D(q3,d)
! CALL OK(MAXVAL(ABS([a,b,c,d]-2.0_DFP)) < 1.0E-11_DFP, &
!   "Area aliases agree for a planar rectangle")
! END PROGRAM main
!```

INTERFACE QuadrangleArea2D
  MODULE PURE SUBROUTINE QuadArea2D(q, ans)
    REAL(DFP), INTENT(IN) :: q(2, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE QuadArea2D
END INTERFACE QuadrangleArea2D

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Quadrangle(con, opt, order, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of the element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Quadrangle(con, opt, order, &
                                                        nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the Face number
    !! The row represents a Face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! This option is not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! Order of the element
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   RefQuadrangleCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct a reference element or return its vertex coordinates.
!
! Use UNIT or BIUNIT explicitly. Quadrangle defaults to BIUNIT and stores two
! intrinsic coordinate rows; Tetrahedron defaults to UNIT and requires nsd=3.
! Pointer constructors allocate new targets owned by the caller.
!
! Arguments:
!   refQuadrangle : CHARACTER(*), INTENT(IN). Reference square domain, UNIT or
! BIUNIT, when this overload supports mapping.
!
! Returns:
!   ans : REAL(DFP)(2, 4).
!
! Formula:
! $$Q=[0,1]^2$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceQuadrangle_
! USE ReferenceQuadrangle_Method, ONLY: Initiate
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
! USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle_Pointer
! USE ReferenceQuadrangle_Method, ONLY: RefCoord_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: RefQuadrangleCoord
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceQuadrangle_) :: a,b
! CLASS(ReferenceQuadrangle_), POINTER :: p
! REAL(DFP) :: x(2,4)
!
! CALL Initiate(obj=a,nsd=2_I4B,domainName="UNIT")
! b = ReferenceQuadrangle(nsd=2_I4B,domainName="UNIT")
! p => ReferenceQuadrangle_Pointer(nsd=2_I4B,domainName="UNIT")
! x = RefCoord_Quadrangle("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! CALL OK(MAXVAL(ABS(x-RefQuadrangleCoord("UNIT"))) &
!   < 1.0E-12_DFP, "Coordinate aliases agree")
! END PROGRAM main
!```

INTERFACE RefCoord_Quadrangle
  MODULE PURE FUNCTION RefQuadrangleCoord(refQuadrangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refQuadrangle
    REAL(DFP) :: ans(2, 4)
  END FUNCTION RefQuadrangleCoord
END INTERFACE RefCoord_Quadrangle

!----------------------------------------------------------------------------
!                                           FaceShapeMetaData_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Canonicalize quadrilateral face ordering and orientation.
!
! This three entry sign code is not the Triangle rotation/sign code.
! localFaces receives cyclic vertex identifiers. Keep it consistent with the
! oriented hierarchical basis overloads.
!
! Arguments:
!   face : INTEGER(I4B), INTENT(INOUT)(:). Triangular face vertex identifiers.
!   sorted_face : INTEGER(I4B), INTENT(INOUT)(:). Canonical sorted face
! identifiers.
!   faceOrient : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Quadrilateral
! orientation code with three signs.
!   localFaces : INTEGER(I4B), OPTIONAL, INTENT(INOUT)(:). Cyclic vertex
! identifiers used for orientation.
!
! Returns:
!   Updates face(:), sorted_face(:), faceOrient(:), localFaces(:).
!
! Formula:
! $$o=(s_1,s_2,s_3),\quad s_i\in\{-1,1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceQuadrangle_Method, ONLY: FaceShapeMetaData_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: face(4),canonical(4),orientation(3),cyclic(4)
!
! face = [1_I4B,2_I4B,3_I4B,4_I4B]
! CALL FaceShapeMetaData_Quadrangle(face,canonical,orientation,cyclic)
! CALL OK(ALL(canonical == face), "Canonical cyclic face ordering")
! CALL OK(ALL(orientation == [1_I4B,1_I4B,1_I4B]), &
!   "Identity orientation has three positive signs")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE FaceShapeMetaData_Quadrangle(face, sorted_face, &
                                                 faceOrient, localFaces)
    INTEGER(I4B), INTENT(INOUT) :: face(:)
    INTEGER(I4B), INTENT(INOUT) :: sorted_face(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceOrient(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaces(:)
  END SUBROUTINE FaceShapeMetaData_Quadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetFaceElemType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
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
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Quadrangle
  MODULE PURE SUBROUTINE GetFaceElemType_Quadrangle1(elemType, faceElemType, &
                                                     opt, tFaceNodes)
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
  END SUBROUTINE GetFaceElemType_Quadrangle1
END INTERFACE GetFaceElemType_Quadrangle

!----------------------------------------------------------------------------
!                                                GetFaceElemType_Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build reference facet topology and connectivity.
!
! Caller allocates output buffers. Quadrangle face connectivity always forces
! option 2, independent of opt. Tetrahedron face connectivity fills all nodes
! only for order 1 and 2; higher order face interiors are not implemented. The
! example uses first order topology.
!
! Arguments:
!   elemType : INTEGER(I4B), INTENT(IN). Element identifier from GlobalData or
! the element type lookup.
!   localFaceNumber : INTEGER(I4B), INTENT(IN). One based local facet index.
!   faceElemType : INTEGER(I4B), INTENT(OUT). Output facet element
! identifier(s).
!   tFaceNodes : INTEGER(I4B), INTENT(INOUT). Output facet node count(s).
!   opt : INTEGER(I4B), OPTIONAL, INTENT(IN). Connectivity or entity selector;
! see the routine description.
!
! Returns:
!   Updates faceElemType, tFaceNodes.
!
! Formula:
! $$\partial Q=\bigcup_{e=1}^4 e,\quad\partial T=\bigcup_{f=1}^4 f$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: Quadrangle4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
! USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Quadrangle4
! INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Quadrangle(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 2), "Linear facet node count")
! CALL FacetTopology_Quadrangle(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Quadrangle
  MODULE PURE SUBROUTINE GetFaceElemType_Quadrangle2( &
    elemType, localFaceNumber, faceElemType, opt, tFaceNodes)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! name of element
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Element names of faces
    INTEGER(I4B), INTENT(INOUT) :: tFaceNodes
    !! Total number of nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Quadrangle2
END INTERFACE GetFaceElemType_Quadrangle

END MODULE ReferenceQuadrangle_Method
