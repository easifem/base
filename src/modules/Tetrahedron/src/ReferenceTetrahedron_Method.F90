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
! summary: ReferenceTetrahedron geometry and interpolation utilities.
!
! Construct reference tetrahedra with nsd=3 and UNIT or BIUNIT coordinates.
! Node, edge, face and cell metadata are distinct from polynomial order.
! Volume is unsigned. Quality routines currently return a placeholder zero.
! Higher order face connectivity is implemented only through quadratic order,
! despite metadata for additional element types.
!
! Arguments:
!   None at module scope; see individual public interfaces.
!
! Returns:
!   Geometry, reference element and interpolation procedures.
!
! Formula:
! $$V=|\det[v_2-v_1,v_3-v_1,v_4-v_1]|/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Initiate
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: a,b
! CLASS(ReferenceTetrahedron_), POINTER :: p
! REAL(DFP) :: x(3,4)
!
! CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
! b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
! x = RefCoord_Tetrahedron("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! END PROGRAM main
!```

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
! USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Tetrahedron("Tetrahedron4")
! CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
! counts = TotalEntities_Tetrahedron(kind)
! CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementName_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology_Tetrahedron
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Tetrahedron(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                 TotalEntities_Tetrahedron
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
! USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Tetrahedron("Tetrahedron4")
! CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
! counts = TotalEntities_Tetrahedron(kind)
! CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                            TotalNodesInElement_Tetrahedron
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
! USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Tetrahedron("Tetrahedron4")
! CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
! counts = TotalEntities_Tetrahedron(kind)
! CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Tetrahedron
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
! USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Tetrahedron("Tetrahedron4")
! CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
! counts = TotalEntities_Tetrahedron(kind)
! CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementType_Tetrahedron(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ElementOrder_Tetrahedron
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
! USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B) :: kind,counts(4)
!
! kind = ElementType_Tetrahedron("Tetrahedron4")
! CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
!   "Element name round trip")
! CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
! CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
! counts = TotalEntities_Tetrahedron(kind)
! CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
!   "Node, edge, face and cell counts")
! CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Tetrahedron(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElements_Tetrahedron
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Tetrahedron1
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                  FacetElements_Tetrahedron
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE FacetElements_Tetrahedron
  MODULE SUBROUTINE FacetElements_Tetrahedron2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Tetrahedron2
END INTERFACE FacetElements_Tetrahedron

!----------------------------------------------------------------------------
!                                                       Initiate@Tetrahedron
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
!   obj : CLASS(ReferenceTetrahedron_), INTENT(INOUT). Reference element
! result; pointer constructors allocate a new target.
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
! $$T=\{(x,y,z):x,y,z\ge0,\ x+y+z\le1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Initiate
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: a,b
! CLASS(ReferenceTetrahedron_), POINTER :: p
! REAL(DFP) :: x(3,4)
!
! CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
! b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
! x = RefCoord_Tetrahedron("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Tetrahedron(obj, nsd, xij, domainName)
    CLASS(ReferenceTetrahedron_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE Initiate_Ref_Tetrahedron
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            ReferenceTetrahedron@Tetrahedron
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
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : TYPE(ReferenceTetrahedron_).
!
! Formula:
! $$T=\{(x,y,z):x,y,z\ge0,\ x+y+z\le1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Initiate
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: a,b
! CLASS(ReferenceTetrahedron_), POINTER :: p
! REAL(DFP) :: x(3,4)
!
! CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
! b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
! x = RefCoord_Tetrahedron("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE ReferenceTetrahedron
  MODULE PURE FUNCTION reference_Tetrahedron(nsd, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceTetrahedron_) :: obj
  END FUNCTION reference_Tetrahedron
END INTERFACE ReferenceTetrahedron

!----------------------------------------------------------------------------
!                                   ReferenceTetrahedron_Pointer@Tetrahedron
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
!   nsd : INTEGER(I4B), INTENT(IN). Physical embedding dimension; does not
! change intrinsic coordinate row count.
!   xij : REAL(DFP), INTENT(IN), OPTIONAL(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!   domainName : CHARACTER(*), OPTIONAL, INTENT(IN). Reference domain name;
! use UNIT or BIUNIT explicitly.
!
! Returns:
!   obj : CLASS(ReferenceTetrahedron_), POINTER.
!
! Formula:
! $$T=\{(x,y,z):x,y,z\ge0,\ x+y+z\le1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Initiate
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: a,b
! CLASS(ReferenceTetrahedron_), POINTER :: p
! REAL(DFP) :: x(3,4)
!
! CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
! b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
! x = RefCoord_Tetrahedron("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE ReferenceTetrahedron_Pointer
  MODULE FUNCTION reference_Tetrahedron_Pointer(nsd, xij, domainName)  &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceTetrahedron_), POINTER :: obj
  END FUNCTION reference_Tetrahedron_Pointer
END INTERFACE ReferenceTetrahedron_Pointer

!----------------------------------------------------------------------------
!                                               LagrangeElement@Tetrahedron
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
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: HighOrderElement_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: ref
! TYPE(ReferenceElement_) :: high
!
! ref = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! CALL HighOrderElement_Tetrahedron(refelem=ref,order=2_I4B, &
!   obj=high,ipType=Equidistance)
! CALL OK(SIZE(high%xij,2) == 10, "Quadratic node count")
! CALL OK(high%order == 2, "Stored interpolation order")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE HighOrderElement_Tetrahedron(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighOrderElement_Tetrahedron
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
!   RefElem : CLASS(ReferenceElement_), INTENT(IN). Reference element carrying
! topology and embedding dimension.
!   XiJ : REAL(DFP), INTENT(IN)(:, :). Point columns; vertices or
! interpolation nodes according to the interface.
!
! Returns:
!   ans : REAL(DFP).
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
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Tetrahedron_Quality
! USE ReferenceTetrahedron_Method, ONLY: Quality_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: ref
! REAL(DFP) :: y,q
!
! ref = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! y = Measure_Simplex_Tetrahedron(ref,ref%xij)
! CALL OK(ABS(y-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, "Reference measure")
! q = Tetrahedron_Quality(ref,ref%xij,measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Tetrahedron(ref,ref%xij,1_I4B) == q, &
!   "Quality aliases currently return a placeholder zero")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Tetrahedron(RefElem, XiJ) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: RefElem
    REAL(DFP), INTENT(IN) :: XiJ(:, :)
    REAL(DFP) :: ans
  END FUNCTION Measure_Simplex_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Tetrahedron_Quality
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
!   ans : REAL(DFP).
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
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Tetrahedron_Quality
! USE ReferenceTetrahedron_Method, ONLY: Quality_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: Measure_Simplex_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: ref
! REAL(DFP) :: y,q
!
! ref = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! y = Measure_Simplex_Tetrahedron(ref,ref%xij)
! CALL OK(ABS(y-(1.0_DFP/6.0_DFP)) < 1.0E-11_DFP, "Reference measure")
! q = Tetrahedron_Quality(ref,ref%xij,measure=1_I4B)
! CALL OK(q == 0.0_DFP .AND. Quality_Tetrahedron(ref,ref%xij,1_I4B) == q, &
!   "Quality aliases currently return a placeholder zero")
! END PROGRAM main
!```

INTERFACE Quality_Tetrahedron
  MODULE FUNCTION Tetrahedron_Quality(refelem, xij, measure) RESULT(ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: ans
  END FUNCTION Tetrahedron_Quality
END INTERFACE Quality_Tetrahedron

!----------------------------------------------------------------------------
!                                                       TetrahedronVolume3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute unsigned tetrahedron volume from vertex columns.
!
!
! Arguments:
!   xij : REAL(DFP), INTENT(IN)(3, 4). Point columns; vertices or
! interpolation nodes according to the interface.
!   ans : REAL(DFP), INTENT(OUT). Result buffer; allocate the full active
! shape before the call.
!
! Returns:
!   Updates ans.
!
! Formula:
! $$V=|\det[v_2-v_1,v_3-v_1,v_4-v_1]|/6$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ReferenceTetrahedron_Method, ONLY: TetrahedronVolume3D
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! REAL(DFP) :: x(3,4),v
!
! x = 0.0_DFP
! x(1,2) = 1.0_DFP
! x(2,3) = 1.0_DFP
! x(3,4) = 1.0_DFP
! CALL TetrahedronVolume3D(x,v)
! CALL OK(ABS(v-1.0_DFP/6.0_DFP) < 1.0E-11_DFP, "Unit simplex volume")
! x(:,[2,3]) = x(:,[3,2])
! CALL TetrahedronVolume3D(x,v)
! CALL OK(ABS(v-1.0_DFP/6.0_DFP) < 1.0E-11_DFP, &
!   "Volume is independent of orientation")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE TetrahedronVolume3D(xij, ans)
    REAL(DFP), INTENT(IN) :: xij(3, 4)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TetrahedronVolume3D
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Tetrahedron(con, opt,  &
    & order, nrow, ncol)
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
    !! Order of the edge
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetEdgeConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetFaceConnectivity
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE GetFaceConnectivity_Tetrahedron(con, opt, order, &
                                                         nrow, ncol)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the face number
    !! The row represents a face
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then face connectivity for hierarchial approximation
    !! If opt =2, then face connectivity for Lagrangian approximation
    !! opt=1 is default
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nrow
    !! Number of rows written in con
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ncol
    !! Numbers of cols written in con
  END SUBROUTINE GetFaceConnectivity_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                       RefCoord_Tetrahedron
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
!   refTetrahedron : CHARACTER(*), INTENT(IN). Reference tetrahedron domain,
! UNIT or BIUNIT.
!
! Returns:
!   ans : REAL(DFP)(3, 4).
!
! Formula:
! $$T=\{(x,y,z):x,y,z\ge0,\ x+y+z\le1\}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE BaseType, ONLY: ReferenceTetrahedron_
! USE ReferenceTetrahedron_Method, ONLY: Initiate
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
! USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron_Pointer
! USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! TYPE(ReferenceTetrahedron_) :: a,b
! CLASS(ReferenceTetrahedron_), POINTER :: p
! REAL(DFP) :: x(3,4)
!
! CALL Initiate(obj=a,nsd=3_I4B,domainName="UNIT")
! b = ReferenceTetrahedron(nsd=3_I4B,domainName="UNIT")
! p => ReferenceTetrahedron_Pointer(nsd=3_I4B,domainName="UNIT")
! x = RefCoord_Tetrahedron("UNIT")
! CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
! CALL OK(ASSOCIATED(p), "Pointer constructor allocates a target")
! CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Reference coordinate rows")
! DEALLOCATE(p)
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION RefCoord_Tetrahedron(refTetrahedron) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    REAL(DFP) :: ans(3, 4)
  END FUNCTION RefCoord_Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetFaceElemType@GeometryMethods
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Tetrahedron
 MODULE PURE SUBROUTINE GetFaceElemType_Tetrahedron1(elemType, faceElemType, &
                                                      tFaceNodes, opt)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elemType
    !! element type for Tetrahedron
    !! default is Tetrahedron4
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceElemType(:)
    !! Face element type
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tFaceNodes(:)
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Tetrahedron1
END INTERFACE GetFaceElemType_Tetrahedron

!----------------------------------------------------------------------------
!                                             GetFaceElemType@GeometryMethods
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
!   tFaceNodes : INTEGER(I4B), INTENT(OUT). Output facet node count(s).
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
! USE GlobalData, ONLY: Tetrahedron4
! USE BaseType, ONLY: ReferenceTopology_
! USE BaseType, ONLY: ReferenceElement_
! USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
! USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
! INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
! INTEGER(I4B) :: types(4),counts(4)
! TYPE(ReferenceTopology_) :: topology(4)
! TYPE(ReferenceElement_) :: facets(4)
!
! CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
! CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
! CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
! CALL GetFaceElemType_Tetrahedron(elemType=kind, &
!   faceElemType=types,tFaceNodes=counts)
! CALL OK(ALL(counts == 3), "Linear facet node count")
! CALL FacetTopology_Tetrahedron(elemType=kind, &
!   nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
! CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
! CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
! CALL OK(ALL(facets%order == 1), "Linear reference facets")
! END PROGRAM main
!```

INTERFACE GetFaceElemType_Tetrahedron
  MODULE PURE SUBROUTINE GetFaceElemType_Tetrahedron2( &
    elemType, localFaceNumber, faceElemType, tFaceNodes, opt)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type for Tetrahedron
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number
    INTEGER(I4B), INTENT(OUT) :: faceElemType
    !! Face element type
    INTEGER(I4B), INTENT(OUT) :: tFaceNodes
    !! total nodes in each face
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! If opt = 2, then edge connectivity for Lagrangian approximation
    !! opt = 1 is default
  END SUBROUTINE GetFaceElemType_Tetrahedron2
END INTERFACE GetFaceElemType_Tetrahedron

END MODULE ReferenceTetrahedron_Method
