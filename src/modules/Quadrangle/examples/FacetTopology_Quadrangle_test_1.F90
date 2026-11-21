!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing FacetTopology_Quadrangle for a 9-node quadrangle

PROGRAM main
USE BaseType, ONLY: ReferenceTopology_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Quadrangle9
USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceTopology_) :: topo(4)
INTEGER(I4B) :: nptrs(9), ii

! Global node numbers of the element, in local node order
nptrs = [(10 * ii, ii=1, 9)]

CALL FacetTopology_Quadrangle(elemType=Quadrangle9, nptrs=nptrs, ans=topo)

! Test 1: each edge is a 3-node line of dimension 1
BLOCK
  CALL OK(ALL(topo%name .EQ. Line3), "Quadrangle9: edges are Line3")
  CALL OK(ALL(topo%xiDimension .EQ. 1), "Quadrangle9: edge dimension 1")
END BLOCK

! Test 2: edges pick the global numbers of [1,2,5], [2,3,6], [3,4,7],
! [4,1,8]. This is the default edge numbering,
! DEFAULT_OPT_QUADRANGLE_EDGE_CON = 2.
BLOCK
  CALL OK(ALL(topo(1)%nptrs .EQ. [10, 20, 50]), "edge 1 nodes")
  CALL OK(ALL(topo(2)%nptrs .EQ. [20, 30, 60]), "edge 2 nodes")
  CALL OK(ALL(topo(3)%nptrs .EQ. [30, 40, 70]), "edge 3 nodes")
  CALL OK(ALL(topo(4)%nptrs .EQ. [40, 10, 80]), "edge 4 nodes")
END BLOCK

END PROGRAM main
