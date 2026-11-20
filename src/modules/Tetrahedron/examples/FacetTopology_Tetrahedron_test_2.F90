!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing FacetTopology_Tetrahedron for a cubic tetrahedron

PROGRAM main
USE BaseType, ONLY: ReferenceTopology_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron20
USE GlobalData, ONLY: Triangle10
USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! A Tetrahedron20 has 4 vertices, 2 nodes on each of the 6 edges
! (nodes 5 to 16) and 1 node inside each of the 4 faces (nodes 17 to 20).
! Each face is a Triangle10: 3 vertices, 6 edge nodes, 1 face node.
INTEGER(I4B) :: ii
INTEGER(I4B) :: nptrs(20)
TYPE(ReferenceTopology_) :: topo(4)

! Global node numbers of the element are 101 to 120
nptrs = [(100 + ii, ii=1, 20)]

CALL FacetTopology_Tetrahedron(elemType=Tetrahedron20, nptrs=nptrs, &
                               ans=topo)

! Test 1: each face is a Triangle10 with 10 nodes
BLOCK
  LOGICAL :: isok
  isok = ALL(topo(:)%name .EQ. Triangle10)
  DO ii = 1, 4
    isok = isok .AND. SIZE(topo(ii)%nptrs) .EQ. 10
  END DO
  CALL OK(isok, "faces are Triangle10 with 10 nodes")
END BLOCK

! Test 2: every face node is a node of the element, with no repeats
BLOCK
  LOGICAL :: isok
  INTEGER(I4B) :: jj
  isok = .TRUE.
  DO ii = 1, 4
    isok = isok .AND. ALL(topo(ii)%nptrs .GE. 101) .AND. &
           ALL(topo(ii)%nptrs .LE. 120)
    DO jj = 1, SIZE(topo(ii)%nptrs)
      isok = isok .AND. COUNT(topo(ii)%nptrs .EQ. topo(ii)%nptrs(jj)) .EQ. 1
    END DO
  END DO
  CALL OK(isok, "face nodes are valid and distinct")
END BLOCK

! Test 3: vertices come first, as for orders 1 and 2
BLOCK
  CALL OK(ALL(topo(1)%nptrs(1:3) .EQ. 100 + [1, 3, 2]), "face 1 vertices")
  CALL OK(ALL(topo(4)%nptrs(1:3) .EQ. 100 + [2, 3, 4]), "face 4 vertices")
END BLOCK

END PROGRAM main
