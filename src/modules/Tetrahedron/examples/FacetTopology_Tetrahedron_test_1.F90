!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing FacetTopology_Tetrahedron for a quadratic tetrahedron

PROGRAM main
USE BaseType, ONLY: ReferenceTopology_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron10
USE GlobalData, ONLY: Triangle6
USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
INTEGER(I4B) :: nptrs(10)
TYPE(ReferenceTopology_) :: topo(4)

! Global node numbers of the element are 101 to 110
nptrs = [(100 + ii, ii=1, 10)]

CALL FacetTopology_Tetrahedron(elemType=Tetrahedron10, nptrs=nptrs, &
                               ans=topo)

! Test 1: each face is a Triangle6 with dimension 2
BLOCK
  LOGICAL :: isok
  isok = ALL(topo(:)%name .EQ. Triangle6) .AND. &
         ALL(topo(:)%xiDimension .EQ. 2)
  CALL OK(isok, "faces are Triangle6 with xiDimension 2")
END BLOCK

! Test 2: face nodes are taken from nptrs, mid nodes included
BLOCK
  CALL OK(ALL(topo(1)%nptrs .EQ. 100 + [1, 3, 2, 6, 8, 5]), &
          "face 1 global nodes")
  CALL OK(ALL(topo(4)%nptrs .EQ. 100 + [2, 3, 4, 8, 10, 9]), &
          "face 4 global nodes")
END BLOCK

END PROGRAM main
