!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Topology of the two end point facets of a line.

PROGRAM main
USE BaseType, ONLY: ReferenceTopology_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: FacetTopology_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceTopology_) :: facets(2)

! A quadratic line with global nodes 10, 20 and 30.
CALL FacetTopology_Line(elemType=Line3, nptrs=[10_I4B, 20_I4B, 30_I4B], &
                        ans=facets)

! Test 1: facets hold the first and the second node
BLOCK
  CALL OK(ALL(facets(1)%nptrs == [10]), "Facet 1 is the first node")
  CALL OK(ALL(facets(2)%nptrs == [20]), "Facet 2 is the second node")
END BLOCK

! Test 2: facets are points of dimension zero
BLOCK
  CALL OK(ALL(facets%name == Point1), "Facets are Point1")
  CALL OK(ALL(facets%xiDimension == 0), "Facet xiDimension is 0")
END BLOCK

END PROGRAM main
