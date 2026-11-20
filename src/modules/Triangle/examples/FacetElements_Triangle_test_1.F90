!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing FacetElements_Triangle built from a reference triangle

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTriangle_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceTriangle_) :: ref
TYPE(ReferenceElement_) :: facets(3)

ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
CALL FacetElements_Triangle(refelem=ref, ans=facets)

! Test 1: every facet is a linear line element
BLOCK
  CALL OK(ALL(facets%order .EQ. 1), "facets are linear")
  CALL OK(ALL(facets%name .EQ. Line2), "facets are Line2")
  CALL OK(ALL(facets%xiDimension .EQ. 1), "facets are one dimensional")
  CALL OK(ALL(facets%nsd .EQ. 2), "facets keep nsd of the triangle")
END BLOCK

! Test 2: entity counts and topology of each facet
BLOCK
  INTEGER(I4B) :: ii
  LOGICAL :: isok

  isok = .TRUE.
  DO ii = 1, 3
    isok = isok .AND. ALL(facets(ii)%entityCounts .EQ. [2, 1, 0, 0])
    isok = isok .AND. (SIZE(facets(ii)%topology) .EQ. 3)
  END DO
  CALL OK(isok, "two points and one line per facet")
END BLOCK

! Test 3: the facets are the edges 1-2, 2-3 and 3-1
BLOCK
  CALL OK(ALL(facets(1)%topology(3)%nptrs .EQ. [1, 2]), "facet 1 is 1-2")
  CALL OK(ALL(facets(2)%topology(3)%nptrs .EQ. [2, 3]), "facet 2 is 2-3")
  CALL OK(ALL(facets(3)%topology(3)%nptrs .EQ. [3, 1]), "facet 3 is 3-1")
END BLOCK

END PROGRAM main
