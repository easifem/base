!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Facet elements of a reference line built from the line itself.

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
USE ReferenceLine_Method, ONLY: FacetElements_Line
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceLine_) :: ref
TYPE(ReferenceElement_) :: facets(2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

ref = ReferenceLine(nsd=2_I4B, domainName="BIUNIT")
CALL FacetElements_Line(refelem=ref, ans=facets)

! Test 1: both facets are point elements of order zero
BLOCK
  CALL OK(ALL(facets%name == Point1), "Facets are Point1 elements")
  CALL OK(ALL(facets%order == 0), "Facet order is zero")
  CALL OK(ALL(facets%xiDimension == 0), "Facet xiDimension is zero")
END BLOCK

! Test 2: facet node pointers come from the line topology
BLOCK
  CALL OK(ALL(facets(1)%topology(1)%nptrs == [1]), "Facet 1 is node 1")
  CALL OK(ALL(facets(2)%topology(1)%nptrs == [2]), "Facet 2 is node 2")
END BLOCK

! Test 3: facets keep the spatial dimension of the line
BLOCK
  CALL OK(ALL(facets%nsd == 2), "Facets keep nsd of the line")
END BLOCK

! Test 4: entity counts of a point element
BLOCK
  CALL OK(ALL(facets(1)%entityCounts == [1, 0, 0, 0]) .AND. &
          ALL(facets(2)%entityCounts == [1, 0, 0, 0]), &
          "Point facets have one node and no other entity")
END BLOCK

! Test 5: facet coordinates are the default line end points
BLOCK
  CALL OK(ALL(SHAPE(facets(1)%xij) == [3, 1]), "Facet xij is 3 by 1")
  CALL OK(MAXVAL(ABS(facets(1)%xij(:, 1) - &
                     DEFAULT_Ref_LINE_COORD(:, 1))) < tol, &
          "Facet 1 sits at the first end point")
  CALL OK(MAXVAL(ABS(facets(2)%xij(:, 1) - &
                     DEFAULT_Ref_LINE_COORD(:, 2))) < tol, &
          "Facet 2 sits at the second end point")
END BLOCK

END PROGRAM main
