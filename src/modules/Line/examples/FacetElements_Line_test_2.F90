!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Facet elements of a line built from the element type and nsd.

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
USE ReferenceLine_Method, ONLY: FacetElements_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceElement_) :: facets(2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

CALL FacetElements_Line(elemType=Line2, nsd=3_I4B, ans=facets)

! Test 1: both facets are point elements of order zero
BLOCK
  CALL OK(ALL(facets%name == Point1), "Facets are Point1 elements")
  CALL OK(ALL(facets%order == 0), "Facet order is zero")
  CALL OK(ALL(facets%nsd == 3), "Facets take nsd from the argument")
  CALL OK(ALL(facets%interpolationPointType == Equidistance), &
          "Interpolation point type is Equidistance")
END BLOCK

! Test 2: facet j is vertex j
BLOCK
  CALL OK(ALL(facets(1)%topology(1)%nptrs == [1]), "Facet 1 is vertex 1")
  CALL OK(ALL(facets(2)%topology(1)%nptrs == [2]), "Facet 2 is vertex 2")
END BLOCK

! Test 3: facet coordinates are the default line end points
BLOCK
  CALL OK(MAXVAL(ABS(facets(1)%xij(:, 1) - &
                     DEFAULT_Ref_LINE_COORD(:, 1))) < tol, &
          "Facet 1 sits at the first end point")
  CALL OK(MAXVAL(ABS(facets(2)%xij(:, 1) - &
                     DEFAULT_Ref_LINE_COORD(:, 2))) < tol, &
          "Facet 2 sits at the second end point")
END BLOCK

END PROGRAM main
