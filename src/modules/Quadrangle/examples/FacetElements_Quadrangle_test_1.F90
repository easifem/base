!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing FacetElements_Quadrangle built from a reference element
! and from an element type

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceQuadrangle_
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle9
USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: HighorderElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceQuadrangle_) :: ref
TYPE(ReferenceElement_) :: high

ref = ReferenceQuadrangle(nsd=2_I4B)

! Test 1: facets of the linear reference element
BLOCK
  TYPE(ReferenceElement_) :: facets(4)

  CALL FacetElements_Quadrangle(refelem=ref, ans=facets)
  CALL OK(ALL(facets%order .EQ. 1), "linear: facet order is 1")
  CALL OK(ALL(facets%nsd .EQ. 2), "linear: facet nsd is 2")
  CALL OK(ALL(facets(4)%topology(3)%nptrs .EQ. [4, 1]), &
          "linear: fourth facet connects nodes 4 and 1")
END BLOCK

! Test 2: facets of the quadratic reference element
BLOCK
  TYPE(ReferenceElement_) :: facets(4)

  CALL HighorderElement_Quadrangle(refelem=ref, order=2_I4B, obj=high, &
                                   ipType=Equidistance)
  CALL FacetElements_Quadrangle(refelem=high, ans=facets)
  CALL OK(ALL(facets%order .EQ. 2), "quadratic: facet order is 2")
  CALL OK(ALL(facets(1)%entityCounts .EQ. [3, 1, 0, 0]), &
          "quadratic: three nodes and one cell per facet")
  CALL OK(ALL(facets(2)%topology(4)%nptrs .EQ. [2, 3, 6]), &
          "quadratic: second facet has nodes 2, 3, 6")
  CALL OK(SIZE(facets(1)%xij, 2) .EQ. 3, "quadratic: three facet nodes")
END BLOCK

! Test 3: facets from the element type
BLOCK
  TYPE(ReferenceElement_) :: facets(4)

  CALL FacetElements_Quadrangle(elemType=Quadrangle9, nsd=3_I4B, &
                                ans=facets)
  CALL OK(ALL(facets%order .EQ. 2), "Quadrangle9: facet order is 2")
  CALL OK(ALL(facets%nsd .EQ. 3), "Quadrangle9: facet nsd is 3")
  CALL OK(ALL(facets(3)%topology(1)%nptrs .EQ. [3]), &
          "Quadrangle9: third facet starts at node 3")
END BLOCK

END PROGRAM main
