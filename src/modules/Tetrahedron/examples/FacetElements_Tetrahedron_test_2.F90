!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing FacetElements_Tetrahedron for a cubic tetrahedron

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron20
USE GlobalData, ONLY: Triangle10
USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! A Triangle10 facet has 10 points, 3 edges and 1 surface, so its
! topology has 14 entries. Entry 14 holds the 10 nodes of the facet.
TYPE(ReferenceElement_) :: facets(4)
INTEGER(I4B) :: ii

CALL FacetElements_Tetrahedron(elemType=Tetrahedron20, nsd=3_I4B, &
                               ans=facets)

! Test 1: every facet is a cubic Triangle10 in 3D
BLOCK
  LOGICAL :: isok
  isok = ALL(facets(:)%name .EQ. Triangle10) .AND. &
         ALL(facets(:)%order .EQ. 3) .AND. &
         ALL(facets(:)%nsd .EQ. 3)
  CALL OK(isok, "facets are cubic Triangle10 elements")
END BLOCK

! Test 2: entity counts and topology size of a facet
BLOCK
  CALL OK(ALL(facets(1)%entityCounts .EQ. [10, 3, 1, 0]), &
          "facet entity counts")
  CALL OK(SIZE(facets(1)%topology) .EQ. 14, "facet topology size")
END BLOCK

! Test 3: the surface entity of each facet holds 10 valid node numbers
BLOCK
  LOGICAL :: isok
  isok = .TRUE.
  DO ii = 1, 4
    isok = isok .AND. SIZE(facets(ii)%topology(14)%nptrs) .EQ. 10
    isok = isok .AND. ALL(facets(ii)%topology(14)%nptrs .GE. 1) .AND. &
           ALL(facets(ii)%topology(14)%nptrs .LE. 20)
  END DO
  CALL OK(isok, "facet nodes are node numbers of the tetrahedron")
END BLOCK

END PROGRAM main
