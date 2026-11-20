!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing FacetElements_Tetrahedron with a reference element

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTetrahedron_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron4
USE GlobalData, ONLY: Triangle3
USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceTetrahedron_) :: ref
TYPE(ReferenceElement_) :: facets(4), facets2(4)

ref = ReferenceTetrahedron(nsd=3_I4B, domainName="UNIT")
CALL FacetElements_Tetrahedron(refelem=ref, ans=facets)

! Test 1: every facet is a linear triangle in 3D
BLOCK
  LOGICAL :: isok
  isok = ALL(facets(:)%name .EQ. Triangle3) .AND. &
         ALL(facets(:)%order .EQ. 1) .AND. &
         ALL(facets(:)%nsd .EQ. 3) .AND. &
         ALL(facets(:)%xiDimension .EQ. 2)
  CALL OK(isok, "facets are linear Triangle3 elements")
END BLOCK

! Test 2: entity counts and topology size of a triangle facet
BLOCK
  CALL OK(ALL(facets(1)%entityCounts .EQ. [3, 3, 1, 0]), &
          "facet entity counts")
  CALL OK(SIZE(facets(1)%topology) .EQ. 7, "facet topology size")
END BLOCK

! Test 3: the surface entity of each facet holds the face nodes
BLOCK
  CALL OK(ALL(facets(1)%topology(7)%nptrs .EQ. [1, 3, 2]), &
          "facet 1 nodes")
  CALL OK(ALL(facets(4)%topology(7)%nptrs .EQ. [2, 3, 4]), &
          "facet 4 nodes")
END BLOCK

! Test 4: the refelem overload agrees with the elemType overload
BLOCK
  INTEGER(I4B) :: ii
  LOGICAL :: isok
  CALL FacetElements_Tetrahedron(elemType=Tetrahedron4, nsd=3_I4B, &
                                 ans=facets2)
  isok = .TRUE.
  DO ii = 1, 4
    isok = isok .AND. facets(ii)%name .EQ. facets2(ii)%name
    isok = isok .AND. ALL(facets(ii)%topology(7)%nptrs &
                          .EQ. facets2(ii)%topology(7)%nptrs)
  END DO
  CALL OK(isok, "refelem and elemType overloads agree")
END BLOCK

END PROGRAM main
