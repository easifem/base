!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing the topology built by HighOrderElement_Tetrahedron

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTetrahedron_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Tetrahedron10
USE GlobalData, ONLY: Tetrahedron20
USE GlobalData, ONLY: Triangle6
USE ReferenceTetrahedron_Method, ONLY: HighOrderElement_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ReferenceTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceTetrahedron_) :: ref

ref = ReferenceTetrahedron(nsd=3_I4B, domainName="UNIT")

! Test 1: order 2 element, entity counts and names
! Topology: 10 points, 6 edges, 4 faces, 1 cell
BLOCK
  TYPE(ReferenceElement_) :: obj
  CALL HighOrderElement_Tetrahedron(refelem=ref, order=2_I4B, obj=obj, &
                                    ipType=Equidistance)
  CALL OK(obj%name .EQ. Tetrahedron10, "order 2: Tetrahedron10")
  CALL OK(ALL(obj%entityCounts .EQ. [10, 6, 4, 1]), "order 2: counts")
  CALL OK(SIZE(obj%topology) .EQ. 21, "order 2: topology size")
  CALL OK(obj%topology(11)%name .EQ. Line3 .AND. &
          ALL(obj%topology(11)%nptrs .EQ. [1, 2, 5]), "order 2: edge 1")
  CALL OK(obj%topology(17)%name .EQ. Triangle6 .AND. &
          ALL(obj%topology(17)%nptrs .EQ. [1, 3, 2, 6, 8, 5]), &
          "order 2: face 1")
END BLOCK

! Test 2: order 3 faces must list 10 valid nodes each
! Topology: 20 points, 6 edges (21-26), 4 faces (27-30), 1 cell
BLOCK
  TYPE(ReferenceElement_) :: obj
  INTEGER(I4B) :: ii
  LOGICAL :: isok
  CALL HighOrderElement_Tetrahedron(refelem=ref, order=3_I4B, obj=obj, &
                                    ipType=Equidistance)
  CALL OK(obj%name .EQ. Tetrahedron20, "order 3: Tetrahedron20")
  isok = .TRUE.
  DO ii = 27, 30
    isok = isok .AND. SIZE(obj%topology(ii)%nptrs) .EQ. 10
    isok = isok .AND. ALL(obj%topology(ii)%nptrs .GE. 1) .AND. &
           ALL(obj%topology(ii)%nptrs .LE. 20)
  END DO
  CALL OK(isok, "order 3: face nodes are valid node numbers")
END BLOCK

END PROGRAM main
