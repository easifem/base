!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HighorderElement_Triangle for order 2 and order 4

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceTriangle_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Triangle15
USE GlobalData, ONLY: Triangle6
USE ReferenceTriangle_Method, ONLY: HighorderElement_Triangle
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ReferenceTriangle_) :: ref
TYPE(ReferenceElement_) :: obj

ref = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")

! Test 1: quadratic element (Triangle6)
BLOCK
  INTEGER(I4B) :: ii, tsize

  CALL HighorderElement_Triangle(refelem=ref, order=2_I4B, obj=obj, &
                                 ipType=Equidistance)

  CALL OK(ALL(SHAPE(obj%xij) .EQ. [2, 6]), "order 2: six nodes")
  CALL OK(obj%name .EQ. Triangle6, "order 2: name is Triangle6")
  CALL OK(ALL(obj%entityCounts .EQ. [6, 3, 1, 0]), &
          "order 2: entity counts")

  tsize = SIZE(obj%topology)
  CALL OK(tsize .EQ. 10, "order 2: 6 points + 3 edges + 1 cell")

  ! the three edges are quadratic lines with end points and mid node
  CALL OK(ALL(obj%topology(7)%nptrs .EQ. [1, 2, 4]), "order 2: edge 1")
  CALL OK(ALL(obj%topology(8)%nptrs .EQ. [2, 3, 5]), "order 2: edge 2")
  CALL OK(ALL(obj%topology(9)%nptrs .EQ. [3, 1, 6]), "order 2: edge 3")
  CALL OK(ALL([(obj%topology(ii)%name .EQ. Line3, ii=7, 9)]), &
          "order 2: edges are Line3")

  ! the cell holds all six nodes
  CALL OK(ALL(obj%topology(10)%nptrs .EQ. [1, 2, 3, 4, 5, 6]), &
          "order 2: cell connectivity")
  CALL OK(obj%topology(10)%name .EQ. Triangle6, "order 2: cell name")

  ! the first three nodes are the vertices of the reference triangle
  CALL OK(MAXVAL(ABS(obj%xij(:, 1:3) - ref%xij(1:2, 1:3))) &
          .LT. 1.0E-12_DFP, "order 2: vertices come first")
END BLOCK

! Test 2: quartic element (Triangle15, complete)
BLOCK
  CALL HighorderElement_Triangle(refelem=ref, order=4_I4B, obj=obj, &
                                 ipType=Equidistance)

  CALL OK(ALL(SHAPE(obj%xij) .EQ. [2, 15]), "order 4: fifteen nodes")
  CALL OK(obj%name .EQ. Triangle15, "order 4: name is Triangle15")
  CALL OK(ALL(obj%entityCounts .EQ. [15, 3, 1, 0]), &
          "order 4: entity counts")
END BLOCK

END PROGRAM main
