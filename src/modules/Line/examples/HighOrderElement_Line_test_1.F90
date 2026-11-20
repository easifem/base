!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Build a quadratic Lagrange line from a linear reference line.

PROGRAM main
USE BaseType, ONLY: ReferenceElement_
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: HighOrderElement_Line
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(ReferenceLine_) :: ref
TYPE(ReferenceElement_) :: high

ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")
CALL HighOrderElement_Line(refelem=ref, order=2_I4B, obj=high, &
                           ipType=Equidistance)

! Test 1: three nodes in VEFC order
BLOCK
  CALL OK(ALL(SHAPE(high%xij) == [1, 3]), "xij is 1 by 3")
  CALL OK(MAXVAL(ABS(high%xij(1, :) - [-1.0_DFP, 1.0_DFP, 0.0_DFP])) &
          < tol, "End points first, then the midpoint")
END BLOCK

! Test 2: element data
BLOCK
  CALL OK(high%order == 2, "Order is 2")
  CALL OK(high%name == Line3, "Element type is Line3")
  CALL OK(ALL(high%entityCounts == [3, 1, 0, 0]), "Three nodes, one edge")
  CALL OK(TRIM(high%domainName) == "BIUNIT", "Domain name is inherited")
  CALL OK(high%nsd == 1, "nsd is inherited")
END BLOCK

! Test 3: topology has one point per node and one line
BLOCK
  CALL OK(SIZE(high%topology) == 4, "Four topology entries")
  CALL OK(ALL(high%topology(1:3)%name == Point1), "Nodes are points")
  CALL OK(ALL(high%topology(4)%nptrs == [1, 2, 3]), "Line holds all nodes")
END BLOCK

END PROGRAM main
