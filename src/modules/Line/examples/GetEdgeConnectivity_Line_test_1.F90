!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Edge connectivity of a line element.

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: one edge from node 1 to node 2
BLOCK
  INTEGER(I4B) :: con(2, 3), nrow, ncol
  con = 0
  CALL GetEdgeConnectivity_Line(con=con, nrow=nrow, ncol=ncol)
  CALL OK(nrow == 1 .AND. ncol == 2, "One row and two columns")
  CALL OK(ALL(con(1, 1:2) == [1, 2]), "Edge joins nodes 1 and 2")
  CALL OK(ALL(con(2, :) == 0) .AND. con(1, 3) == 0, &
          "Rest of con is unchanged")
END BLOCK

! Test 2: the order does not change the result
BLOCK
  INTEGER(I4B) :: con(1, 2)
  CALL GetEdgeConnectivity_Line(con=con, order=3_I4B)
  CALL OK(ALL(con(1, :) == [1, 2]), "Order 3 gives the same edge")
END BLOCK

END PROGRAM main
