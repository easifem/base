!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Face connectivity of a line element; the faces are the two end
! points.

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: face j is local node j
BLOCK
  INTEGER(I4B) :: con(1, 2), nrow, ncol
  CALL GetFaceConnectivity_Line(con=con, nrow=nrow, ncol=ncol)
  CALL OK(nrow == 1 .AND. ncol == 2, "One row and two columns")
  CALL OK(con(1, 1) == 1, "Face 1 is node 1")
  CALL OK(con(1, 2) == 2, "Face 2 is node 2")
END BLOCK

! Test 2: the order does not change the result
BLOCK
  INTEGER(I4B) :: con(1, 2)
  CALL GetFaceConnectivity_Line(con=con, order=4_I4B)
  CALL OK(ALL(con(1, :) == [1, 2]), "Order 4 gives the same faces")
END BLOCK

END PROGRAM main
