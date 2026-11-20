!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetEdgeConnectivity_Tetrahedron for order 3

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: order 3 has two interior nodes on each edge
! Edge interior nodes are numbered after the 4 vertices, edge by edge.
BLOCK
  INTEGER(I4B) :: con(4, 6), nrow, ncol
  CALL GetEdgeConnectivity_Tetrahedron(con=con, order=3_I4B, nrow=nrow, &
                                       ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 6, "order 3: 4 rows and 6 edges")
  CALL OK(ALL(con(:, 1) .EQ. [1, 2, 5, 6]), "order 3: edge 1")
  CALL OK(ALL(con(:, 4) .EQ. [2, 3, 11, 12]), "order 3: edge 4")
  CALL OK(ALL(con(:, 6) .EQ. [3, 4, 15, 16]), "order 3: edge 6")
END BLOCK

END PROGRAM main
