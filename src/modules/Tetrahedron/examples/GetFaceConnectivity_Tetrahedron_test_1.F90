!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetFaceConnectivity_Tetrahedron for order 1 and 2

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: order omitted, vertex nodes of the four faces
BLOCK
  INTEGER(I4B) :: con(3, 4), nrow, ncol
  CALL GetFaceConnectivity_Tetrahedron(con=con, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 4, "order 1: 3 rows and 4 faces")
  CALL OK(ALL(con(:, 1) .EQ. [1, 3, 2]), "order 1: face 1 is (1,3,2)")
  CALL OK(ALL(con(:, 2) .EQ. [1, 2, 4]), "order 1: face 2 is (1,2,4)")
  CALL OK(ALL(con(:, 3) .EQ. [1, 4, 3]), "order 1: face 3 is (1,4,3)")
  CALL OK(ALL(con(:, 4) .EQ. [2, 3, 4]), "order 1: face 4 is (2,3,4)")
END BLOCK

! Test 2: order 2 adds the edge mid nodes of each face
! Edge mid nodes: 5=(1,2), 6=(1,3), 7=(1,4), 8=(2,3), 9=(2,4), 10=(3,4)
BLOCK
  INTEGER(I4B) :: con(6, 4), nrow, ncol
  CALL GetFaceConnectivity_Tetrahedron(con=con, order=2_I4B, nrow=nrow, &
                                       ncol=ncol)
  CALL OK(nrow .EQ. 6 .AND. ncol .EQ. 4, "order 2: 6 rows and 4 faces")
  CALL OK(ALL(con(:, 1) .EQ. [1, 3, 2, 6, 8, 5]), "order 2: face 1")
  CALL OK(ALL(con(:, 2) .EQ. [1, 2, 4, 5, 9, 7]), "order 2: face 2")
  CALL OK(ALL(con(:, 3) .EQ. [1, 4, 3, 7, 10, 6]), "order 2: face 3")
  CALL OK(ALL(con(:, 4) .EQ. [2, 3, 4, 8, 10, 9]), "order 2: face 4")
END BLOCK

END PROGRAM main
