!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing GetEdgeConnectivity_Quadrangle and
! GetFaceConnectivity_Quadrangle for both options and a cubic element

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: option 1 (default) gives the hierarchical edge layout
BLOCK
  INTEGER(I4B) :: con(2, 4), nrow, ncol

  CALL GetEdgeConnectivity_Quadrangle(con=con, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 4, "opt=1: nrow and ncol")
  CALL OK(ALL(con .EQ. RESHAPE([1, 2, 4, 3, 1, 4, 2, 3], [2, 4])), &
          "opt=1: edges [1,2], [4,3], [1,4], [2,3]")
END BLOCK

! Test 2: option 2 gives the cyclic (Lagrange) edge layout
BLOCK
  INTEGER(I4B) :: con(2, 4)

  CALL GetEdgeConnectivity_Quadrangle(con=con, opt=2_I4B)
  CALL OK(ALL(con .EQ. RESHAPE([1, 2, 2, 3, 3, 4, 4, 1], [2, 4])), &
          "opt=2: edges [1,2], [2,3], [3,4], [4,1]")
END BLOCK

! Test 3: cubic element, interior edge nodes follow the vertices
BLOCK
  INTEGER(I4B) :: con(4, 4), nrow, ncol

  CALL GetEdgeConnectivity_Quadrangle(con=con, opt=2_I4B, order=3_I4B, &
                                      nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 4, "order=3: nrow and ncol")
  CALL OK(ALL(con(:, 1) .EQ. [1, 2, 5, 6]) .AND. &
          ALL(con(:, 4) .EQ. [4, 1, 11, 12]), &
          "order=3: first and last edge nodes")
END BLOCK

! Test 4: face connectivity ignores opt and uses the cyclic layout
BLOCK
  INTEGER(I4B) :: con(3, 4), nrow, ncol

  CALL GetFaceConnectivity_Quadrangle(con=con, opt=1_I4B, order=2_I4B, &
                                      nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 4, "face order=2: nrow and ncol")
  CALL OK(ALL(con(:, 2) .EQ. [2, 3, 6]), &
          "face order=2: second face is [2, 3, 6]")
END BLOCK

END PROGRAM main
