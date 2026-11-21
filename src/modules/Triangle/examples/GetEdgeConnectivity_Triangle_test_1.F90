!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetEdgeConnectivity_Triangle and
! GetFaceConnectivity_Triangle for quadratic and cubic triangles

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: quadratic triangle, opt=2 (edges 1-2, 2-3, 3-1)
BLOCK
  INTEGER(I4B) :: con(3, 3), nrow, ncol

  CALL GetEdgeConnectivity_Triangle(con=con, opt=2_I4B, order=2_I4B, &
                                    nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 3, "order 2: size of con")
  CALL OK(ALL(con(:, 1) .EQ. [1, 2, 4]), "order 2, opt 2: edge 1")
  CALL OK(ALL(con(:, 2) .EQ. [2, 3, 5]), "order 2, opt 2: edge 2")
  CALL OK(ALL(con(:, 3) .EQ. [3, 1, 6]), "order 2, opt 2: edge 3")
END BLOCK

! Test 2: cubic triangle, opt=2
! nodes 4,5 are on edge 1-2; 6,7 on edge 2-3; 8,9 on edge 3-1
BLOCK
  INTEGER(I4B) :: con(4, 3), nrow, ncol

  CALL GetEdgeConnectivity_Triangle(con=con, opt=2_I4B, order=3_I4B, &
                                    nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 4 .AND. ncol .EQ. 3, "order 3: size of con")
  CALL OK(ALL(con(:, 1) .EQ. [1, 2, 4, 5]), "order 3, opt 2: edge 1")
  CALL OK(ALL(con(:, 2) .EQ. [2, 3, 6, 7]), "order 3, opt 2: edge 2")
  CALL OK(ALL(con(:, 3) .EQ. [3, 1, 8, 9]), "order 3, opt 2: edge 3")
END BLOCK

! Test 3: cubic triangle, opt=1 (edges 1-2, 1-3, 2-3)
! edge 1-2 has nodes 4,5; edge 2-3 has 6,7; edge 1-3 has 8,9
BLOCK
  INTEGER(I4B) :: con(4, 3)

  CALL GetEdgeConnectivity_Triangle(con=con, opt=1_I4B, order=3_I4B)
  CALL OK(ALL(con(:, 1) .EQ. [1, 2, 4, 5]), "order 3, opt 1: edge 1")
  CALL OK(ALL(con(:, 2) .EQ. [1, 3, 8, 9]), "order 3, opt 1: edge 2")
  CALL OK(ALL(con(:, 3) .EQ. [2, 3, 6, 7]), "order 3, opt 1: edge 3")
END BLOCK

! Test 4: face connectivity is the opt=2 edge connectivity
BLOCK
  INTEGER(I4B) :: con(4, 3), face(4, 3)

  CALL GetEdgeConnectivity_Triangle(con=con, opt=2_I4B, order=3_I4B)
  CALL GetFaceConnectivity_Triangle(con=face, opt=1_I4B, order=3_I4B)
  CALL OK(ALL(con .EQ. face), "face connectivity uses opt=2")
END BLOCK

END PROGRAM main
