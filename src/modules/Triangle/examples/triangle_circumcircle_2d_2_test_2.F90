!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing triangle_circumcircle_2d_2 for a degenerate triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE Triangle_Method, ONLY: triangle_circumcircle_2d
USE Triangle_Method, ONLY: triangle_circumcircle_2d_2
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: collinear vertices have no circumcircle, r is -1
BLOCK
  REAL(DFP) :: t(2, 3), r, pc(2)

  t(:, 1) = [0.0_DFP, 0.0_DFP]
  t(:, 2) = [1.0_DFP, 0.0_DFP]
  t(:, 3) = [2.0_DFP, 0.0_DFP]
  CALL triangle_circumcircle_2d_2(t, r, pc)
  CALL OK(r .EQ. -1.0_DFP, "collinear: r is -1")
  CALL OK(ALL(pc .EQ. 0.0_DFP), "collinear: center is zero")
END BLOCK

! Test 2: both circumcircle routines agree for a general triangle
BLOCK
  REAL(DFP) :: t(2, 3), r1, r2, pc1(2), pc2(2)

  t(:, 1) = [0.0_DFP, 0.0_DFP]
  t(:, 2) = [3.0_DFP, 0.5_DFP]
  t(:, 3) = [1.0_DFP, 2.0_DFP]
  CALL triangle_circumcircle_2d(t, r1, pc1)
  CALL triangle_circumcircle_2d_2(t, r2, pc2)
  CALL OK(ABS(r1 - r2) .LT. tol .AND. MAXVAL(ABS(pc1 - pc2)) .LT. tol, &
          "general: both routines agree")
END BLOCK

END PROGRAM main
