!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing that triangle_sample gives uniform samples

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE Triangle_Method, ONLY: triangle_sample
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 4000
REAL(DFP) :: t(2, 3), p(2, n)

! triangle (0,0), (2,0), (0,1)
t(:, 1) = [0.0_DFP, 0.0_DFP]
t(:, 2) = [2.0_DFP, 0.0_DFP]
t(:, 3) = [0.0_DFP, 1.0_DFP]

p = triangle_sample(t, n, seed=2026_I4B)

! Test 1: all samples are inside the triangle
BLOCK
  CALL OK(ALL(p .GE. 0.0_DFP) .AND. &
          ALL(p(1, :) / 2.0_DFP + p(2, :) .LE. 1.0_DFP), &
          "samples are inside")
END BLOCK

! Test 2: the mean of uniform samples is the centroid (2/3, 1/3)
BLOCK
  REAL(DFP) :: mean(2)

  mean = SUM(p, DIM=2) / REAL(n, DFP)
  CALL OK(ABS(mean(1) - 2.0_DFP / 3.0_DFP) .LT. 0.03_DFP .AND. &
          ABS(mean(2) - 1.0_DFP / 3.0_DFP) .LT. 0.02_DFP, &
          "mean of samples is near the centroid")
END BLOCK

! Test 3: a quarter of uniform samples fall in each of the four
! sub-triangles made by the edge midpoints; check the corner at vertex 1
BLOCK
  REAL(DFP) :: frac

  frac = COUNT(p(1, :) / 2.0_DFP + p(2, :) .LE. 0.5_DFP) / REAL(n, DFP)
  CALL OK(ABS(frac - 0.25_DFP) .LT. 0.03_DFP, &
          "a quarter of samples near vertex 1")
END BLOCK

END PROGRAM main
