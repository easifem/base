!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing triangle_diameter_2d for acute and obtuse triangles

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE Triangle_Method, ONLY: triangle_diameter_2d
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: equilateral triangle with side 1, diameter is 2R = 2/SQRT(3)
BLOCK
  REAL(DFP) :: t(2, 3)

  t(:, 1) = [0.0_DFP, 0.0_DFP]
  t(:, 2) = [1.0_DFP, 0.0_DFP]
  t(:, 3) = [0.5_DFP, SQRT(3.0_DFP) / 2.0_DFP]
  CALL OK(ABS(triangle_diameter_2d(t) - 2.0_DFP / SQRT(3.0_DFP)) .LT. tol, &
          "equilateral: diameter of the circumcircle")
END BLOCK

! Test 2: obtuse triangle, diameter is the longest side
BLOCK
  REAL(DFP) :: t(2, 3)

  t(:, 1) = [0.0_DFP, 0.0_DFP]
  t(:, 2) = [4.0_DFP, 0.0_DFP]
  t(:, 3) = [1.0_DFP, 1.0_DFP]
  CALL OK(ABS(triangle_diameter_2d(t) - 4.0_DFP) .LT. tol, &
          "obtuse: diameter is the longest side")
END BLOCK

! Test 3: the diameter does not depend on the vertex order
BLOCK
  REAL(DFP) :: t(2, 3)

  t(:, 1) = [0.0_DFP, 1.0_DFP]
  t(:, 2) = [0.0_DFP, 0.0_DFP]
  t(:, 3) = [2.0_DFP, 0.0_DFP]
  CALL OK(ABS(triangle_diameter_2d(t) - SQRT(5.0_DFP)) .LT. tol, &
          "right triangle, other order: hypotenuse")
END BLOCK

END PROGRAM main
