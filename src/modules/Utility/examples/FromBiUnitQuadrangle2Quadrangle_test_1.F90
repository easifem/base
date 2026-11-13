!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2Quadrangle maps [-1,1]^2 to a quadrangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 6), x1(2), x2(2), x3(2), x4(2), ans(2, 6)

! vertices, centre and mid point of edge 2-3 of the biunit quadrangle
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
               -1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
              [2_I4B, 6_I4B])

! a general quadrangle, vertices in counter clockwise order
x1 = [0.0_DFP, 0.0_DFP]
x2 = [4.0_DFP, 0.0_DFP]
x3 = [4.0_DFP, 2.0_DFP]
x4 = [0.0_DFP, 3.0_DFP]

ans = FromBiUnitQuadrangle2Quadrangle(xin, x1, x2, x3, x4)
CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "(-1,-1) goes to x1")
CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "(1,-1) goes to x2")
CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "(1,1) goes to x3")
CALL OK(ALL(ABS(ans(:, 4) - x4) .LT. tol), "(-1,1) goes to x4")
CALL OK(ALL(ABS(ans(:, 5) - [2.0_DFP, 1.25_DFP]) .LT. tol), &
        "centre goes to the average of the vertices")
CALL OK(ALL(ABS(ans(:, 6) - [4.0_DFP, 1.0_DFP]) .LT. tol), &
        "edge mid point goes to edge mid point")

END PROGRAM main
