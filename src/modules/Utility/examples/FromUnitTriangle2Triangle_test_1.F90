!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitTriangle2Triangle maps the unit triangle to a triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitTriangle2Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 4), x1(2), x2(2), x3(2), ans(2, 4), ans3(3, 4)

! vertices and centroid of the unit triangle
xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
               third, third], [2_I4B, 4_I4B])

! a triangle in 2D
x1 = [1.0_DFP, 1.0_DFP]
x2 = [4.0_DFP, 1.0_DFP]
x3 = [1.0_DFP, 3.0_DFP]

ans = FromUnitTriangle2Triangle(xin, x1, x2, x3)
CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "(0,0) goes to x1")
CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "(1,0) goes to x2")
CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "(0,1) goes to x3")
CALL OK(ALL(ABS(ans(:, 4) - (x1 + x2 + x3) / 3.0_DFP) .LT. tol), &
        "centroid goes to centroid")

! a triangle in 3D, the number of rows of ans is SIZE(x1)
ans3 = FromUnitTriangle2Triangle(xin, [0.0_DFP, 0.0_DFP, 1.0_DFP], &
                                 [1.0_DFP, 0.0_DFP, 1.0_DFP], &
                                 [0.0_DFP, 1.0_DFP, 1.0_DFP])
CALL OK(ALL(ABS(ans3(:, 4) - [third, third, 1.0_DFP]) .LT. tol), &
        "centroid in 3D")

END PROGRAM main
