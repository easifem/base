!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromTriangle2Square_ maps a reference triangle to the square

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromTriangle2Square_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xb(2, 3), xu(2, 3), ans(2, 3), expected(2, 3)

! vertex 1, vertex 2 and centroid of the biunit triangle
xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third], &
             [2_I4B, 3_I4B])
! vertex 1, vertex 2 and centroid of the unit triangle
xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, third, third], &
             [2_I4B, 3_I4B])
expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                    0.0_DFP, -third], [2_I4B, 3_I4B])

! biunit triangle to biunit square
CALL FromTriangle2Square_(xin=xb, ans=ans, from="B", to="B")
CALL OK(ALL(ABS(ans - expected) .LT. tol), "BIUNIT triangle to square")

! unit triangle to biunit square
CALL FromTriangle2Square_(xin=xu, ans=ans, from="UNIT", to="BIUNIT")
CALL OK(ALL(ABS(ans - expected) .LT. tol), "UNIT triangle to square")

END PROGRAM main
