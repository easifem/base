!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitTriangle2Triangle_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromUnitTriangle2Triangle
USE MappingUtility, ONLY: FromUnitTriangle2Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 4), x1(2), x2(2), x3(2), ans(3, 10)
INTEGER(I4B) :: nrow, ncol

xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
               third, third], [2_I4B, 4_I4B])
x1 = [1.0_DFP, 1.0_DFP]
x2 = [4.0_DFP, 1.0_DFP]
x3 = [1.0_DFP, 3.0_DFP]

! ans can be larger, only ans(1:nrow, 1:ncol) is written
CALL FromUnitTriangle2Triangle_(xin, x1, x2, x3, ans, nrow, ncol)
CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
CALL OK(ncol .EQ. 4_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
                FromUnitTriangle2Triangle(xin, x1, x2, x3)) .LT. tol), &
        "same as FromUnitTriangle2Triangle")

END PROGRAM main
