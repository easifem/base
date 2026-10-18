!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ARTH returns a real arithmetic progression

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: ARTH
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ii
REAL(DFP) :: expected(20)

CALL OK(ALL(ABS(ARTH(0.0_DFP, 0.5_DFP, 4_I4B) &
                - [0.0_DFP, 0.5_DFP, 1.0_DFP, 1.5_DFP]) .LT. tol), &
        "ARTH(0, 0.5, 4)")
CALL OK(SIZE(ARTH(1.0_DFP, 1.0_DFP, 0_I4B)) .EQ. 0_I4B, "n = 0 is empty")

! more than 16 terms
expected = [(0.5_DFP + REAL(ii - 1, DFP) * 0.25_DFP, ii=1, 20)]
CALL OK(ALL(ABS(ARTH(0.5_DFP, 0.25_DFP, 20_I4B) - expected) .LT. tol), &
        "n = 20")

END PROGRAM main
