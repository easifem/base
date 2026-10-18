!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ARTH returns an integer arithmetic progression

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: ARTH
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
INTEGER(I4B) :: expected(40)

CALL OK(ALL(ARTH(1_I4B, 2_I4B, 5_I4B) .EQ. [1, 3, 5, 7, 9]), &
        "ARTH(1, 2, 5)")
CALL OK(ALL(ARTH(10_I4B, -3_I4B, 4_I4B) .EQ. [10, 7, 4, 1]), &
        "negative increment")
CALL OK(ALL(ARTH(4_I4B, 0_I4B, 3_I4B) .EQ. [4, 4, 4]), "zero increment")
CALL OK(SIZE(ARTH(1_I4B, 1_I4B, 0_I4B)) .EQ. 0_I4B, "n = 0 is empty")

! more than 16 terms
expected = [(3_I4B + (ii - 1_I4B) * 5_I4B, ii=1, 40)]
CALL OK(ALL(ARTH(3_I4B, 5_I4B, 40_I4B) .EQ. expected), "n = 40")

END PROGRAM main
