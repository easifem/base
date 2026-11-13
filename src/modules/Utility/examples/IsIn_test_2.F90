!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: a .isin. b for an integer scalar and a vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: OPERATOR(.isin.)
USE IntegerUtility, ONLY: OPERATOR(.in.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: b(4)

b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]

! for a scalar, .isin. returns a scalar logical
CALL OK(12_I4B .isin. b, "12 is in b")
CALL OK(.NOT. (13_I4B .isin. b), "13 is not in b")

! for a scalar, .isin. is the same as .in.
CALL OK((10_I4B .isin. b) .EQV. (10_I4B .in. b), "same as .in.")

END PROGRAM main
