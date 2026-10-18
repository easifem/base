!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: a .in. b for an integer scalar and a vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: OPERATOR(.in.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: b(4)

b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]

CALL OK(12_I4B .in. b, "12 is in b")
CALL OK(9_I4B .in. b, "9 is in b")
CALL OK(.NOT. (13_I4B .in. b), "13 is not in b")
CALL OK(.NOT. (-9_I4B .in. b), "-9 is not in b")

END PROGRAM main
