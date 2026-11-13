!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3), b(3), c(3)

a = [1_I4B, 2_I4B, 3_I4B]
b = [4_I4B, 5_I4B, 6_I4B]

! option is present
c = Input(a, b)
CALL OK(ALL(c .EQ. b), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(c .EQ. a), "option is absent")

! the size of the result is the size of default
CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")

! array sections
CALL OK(ALL(Input(a(1:2), b(2:3)) .EQ. [5_I4B, 6_I4B]), &
        "option is an array section")
CALL OK(ALL(Input(a(2:3)) .EQ. [2_I4B, 3_I4B]), &
        "default is an array section")

END PROGRAM main
