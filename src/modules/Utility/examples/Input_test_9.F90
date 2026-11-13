!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for logical vectors

PROGRAM main
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT) :: a(3), b(3), c(3)

a = [.TRUE., .FALSE., .TRUE.]
b = .NOT. a

! option is present
c = Input(a, b)
CALL OK(ALL(c .EQV. b), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(c .EQV. a), "option is absent")

! the size of the result is the size of default
CALL OK(SIZE(Input(a)) .EQ. 3_I4B, "size of the result")
CALL OK(SIZE(Input(a, b)) .EQ. 3_I4B, "size of the result with option")

! array sections
CALL OK(ALL(Input(a(1:2), b(2:3)) .EQV. b(2:3)), &
        "option is an array section")

END PROGRAM main
