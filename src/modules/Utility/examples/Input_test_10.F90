!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for logical matrices (rank-2 arrays)

PROGRAM main
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT) :: a(2, 3), b(2, 3), c(2, 3)

a = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE., .TRUE., .FALSE.], &
            [2_I4B, 3_I4B])
b = .NOT. a

! option is present
c = Input(a, b)
CALL OK(ALL(c .EQV. b), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(c .EQV. a), "option is absent")

! the shape of the result is the shape of default
CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
        "shape of the result with option")

! array sections
CALL OK(ALL(Input(a(:, 1:2), b(:, 2:3)) .EQV. b(:, 2:3)), &
        "option is an array section")

END PROGRAM main
