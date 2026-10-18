!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for integer matrices (rank-2 arrays)

PROGRAM main
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(2, 3), b(2, 3), c(2, 3)

a = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], &
            [2_I4B, 3_I4B])
b = 10_I4B * a

! option is present
c = Input(a, b)
CALL OK(ALL(c .EQ. b), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(c .EQ. a), "option is absent")

! the shape of the result is the shape of default
CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
        "shape of the result with option")

! array sections
CALL OK(ALL(Input(a(:, 1:2), b(:, 2:3)) .EQ. b(:, 2:3)), &
        "option is an array section")
CALL OK(ALL(Input(a(:, 2:3)) .EQ. a(:, 2:3)), &
        "default is an array section")

END PROGRAM main
