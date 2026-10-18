!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Ones for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE OnesUtility, ONLY: Ones
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:)

a = Ones(4_I4B, 1_I4B)
CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")

! only the kind of datatype matters, not its value
a = Ones(3_I4B, 7_I4B)
CALL OK(ALL(a .EQ. 1_I4B), "value of datatype is ignored")
CALL OK(KIND(Ones(3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")

! zero size
a = Ones(0_I4B, 1_I4B)
CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")

END PROGRAM main
