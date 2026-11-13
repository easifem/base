!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Copy(x, y) for allocatable integer arrays

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Copy
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: x(:)

ALLOCATE (x(10))
CALL Copy(x=x, y=[1_I4B, 2_I4B, 3_I4B])
CALL OK(SIZE(x) .EQ. 3, "Copy: x reallocated to SIZE(y)")
CALL OK(ALL(x .EQ. [1, 2, 3]), "Copy: values")

END PROGRAM main
