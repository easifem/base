!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Convert(from, to) of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: Convert
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj
INTEGER(I4B), ALLOCATABLE :: val(:)

obj = [5_I4B, 6_I4B]
CALL Convert(from=obj, to=val)
CALL OK(ALL(val .EQ. [5, 6]), "Convert: values")

END PROGRAM main
