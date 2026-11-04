!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing GetTotalDimension of IntVector_

PROGRAM main
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: GetTotalDimension
USE IntVector_Method, ONLY: Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(IntVector_) :: obj

CALL Initiate(obj=obj, tSize=4_I4B)
CALL OK(GetTotalDimension(obj) .EQ. 1, "GetTotalDimension: rank 1")

END PROGRAM main
