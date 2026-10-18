!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetIntersection of two integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetIntersection
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: c(5)
INTEGER(I4B) :: tsize

! a is smaller: c follows the order of a
CALL GetIntersection([1_I4B, 2_I4B, 3_I4B, 4_I4B], &
                     [7_I4B, 4_I4B, 5_I4B, 6_I4B, 3_I4B], c, tsize)
CALL OK(tsize .EQ. 2_I4B, "size of the intersection")
CALL OK(ALL(c(1:tsize) .EQ. [3_I4B, 4_I4B]), "intersection")

! b is smaller: c follows the order of b
CALL GetIntersection(a=[5_I4B, 1_I4B, 3_I4B, 9_I4B, 7_I4B], &
                     b=[3_I4B, 1_I4B], c=c, tsize=tsize)
CALL OK(tsize .EQ. 2_I4B, "size of the intersection (b smaller)")
CALL OK(ALL(c(1:tsize) .EQ. [3_I4B, 1_I4B]), "intersection (b smaller)")

! no common entry
CALL GetIntersection([1_I4B, 2_I4B], [3_I4B, 4_I4B], c, tsize)
CALL OK(tsize .EQ. 0_I4B, "empty intersection")

END PROGRAM main
